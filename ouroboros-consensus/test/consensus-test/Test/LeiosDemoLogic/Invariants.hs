{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Sequence-level invariant tests for the Leios fetch state.
--
-- The sibling "Test.LeiosDemoLogic" checks that the /pure/ decision function
-- makes the right choice at a single instant. This module instead drives the
-- /real, effectful/ handlers ('msgLeiosBlock', 'msgLeiosBlockTxs',
-- 'recordAnnouncedEb', 'leiosFetchLogicIteration') over sequences of interleaved
-- message arrivals and decisions, in 'IOSim' against an in-memory 'LeiosDb', a
-- 'nullLeiosTxCache', and plain 'MVar's — then asserts that a state invariant
-- holds after every step.
--
-- NOTE. This test suite initially exists as a specific regression tests: every
-- tx tracked in 'Leios.missingEbTxs' must still be resolvable in
-- 'Leios.reverseEbIndexByTx' (same keying 'leiosFetchLogicIteration' relies
-- on). We check it structurally after each command, and — belt and suspenders —
-- force a 'Decide' through the real fetch logic so a stray @impossible!@
-- surfaces even if the structural check missed a shape.
--
-- NOTE. Offers are deliberately not a command, for now. A 'MsgLeiosBlockOffer'
-- does two things: record the EB as missing (covered here by 'Announce') and
-- populate the per-peer offerings map. It's simply not required for the current
-- scope of these tests.
module Test.LeiosDemoLogic.Invariants (tests) where

import Control.Concurrent.Class.MonadMVar
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , readMVar
  )
import Control.Monad.Class.MonadThrow (SomeException, try)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word64)
import LeiosDemoDb (withLeiosDb)
import qualified LeiosDemoDb as LeiosDb
import LeiosDemoLogic
  ( LeiosFetchDecisions (..)
  , leiosFetchLogicIteration
  , msgLeiosBlock
  , msgLeiosBlockTxs
  , recordAnnouncedEb
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEb (..)
  , LeiosOutstanding (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , PeerId (..)
  , TxHash
  , demoLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , hashLeiosEb
  , hashLeiosTx
  , leiosEbBytesSize
  )
import qualified LeiosDemoTypes as Leios
import LeiosTxCache (LeiosTxCache, nullLeiosTxCache)
import Ouroboros.Consensus.Util.IOLike (evaluate)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "LeiosDemoLogic.Invariants"
    [ testGroup
        "curated sequences"
        [ testCase "same EB hash at two slots: delivery clears both" $
            runCmds reproMultiSlot @?= Right ()
        , testCase "tx shared across two EBs: delivery discharges both" $
            case runCmds' reproSharedTx of
              Left msg -> assertFailure msg
              Right o ->
                assertBool
                  "shared tx still tracked as missing after delivery"
                  (not (txStillMissing (txHashOf 1) o))
        ]
    , testProperty
        "missingEbTxs stays in sync with reverseEbIndexByTx across arbitrary sequences"
        prop_invariants
    ]

------------------------------------------------------------
-- Commands
------------------------------------------------------------

-- | A test EB is a list of (globally distinct) tx ids; the same list means the
-- same 'LeiosEb', hence the same 'EbHash' — so the same EB announced at two
-- slots is genuinely one hash at two 'LeiosPoint's (the crash arming).
type TestEb = [Int]

data Cmd
  = -- | @recordAnnouncedEb@: announce/offer this EB at this slot.
    Announce TestEb Word
  | -- | @msgLeiosBlock@: the EB body arrives for that point.
    ArriveBody TestEb Word
  | -- | @msgLeiosBlockTxs@: deliver the tx at this /index within the EB/.
    ArriveTx TestEb Word Int
  | -- | @leiosFetchLogicIteration@ at this current slot.
    Decide Word
  deriving (Eq, Show)

------------------------------------------------------------
-- Self-consistent EB/tx construction
--
-- The handlers validate @hashLeiosEb eb == ebHash@ and @hashLeiosTx tx ==
-- txHash@, so we derive hashes from the bytes rather than inventing them.
------------------------------------------------------------

txBytesOf :: Int -> BS.ByteString
txBytesOf i = BS.pack (fromIntegral (i + 1) : replicate 31 0)

leiosTxOf :: Int -> LeiosTx
leiosTxOf = MkLeiosTx . txBytesOf

txHashOf :: Int -> TxHash
txHashOf = hashLeiosTx . leiosTxOf

txSizeOf :: Int -> BytesSize
txSizeOf = fromIntegral . BS.length . txBytesOf

ebOf :: TestEb -> LeiosEb
ebOf ids = MkLeiosEb (V.fromList [(txHashOf i, txSizeOf i) | i <- ids])

pointOf :: TestEb -> Word -> LeiosPoint
pointOf ids slot = MkLeiosPoint (fromIntegral slot) (hashLeiosEb (ebOf ids))

------------------------------------------------------------
-- Harness
------------------------------------------------------------

-- | Run a command sequence in 'IOSim' against in-memory dependencies, checking
-- the sync invariant after each command. 'Left' names the first failing
-- command; 'Right' returns the final outstanding state (for extra assertions).
runCmds' :: [Cmd] -> Either String (LeiosOutstanding Int)
runCmds' cmds = runSimOrThrow (go cmds)
 where
  go :: forall s. [Cmd] -> IOSim s (Either String (LeiosOutstanding Int))
  go cs0 = do
    dbHandle <- LeiosDb.newLeiosDBInMemory
    withLeiosDb dbHandle $ \conn -> do
      outstandingVar <- newMVar emptyLeiosOutstanding
      readyVar <- newEmptyMVar
      let kv = (outstandingVar, readyVar)
          txCache = nullLeiosTxCache
          peerId = MkPeerId (0 :: Int)
          loop [] = Right <$> readMVar outstandingVar
          loop (c : cs) = do
            r <-
              try (applyCmd conn txCache kv peerId c)
                :: IOSim s (Either SomeException ())
            case r of
              Left e -> pure (Left ("exception on " <> show c <> ": " <> show e))
              Right () -> do
                outstanding <- readMVar outstandingVar
                case checkInvariant outstanding of
                  Left msg -> pure (Left (msg <> " (after " <> show c <> ")"))
                  Right () -> loop cs
      loop cs0

-- | As 'runCmds'', but discarding the final state.
runCmds :: [Cmd] -> Either String ()
runCmds = fmap (const ()) . runCmds'

applyCmd ::
  forall s.
  LeiosDb.LeiosDbConnection (IOSim s) ->
  LeiosTxCache (IOSim s) () () Leios.SerializedEbBody ->
  (MVar (IOSim s) (LeiosOutstanding Int), MVar (IOSim s) ()) ->
  PeerId Int ->
  Cmd ->
  IOSim s ()
applyCmd conn txCache kv peerId = \case
  Announce ids slot ->
    recordAnnouncedEb txCache kv (pointOf ids slot, leiosEbBytesSize (ebOf ids))
  ArriveBody ids slot -> do
    let eb = ebOf ids
        req = MkLeiosBlockRequest (pointOf ids slot) (leiosEbBytesSize eb)
    msgLeiosBlock nullTracer nullTracer kv txCache conn peerId req eb
  ArriveTx ids slot idx -> do
    let txId = ids !! idx
        req =
          MkLeiosBlockTxsRequest
            (pointOf ids slot)
            (offsetsToBitmaps [idx])
            (V.singleton (txHashOf txId))
    msgLeiosBlockTxs nullTracer nullTracer kv txCache conn peerId req (V.singleton (leiosTxOf txId))
  Decide slot -> do
    outstanding <- readMVar (fst kv)
    let ebs = referencedEbs outstanding
        offerings = Map.singleton peerId (ebs, ebs)
        (out', decs) =
          leiosFetchLogicIteration demoLeiosFetchStaticEnv (Just (fromIntegral slot)) offerings outstanding
    -- Force the fetch logic so any 'impossible!' surfaces (caught by 'go').
    -- Forcing @out'@ to WHNF drives 'go1' to completion (its reverse lookups);
    -- 'forceDecisions' additionally forces the per-request offset lookups.
    _ <- evaluate out'
    _ <- evaluate (forceDecisions decs)
    modifyMVar_ (fst kv) (\_ -> pure out')

-- | Every EbHash currently referenced by the outstanding state (bodies + txs),
-- as an all-offering peer's body\/closure sets.
referencedEbs :: LeiosOutstanding Int -> Set.Set EbHash
referencedEbs o =
  Set.fromList $
    map (.pointEbHash) (Map.keys (Leios.missingEbBodies o))
      <> map (.pointEbHash) (Map.keys (Leios.missingEbTxs o))

-- | Force the decision structure, including each tx request's resolved offset
-- (the @goTx2@ lookup), to a scalar.
forceDecisions :: LeiosFetchDecisions pid -> Int
forceDecisions (MkLeiosFetchDecisions m) =
  sum
    [ offset + fromIntegral sz
    | slotMap <- Map.elems m
    , (txs, _ebs) <- Map.elems slotMap
    , (_txHash, sz, _ebHash, offset) <- DList.toList txs
    ]

------------------------------------------------------------
-- The invariant
------------------------------------------------------------

-- | Every tx tracked as missing must be resolvable in the reverse index at the
-- exact (EbHash, slot, offset) 'go1'/'goTx2' will look it up by. Its violation
-- is what @impossible! leiosFetchLogicIteration go1@ reports.
checkInvariant :: LeiosOutstanding Int -> Either String ()
checkInvariant o =
  case
    [ msg
    | (p, txs) <- Map.toList (Leios.missingEbTxs o)
    , (off, (txHash, _sz)) <- IntMap.toList txs
    , Left msg <- [resolvable p off txHash]
    ] of
    [] -> Right ()
    (msg : _) -> Left msg
 where
  rev = Leios.reverseEbIndexByTx o
  resolvable p off txHash =
    case Map.lookup txHash rev of
      Nothing ->
        Left ("missingEbTxs tx absent from reverseEbIndexByTx: " <> show (p.pointSlotNo, off))
      Just ebm -> case Map.lookup (pointEbHash p) ebm of
        Nothing -> Left ("reverseEbIndexByTx lacks this EB for a missing tx: " <> show p.pointSlotNo)
        Just (off', _sz')
          | off' == off -> Right ()
          | otherwise -> Left ("reverseEbIndexByTx offset mismatch at " <> show p.pointSlotNo)

-- | Is this tx still tracked as missing for any point? After a tx is delivered
-- the deduping LeiosDb should discharge it for every EB that referenced it, so
-- this is 'False' for a delivered tx.
txStillMissing :: TxHash -> LeiosOutstanding Int -> Bool
txStillMissing txHash o =
  any (elem txHash . map fst . IntMap.elems) (Map.elems (Leios.missingEbTxs o))

------------------------------------------------------------
-- Curated repros
------------------------------------------------------------

-- | The same EB (hash) bodied at two slots; delivering its tx for one slot must
-- clear it for the other too. Pre-fix, the second 'Decide' hits @impossible!@.
reproMultiSlot :: [Cmd]
reproMultiSlot =
  [ ArriveBody [0] 10
  , ArriveBody [0] 11
  , Decide 11
  , ArriveTx [0] 10 0
  , Decide 11
  ]

-- | A tx shared by two distinct EBs; delivering it via one must discharge it
-- for the other (the deduping-LeiosDb behaviour the fix relies on).
reproSharedTx :: [Cmd]
reproSharedTx =
  [ ArriveBody [0, 1] 10
  , ArriveBody [1, 2] 11
  , Decide 12
  , ArriveTx [0, 1] 10 1 -- deliver the shared tx (id 1)
  , Decide 12
  ]

------------------------------------------------------------
-- Property
------------------------------------------------------------

worldEbs :: [TestEb]
worldEbs = [[0, 1], [1, 2], [0], [2, 3, 4]]

worldSlots :: [Word]
worldSlots = [10, 11, 12]

genCmd :: Gen Cmd
genCmd = do
  ids <- elements worldEbs
  slot <- elements worldSlots
  oneof
    [ pure (Announce ids slot)
    , pure (ArriveBody ids slot)
    , ArriveTx ids slot <$> choose (0, length ids - 1)
    , Decide <$> elements worldSlots
    ]

prop_invariants :: Property
prop_invariants =
  forAllShrink (listOf genCmd) (shrinkList (const [])) $ \cmds ->
    runCmds cmds === Right ()

offsetsToBitmaps :: [Int] -> [(Word16, Word64)]
offsetsToBitmaps offs =
  [ (fromIntegral q, bm)
  | (q, bm) <-
      IntMap.toAscList $
        foldr
          (\o -> let (q, r) = o `divMod` 64 in IntMap.insertWith (Bits..|.) q (Bits.bit (63 - r)))
          IntMap.empty
          offs
  ]
