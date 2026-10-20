{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'inlineCertifiedEbsFollower': which blocks a node-to-client
-- ChainSync client gets when the chain has Leios blocks. Also tests how rule 3
-- reads the chain.
--
-- Each scenario test gives the server a list of chain changes, in batches. After each
-- batch, the client reads everything the server has for it. Several batches
-- act like a client that is up to date, getting new blocks as they arrive. One
-- batch acts like a client that is behind and catching up. A client that
-- reconnects first intersects at a point, then reads the batches.
module Test.Consensus.MiniProtocol.ChainSync.Server (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (void)
import Control.Monad.IOSim (Failure (..), IOSim, runSim, runSimOrThrow)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import Data.Maybe (isJust)
import LeiosDemoTypes (BytesSize, LeiosCert, LeiosEb, LeiosPoint, RbHash (..), TxHash)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( RbParentNotFound (..)
  , chainSyncBlockServerFollower
  , findParentPoint
  , headerOnChain
  , inlineCertifiedEbsFollower
  , inlineCertifiedEbsFollowerOnChainDB
  , parentOnChain
  )
import Ouroboros.Consensus.Storage.ChainDB.API
  ( BlockComponent (GetHeader, GetSlot)
  , Follower (..)
  , Predecessor (..)
  , WithPoint (..)
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDBArgs
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import qualified Ouroboros.Consensus.Storage.LeiosDB as LeiosDb
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (ChainUpdate (..), Serialised (..))
import Test.Cardano.Crypto.Leios.Gen (TestCommittee, genCommittee)
import Test.LeiosValidClaims (mkCert)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (MinimalChainDbArgs (..), emptyNodeDBs, fromMinimalChainDbArgs)
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "ChainSync.Server (inlining certified EBs)"
    [ readingGroup "reading without waiting" NoWait
    , readingGroup "reading and waiting, as a client at the tip does" Wait
    , readingGroup "reading as the server does, with blocks arriving while it waits" AsServer
    , testGroup "finding a block's parent" parentTests
    , testGroup "reading the chain from a ChainDB" chainDbTests
    , testGroup "end to end, on a ChainDB" endToEndTests
    , testGroup "a block that is gone" goneTests
    , testGroup "RB's parent not found" parentNotFoundTests
    ]
 where
  readingGroup name how =
    testGroup name $
      map (scenario how) scenarios
        <> [testGroup "reconnecting" (map (reconnection how) reconnections)]
  scenario how (name, batches, expected) =
    testCase name $ run how Nothing batches @?= expected
  reconnection how (name, at, batches, expected) =
    testCase name $ run how (Just at) batches @?= expected

-- | A name, the chain changes in batches, and what the client must get.
scenarios :: [(String, [[ChainUpdate Blk Blk]], [ChainUpdate Blk Blk])]
scenarios =
  [
    ( "a client catching up gets RB⁺ directly, with no rollback"
    , [[add b0, add rb, add certRb, add b3]]
    , [add b0, add rbPlus, add certRb, add b3]
    )
  ,
    ( "a client at the tip gets RB, then a rollback, RB⁺ and the CertRB"
    , [[add b0, add rb], [add certRb, add b3]]
    , [add b0, add rb, RollBack (blockPoint b0), add rbPlus, add certRb, add b3]
    )
  ,
    ( "RB is sent plain when its EB is not certified (catching up)"
    , [[add b0, add rb, add uncertified]]
    , [add b0, add rb, add uncertified]
    )
  ,
    ( "RB is sent plain when its EB is not certified (at the tip)"
    , [[add b0, add rb], [add uncertified]]
    , [add b0, add rb, add uncertified]
    )
  ,
    ( "RB is sent plain when a rollback comes next"
    , [[add b0, add rb, RollBack (blockPoint b0)]]
    , [add b0, add rb, RollBack (blockPoint b0)]
    )
  ,
    ( "a rollback that keeps RB goes one block deeper, and RB⁺ follows"
    , [[add b0, add rb], [RollBack (blockPoint rb), add certRb]]
    , [add b0, add rb, RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "a chain switch that keeps RB but drops the certificate replaces RB⁺ with RB"
    , [[add b0, add rb, add certRb], [RollBack (blockPoint rb), add uncertified]]
    , [add b0, add rbPlus, add certRb, RollBack (blockPoint b0), add rb, add uncertified]
    )
  ,
    ( "a chain switch that keeps RB and adds a certificate replaces RB with RB⁺"
    , [[add b0, add rb, add uncertified], [RollBack (blockPoint rb), add certRb]]
    , [add b0, add rb, add uncertified, RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "a chain switch onto a CertRB that announces an EB keeps RB⁺ below it"
    ,
      [ [add b0, add rb, add certRbAnnouncing, add certRb2]
      , [RollBack (blockPoint certRbAnnouncing), add uncertified2]
      ]
    ,
      [ add b0
      , add rbPlus
      , add certRbAnnouncingPlus
      , add certRb2
      , RollBack (blockPoint rb)
      , add certRbAnnouncing
      , add uncertified2
      ]
    )
  ,
    ( "a rollback past RB forgets it, so a later CertRB is sent as it is"
    , [[add b0, add rb], [RollBack (blockPoint b0), add certRbOnFork]]
    , [add b0, add rb, RollBack (blockPoint b0), add certRbOnFork]
    )
  ,
    ( "a CertRB that announces an EB of its own is its RB⁺ (catching up)"
    , [[add b0, add rb, add certRbAnnouncing, add certRb2]]
    , [add b0, add rbPlus, add certRbAnnouncingPlus, add certRb2]
    )
  ,
    ( "a CertRB that announces an EB of its own is its RB⁺ (at the tip)"
    , [[add b0, add rb], [add certRbAnnouncing], [add certRb2]]
    ,
      [ add b0
      , add rb
      , RollBack (blockPoint b0)
      , add rbPlus
      , add certRbAnnouncing
      , RollBack (blockPoint rb)
      , add certRbAnnouncingPlus
      , add certRb2
      ]
    )
  ]

-- | A name, where the client intersects, the chain changes after that in
-- batches, and what the client must get.
reconnections :: [(String, Point Blk, [[ChainUpdate Blk Blk]], [ChainUpdate Blk Blk])]
reconnections =
  [
    ( "at RB, with the CertRB already on the chain: RB⁺"
    , blockPoint rb
    , [[add certRb]]
    , [RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "at RB, at the tip: RB, then RB⁺ when the CertRB comes"
    , blockPoint rb
    , [[], [add certRb]]
    , [RollBack (blockPoint b0), add rb, RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "at RB, when its EB is not certified: plain RB"
    , blockPoint rb
    , [[add uncertified]]
    , [RollBack (blockPoint b0), add rb, add uncertified]
    )
  ,
    ( "at a block that announces nothing: no deeper rollback"
    , blockPoint b0
    , [[add rb, add certRb]]
    , [RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "at the first block, which announces an EB: back to genesis"
    , blockPoint genesisRb
    , [[add genesisCertRb]]
    , [RollBack GenesisPoint, add genesisRbPlus, add genesisCertRb]
    )
  ,
    ( "at a CertRB that announces an EB: back to its parent, which stays RB⁺"
    , blockPoint certRbAnnouncing
    , [[add certRb2]]
    , [RollBack (blockPoint rb), add certRbAnnouncingPlus, add certRb2]
    )
  ,
    ( "at genesis: no deeper rollback"
    , GenesisPoint
    , [[add b0, add rb, add certRb]]
    , [RollBack GenesisPoint, add b0, add rbPlus, add certRb]
    )
  ]

-- | 'findParentPoint' in the cases 'chainDbTests' does not reach.
parentTests :: [TestTree]
parentTests =
  [ testCase "with a lookup that finds by hash alone: the first try, at the slot it returns" $
      parentOf byHashAlone (getHeader b3) @?= (Just (blockPoint certRb), 1)
  , -- There is no lower bound on the search. See 'findParentPoint'.
    testCase "missing: Nothing, after trying every slot back to 0" $
      parentOf (const Nothing) (getHeader b3) @?= (Nothing, fromIntegral (unSlotNo (blockSlot b3)) + 1)
  ]
 where
  -- The result, and how many lookups it took.
  parentOf :: (RealPoint Blk -> Maybe SlotNo) -> Header Blk -> (Maybe (Point Blk), Int)
  parentOf slotOf hdr = runSimOrThrow $ countCalls (`findParentPoint` hdr) (pure . slotOf)

  byHashAlone rp = blockSlot <$> find ((== realPointHash rp) . blockHash) onChain

-- | Rule 3's reads as the server runs them: 'headerOnChain' and
-- 'parentOnChain' against a real ChainDB, over a mocked filesystem. The
-- number of slot lookups shows how the parent was found: none from the current
-- chain, and one per slot back to it from the ImmutableDB.
--
-- The chain is long enough that its first blocks become immutable, and these
-- are then removed from the VolatileDB, as the node's background tasks do. A
-- shorter fork, which the node does not select, stays in the VolatileDB.
chainDbTests :: [TestTree]
chainDbTests =
  [ testCase "the parent of a block in the current chain, with no lookup" $
      parentOf c6 @?= (Just (blockPoint c5), 0)
  , testCase "the parent of the first block after the immutable tip: the tip, with no lookup" $
      parentOf c5 @?= (Just (blockPoint c4), 0)
  , testCase "the parent of a block more than k blocks back, one lookup per slot back to it" $
      parentOf c2 @?= (Just (blockPoint c1), fromIntegral (unSlotNo (blockSlot c2 - blockSlot c1)) + 1)
  , testCase "the parent of the immutable tip, one lookup per slot back to it" $
      parentOf c4 @?= (Just (blockPoint c3), fromIntegral (unSlotNo (blockSlot c4 - blockSlot c3)) + 1)
  , testCase "the parent of the first block: genesis" $
      parentOf c1 @?= (Just GenesisPoint, 0)
  , testCase "the parent of a block on a fork: Nothing, with no search" $
      parentOf fork @?= (Nothing, 0)
  , testCase "the header of a block in the current chain" $
      headerOf c6 @?= (Just (getHeader c6), True)
  , testCase "the header of the immutable tip" $
      headerOf c4 @?= (Just (getHeader c4), True)
  , testCase "the header of a block in the ImmutableDB" $
      headerOf c2 @?= (Just (getHeader c2), True)
  , testCase "the header of a block on a fork: Nothing, though the ChainDB has it" $
      headerOf fork @?= (Nothing, True)
  ]
 where
  -- Gaps between the blocks, so an ImmutableDB lookup takes several tries.
  c1 = firstLeiosBlock 0
  c2 = atSlot (blockSlot c1 + 50) (successorLeiosBlock c1)
  c3 = successorLeiosBlock c2
  c4 = successorLeiosBlock c3
  c5 = atSlot (blockSlot c4 + 5) (successorLeiosBlock c4)
  c6 = successorLeiosBlock c5

  -- With k = 2, c1 to c4 become immutable, and c5 and c6 stay volatile.
  chain = [c1, c2, c3, c4, c5, c6]

  -- After c4, and shorter than the selected chain.
  fork = forkLeiosBlock c5

  -- The result, and how many slot lookups it took.
  parentOf :: Blk -> (Maybe (Point Blk), Int)
  parentOf blk = runSimOrThrow $ withChain $ \chainDB ->
    countCalls
      (\slotOf -> parentOnChain (ChainDB.getCurrentChain chainDB) slotOf (getHeader blk))
      (ChainDB.getBlockComponent chainDB GetSlot)

  -- The result, and whether the ChainDB has the block.
  headerOf :: Blk -> (Maybe (Header Blk), Bool)
  headerOf blk = runSimOrThrow $ withChain $ \chainDB -> do
    let readHeader = ChainDB.getBlockComponent chainDB GetHeader
    res <- headerOnChain (ChainDB.getCurrentChain chainDB) readHeader (blockRealPoint blk)
    (,) res . isJust <$> readHeader (blockRealPoint blk)

  withChain body = withChainDB $ \chainDB internal -> do
    addChain chainDB chain
    addBlock chainDB (Just c4) fork
    makeImmutable internal
    body chainDB

-- | Rule 3 as 'chainSyncBlocksServer' runs it, on a real ChainDB and follower.
-- The client reconnects at an RB more than k blocks back, so RB and its parent
-- come from the ImmutableDB. RB's EB is not certified, so RB is sent plain.
--
-- TODO: test RB⁺ on a ChainDB. A ChainDB only selects a CertRB once its EB's
-- closure is acquired.
endToEndTests :: [TestTree]
endToEndTests =
  [ testCase "a client reconnecting at an RB more than k blocks back gets it again" $
      reconnectAt e2 @?= [RollBack (blockPoint e1), add e2, add e3, add e4, add e5]
  , testCase "a client reconnecting at a block that announces nothing gets no deeper rollback" $
      reconnectAt e3 @?= [RollBack (blockPoint e3), add e4, add e5]
  ]
 where
  e1 = firstLeiosBlock 0
  -- RB, after a gap, so finding its parent takes several lookups.
  e2 = announcing (ebPoint (blockSlot e2') eb) ebSize e2'
   where
    e2' = atSlot (blockSlot e1 + 10) (successorLeiosBlock e1)
  e3 = successorLeiosBlock e2
  e4 = successorLeiosBlock e3
  e5 = successorLeiosBlock e4

  -- With k = 2, e1 to e3 become immutable.
  chain = [e1, e2, e3, e4, e5]

  -- Everything the client gets after it intersects at the block, until it is
  -- at the tip.
  reconnectAt :: Blk -> [ChainUpdate Blk Blk]
  reconnectAt blk = runSimOrThrow $ withChainDB $ \chainDB internal -> do
    addChain chainDB chain
    makeImmutable internal
    leiosDb <- LeiosDb.newLeiosDBInMemory
    LeiosDb.withReader leiosDb $ \reader -> withRegistry $ \registry -> do
      inner <- chainSyncBlockServerFollower chainDB registry
      outer <- inlineCertifiedEbsFollowerOnChainDB chainDB LeiosTestCodecConfig reader inner
      _ <- followerForward outer [blockPoint blk]
      readAll (followerInstruction outer)

-- | Add the blocks in order, each extending the one before.
addChain :: ChainDB.ChainDB (IOSim s) Blk -> [Blk] -> IOSim s ()
addChain chainDB chain = sequence_ $ zipWith (addBlock chainDB) (Nothing : map Just chain) chain

-- | Add a block after the given predecessor. Returns once the ChainDB has
-- processed it, so the selected chain is known.
addBlock :: ChainDB.ChainDB (IOSim s) Blk -> Maybe Blk -> Blk -> IOSim s ()
addBlock chainDB predecessor =
  ChainDB.addBlock_ chainDB Punishment.noPunishment $ case predecessor of
    Nothing -> NoPredecessor
    Just p -> Predecessor (blockSlot p) (LeiosTestView Nothing maxBound)

-- | Copy every block more than k back to the ImmutableDB, then remove those
-- from the VolatileDB, as the node's background tasks do.
makeImmutable :: ChainDBImpl.Internal (IOSim s) Blk -> IOSim s ()
makeImmutable internal =
  ChainDBImpl.intCopyToImmutableDB internal >>= \case
    NotOrigin s -> ChainDBImpl.intGarbageCollect internal (s + 1)
    Origin -> error "nothing became immutable"

-- | Open a ChainDB of 'LeiosTestBlock's, with k = 2, over a mocked filesystem.
-- Its background tasks are not started, so the test decides when blocks become
-- immutable.
withChainDB ::
  (ChainDB.ChainDB (IOSim s) Blk -> ChainDBImpl.Internal (IOSim s) Blk -> IOSim s a) ->
  IOSim s a
withChainDB body = withRegistry $ \registry -> do
  mcdbNodeDBs <- emptyNodeDBs
  mcdbLeiosDb <- LeiosDb.newLeiosDBInMemory
  let securityParam = SecurityParam $ knownNonZeroBounded @2
      eraParams = HardFork.defaultEraParams securityParam (slotLengthFromSec 1) HardFork.NoPerasEnabled
      ledgerConfig = leiosTestLedgerConfig eraParams 100 (const Nothing)
      mcdbTopLevelConfig = singleNodeLeiosTestConfig ledgerConfig securityParam
      mcdbChunkInfo = simpleChunkInfo (HardFork.eraEpochSize eraParams)
      mcdbInitLedger = leiosTestInitExtLedger ledgerConfig IntMap.empty
      mcdbRegistry = registry
      args = ChainDBArgs.updateTracer nullTracer $ fromMinimalChainDbArgs MinimalChainDbArgs{..}
  bracket (ChainDBImpl.openDBInternal args False) (ChainDB.closeDB . fst) $ \(chainDB, internal) -> do
    _ <- forkLinkedThread registry "AddBlockRunner" $ ChainDBImpl.intAddBlockRunner internal
    body chainDB internal

-- | A rollback point that has left the selected chain cannot be read. The
-- rollback is sent as it is, because the inner follower rolls back past it
-- next.
goneTests :: [TestTree]
goneTests =
  [ testCase "with its header gone, the rollback is sent as it is" $
      gone onChainLookups{lookupHeader = \_ -> Nothing}
  , testCase "with only its raw block gone, the rollback is sent as it is" $
      gone onChainLookups{lookupRawBlock = \_ -> Nothing}
  ]
 where
  gone lookups =
    runSimOrThrow (runWith lookups NoWait Nothing rollBackToRb)
      @?= [add b0, add rb, RollBack (blockPoint rb)]

-- | Rule 3 stops serving the client when it cannot find RB's parent.
parentNotFoundTests :: [TestTree]
parentNotFoundTests =
  [ testCase "throws RbParentNotFound" $
      case runSim (runWith onChainLookups{lookupParent = \_ -> Nothing} NoWait Nothing rollBackToRb) of
        Left (FailureException e)
          | Just (RbParentNotFound rp) <- fromException e -> rp @?= blockRealPoint rb
        res -> assertFailure ("expected RbParentNotFound, got " <> show (fmap (map (fmap blockPoint)) res))
  ]

-- | The client gets RB, then the chain rolls back to RB.
rollBackToRb :: [[ChainUpdate Blk Blk]]
rollBackToRb = [[add b0, add rb], [RollBack (blockPoint rb)]]

type Blk = LeiosTestBlock

add :: Blk -> ChainUpdate Blk Blk
add = AddBlock

{-------------------------------------------------------------------------------
  The chain
-------------------------------------------------------------------------------}

tx :: Int -> LeiosTestTx
tx k = LeiosTestTx{ltxAsserts = IntMap.empty, ltxWrites = IntMap.singleton k 'a'}

-- | An ordinary block with a transaction of its own.
b0 :: Blk
b0 = withTxs [tx 0] (firstLeiosBlock 0)

-- | RB: it has a transaction of its own, and it announces 'eb'.
rb :: Blk
rb = withTxs [tx 1] (announcing (ebPoint ebSlot eb) ebSize (successorLeiosBlock b0))
 where
  ebSlot = blockSlot (successorLeiosBlock b0)

-- | What RB⁺ must be: RB's own transaction, then the EB's.
rbPlus :: Blk
rbPlus = withTxs ([tx 1] <> ebTxs) rb

-- | The CertRB over 'rb'.
certRb :: Blk
certRb = certifying (certFor rb) (successorLeiosBlock rb)

-- | A block after 'rb' that does not certify its EB. Forked, so it does not
-- share a hash with 'certRb': rule 3 reads blocks by point.
uncertified :: Blk
uncertified = withTxs [tx 9] (forkLeiosBlock (successorLeiosBlock rb))

b3 :: Blk
b3 = successorLeiosBlock certRb

-- | A block after 'certRbAnnouncing' that does not certify its EB.
uncertified2 :: Blk
uncertified2 = withTxs [tx 8] (forkLeiosBlock (successorLeiosBlock certRbAnnouncing))

-- | A first block that announces 'eb', and its CertRB, for a rollback to
-- genesis.
genesisRb, genesisRbPlus, genesisCertRb :: Blk
genesisRb = announcing (ebPoint (blockSlot (firstLeiosBlock 1)) eb) ebSize (firstLeiosBlock 1)
genesisRbPlus = withTxs ebTxs genesisRb
genesisCertRb = certifying (certFor genesisRb) (successorLeiosBlock genesisRb)

atSlot :: SlotNo -> Blk -> Blk
atSlot s b = b{ltbHeader = (ltbHeader b){lthSlot = s}}

-- | Every block on a chain the node has, for rule 3 to read.
onChain :: [Blk]
onChain =
  [ b0
  , rb
  , certRb
  , uncertified
  , b3
  , certRbOnFork
  , certRbAnnouncing
  , certRb2
  , uncertified2
  , genesisRb
  , genesisCertRb
  ]

-- | A CertRB on a fork after 'b0', where there is no 'rb'.
certRbOnFork :: Blk
certRbOnFork = certifying (certFor b0) (forkLeiosBlock (successorLeiosBlock b0))

-- | A CertRB over 'rb' that also announces 'eb2'. Forked twice, so it does
-- not share a hash with 'certRb' or 'uncertified'.
certRbAnnouncing :: Blk
certRbAnnouncing =
  announcing (ebPoint ebSlot eb2) eb2Size $
    certifying (certFor rb) (forkLeiosBlock (forkLeiosBlock (successorLeiosBlock rb)))
 where
  ebSlot = blockSlot (successorLeiosBlock rb)

certRbAnnouncingPlus :: Blk
certRbAnnouncingPlus = withTxs eb2Txs certRbAnnouncing

-- | The CertRB over 'certRbAnnouncing'.
certRb2 :: Blk
certRb2 = certifying (certFor certRbAnnouncing) (successorLeiosBlock certRbAnnouncing)

{-------------------------------------------------------------------------------
  The endorser blocks
-------------------------------------------------------------------------------}

ebTxs, eb2Txs :: [LeiosTestTx]
ebTxs = [tx 2, tx 3]
eb2Txs = [tx 4]

eb, eb2 :: LeiosEb
ebSize, eb2Size :: BytesSize
ebClosure, eb2Closure :: [(TxHash, BS.ByteString)]
(eb, ebClosure, ebSize) = mkLeiosTestEb ebTxs
(eb2, eb2Closure, eb2Size) = mkLeiosTestEb eb2Txs

ebPoint :: SlotNo -> LeiosEb -> LeiosPoint
ebPoint = leiosTestEbPoint

{-------------------------------------------------------------------------------
  Certificates
-------------------------------------------------------------------------------}

-- | The server does not check certificates, so any will do.
certFor :: Blk -> LeiosCert
certFor announcer = mkCert testCommittee claim
 where
  claim :: RbHash
  claim = MkRbHash $ toRawHash (Proxy @Blk) (blockHash announcer)

testCommittee :: TestCommittee
testCommittee = unGen genCommittee (mkQCGen 42) 10

{-------------------------------------------------------------------------------
  Running the server
-------------------------------------------------------------------------------}

-- | How the client reads.
data Reading
  = -- | Each batch is added, then the client reads it with
    -- 'followerInstruction'.
    NoWait
  | -- | Each batch is added, then the client reads it with
    -- 'followerInstructionBlocking'.
    Wait
  | -- | As the ChainSync server does: 'followerInstruction', and when that has
    -- nothing, 'followerInstructionBlocking'. Each batch arrives while the
    -- client waits.
    AsServer

-- | How rule 3 reads the chain.
data Lookups = Lookups
  { lookupHeader :: RealPoint Blk -> Maybe Blk
  , lookupRawBlock :: RealPoint Blk -> Maybe Blk
  , lookupParent :: Header Blk -> Maybe (Point Blk)
  }

-- | Rule 3 reading the blocks in 'onChain'.
onChainLookups :: Lookups
onChainLookups =
  Lookups
    { lookupHeader = blockOnChain
    , lookupRawBlock = blockOnChain
    , lookupParent = parentFromList
    }

blockOnChain :: RealPoint Blk -> Maybe Blk
blockOnChain rp = find ((== rp) . blockRealPoint) onChain

-- | As the ImmutableDB finds a block: by slot and hash.
parentFromList :: Header Blk -> Maybe (Point Blk)
parentFromList = runIdentity . findParentPoint (pure . fmap blockSlot . blockOnChain)

-- | Store the EBs, then play the batches. After each batch, the client reads
-- everything the server has for it. Returns all the client got.
--
-- If given a point, the client first intersects there, as a client that
-- reconnects does.
run :: Reading -> Maybe (Point Blk) -> [[ChainUpdate Blk Blk]] -> [ChainUpdate Blk Blk]
run how at batches = runSimOrThrow (runWith onChainLookups how at batches)

runWith ::
  forall s.
  Lookups -> Reading -> Maybe (Point Blk) -> [[ChainUpdate Blk Blk]] -> IOSim s [ChainUpdate Blk Blk]
runWith lookups how at batches = do
  db <- LeiosDb.newLeiosDBInMemory
  LeiosDb.withWriter db $ \w -> do
    storeEb w (ebPoint (blockSlot rb) eb) eb ebSize ebClosure
    storeEb w (ebPoint (blockSlot certRbAnnouncing) eb2) eb2 eb2Size eb2Closure
  LeiosDb.withReader db $ \reader -> do
    (scriptVar, inner) <- scriptedFollower
    outer <-
      inlineCertifiedEbsFollower
        LeiosTestCodecConfig
        reader
        (pure . fmap getHeader . lookupHeader lookups)
        (pure . fmap (Serialised . serialise) . lookupRawBlock lookups)
        (pure . lookupParent lookups)
        inner
    let push batch = atomically $ modifyTVar scriptVar (<> map (fmap toInner) batch)
    -- A client that reconnects finds the first batch already on the chain.
    later <- case at of
      Nothing -> pure batches
      Just pt -> do
        void $ followerForward outer [pt]
        case batches of
          [] -> pure []
          now : rest -> case how of
            AsServer -> push now >> pure rest
            _ -> pure batches
    case how of
      AsServer -> do
        void $ forkIO $ mapM_ (\batch -> threadDelay 1 >> push batch) later
        drain outer
      _ -> concat <$> mapM (\batch -> push batch >> drain outer) later
 where
  storeEb w pt body size closure = do
    p1 <- LeiosDb.writeEbPoint w pt size
    p2 <- LeiosDb.writeEbBody w pt body []
    p3 <- LeiosDb.writeTxs w pt [(off, bs) | (off, (_, bs)) <- zip [0 ..] closure]
    void (LeiosDb.await p1) >> void (LeiosDb.await p2) >> void (LeiosDb.await p3)

  toInner :: Blk -> WithPoint Blk (Header Blk, Serialised Blk)
  toInner b = WithPoint (getHeader b, Serialised (serialise b)) (blockPoint b)

  drain :: Follower (IOSim s) Blk (WithPoint Blk (Serialised Blk)) -> IOSim s [ChainUpdate Blk Blk]
  drain outer = readAll next
   where
    next = case how of
      NoWait -> followerInstruction outer
      Wait -> waitForNext
      AsServer -> followerInstruction outer >>= maybe waitForNext (pure . Just)

    -- Nothing comes for two seconds: everything has been read.
    waitForNext = either (const Nothing) Just <$> race (threadDelay 2) (followerInstructionBlocking outer)

-- | Read updates until there are none, with each block decoded.
readAll ::
  Monad m => m (Maybe (ChainUpdate Blk (WithPoint Blk (Serialised Blk)))) -> m [ChainUpdate Blk Blk]
readAll next =
  next >>= \case
    Nothing -> pure []
    Just u -> (fmap (\(WithPoint (Serialised bs) _) -> deserialise bs) u :) <$> readAll next

-- | Run with a function that counts its calls. Returns the result and the count.
countCalls :: ((a -> IOSim s b) -> IOSim s r) -> (a -> IOSim s b) -> IOSim s (r, Int)
countCalls use f = do
  calls <- uncheckedNewTVarM (0 :: Int)
  r <- use (\a -> atomically (modifyTVar calls (+ 1)) >> f a)
  (,) r <$> readTVarIO calls

-- | A follower that plays the updates in the returned variable, in order.
-- With none left, it is at the tip.
--
-- A client that reconnects at a point calls 'followerForward'. Here that always
-- succeeds, and the next update is a rollback to that point. The ChainDB's
-- follower does the same.
scriptedFollower :: IOLike m => m (StrictTVar m [ChainUpdate Blk b], Follower m Blk b)
scriptedFollower = do
  scriptVar <- uncheckedNewTVarM []
  let forward = \case
        [] -> pure Nothing
        pt : _ -> atomically (writeTVar scriptVar [RollBack pt]) >> pure (Just pt)
      pop = do
        script <- readTVar scriptVar
        case script of
          [] -> pure Nothing
          u : us -> writeTVar scriptVar us >> pure (Just u)
  pure
    ( scriptVar
    , Follower
        { followerInstruction = atomically pop
        , followerInstructionBlocking = atomically $ pop >>= maybe retry pure
        , followerForward = forward
        , followerClose = pure ()
        }
    )
