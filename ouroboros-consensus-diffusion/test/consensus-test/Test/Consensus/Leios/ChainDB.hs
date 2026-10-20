{-# LANGUAGE TypeApplications #-}

-- | Tests of how the ChainDB and the LeiosDB interact: which endorser blocks the
-- ChainDB has the LeiosDB keep, once the blocks that announce and certify them
-- are immutable, and when ChainSel may select a block certifying one.
module Test.Consensus.Leios.ChainDB (tests) where

import Cardano.Crypto.Leios (VerificationError (..))
import Control.Monad (forM_, void)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , Point
  , Proxy (..)
  , WithOrigin (..)
  , blockHash
  , blockPoint
  , blockSlot
  )
import Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import Ouroboros.Consensus.Leios.Cert (LeiosCertRejection (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  )
import Ouroboros.Consensus.Util.STM (forgetFingerprint)
import Test.Consensus.Leios.Committee (certOf)
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "ChainDB and LeiosDB"
    [ testCase "ChainSel rejects a block whose certificate a minority made" $
        runSimOrThrow
          (whyInvalid (\nut -> addBlocks nut [announcingA, minorityCertifyingA]) minorityCertifyingA)
          @?= Just (ExtValidationErrorLeiosCert (LeiosCertInvalid (InsufficientWeight (1 / 4))))
    , testCase "ChainSel does not verify a certificate again for a claim it established" $
        runSimOrThrow (whyInvalid addSibling minoritySibling) @?= Nothing
    , testCase "a restart forgets a certifying block off the selection, until it arrives again" $
        runSimOrThrow (heldAcrossRestart BodyOfA) @?= (True, False, True)
    , testCase "a restart keeps a certifying block on the selection" $
        runSimOrThrow (heldAcrossRestart ClosureOfA) @?= (True, True, True)
    , testCase "the GC keeps an endorser block an immutable block certifies" $
        fst (runSimOrThrow bodiesAfterGc) @?= V.toList (leiosEbTxs ebA)
    , testCase "the GC evicts an endorser block no block certifies" $
        snd (runSimOrThrow bodiesAfterGc) @?= []
    , testCase "ChainSel parks a certifying block until the closure arrives" $
        runSimOrThrow tipsAroundClosure
          @?= (blockPoint announcingA, blockPoint certifyingA)
    , testCase "ChainSel selects a certifying block whose closure the LeiosDB held at start" $
        runSimOrThrow tipWithClosureBeforeStart @?= blockPoint certifyingA
    ]

-- | The node holds endorser blocks A and B, with their transactions. The
-- chain's first block announces A, its second certifies A, and its third
-- announces B, which nothing certifies. Once the immutable tip has passed both
-- announcements and the GC has run, these are the bodies of A and B that the
-- LeiosDB still holds.
bodiesAfterGc :: IOSim s ([(TxHash, BytesSize)], [(TxHash, BytesSize)])
bodiesAfterGc = withNode $ \nut -> do
  let leiosDb = getLeiosDB (nutKernel nut)
  withWriter leiosDb $ \writer ->
    forM_ [(pointA, ebA), (pointB, ebB)] $ \(point, eb) -> do
      await =<< writeEbPoint writer point (encodeLeiosEbSize eb)
      void $ await =<< writeEbBody writer point eb
      void $
        await
          =<< writeTxs
            writer
            [(txHash, BS.replicate (fromIntegral size) 0) | (txHash, size) <- V.toList (leiosEbTxs eb)]
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  addBlocks nut chain
  -- Long enough for the copy to the ImmutableDB and for the delayed GC.
  threadDelay 100
  withReader leiosDb $ \reader ->
    (,)
      <$> lookupEbBody reader (pointEbHash pointA)
      <*> lookupEbBody reader (pointEbHash pointB)
 where
  -- k is 10, so with sixteen blocks the immutable tip is the sixth, past both
  -- announcements.
  chain = take 16 $ announcingA : certifyingA : iterate successorLeiosBlock announcingB

-- | Add the blocks with the given action, then report why the ChainDB judges
-- the given block invalid, if it does.
whyInvalid ::
  (NodeUnderTest (IOSim s) -> IOSim s ()) ->
  LeiosTestBlock ->
  IOSim s (Maybe (ExtValidationError LeiosTestBlock))
whyInvalid add blk = withNode $ \nut -> do
  -- Let the slots of the blocks pass, so none is from the future.
  threadDelay 20
  add nut
  threadDelay 1
  isInvalid <- atomically $ forgetFingerprint <$> ChainDB.getIsInvalidBlock (nutChainDB nut)
  pure $ isInvalid (blockHash blk)

-- | The node adds 'announcingA' and 'certifyingA', restarts, and then adds
-- 'certifyingA' again. Whether the ChainDB holds 'certifyingA' before the
-- restart, after it, and after the second add.
--
-- Without A's transactions, 'certifyingA' is parked and so not on the
-- selection.
heldAcrossRestart :: WhatOfA -> IOSim s (Bool, Bool, Bool)
heldAcrossRestart whatOfA = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer -> do
    await =<< writeEbPoint writer pointA (encodeLeiosEbSize ebA)
    void $ await =<< writeEbBody writer pointA ebA
    case whatOfA of
      BodyOfA -> pure ()
      ClosureOfA ->
        void $
          await
            =<< writeTxs
              writer
              [(txHash, BS.replicate (fromIntegral size) 0) | (txHash, size) <- V.toList (leiosEbTxs ebA)]
  beforeRestart <- withNodeOverDBs nodeDBs leiosDb $ \nut -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    addBlocks nut [announcingA, certifyingA]
    threadDelay 1
    holds nut
  withNodeOverDBs nodeDBs leiosDb $ \nut -> do
    afterRestart <- holds nut
    addBlocksAfter nut (NotOrigin (blockSlot announcingA)) [certifyingA]
    threadDelay 1
    afterAddingAgain <- holds nut
    pure (beforeRestart, afterRestart, afterAddingAgain)
 where
  holds nut = atomically $ ($ blockPoint certifyingA) <$> ChainDB.getIsFetched (nutChainDB nut)

-- | What the LeiosDB holds of A
data WhatOfA
  = -- | Its body, but not its transactions
    BodyOfA
  | -- | Its body and its transactions
    ClosureOfA

-- | 'certifyingA', except a minority made its certificate.
minorityCertifyingA :: LeiosTestBlock
minorityCertifyingA =
  setLeiosCert (certOf (rbHashOf announcingA) [0]) (successorLeiosBlock announcingA)

-- | A sibling of 'certifyingA', making the same claim, with a certificate a
-- minority made.
minoritySibling :: LeiosTestBlock
minoritySibling =
  setLeiosCert (certOf (rbHashOf announcingA) [0]) (forkLeiosBlock certifyingA)

-- | Add 'certifyingA', which establishes its claim, and then 'minoritySibling'.
addSibling :: NodeUnderTest (IOSim s) -> IOSim s ()
addSibling nut = do
  addBlocks nut [announcingA, certifyingA]
  addBlocksAfter nut (NotOrigin (blockSlot announcingA)) [minoritySibling]

rbHashOf :: LeiosTestBlock -> RbHash
rbHashOf = MkRbHash . toRawHash (Proxy @LeiosTestBlock) . blockHash

pointA, pointB :: LeiosPoint
pointA = MkLeiosPoint (blockSlot announcingA) (MkEbHash (BS.replicate 32 1))
pointB = MkLeiosPoint (blockSlot announcingB) (MkEbHash (BS.replicate 32 2))

-- | The first block, which announces A; the second, which certifies A; and
-- the third, which announces B.
announcingA, certifyingA, announcingB :: LeiosTestBlock
announcingA = setAnnouncement (pointEbHash pointA) (encodeLeiosEbSize ebA) (firstLeiosBlock 1)
certifyingA =
  setLeiosCert (certOf (rbHashOf announcingA) [0, 1, 2]) (successorLeiosBlock announcingA)
announcingB = setAnnouncement (pointEbHash pointB) (encodeLeiosEbSize ebB) (successorLeiosBlock certifyingA)

ebA, ebB :: LeiosEb
ebA = MkLeiosEb (V.fromList [(MkTxHash (BS.replicate 32 1), 10)])
ebB = MkLeiosEb (V.fromList [(MkTxHash (BS.replicate 32 2), 10)])

-- | The chain announces A and then certifies it, while the LeiosDB holds A's
-- body but not its transactions. The tip of the selected chain before they
-- arrive, and after.
tipsAroundClosure :: IOSim s (Point LeiosTestBlock, Point LeiosTestBlock)
tipsAroundClosure = withNode $ \nut -> do
  let leiosDb = getLeiosDB (nutKernel nut)
      chainDB = nutChainDB nut
  withWriter leiosDb $ \writer -> do
    await =<< writeEbPoint writer pointA (encodeLeiosEbSize ebA)
    void $ await =<< writeEbBody writer pointA ebA
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  addBlocks nut [announcingA, certifyingA]
  threadDelay 1
  tipBefore <- atomically $ ChainDB.getTipPoint chainDB
  withWriter leiosDb $ \writer ->
    void $
      await
        =<< writeTxs
          writer
          [(txHash, BS.replicate (fromIntegral size) 0) | (txHash, size) <- V.toList (leiosEbTxs ebA)]
  threadDelay 1
  tipAfter <- atomically $ ChainDB.getTipPoint chainDB
  pure (tipBefore, tipAfter)

-- | As 'tipsAroundClosure', except that the LeiosDB holds A's closure before
-- the node starts. The tip of the selected chain.
tipWithClosureBeforeStart :: IOSim s (Point LeiosTestBlock)
tipWithClosureBeforeStart = do
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer -> do
    await =<< writeEbPoint writer pointA (encodeLeiosEbSize ebA)
    void $ await =<< writeEbBody writer pointA ebA
    void $
      await
        =<< writeTxs
          writer
          [(txHash, BS.replicate (fromIntegral size) 0) | (txHash, size) <- V.toList (leiosEbTxs ebA)]
  withNodeOver leiosDb $ \nut -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    addBlocks nut [announcingA, certifyingA]
    threadDelay 1
    atomically $ ChainDB.getTipPoint (nutChainDB nut)
