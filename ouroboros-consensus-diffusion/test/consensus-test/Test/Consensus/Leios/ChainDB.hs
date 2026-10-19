-- | Tests of how the ChainDB and the LeiosDB interact: which endorser blocks the
-- ChainDB has the LeiosDB keep, once the blocks that announce and certify them
-- are immutable, and when ChainSel may select a block certifying one.
module Test.Consensus.Leios.ChainDB (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Control.Monad (forM_, void)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.Block (Point, blockPoint, blockSlot)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (noPunishment)
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  )
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ChainDB and LeiosDB"
    [ testCase "the GC keeps an endorser block an immutable block certifies" $
        fst (runSimOrThrow bodiesAfterGc) @?= endorserBlockReferences ebA
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
      await =<< writeEbPoint writer point (encodeEndorserBlockSize eb)
      void $ await =<< writeEbBody writer point eb
      void $
        await
          =<< writeTxs
            writer
            point
            [ (offset, BS.replicate (fromIntegral size) 0)
            | (offset, (_txHash, size)) <- zip [0 ..] (endorserBlockReferences eb)
            ]
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  mapM_ (ChainDB.addBlock_ (nutChainDB nut) noPunishment) chain
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

pointA, pointB :: LeiosPoint
pointA = MkLeiosPoint (blockSlot announcingA) (UnsafeHash (SBS.replicate 32 1))
pointB = MkLeiosPoint (blockSlot announcingB) (UnsafeHash (SBS.replicate 32 2))

-- | The first block, which announces A; the second, which certifies A; and
-- the third, which announces B.
announcingA, certifyingA, announcingB :: LeiosTestBlock
announcingA = setAnnouncement (pointEbHash pointA) (encodeEndorserBlockSize ebA) (firstLeiosBlock 1)
certifyingA = setLeiosCert (successorLeiosBlock announcingA)
announcingB =
  setAnnouncement (pointEbHash pointB) (encodeEndorserBlockSize ebB) (successorLeiosBlock certifyingA)

ebA, ebB :: EndorserBlock
ebA = mkEndorserBlock [(UnsafeHash (SBS.replicate 32 1), 10)]
ebB = mkEndorserBlock [(UnsafeHash (SBS.replicate 32 2), 10)]

-- | The chain announces A and then certifies it, while the LeiosDB holds A's
-- body but not its transactions. The tip of the selected chain before they
-- arrive, and after.
tipsAroundClosure :: IOSim s (Point LeiosTestBlock, Point LeiosTestBlock)
tipsAroundClosure = withNode $ \nut -> do
  let leiosDb = getLeiosDB (nutKernel nut)
      chainDB = nutChainDB nut
  withWriter leiosDb $ \writer -> do
    await =<< writeEbPoint writer pointA (encodeEndorserBlockSize ebA)
    void $ await =<< writeEbBody writer pointA ebA
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  mapM_ (ChainDB.addBlock_ chainDB noPunishment) [announcingA, certifyingA]
  threadDelay 1
  tipBefore <- atomically $ ChainDB.getTipPoint chainDB
  withWriter leiosDb $ \writer ->
    void $
      await
        =<< writeTxs
          writer
          pointA
          [ (offset, BS.replicate (fromIntegral size) 0)
          | (offset, (_txHash, size)) <- zip [0 ..] (endorserBlockReferences ebA)
          ]
  threadDelay 1
  tipAfter <- atomically $ ChainDB.getTipPoint chainDB
  pure (tipBefore, tipAfter)

-- | As 'tipsAroundClosure', except that the LeiosDB holds A's closure before
-- the node starts. The tip of the selected chain.
tipWithClosureBeforeStart :: IOSim s (Point LeiosTestBlock)
tipWithClosureBeforeStart = do
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer -> do
    await =<< writeEbPoint writer pointA (encodeEndorserBlockSize ebA)
    void $ await =<< writeEbBody writer pointA ebA
    void $
      await
        =<< writeTxs
          writer
          pointA
          [ (offset, BS.replicate (fromIntegral size) 0)
          | (offset, (_txHash, size)) <- zip [0 ..] (endorserBlockReferences ebA)
          ]
  withNodeOver leiosDb $ \nut -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    mapM_ (ChainDB.addBlock_ (nutChainDB nut) noPunishment) [announcingA, certifyingA]
    threadDelay 1
    atomically $ ChainDB.getTipPoint (nutChainDB nut)
