-- | Tests of which endorser blocks the ChainDB has the LeiosDB keep, once the
-- blocks that announce and certify them are immutable.
module Test.Consensus.Leios.ChainDB (tests) where

import Control.Monad (forM_, void)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (noPunishment)
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
        fst (runSimOrThrow bodiesAfterGc) @?= V.toList (leiosEbTxs ebA)
    , testCase "the GC evicts an endorser block no block certifies" $
        snd (runSimOrThrow bodiesAfterGc) @?= []
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
            point
            [ (offset, BS.replicate (fromIntegral size) 0)
            | (offset, (_txHash, size)) <- zip [0 ..] (V.toList (leiosEbTxs eb))
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
  pointA = MkLeiosPoint (blockSlot announcingA) (MkEbHash (BS.replicate 32 1))
  pointB = MkLeiosPoint (blockSlot announcingB) (MkEbHash (BS.replicate 32 2))

  announcingA = setAnnouncement (pointEbHash pointA) (encodeLeiosEbSize ebA) (firstLeiosBlock 1)
  certifyingA = setLeiosCert (successorLeiosBlock announcingA)
  announcingB = setAnnouncement (pointEbHash pointB) (encodeLeiosEbSize ebB) (successorLeiosBlock certifyingA)

  -- k is 10, so with sixteen blocks the immutable tip is the sixth, past both
  -- announcements.
  chain = take 16 $ announcingA : certifyingA : iterate successorLeiosBlock announcingB

ebA, ebB :: LeiosEb
ebA = MkLeiosEb (V.fromList [(MkTxHash (BS.replicate 32 1), 10)])
ebB = MkLeiosEb (V.fromList [(MkTxHash (BS.replicate 32 2), 10)])
