{-# LANGUAGE TypeApplications #-}

-- | A header's claim that its body carries a Leios certificate is the one
-- Leios claim checkable from the block alone.
module Test.Consensus.Shelley.Integrity (tests) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.MemoBytes (getMemoRawType)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Data.Proxy (Proxy (Proxy))
import Ouroboros.Consensus.Block (blockMatchesHeader, getHeader)
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger
  ( Header
  , mkShelleyHeader
  , shelleyHeaderRaw
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract (pHeaderBodyHash)
import Test.Consensus.Shelley.Examples (examplesDijkstra)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Util.Serialisation.Examples (exampleBlock)

tests :: TestTree
tests =
  testGroup
    "Integrity"
    [ testCase "an example Dijkstra block matches its header" $
        mapM_
          (\blk -> assertBool "does not match" (blockMatchesHeader (getHeader blk) blk))
          exampleDijkstraBlocks
    , testCase "a Dijkstra header claiming a Leios certificate its body lacks does not match" $
        mapM_
          ( \blk -> do
              let lying = claimLeiosCert (getHeader blk)
              -- Without this, the mismatch could be the body hash's doing.
              assertEqual
                "the body hash moved too"
                (pHeaderBodyHash (shelleyHeaderRaw (getHeader blk)))
                (pHeaderBodyHash (shelleyHeaderRaw lying))
              assertBool "matches despite the false claim" $
                not (blockMatchesHeader lying blk)
          )
          exampleDijkstraBlocks
    ]

exampleDijkstraBlocks :: [StandardDijkstraBlock]
exampleDijkstraBlocks = snd <$> exampleBlock examplesDijkstra

-- | The same header, claiming its body carries a Leios certificate.
--
-- The body hash is left alone, so only the claim disagrees.
claimLeiosCert :: Header StandardDijkstraBlock -> Header StandardDijkstraBlock
claimLeiosCert hdr =
  mkShelleyHeader $
    -- TODO: should be able to use lenses, but blockBodyContainsLeiosCert has none yet in ledger
    Leios.mkHeader
      era
      (Leios.mkHeaderBody era rawBody{Leios.hbrBlockBodyContainsLeiosCert = True})
      (Leios.headerSig raw)
 where
  era = Proxy @DijkstraEra
  raw = shelleyHeaderRaw hdr
  rawBody = getMemoRawType (Leios.headerBody raw)
