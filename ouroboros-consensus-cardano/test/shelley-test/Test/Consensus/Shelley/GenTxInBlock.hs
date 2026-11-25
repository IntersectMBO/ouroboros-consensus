{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Consensus.Shelley.GenTxInBlock (tests) where

import qualified Cardano.Ledger.Api as L
import Cardano.Ledger.Core (TopTx, Tx, mkBasicBlockBody, txSeqBlockBodyL)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.MempoolTx (unwrapMempoolTx)
import qualified Cardano.Ledger.Shelley.API as SL
import Data.SOP.BasicFunctors (I (..))
import Data.SOP.Strict (NS (..))
import qualified Data.Sequence.Strict as Seq
import Lens.Micro ((&), (.~))
import qualified Ouroboros.Consensus.HardFork.Combinator as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Embed.Unary as Unary
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( HasTxs (..)
  , extractAsMempoolTxs
  )
import Ouroboros.Consensus.Shelley.HFEras (StandardConwayBlock, StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..), mkShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( GenTx (ShelleyTx)
  , GenTxInBlock (ShelleyTxInBlock)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Consensus.Shelley.Examples (examplesConway, examplesDijkstra)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util.Serialisation.Examples (Examples, Labelled)
import qualified Test.Util.Serialisation.Examples as Examples

tests :: TestTree
tests =
  testGroup
    "GenTxInBlock"
    [ testCase "extraction preserves full Dijkstra transactions and their validity flags" $
        [tx | ShelleyTxInBlock tx <- extractTxs dijkstraBlock] @?= dijkstraTxs
    , testCase "Dijkstra resubmission resets the flag without changing other transaction fields" $ do
        let submitted :: GenTx StandardDijkstraBlock
            submitted = txInBlockToMempoolTx (ShelleyTxInBlock invalidDijkstraTx)
            ShelleyTx _ tx = submitted
        unwrapMempoolTx tx @?= (invalidDijkstraTx & L.isPhase2ValidTxL .~ L.Phase2Valid)
    , testCase "Conway resubmission retains its accepted validity flag" $ do
        let ShelleyTx _ exampleTx = example Examples.exampleGenTx examplesConway
            invalidTx = unwrapMempoolTx exampleTx & L.isPhase2ValidTxL .~ L.Phase2Invalid
            submitted :: GenTx StandardConwayBlock
            submitted = txInBlockToMempoolTx (ShelleyTxInBlock invalidTx)
            ShelleyTx _ tx = submitted
        unwrapMempoolTx tx @?= invalidTx
    , testCase "the HFC preserves the era and dispatches the submission conversion" $ do
        let block :: HFC.HardForkBlock '[StandardConwayBlock, StandardDijkstraBlock]
            block = HFC.HardForkBlock (HFC.OneEraBlock (S (Z (I dijkstraBlock))))
        [ tx
          | HFC.HardForkGenTxInBlock (HFC.OneEraGenTxInBlock (S (Z (ShelleyTxInBlock tx)))) <- extractTxs block
          ]
          @?= dijkstraTxs
        [ unwrapMempoolTx tx
          | HFC.HardForkGenTx (HFC.OneEraGenTx (S (Z (ShelleyTx _ tx)))) <- extractAsMempoolTxs block
          ]
          @?= map (L.isPhase2ValidTxL .~ L.Phase2Valid) dijkstraTxs
    , testCase "unary HFC injection and projection preserve the full transaction" $ do
        let injected :: GenTxInBlock (HFC.HardForkBlock '[StandardDijkstraBlock])
            injected = Unary.inject (ShelleyTxInBlock invalidDijkstraTx)
            ShelleyTxInBlock tx = Unary.project injected :: GenTxInBlock StandardDijkstraBlock
        tx @?= invalidDijkstraTx
    ]

invalidDijkstraTx :: Tx TopTx DijkstraEra
invalidDijkstraTx = unwrapMempoolTx tx & L.isPhase2ValidTxL .~ L.Phase2Invalid
 where
  ShelleyTx _ tx = example Examples.exampleGenTx examplesDijkstra

dijkstraTxs :: [Tx TopTx DijkstraEra]
dijkstraTxs = [invalidDijkstraTx, invalidDijkstraTx & L.isPhase2ValidTxL .~ L.Phase2Valid]

-- Extraction must work without validation evidence. Reusing the example's
-- header with a different body deliberately does not produce a valid block.
dijkstraBlock :: StandardDijkstraBlock
dijkstraBlock =
  mkShelleyBlock $
    SL.Block header $
      mkBasicBlockBody & txSeqBlockBodyL .~ Seq.fromList dijkstraTxs
 where
  SL.Block header _ = shelleyBlockRaw (example Examples.exampleBlock examplesDijkstra)

example :: (Examples blk -> Labelled a) -> Examples blk -> a
example field examples = case field examples of
  (_, x) : _ -> x
  [] -> error "no example"
