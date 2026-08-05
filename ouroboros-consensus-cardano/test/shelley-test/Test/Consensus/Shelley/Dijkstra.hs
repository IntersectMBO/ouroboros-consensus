{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Dijkstra (tests) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as SL (pattern Block)
import qualified Cardano.Ledger.Core as Core (hashBlockBody, txSeqBlockBodyL)
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import Cardano.Protocol.Crypto (StandardCrypto)
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Header
  ( HeaderBody (..)
  , HeaderLeiosExtension (..)
  , pattern Header
  )
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary
  ( genSmallDijkstraCertBlockBody
  , genSmallDijkstraTxsBlockBody
  )
import Test.Consensus.Cardano.Generators ()
import Test.Consensus.Shelley.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Dijkstra"
    [ testGroup
        "Leios"
        [ testProperty
            "'blockMatchesHeader' must reject block bodies carrying both a Leios certificate and transactions, since cert and txs are mutually exclusive"
            prop_leiosBodyWithCertAndTxsRejected
        , testProperty
            "'blockMatchesHeader' must reject blocks where the header's 'containsCert' flag disagrees with the body's actual content"
            prop_leiosCertFlagMismatchRejected
        ]
    ]

prop_leiosBodyWithCertAndTxsRejected :: Property
prop_leiosBodyWithCertAndTxsRejected =
  forAll genBlockWithTxsAndCert $ \blk ->
    counterexample
      "blockMatchesHeader should be False when body contains both Cert and Txs"
      (not (blockMatchesHeader (getHeader blk) blk))
 where
  genBlockWithTxsAndCert :: Gen (ShelleyBlock (Praos StandardCrypto) DijkstraEra)
  genBlockWithTxsAndCert = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos StandardCrypto) DijkstraEra))
    txsBody <-
      genSmallDijkstraTxsBlockBody @DijkstraEra `suchThat` (\b -> not (null (b ^. Core.txSeqBlockBodyL)))
    cert <- arbitrary :: Gen LeiosCert
    containsCert <- arbitrary :: Gen Bool
    let Header hbody sig = shelleyHeaderRaw shelleyHdr
        body = txsBody & leiosCertBlockBodyL .~ SJust cert
    return $
      mkShelleyBlock $
        SL.Block
          ( Header
              hbody
                { hbBodyHash = Core.hashBlockBody body
                , hbLeiosExt = SJust HeaderLeiosExtension{containsCert, ebAnnouncement = SNothing}
                }
              sig -- invalid
          )
          body

prop_leiosCertFlagMismatchRejected :: Property
prop_leiosCertFlagMismatchRejected =
  ( forAll genBlockCertHeaderAndTxsBody $ \blk ->
      counterexample "containsCert=True but body has no Leios cert" $
        not (blockMatchesHeader (getHeader blk) blk)
  )
    .&&. ( forAll genBlockTxsHeaderAndCertBody $ \blk ->
             counterexample "containsCert=False but body has a Leios cert" $
               not (blockMatchesHeader (getHeader blk) blk)
         )
 where
  genBlockCertHeaderAndTxsBody :: Gen (ShelleyBlock (Praos StandardCrypto) DijkstraEra)
  genBlockCertHeaderAndTxsBody = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos StandardCrypto) DijkstraEra))
    txsBody <- genSmallDijkstraTxsBlockBody @DijkstraEra
    let Header hbody sig = shelleyHeaderRaw shelleyHdr
    return $
      mkShelleyBlock $
        SL.Block
          ( Header
              hbody
                { hbBodyHash = Core.hashBlockBody txsBody
                , hbLeiosExt = SJust HeaderLeiosExtension{containsCert = True, ebAnnouncement = SNothing}
                }
              sig -- invalid
          )
          txsBody

  genBlockTxsHeaderAndCertBody :: Gen (ShelleyBlock (Praos StandardCrypto) DijkstraEra)
  genBlockTxsHeaderAndCertBody = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos StandardCrypto) DijkstraEra))
    certBody <- genSmallDijkstraCertBlockBody @DijkstraEra
    let Header hbody sig = shelleyHeaderRaw shelleyHdr
    return $
      mkShelleyBlock $
        SL.Block
          (Header hbody{hbBodyHash = Core.hashBlockBody certBody, hbLeiosExt = SNothing} sig)
          certBody
