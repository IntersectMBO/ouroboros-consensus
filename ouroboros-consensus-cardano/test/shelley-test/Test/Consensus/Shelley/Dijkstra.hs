{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Dijkstra (tests) where

import Cardano.Binary (serialize')
import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as SL (pattern Block)
import qualified Cardano.Ledger.Core as Core (BlockBody, hashBlockBody, sizeTxF, txSeqBlockBodyL)
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.Leios.BlockHeader
  ( HeaderBody (..)
  , HeaderBodyRaw (..)
  , headerBody
  , headerSig
  , mkHeader
  , mkHeaderBody
  )
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs, txId)
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Consensus.Storage.LedgerDB (ResolveLeiosBlock (..))
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
        , testProperty
            "the mempool never charges a transaction less than 'forgeLeiosEb' writes for it"
            prop_leiosEbItemChargeCoversEncoder
        , testProperty
            "'inlineLeiosClosure' appends the EB's transactions after the block's own, in order"
            prop_inlineLeiosClosureAppends
        ]
    ]

-- | An endorser-block reference records the length of the serialised
-- transaction ('serialize'', the mempool-submission encoding), while the
-- mempool charges the ledger's size computation ('sizeTxF'). At the current
-- ledger pin the two encodings agree exactly; this fails on a ledger bump
-- that lets the wire size drift above the charge, before that drift can
-- produce an endorser block no peer accepts.
prop_leiosEbItemChargeCoversEncoder :: Property
prop_leiosEbItemChargeCoversEncoder =
  forAll (arbitrary :: Gen (GenTx (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))) $
    \(ShelleyTx _txid tx) ->
      let wireSize = toInteger $ BS.length (serialize' tx)
          chargedSize = toInteger $ tx ^. Core.sizeTxF
       in counterexample
            ("serialize': " <> show wireSize <> " bytes, sizeTxF: " <> show chargedSize)
            $ wireSize <= chargedSize

-- | 'inlineLeiosClosure' returns the block's own transactions followed by the
-- EB's, and leaves the header unchanged.
prop_inlineLeiosClosureAppends :: Property
prop_inlineLeiosClosureAppends =
  forAll genBlock $ \blk ->
    -- Generating a Dijkstra tx is slow, so keep the EB small: 0 to 3 txs
    -- covers none, one and > one, which is enough to check the order.
    forAll (choose (0, 3) >>= \n -> vectorOf n arbitrary) $ \ebTxs ->
      let inlined = inlineLeiosClosure blk ebTxs
       in conjoin
            [ map txId (extractTxs inlined) === map txId (extractTxs blk <> ebTxs)
                & counterexample "transactions are not the block's own followed by the EB's"
            , getHeader inlined === getHeader blk
                & counterexample "header changed"
            ]
            & tabulate "block's own transactions" [if null (extractTxs blk) then "none" else "some"]
            & tabulate "EB transactions" [if null ebTxs then "none" else "some"]
 where
  genBlock :: Gen (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra)
  genBlock = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))
    body <-
      oneof
        [ genSmallDijkstraTxsBlockBody @DijkstraEra
        , genSmallDijkstraCertBlockBody @DijkstraEra
        ]
    let hbody = headerBody (shelleyHeaderRaw shelleyHdr)
        sig = headerSig (shelleyHeaderRaw shelleyHdr)
        containsCert = case body ^. leiosCertBlockBodyL of
          SJust _ -> True
          SNothing -> False
    return $
      mkShelleyBlock $
        SL.Block (mkHeader (Proxy @DijkstraEra) (withBodyClaims hbody body containsCert) sig) body

prop_leiosBodyWithCertAndTxsRejected :: Property
prop_leiosBodyWithCertAndTxsRejected =
  forAll genBlockWithTxsAndCert $ \blk ->
    counterexample
      "blockMatchesHeader should be False when body contains both Cert and Txs"
      (not (blockMatchesHeader (getHeader blk) blk))
 where
  genBlockWithTxsAndCert :: Gen (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra)
  genBlockWithTxsAndCert = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))
    txsBody <-
      genSmallDijkstraTxsBlockBody @DijkstraEra `suchThat` (\b -> not (null (b ^. Core.txSeqBlockBodyL)))
    cert <- arbitrary :: Gen LeiosCert
    containsCert <- arbitrary :: Gen Bool
    let hbody = headerBody (shelleyHeaderRaw shelleyHdr)
        sig = headerSig (shelleyHeaderRaw shelleyHdr)
        body = txsBody & leiosCertBlockBodyL .~ SJust cert
    return $
      mkShelleyBlock $
        SL.Block
          ( mkHeader
              (Proxy @DijkstraEra)
              (withBodyClaims hbody body containsCert)
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
  genBlockCertHeaderAndTxsBody :: Gen (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra)
  genBlockCertHeaderAndTxsBody = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))
    txsBody <- genSmallDijkstraTxsBlockBody @DijkstraEra
    let hbody = headerBody (shelleyHeaderRaw shelleyHdr)
        sig = headerSig (shelleyHeaderRaw shelleyHdr)
    return $
      mkShelleyBlock $
        SL.Block
          ( mkHeader
              (Proxy @DijkstraEra)
              (withBodyClaims hbody txsBody True)
              sig -- invalid
          )
          txsBody

  genBlockTxsHeaderAndCertBody :: Gen (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra)
  genBlockTxsHeaderAndCertBody = do
    shelleyHdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))
    certBody <- genSmallDijkstraCertBlockBody @DijkstraEra
    let hbody = headerBody (shelleyHeaderRaw shelleyHdr)
        sig = headerSig (shelleyHeaderRaw shelleyHdr)
    return $
      mkShelleyBlock $
        SL.Block
          ( mkHeader
              (Proxy @DijkstraEra)
              (withBodyClaims hbody certBody False)
              sig -- invalid
          )
          certBody

-- | The header body for another block body and Leios certificate claim, with no
-- announcement.
withBodyClaims ::
  HeaderBody StandardCrypto ->
  Core.BlockBody DijkstraEra ->
  Bool ->
  HeaderBody StandardCrypto
withBodyClaims hb body containsCert =
  mkHeaderBody (Proxy @DijkstraEra) $
    HeaderBodyRaw
      { hbrBlockNo = hbBlockNo hb
      , hbrSlotNo = hbSlotNo hb
      , hbrPrev = hbPrev hb
      , hbrVk = hbVk hb
      , hbrVrfVk = hbVrfVk hb
      , hbrVrfRes = hbVrfRes hb
      , hbrBodySize = hbBodySize hb
      , hbrBodyHash = Core.hashBlockBody body
      , hbrOCert = hbOCert hb
      , hbrVersionInfo = hbVersionInfo hb
      , hbrBlockBodyContainsLeiosCert = containsCert
      , hbrEbReferencesAnnouncement = SNothing
      }
