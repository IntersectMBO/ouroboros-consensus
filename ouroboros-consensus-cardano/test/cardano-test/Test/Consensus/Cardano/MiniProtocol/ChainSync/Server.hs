{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Checks what a client gets when the node sends it a CertRB over
-- node-to-client ChainSync.
--
-- Ogmios, Kupo, @cardano-db-sync@, @cardano-wallet@ and @cardano-api@ decode
-- blocks with this repository's codecs. These tests use the same codecs, so
-- they see the same block those clients do.
module Test.Consensus.Cardano.MiniProtocol.ChainSync.Server (tests) where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (isPhase2ValidTxL), IsPhase2Valid)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Core (Tx, TxLevel (TopTx))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.TxIn as SL (TxId)
import Cardano.Protocol.Leios.BlockHeader
  ( HeaderBody (..)
  , HeaderBodyRaw (..)
  , headerBody
  , headerSig
  , mkHeader
  , mkHeaderBody
  )
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import LeiosDemoTypes (serializeTxForLeios)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), hashTxBytes, mkEbHash)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (inlineCertifiedEbTxs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))
import Ouroboros.Network.Block (Serialised (..))
import Test.Cardano.Ledger.Dijkstra.Arbitrary
  ( genSmallDijkstraCertBlockBody
  , genSmallDijkstraTxsBlockBody
  )
import Test.Consensus.Cardano.Examples (codecConfig)
import Test.Consensus.Shelley.Generators ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "ChainSync server (CertRBs)"
    [ testProperty
        "a client decodes a CertRB with its EB's transactions, under the CertRB's hash"
        prop_clientDecodesCertRb
    ]

-- | For every node-to-client version the node supports, the client decodes
-- the CertRB the server sends: the same hash and certificate as the stored
-- CertRB, with the EB's transactions in order.
--
-- This block has both a certificate and transactions, so
-- 'blockMatchesHeader' rejects it. Clients must not use that check here.
prop_clientDecodesCertRb :: Property
prop_clientDecodesCertRb =
  forAllShow genCertRb (show . fst) $ \(certRb, ebTxs) ->
    conjoin
      [ counterexample (show version) $ ioProperty $ do
          sent <-
            inlineCertifiedEbTxs
              codecConfig
              (ebReader ebTxs)
              (Just ebPoint)
              (blockPoint certRb)
              (getHeader certRb)
              (stored certRb)
          pure $ case decodeAsClient version sent of
            Left err -> counterexample err False
            Right got ->
              conjoin
                [ counterexample "hash" $ blockHash got === blockHash certRb
                , counterexample "certificate" $ blockLeiosCert got === blockLeiosCert certRb
                , counterexample "transactions" $ txsOf got === map txKey ebTxs
                , counterexample "blockMatchesHeader" $
                    not (blockMatchesHeader (getHeader got) got)
                ]
      | version <- versions
      ]
 where
  versions = Map.elems (supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto)))

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | A CertRB as stored: a certificate and no transactions, and a header that
-- says so. With the transactions of the EB it certifies.
--
-- The EB has 1 to 5 transactions(in order) of small size: Dijkstra
-- transactions are slow to generate.
genCertRb :: Gen (CardanoBlock StandardCrypto, [Tx TopTx DijkstraEra])
genCertRb = do
  hdr <- arbitrary :: Gen (Header (ShelleyBlock (Praos2 StandardCrypto) DijkstraEra))
  body <- genSmallDijkstraCertBlockBody @DijkstraEra
  numTxs <- choose (1, 5)
  ebTxs <- (concat <$> vectorOf numTxs (resize 5 genTxs)) `suchThat` (not . null)
  let raw = shelleyHeaderRaw hdr
      certRb =
        mkShelleyBlock $
          SL.Block
            (mkHeader (Proxy @DijkstraEra) (claimCert (headerBody raw) body) (headerSig raw))
            body
  pure (BlockDijkstra certRb, ebTxs)
 where
  genTxs = toList . (^. Core.txSeqBlockBodyL) <$> genSmallDijkstraTxsBlockBody @DijkstraEra

-- | The header body for a CertRB with this body.
claimCert :: HeaderBody StandardCrypto -> Core.BlockBody DijkstraEra -> HeaderBody StandardCrypto
claimCert hb body =
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
      , hbrBlockBodyContainsLeiosCert = True
      , hbrEbReferencesAnnouncement = SNothing
      }

{-------------------------------------------------------------------------------
  Server and client
-------------------------------------------------------------------------------}

-- | The EB the block before the CertRB announced.
ebPoint :: LeiosPoint
ebPoint =
  MkLeiosPoint
    { pointSlotNo = SlotNo 0
    , pointEbHash = fromJust $ mkEbHash (BS.replicate 32 0)
    }

-- | A LeiosDB that has the EB's transactions.
ebReader :: [Tx TopTx DijkstraEra] -> LeiosDbReader IO
ebReader ebTxs =
  LeiosDbReader
    { closeReader = pure ()
    , lookupEbBody = \_ -> unused "lookupEbBody"
    , lookupEbClosure = \_ ->
        pure (Just [(hashTxBytes bs, bs) | bs <- map serializeTxForLeios ebTxs])
    , batchRetrieveTxs = \_ _ -> unused "batchRetrieveTxs"
    , testScanEbPoints = unused "testScanEbPoints"
    , scanCompleteEbClosuresNotOlderThanSlot = \_ -> unused "scanCompleteEbClosuresNotOlderThanSlot"
    }
 where
  unused name = error $ "ebReader: " <> name <> " is not used by the ChainSync server"

-- | The CertRB as the server reads it from the ChainDB.
stored :: CardanoBlock StandardCrypto -> Serialised (CardanoBlock StandardCrypto)
stored = Serialised . CBOR.toLazyByteString . encodeDisk codecConfig

-- | What a client decodes from a block the server sends.
decodeAsClient ::
  BlockNodeToClientVersion (CardanoBlock StandardCrypto) ->
  Serialised (CardanoBlock StandardCrypto) ->
  Either String (CardanoBlock StandardCrypto)
decodeAsClient version sblk =
  case CBOR.deserialiseFromBytes (decodeNodeToClient codecConfig version) bytes of
    Left err -> Left (show err)
    Right (rest, blk)
      | Lazy.null rest -> Right blk
      | otherwise -> Left "trailing bytes"
 where
  bytes = CBOR.toLazyByteString (encodeNodeToClient codecConfig version sblk)

-- | Each transaction's ID and phase-2 validity flag, in order.
txsOf :: CardanoBlock StandardCrypto -> [(SL.TxId, IsPhase2Valid)]
txsOf = \case
  BlockDijkstra blk ->
    let SL.Block _ body = shelleyBlockRaw blk
     in map txKey (toList (body ^. Core.txSeqBlockBodyL))
  _ -> []

txKey :: Tx TopTx DijkstraEra -> (SL.TxId, IsPhase2Valid)
txKey tx = (Core.txIdTx tx, tx ^. isPhase2ValidTxL)
