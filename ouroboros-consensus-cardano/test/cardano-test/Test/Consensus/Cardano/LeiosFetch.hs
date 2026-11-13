{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | The node's LeiosFetch codec and exact local transaction projection.
module Test.Consensus.Cardano.LeiosFetch (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (TopTx, Tx)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Short as SBS
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (AnnotatedCodec, AnyMessage (..), CodecF (..))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, pattern GenTxConway, pattern GenTxDijkstra)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.SupportsMempool (LedgerSupportsMempool (txBytesForLeios))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , maxTxsPerEndorserBlock
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Ledger.TxBytes (serializeTxForLeios)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Serialisation.CDDL (cddlTest, isCDDLCDisabled)

tests :: TestTree
tests =
  testGroup "LeiosFetch" $
    [ testProperty
        "hard-fork projection preserves exact whole Leios bytes"
        (withNumTests 10 prop_exactLocalProjection)
    , testProperty "hard-fork projection stays inert for Conway" (withNumTests 10 prop_noConwayProjection)
    ]
      <> [ testProperty "the node's codec encodes each message as the CDDL specifies" $
             prop_nodeCodecMatchesCDDL
         | not isCDDLCDisabled
         ]

prop_nodeCodecMatchesCDDL :: Property
prop_nodeCodecMatchesCDDL =
  forAll genMessage $ \(AnyMessage msg) ->
    ioProperty $ do
      result <-
        cddlTest
          (pure $ toStrict $ encode nodeCodec msg)
          "ntnleiosfetch.cddl"
          (T.pack "leiosFetchMessage")
      pure $ either (`counterexample` False) (const $ property True) result
 where
  nodeCodec ::
    AnnotatedCodec (LeiosFetch LeiosPoint EndorserBlock LeiosTx) DeserialiseFailure IO ByteString
  nodeCodec =
    codecLeiosFetch
      maxTxsPerEndorserBlock
      encodeLeiosPoint
      decodeLeiosPoint
      encodeEndorserBlock
      decodeEndorserBlock
      encodeLeiosTx
      decodeLeiosTx

  genMessage =
    oneof
      [ pure $ AnyMessage MsgDone
      , AnyMessage . MsgLeiosBlockRequest <$> genPoint
      , AnyMessage . MsgLeiosBlock <$> genEb
      , fmap AnyMessage $ MsgLeiosBlockTxsRequest <$> genPoint <*> genBitmaps
      , AnyMessage . MsgLeiosBlockTxs . V.fromList <$> listOf genTx
      ]

  genPoint = MkLeiosPoint <$> (SlotNo <$> arbitrary) <*> genHash

  -- strictly ascending indices, each with a non-zero bitmap, as the decoder
  -- requires
  genBitmaps = do
    indices <- Set.toAscList . Set.fromList <$> arbitrary
    traverse (\i -> (,) i . getPositive <$> arbitrary) indices

  genEb = mkEndorserBlock <$> listOf1 ((,) <$> genHash <*> arbitrary)

  genTx = MkLeiosTx . BS.pack <$> arbitrary

  genHash = UnsafeHash . SBS.pack <$> vectorOf 32 arbitrary

prop_exactLocalProjection :: Tx TopTx DijkstraEra -> Property
prop_exactLocalProjection tx =
  txBytesForLeios (Proxy @(CardanoBlock StandardCrypto)) (GenTxDijkstra (mkShelleyTx tx))
    === Just (serializeTxForLeios tx)

prop_noConwayProjection :: Tx TopTx ConwayEra -> Property
prop_noConwayProjection tx =
  txBytesForLeios (Proxy @(CardanoBlock StandardCrypto)) (GenTxConway (mkShelleyTx tx))
    === Nothing
