{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The node's LeiosFetch codec against the CDDL.
module Test.Consensus.Cardano.LeiosFetch (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Core (bodyTxL, certsTxBodyL)
import Cardano.Ledger.Dijkstra.TxBody (subTransactionsTxBodyL)
import Codec.CBOR.Read (DeserialiseFailure)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Lens.Micro ((&), (.~))
import Network.TypedProtocol.Codec (AnyMessage (..), Codec, CodecF (..))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised
  , decodeLeiosPoint
  , decodeLeiosTx
  , decodeSerialisedEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , encodeSerialisedEndorserBlock
  , maxTxsPerEndorserBlock
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Shelley.Ledger.Forge (serializeTxForLeios)
import Test.Cardano.Ledger.Dijkstra.Examples (exampleDijkstraTx)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Serialisation.CDDL (cddlTest, isCDDLCDisabled)

tests :: TestTree
tests =
  testGroup "LeiosFetch" $
    [ testProperty "the node's codec encodes each message as the CDDL specifies" $
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
    Codec (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx) DeserialiseFailure IO ByteString
  nodeCodec =
    codecLeiosFetch
      maxTxsPerEndorserBlock
      encodeLeiosPoint
      decodeLeiosPoint
      encodeSerialisedEndorserBlock
      decodeSerialisedEndorserBlock
      encodeLeiosTx
      decodeLeiosTx

  genMessage =
    oneof
      [ pure $ AnyMessage MsgDone
      , AnyMessage . MsgLeiosBlockRequest <$> genPoint
      , AnyMessage . MsgLeiosBlock <$> genEb
      , fmap AnyMessage $ MsgLeiosBlockTxsRequest <$> genPoint <*> genBitmaps
      , AnyMessage . MsgLeiosBlockTxs . V.fromList <$> listOf (pure tx)
      ]

  genPoint = MkLeiosPoint <$> (SlotNo <$> arbitrary) <*> genHash

  -- strictly ascending indices, each with a non-zero bitmap, as the decoder
  -- requires
  genBitmaps = do
    indices <- Set.toAscList . Set.fromList <$> arbitrary
    traverse (\i -> (,) i . getPositive <$> arbitrary) indices

  genEb = serialiseEndorserBlock . mkEndorserBlock <$> listOf1 ((,) <$> genHash <*> arbitrary)

  -- As the forge writes it, but without pool registrations, which cuddle
  -- rejects for the optional bls_key amid their fields.
  tx =
    MkLeiosTx . serializeTxForLeios $
      exampleDijkstraTx
        & bodyTxL . certsTxBodyL .~ mempty
        & bodyTxL . subTransactionsTxBodyL .~ mempty

  genHash = UnsafeHash . SBS.pack <$> vectorOf 32 arbitrary
