{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The node's LeiosFetch codec against the CDDL.
module Test.Consensus.Cardano.LeiosFetch (tests) where

import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (AnyMessage (..), Codec, CodecF (..))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash (..)
  , decodeLeiosEb
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeLeiosEb
  , encodeLeiosPoint
  , encodeLeiosTx
  , maxTxsPerEb
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
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
  nodeCodec :: Codec (LeiosFetch LeiosPoint LeiosEb LeiosTx) DeserialiseFailure IO ByteString
  nodeCodec =
    codecLeiosFetch
      maxTxsPerEb
      encodeLeiosPoint
      decodeLeiosPoint
      encodeLeiosEb
      decodeLeiosEb
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

  genPoint = MkLeiosPoint <$> (SlotNo <$> arbitrary) <*> (MkEbHash <$> genHash)

  -- strictly ascending indices, each with a non-zero bitmap, as the decoder
  -- requires
  genBitmaps = do
    indices <- Set.toAscList . Set.fromList <$> arbitrary
    traverse (\i -> (,) i . getPositive <$> arbitrary) indices

  genEb = MkLeiosEb . V.fromList <$> listOf1 ((,) <$> (MkTxHash <$> genHash) <*> arbitrary)

  -- 'tx' must hold CBOR, so the bytes are a CBOR byte string.
  genTx = MkLeiosTx . CBOR.toStrictByteString . CBOR.encodeBytes . BS.pack <$> arbitrary

  genHash = BS.pack <$> vectorOf 32 arbitrary
