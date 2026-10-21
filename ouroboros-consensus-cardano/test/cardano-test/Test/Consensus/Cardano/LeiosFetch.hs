{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The node's LeiosFetch codec against the CDDL.
module Test.Consensus.Cardano.LeiosFetch (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (AnnotatedCodec, AnyMessage (..), CodecF (..))
import Ouroboros.Consensus.Block (SlotNo (..))
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
