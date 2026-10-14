{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | The node's LeiosNotify codec, at the Cardano block, against the CDDL.
module Test.Consensus.Cardano.LeiosNotify (tests) where

import Cardano.Crypto.DSIGN.Class (genKeyDSIGN, signDSIGN)
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios (LeiosSeatId (..))
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Short as SBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Network.TypedProtocol.Codec (AnyMessage (..), Codec, CodecF (..))
import Ouroboros.Consensus.Block (Header, SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosVote (..)
  , RankingBlockHash (..)
  , decodeLeiosPoint
  , decodeLeiosVote
  , encodeLeiosPoint
  , encodeLeiosVote
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( NodeToNodeVersion (..)
  , SupportedNetworkProtocolVersion (..)
  )
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
import qualified Test.Consensus.Cardano.Examples as Cardano.Examples
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Serialisation.CDDL (cddlTest, isCDDLCDisabled)
import Test.Util.Serialisation.Examples (Examples (..))

type Blk = CardanoBlock StandardCrypto

tests :: TestTree
tests =
  testGroup "LeiosNotify" $
    [ testProperty "the node's codec encodes each message as the CDDL specifies" $
        prop_nodeCodecMatchesCDDL
    | not isCDDLCDisabled
    ]

prop_nodeCodecMatchesCDDL :: Property
prop_nodeCodecMatchesCDDL =
  forAll genMessage $ \(AnyMessage msg) ->
    ioProperty $ do
      let codec ::
            Codec (LeiosNotify LeiosPoint (Header Blk) LeiosVote) DeserialiseFailure IO ByteString
          codec =
            codecLeiosNotify
              encodeLeiosPoint
              decodeLeiosPoint
              (encodeNodeToNode Cardano.Examples.codecConfig leiosVersion)
              (decodeNodeToNode Cardano.Examples.codecConfig leiosVersion)
              encodeLeiosVote
              decodeLeiosVote
      result <-
        cddlTest
          (pure $ toStrict $ encode codec msg)
          "ntnleiosnotify.cddl"
          (T.pack "leiosNotifyMessage")
      pure $ either (`counterexample` False) (const $ property True) result
 where
  genMessage :: Gen (AnyMessage (LeiosNotify LeiosPoint (Header Blk) LeiosVote))
  genMessage =
    oneof
      [ pure $ AnyMessage MsgDone
      , pure $ AnyMessage MsgQuit
      , pure $ AnyMessage MsgCanceled
      , pure $ AnyMessage MsgLeiosNotificationRequestNext
      , -- The golden tests check these headers against the CDDL.
        --
        -- TODO Generate the headers instead, Dijkstra's among them, once
        -- TODO(dijkstra_serialisation) is resolved and the Shelley header
        -- generators choose a major protocol version the era admits, as the
        -- ledger's @genEraProtVer@ does.
        AnyMessage . MsgLeiosBlockAnnouncement
          <$> elements (map snd (exampleHeader Cardano.Examples.examples))
      , fmap AnyMessage $ MsgLeiosBlockOffer <$> genPoint <*> arbitrary
      , AnyMessage . MsgLeiosBlockTxsOffer <$> genPoint
      , do
          n <- choose (1, maxLeiosVotesPerMessage)
          AnyMessage . MsgLeiosVotes . NE.fromList <$> vectorOf n genVote
      ]

  -- LeiosNotify runs only from 'NodeToNodeV_16' on.
  leiosVersion = supportedNodeToNodeVersions (Proxy @Blk) Map.! NodeToNodeV_16

  genPoint = MkLeiosPoint <$> (SlotNo <$> arbitrary) <*> genHash

  -- The codec does not check a vote's signature, so any key will do.
  genVote = do
    rbHash <- MkRankingBlockHash <$> genHash
    seat <- LeiosSeatId <$> arbitrary
    pure $
      MkLeiosVote rbHash seat $
        signDSIGN
          ()
          rbHash
          (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0)))

  genHash = UnsafeHash . SBS.pack <$> vectorOf 32 arbitrary
