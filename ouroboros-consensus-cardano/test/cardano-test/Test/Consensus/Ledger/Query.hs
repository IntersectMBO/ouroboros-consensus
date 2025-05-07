{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Serialization tests for local state queries and results.
module Test.Consensus.Ledger.Query (tests) where

import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (throwIO)
import qualified Data.ByteString.Base16.Lazy as LBase16
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Network.TypedProtocol.Stateful.Codec (SomeMessage (..), decode, runDecoder)
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import Ouroboros.Consensus.Block (HeaderHash, Point, StandardHash)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, ConwayEra, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Query (
    BlockSupportsLedgerQuery,
    Query,
    QueryVersion (..),
    queryDecodeNodeToClient,
    queryEncodeNodeToClient,
 )
import Ouroboros.Consensus.Network.NodeToClient (cStateQueryCodec, clientCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion)
import Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.NodeToClient (NodeToClientVersion (NodeToClientV_20))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery, Message (MsgQuery))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

type Block = CardanoBlock StandardCrypto

-- | We only test the latest query version and n2c protocol version against the API.
stateQueryCodec ::
    (MonadST m) =>
    Stateful.Codec (LocalStateQuery Block (Point Block) (Query Block)) DeserialiseFailure LocalStateQuery.State m LBS.ByteString
stateQueryCodec =
    cStateQueryCodec $
        clientCodecs mockCodecConfig mockBlockNodeToClientVersion latestN2CVersion
  where
    latestN2CVersion = NodeToClientV_20

    -- NOTE: Not used for non-block queries
    mockCodecConfig = error "CodecConfig used unexpectedly"

    -- NOTE: Not used for non-block queries
    mockBlockNodeToClientVersion = error "BlockNodeToClientVersion used unexpectedly"

tests :: TestTree
tests =
    testGroup
        "LocalStateQuery"
        [ -- TODO: do this for all queries
          testGroup
            "getSystemStart"
            [ testCase "Query example" $ do
                -- Decode blueprint example query
                exampleHex <- LBS.readFile "../cardano-blueprint/src/api/examples/getSystemStart/query.cbor"
                print exampleHex
                decoder <- decode stateQueryCodec LocalStateQuery.SingAcquired LocalStateQuery.StateAcquired
                runDecoder [LBase16.decodeLenient exampleHex] decoder >>= \case
                    Left err -> throwIO err
                    -- XXX: can't bind monadically into test case?
                    Right (SomeMessage (MsgQuery query)) -> do
                        print query
                        -- TODO: encode query with queryEncodeNodeToClient
                        let encoded =
                                toLazyByteString $
                                    undefined
                        -- queryEncodeNodeToClient @(MockBlock ())
                        --   mockCodecConfig
                        --   latestVersion
                        --   mockBlockNodeToClientVersion
                        --   someQuery
                        print $ LBase16.encode encoded
                        -- TODO: check whether cbor is equal
                        -- TODO: validate cbor against cddl
                        fail "TODO"
                    Right (SomeMessage _) -> fail "Decoded unexpected message"
            , testCase "Query roundtrip" $ do
                -- TODO: generate arbitrary query terms given cddl
                -- TODO: decode with queryDecodeNodeToClient
                -- TODO: encode query with queryEncodeNodeToClient
                -- TODO: check whether cbor is equal
                -- TODO: validate cbor against cddl
                fail "TODO"
            , testCase "Query conformance" $ do
                -- TODO: generate arbitrary 'GetSystemStart :: Query Block SystemStart' values
                -- TODO: encode with queryEncodeNodeToClient
                -- TODO: validate cbor against cddl
                fail "TODO"
            , testCase "Result example" $ do
                -- TODO: decode blueprint example result with decodeResult
                -- TODO: encode result with encodeResult
                -- TODO: check whether cbor is equal
                -- TODO: validate cbor against cddl
                fail "TODO"
            , testCase "Query roundtrip" $ do
                -- TODO: generate arbitrary result terms given cddl
                -- TODO: decode result with decodeResult
                -- TODO: encode result with encodeResult
                -- TODO: check whether cbor is equal
                -- TODO: validate cbor against cddl
                fail "TODO"
            , testCase "Result conformance" $ do
                -- TODO: generate arbitrary 'SystemStart' values
                -- TODO: encode result with encodeResult
                -- TODO: validate cbor against cddl
                fail "TODO"
            ]
        ]
