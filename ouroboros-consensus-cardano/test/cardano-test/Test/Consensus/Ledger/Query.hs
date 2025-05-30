{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Serialization tests for local state queries and results.
module Test.Consensus.Ledger.Query (tests) where

import Codec.CBOR.Read (DeserialiseFailure)
import Control.Monad.Class.MonadThrow (throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16.Lazy as LBase16
import qualified Data.ByteString.Lazy as LBS
import Network.TypedProtocol.Stateful.Codec (Codec (encode), SomeMessage (..), decode, runDecoder)
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Query (Query (GetSystemStart))
import Ouroboros.Consensus.Network.NodeToClient (cStateQueryCodec, clientCodecs)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.NodeToClient (NodeToClientVersion (NodeToClientV_20))
import Ouroboros.Network.Protocol.LocalStateQuery.Type
  ( LocalStateQuery
  , Message (MsgQuery, MsgResult)
  , SingLocalStateQuery (SingAcquired, SingQuerying)
  , State (StateAcquired, StateQuerying)
  )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import System.IO.Temp (withSystemTempDirectory)
import Test.Cardano.Ledger.Binary.Cddl (validateCddlConformance)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

type Block = CardanoBlock StandardCrypto

-- | We only test the latest query version and n2c protocol version against the API.
stateQueryCodec ::
  Stateful.Codec
    (LocalStateQuery Block (Point Block) (Query Block))
    DeserialiseFailure
    LocalStateQuery.State
    IO
    LBS.ByteString
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
      -- TODO: reuse cddlRoundTripExpectation from cardano-ledger-binary?
      testGroup
        "getSystemStart"
        [ testCase "Query example" $ do
            -- Decode blueprint example query
            exampleHex <- LBS.readFile $ blueprintDir </> "examples" </> "getSystemStart" </> "query.cddl"
            decoder <- decode stateQueryCodec SingAcquired StateAcquired
            runDecoder [LBase16.decodeLenient exampleHex] decoder >>= \case
              Left err -> throwIO err
              -- XXX: can't bind monadically into test case?
              Right (SomeMessage (MsgQuery query)) -> do
                -- Re-encode query
                let encoded = encode stateQueryCodec StateAcquired (MsgQuery query)
                -- Check whether re-encoded cbor is equal
                -- NOTE: Using hex-encoded bytes for better debugging
                assertEqual "re-encoded query" exampleHex (LBase16.encode encoded)
                -- Validate against composed cddl
                -- REVIEW: Do we want to use $sockets or the ;# include module extension?
                protocolCddlBytes <- BS.readFile $ blueprintDir </> "local-state-query.cddl"
                queryCddlBytes <- BS.readFile $ blueprintDir </> "getSystemStart.cddl"
                withSystemTempDirectory "ouroboros-consensus-cardano-test" $ \dir -> do
                  let cddlFile = dir <> "/composed.cddl"
                  BS.writeFile cddlFile (protocolCddlBytes <> queryCddlBytes)
                  validateCddlConformance cddlFile encoded >>= either fail (const $ pure ())
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
            -- Decode blueprint example query
            exampleHex <- LBS.readFile $ blueprintDir </> "local-state-query.cddl"
            decoder <- decode stateQueryCodec SingQuerying (StateQuerying GetSystemStart)
            result <-
              runDecoder [LBase16.decodeLenient exampleHex] decoder >>= \case
                Left err -> throwIO err
                Right (SomeMessage (MsgResult result)) -> pure result

            -- Re-encode result
            let encoded = encode stateQueryCodec (StateQuerying GetSystemStart) (MsgResult result)
            -- Check whether re-encoded cbor is equal
            -- NOTE: Using hex-encoded bytes for better debugging
            -- FIXME: this fails because 'fromOrdinalDate' is used in 'FromCBOR UTCTime'
            assertEqual "re-encoded result" exampleHex (LBase16.encode encoded)

            -- Validate against composed cddl
            -- REVIEW: Do we want to use $sockets or the ;# include module extension?
            protocolCddlBytes <- BS.readFile $ blueprintDir </> "local-state-query.cddl"
            queryCddlBytes <- BS.readFile $ blueprintDir </> "getSystemStart.cddl"
            withSystemTempDirectory "ouroboros-consensus-cardano-test" $ \dir -> do
              let cddlFile = dir <> "/composed.cddl"
              BS.writeFile cddlFile (protocolCddlBytes <> queryCddlBytes)
              validateCddlConformance cddlFile encoded >>= either fail (const $ pure ())
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

blueprintDir :: FilePath
blueprintDir = "../cardano-blueprint/src/client/node-to-client/state-query"
