{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Utility functions to deserialize the hexadecimal representation of a CBOR
-- encoded Cardano transaction.
--
-- To use from the repl run:
--
-- > cabal repl ouroboros-consensus-cardano:test:cardano-test
-- > import Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.ByteStringTxParser
--
module Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.ByteStringTxParser (
    cardanoCodecCfg
  , deserialiseTx
  , printDeserializedTx
  ) where

import           Cardano.Chain.Epoch.File (mainnetEpochSlots)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Data.ByteString.Base16.Lazy (decodeLenient)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (latestReleasedNodeVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (decodeNodeToClient))
import           Ouroboros.Consensus.Shelley.Ledger
                     (CodecConfig (ShelleyCodecConfig))
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Text.Pretty.Simple (pPrint)

cardanoCodecCfg :: CodecConfig (CardanoBlock StandardCrypto)
cardanoCodecCfg =
  CardanoCodecConfig
            (ByronCodecConfig mainnetEpochSlots)
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig

deserialiseTx ::
     BL.ByteString
  -> Either DeserialiseFailure (BL.ByteString, GenTx (CardanoBlock StandardCrypto))
deserialiseTx = deserialiseFromBytes cborDecoder . decodeLenient
  where
    cborDecoder = decodeNodeToClient cardanoCodecCfg latestReleasedBlockNodeToClientVersion
    latestReleasedBlockNodeToClientVersion =
        case latestReleasedNodeVersion p of
          (_, Just n2c) -> supportedNodeToClientVersions p Map.! n2c
          _             -> error "no latest released Cardano NodeToClient version"
      where
        p = Proxy @(CardanoBlock StandardCrypto)

printDeserializedTx :: BL.ByteString -> IO ()
printDeserializedTx bs =
  case deserialiseTx bs of
    Left  err             -> pPrint err
    Right (_rest, result) -> pPrint result
