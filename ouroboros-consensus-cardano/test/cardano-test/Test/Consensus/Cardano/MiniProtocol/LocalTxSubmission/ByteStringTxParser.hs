{-# LANGUAGE OverloadedStrings #-}
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


import           Cardano.Chain.Slotting (EpochSlots (..))
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Data.ByteString.Base16.Lazy (decodeLenient)
import qualified Data.ByteString.Lazy as BL
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (decodeNodeToClient))
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Shelley.Ledger
                     (CodecConfig (ShelleyCodecConfig))
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Text.Pretty.Simple (pPrint)

cardanoCodecCfg :: CodecConfig (CardanoBlock StandardCrypto)
cardanoCodecCfg =
  CardanoCodecConfig
            (ByronCodecConfig (EpochSlots 21600))
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
    cborDecoder = decodeNodeToClient cardanoCodecCfg CardanoNodeToClientVersion11

printDeserializedTx :: BL.ByteString -> IO ()
printDeserializedTx bs =
  case deserialiseTx bs of
    Left  err             -> pPrint err
    Right (_rest, result) -> pPrint result
