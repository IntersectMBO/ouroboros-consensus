{-# LANGUAGE ScopedTypeVariables #-}

-- | Transaction generation for 'Cardano.Tools.DBSynthesizer.Run.synthesize'.
--
-- If the tool has no payment signing key, it forges empty blocks. If it has
-- one, each forged block spends the output that the block before it made, and
-- makes one new output.
module Cardano.Tools.DBSynthesizer.TxGen
  ( mkRespendTxGen
  ) where

import Cardano.Api.Any (displayError)
import Cardano.Api.Key (AsType (AsSigningKey), Key (SigningKey))
import Cardano.Api.KeysShelley (AsType (AsPaymentKey), PaymentKey)
import Cardano.Api.SerialiseTextEnvelope (readFileTextEnvelope)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Tools.DBSynthesizer.Forging (GenTxs)
import Data.Bifunctor (first)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig)

-- | Build the generator that the forge loop runs on each slot that the tool
-- leads.
--
-- If the path is 'Nothing', the generator returns no transactions.
mkRespendTxGen ::
  Maybe FilePath ->
  IO
    ( Either
        String
        (TopLevelConfig (CardanoBlock StandardCrypto) -> GenTxs (CardanoBlock StandardCrypto))
    )
mkRespendTxGen Nothing = pure $ Right $ \_cfg _slot _forker _ticked -> pure []
mkRespendTxGen (Just keyFile) = fmap respendTxGen <$> readPaymentSigningKey keyFile

-- | Read a payment signing key from the JSON key file that
-- @cardano-cli address key-gen@ writes.
readPaymentSigningKey :: FilePath -> IO (Either String (SigningKey PaymentKey))
readPaymentSigningKey path =
  first displayError <$> readFileTextEnvelope (AsSigningKey AsPaymentKey) path

-- | Spend the first output that this key owns, and pay it back to the same
-- address. The generator makes one transaction for each block.
respendTxGen ::
  SigningKey PaymentKey ->
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  GenTxs (CardanoBlock StandardCrypto)
respendTxGen = undefined
