{-# LANGUAGE ScopedTypeVariables #-}

-- | Transaction generation for 'Cardano.Tools.DBSynthesizer.Run.synthesize'.
--
-- If the tool has no payment signing key, it forges empty blocks. If it has
-- one, each forged block spends the output that the block before it made, and
-- makes one new output.
module Cardano.Tools.DBSynthesizer.TxGen
  ( mkRespendTxGen
  ) where

import Cardano.Api.Key (Key (SigningKey))
import Cardano.Api.KeysShelley (PaymentKey)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Tools.DBSynthesizer.Forging (GenTxs)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig)

-- | Build the generator that the forge loop runs on each slot that the tool
-- leads.
--
-- If the path is 'Nothing', the generator returns no transactions. This is the
-- behaviour of 'synthesize' before this module existed.
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
readPaymentSigningKey = undefined

-- | Spend the first output that this key owns, and pay it back to the same
-- address. The generator makes one transaction for each block.
respendTxGen ::
  SigningKey PaymentKey ->
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  GenTxs (CardanoBlock StandardCrypto)
respendTxGen = undefined
