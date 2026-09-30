{-# LANGUAGE LambdaCase #-}

-- | What the transaction generators of 'Cardano.Tools.DBSynthesizer.Run.synthesize'
-- share.
--
-- The generators themselves are 'Cardano.Tools.DBSynthesizer.TxGen.Respend',
-- which builds a chain of transactions in the forge loop, and
-- 'Cardano.Tools.DBSynthesizer.TxGen.File', which replays transactions written
-- ahead of time.
module Cardano.Tools.DBSynthesizer.TxGen
  ( ownsAddr
  , readPaymentSigningKey
  ) where

import Cardano.Api.Any (displayError)
import Cardano.Api.Key (AsType (AsSigningKey), Key (SigningKey))
import Cardano.Api.KeysShelley (AsType (AsPaymentKey), PaymentKey)
import Cardano.Api.SerialiseTextEnvelope (readFileTextEnvelope)
import Cardano.Crypto.DSIGN (SignKeyDSIGN)
import Cardano.Ledger.Api (Addr (Addr))
import qualified Cardano.Ledger.Keys as LK
import Data.Bifunctor (first)
import Test.ThreadNet.Infra.Shelley (mkCredential)

-- | Read a payment signing key from the JSON key file that
-- @cardano-cli address key-gen@ writes.
readPaymentSigningKey :: FilePath -> IO (Either String (SigningKey PaymentKey))
readPaymentSigningKey path =
  first displayError <$> readFileTextEnvelope (AsSigningKey AsPaymentKey) path

-- | Whether the payment credential of an address is this key's.
--
-- The staking part is not looked at, so one key owns every base address that
-- shares its payment credential. That is what lets a genesis set up many
-- outputs under one key, which is what a queue of them is built from.
ownsAddr :: SignKeyDSIGN LK.DSIGN -> Addr -> Bool
ownsAddr signKey = \case
  Addr _ credential _ -> credential == mkCredential signKey
  _ -> False
