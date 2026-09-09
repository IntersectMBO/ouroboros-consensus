{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The BLS signing key that the forger votes with.
module Cardano.Tools.DBSynthesizer.BlsKey
  ( readBlsSigningKey
  ) where

import Cardano.Api.Any
  ( FromCBOR
  , HasTypeProxy (..)
  , SerialiseAsCBOR
  , ToCBOR
  , displayError
  )
import Cardano.Api.SerialiseTextEnvelope
  ( HasTextEnvelope (..)
  , readFileTextEnvelope
  )
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import Cardano.Crypto.Leios (LeiosDSIGN, LeiosSigningKey)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy (..))
import Data.String (fromString)

-- | A stake pool's BLS signing key, as @cardano-cli node key-gen-BLS@ writes
-- it. The pool registers the matching verification key as its @leiosKey@, and
-- the ledger builds the voting committee from those registrations.
--
-- @Cardano.Api.Key.Internal.Leios@ holds the same type, with the same envelope
-- type. That module is not exposed, and @Cardano.Api.Key@ re-exports no
-- constructor for its signing key, so this copy tracks it by hand.
newtype BlsSigningKey = BlsSigningKey {unBlsSigningKey :: LeiosSigningKey}
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy BlsSigningKey where
  data AsType BlsSigningKey = AsBlsSigningKey
  proxyToAsType _ = AsBlsSigningKey

instance HasTextEnvelope BlsSigningKey where
  -- This is "BlsSigningKey_bls12-381-BLS-Signature-Mininimal-Signature-Size",
  -- the type that cardano-cli writes into the key file. The typo in "Mininimal"
  -- is upstream.
  textEnvelopeType _ =
    "BlsSigningKey_" <> fromString (Crypto.algorithmNameDSIGN (Proxy @LeiosDSIGN))

-- | Read the key from its JSON envelope.
--
-- The envelope names its own type, and this reader checks it. If you pass a VRF
-- or KES key by mistake, the error names both types.
readBlsSigningKey :: FilePath -> IO (Either String LeiosSigningKey)
readBlsSigningKey path =
  bimap displayError unBlsSigningKey <$> readFileTextEnvelope AsBlsSigningKey path
