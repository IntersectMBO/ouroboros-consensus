{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The forging credentials, as the consensus layer wants them.
--
-- Reading the credential files and turning their bytes into key material is
-- @cardano-config@'s job (@cardano-config:keys@ decodes, and
-- "Cardano.Configuration.Credentials" opens the files). What is left here is
-- the last step, which cannot live there because it produces consensus types:
-- mapping that key material onto 'ByronLeaderCredentials' and
-- 'ShelleyLeaderCredentials'.
module Cardano.Tools.Credentials
  ( LeaderCredentials (..)
  , readLeaderCredentials
  ) where

import qualified Cardano.Chain.Genesis as Byron.Genesis
import Cardano.Config.Key.Byron (SigningKey (ByronSigningKey))
import Cardano.Config.Key.OperationalCertificate (OperationalCertificate (..))
import Cardano.Config.Key.Praos (SigningKey (KesSigningKey, VrfSigningKey))
import Cardano.Config.Key.Shelley (VerificationKey (StakePoolVerificationKey))
import qualified Cardano.Configuration.CliArgs as CLI
import qualified Cardano.Configuration.Credentials as Creds
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Protocol.Crypto (StandardCrypto)
import Data.Bifunctor (first)
import qualified Data.Text as Text
import Ouroboros.Consensus.Byron.Node
  ( ByronLeaderCredentials
  , mkByronLeaderCredentials
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosCanBeLeader (..)
  , PraosCredentialsSource (..)
  )
import Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials (..))

-- | The credentials a Cardano protocol forges with: at most one Byron-era set,
-- and any number of Shelley-based ones.
data LeaderCredentials = LeaderCredentials
  { byronLeaderCredentials :: Maybe ByronLeaderCredentials
  , shelleyLeaderCredentials :: [ShelleyLeaderCredentials StandardCrypto]
  }

-- | Read the credential files named by the configuration and map them onto the
-- consensus leader credentials.
--
-- The Byron genesis is needed because Byron credentials are only meaningful
-- relative to it: the delegation certificate has to be issued by one of its
-- genesis keys.
readLeaderCredentials ::
  Byron.Genesis.Config ->
  CLI.Credentials ->
  IO (Either String LeaderCredentials)
readLeaderCredentials byronGenesis creds =
  either (Left . Text.unpack . Creds.renderCredentialsError) fromDecoded
    <$> Creds.readCredentials creds
 where
  fromDecoded decoded = do
    byron <- traverse (mkByronCredentials byronGenesis) (Creds.byronCredentials decoded)
    pure
      LeaderCredentials
        { byronLeaderCredentials = byron
        , shelleyLeaderCredentials =
            map mkShelleyCredentials (Creds.shelleyCredentials decoded)
        }

mkByronCredentials ::
  Byron.Genesis.Config -> Creds.ByronCredentials -> Either String ByronLeaderCredentials
mkByronCredentials byronGenesis Creds.ByronCredentials{Creds.byronCertificate, Creds.byronSigningKey} =
  first (\err -> "Byron leader credentials error: " <> show err) $
    mkByronLeaderCredentials byronGenesis signingKey byronCertificate "Byron"
 where
  ByronSigningKey signingKey = byronSigningKey

mkShelleyCredentials :: Creds.ShelleyCredentials -> ShelleyLeaderCredentials StandardCrypto
mkShelleyCredentials creds =
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader =
        PraosCanBeLeader
          { praosCanBeLeaderColdVerKey = coerceKeyRole coldVerKey
          , praosCanBeLeaderSignKeyVRF = vrfSignKey
          , praosCanBeLeaderCredentialsSource = credentialsSource
          }
    , -- Not the provenance label 'Creds.credentialsLabel' carries: consensus uses
      -- this one to name the era these credentials forge in.
      shelleyLeaderCredentialsLabel = "Shelley"
    }
 where
  OperationalCertificate opCert (StakePoolVerificationKey coldVerKey) =
    Creds.operationalCertificate creds
  VrfSigningKey vrfSignKey = Creds.vrfSigningKey creds
  credentialsSource = case Creds.kesCredentials creds of
    Creds.KESCredentialsKey (KesSigningKey kesSignKey) ->
      PraosCredentialsUnsound opCert kesSignKey
    Creds.KESCredentialsAgent socketPath ->
      PraosCredentialsAgent socketPath
