{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The forging credentials, as the consensus layer wants them.
--
-- Which credential files a tool was pointed at is @cardano-config@'s business
-- (they are ordinary node command-line options, so they arrive here as a
-- 'CLI.Credentials'). Turning the bytes in those files into key material is
-- @cardano-keys@'s business. What is left for this module is the last step,
-- which cannot live in either of them because it produces consensus types:
-- mapping that key material onto 'ByronLeaderCredentials' and
-- 'ShelleyLeaderCredentials'.
--
-- Note that @cardano-keys@ is currently a package skeleton: it exposes a
-- placeholder module and none of the key types or decoders this module needs
-- from it. Everything around the decoding is therefore real -- which
-- combinations of credential files are accepted, which file contributes what,
-- and how the result maps onto the consensus types -- but the decoders
-- themselves are the stubs under \"What cardano-keys still owes us\" below, so
-- that a tool actually asked to forge fails on 'undefined'. Filling those in is
-- all that the switch to a grown-up @cardano-keys@ should take.
module Cardano.Tools.Credentials
  ( LeaderCredentials (..)
  , readLeaderCredentials
  ) where

import qualified Cardano.Chain.Delegation as Byron.Delegation
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Configuration.CliArgs as CLI
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.VRF as VRF
-- The dependency is deliberate although the skeleton exports nothing we can
-- use yet: it is what the stubs at the bottom of this module are to be written
-- against, so keeping it wired up means growing them is an import change.
import Cardano.Keys ()
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Keys (KeyRole (StakePool), VKey, coerceKeyRole)
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Bifunctor (first)
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

-- | Read the credential files named on the command line and map them onto the
-- consensus leader credentials.
--
-- As in the node, the Shelley-based credentials are the sum of what the
-- individual @--shelley-*@ options name and what the bulk credentials file
-- holds, and supplying no credential at all is not an error: it simply yields
-- no forgers.
--
-- The Byron genesis is needed because Byron credentials are only meaningful
-- relative to it: the delegation certificate has to be issued by one of its
-- genesis keys.
readLeaderCredentials ::
  Byron.Genesis.Config ->
  CLI.Credentials ->
  IO (Either String LeaderCredentials)
readLeaderCredentials byronGenesis creds = runExceptT $ do
  byron <- readByron byronGenesis creds
  shelley <- readShelley creds
  bulk <- readShelleyBulk creds
  pure
    LeaderCredentials
      { byronLeaderCredentials = byron
      , shelleyLeaderCredentials = shelley <> bulk
      }

--
-- Byron
--

-- | The Byron delegation certificate and the signing key it delegates to, which
-- are only useful together: either both are given or neither is.
readByron ::
  Byron.Genesis.Config ->
  CLI.Credentials ->
  ExceptT String IO (Maybe ByronLeaderCredentials)
readByron byronGenesis creds =
  case (CLI.byronDelegationCertificate creds, CLI.byronSigningKey creds) of
    (SNothing, SNothing) -> pure Nothing
    (SJust _, SNothing) -> throwE $ missingOption "byron-signing-key"
    (SNothing, SJust _) -> throwE $ missingOption "byron-delegation-certificate"
    (SJust certFile, SJust keyFile) -> do
      cert <- readByronDelegationCertificate certFile
      signingKey <- readByronSigningKey keyFile
      fmap Just . except . first renderError $
        mkByronLeaderCredentials byronGenesis signingKey cert "Byron"
 where
  renderError err = "Byron leader credentials error: " <> show err

--
-- Shelley
--

-- | The Shelley-based credentials named by the individual @--shelley-*@
-- options: the operational certificate, the VRF signing key and the KES source
-- are useful only together, so either all three are given or none is.
readShelley ::
  CLI.Credentials -> ExceptT String IO [ShelleyLeaderCredentials StandardCrypto]
readShelley creds =
  case ( CLI.shelleyOperationalCertificate creds
       , CLI.shelleyVRFKey creds
       , CLI.shelleyKES creds
       ) of
    (SNothing, SNothing, SNothing) -> pure []
    (SNothing, _, _) -> throwE $ missingOption "shelley-operational-certificate"
    (_, SNothing, _) -> throwE $ missingOption "shelley-vrf-key"
    (_, _, SNothing) -> throwE $ missingOption "shelley-kes-key"
    (SJust certFile, SJust vrfFile, SJust kesSource) -> do
      (opCert, coldVerKey) <- readOperationalCertificate certFile
      vrfSignKey <- readVrfSigningKey vrfFile
      credentialsSource <- case kesSource of
        -- The unsound variant: the KES signing key sits in a file on disk,
        -- rather than never leaving a KES agent's memory.
        CLI.KESKeyFilePath kesFile ->
          PraosCredentialsUnsound opCert <$> readKesSigningKey kesFile
        CLI.KESAgentSocketPath socketPath ->
          pure $ PraosCredentialsAgent socketPath
      pure [mkShelleyCredentials coldVerKey vrfSignKey credentialsSource]

-- | The Shelley-based credentials in the bulk credentials file, which holds any
-- number of them. A bulk file only ever carries KES signing keys, never a KES
-- agent's socket.
readShelleyBulk ::
  CLI.Credentials -> ExceptT String IO [ShelleyLeaderCredentials StandardCrypto]
readShelleyBulk creds = case CLI.bulkCredentialsFile creds of
  SNothing -> pure []
  SJust file -> map fromBulkEntry <$> readBulkCredentials file
 where
  fromBulkEntry (opCert, coldVerKey, vrfSignKey, kesSignKey) =
    mkShelleyCredentials
      coldVerKey
      vrfSignKey
      (PraosCredentialsUnsound opCert kesSignKey)

mkShelleyCredentials ::
  VKey StakePool ->
  VRF.SignKeyVRF (VRF StandardCrypto) ->
  PraosCredentialsSource StandardCrypto ->
  ShelleyLeaderCredentials StandardCrypto
mkShelleyCredentials coldVerKey vrfSignKey credentialsSource =
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader =
        PraosCanBeLeader
          { praosCanBeLeaderColdVerKey = coerceKeyRole coldVerKey
          , praosCanBeLeaderSignKeyVRF = vrfSignKey
          , praosCanBeLeaderCredentialsSource = credentialsSource
          }
    , -- Consensus uses this to name the era these credentials forge in.
      shelleyLeaderCredentialsLabel = "Shelley"
    }

--
-- What cardano-keys still owes us
--

-- Each of the following reads one credential file and decodes it. Together they
-- are the entire surface this module needs from @cardano-keys@, and all of them
-- are stubbed out until that package grows the key types and the text-envelope
-- decoder they are written against.

-- TODO @js: implement via cardano-keys, which is to hold the Byron signing key
-- type and its decoder.
readByronSigningKey :: FilePath -> ExceptT String IO Byron.Crypto.SigningKey
readByronSigningKey = undefined

-- TODO @js: implement via cardano-keys, which is to hold the canonical-JSON
-- decoder for a Byron delegation certificate.
readByronDelegationCertificate ::
  FilePath -> ExceptT String IO Byron.Delegation.Certificate
readByronDelegationCertificate = undefined

-- | The operational certificate together with the stake pool cold verification
-- key it names, which the file carries alongside it.
--
-- TODO @js: implement via cardano-keys, which is to hold the operational
-- certificate type and its decoder.
readOperationalCertificate ::
  FilePath -> ExceptT String IO (OCert.OCert StandardCrypto, VKey StakePool)
readOperationalCertificate = undefined

-- TODO @js: implement via cardano-keys, which is to hold the VRF key types and
-- their decoders.
readVrfSigningKey :: FilePath -> ExceptT String IO (VRF.SignKeyVRF (VRF StandardCrypto))
readVrfSigningKey = undefined

-- TODO @js: implement via cardano-keys, which is to hold the KES key types and
-- their decoders.
readKesSigningKey ::
  FilePath -> ExceptT String IO (KES.UnsoundPureSignKeyKES (KES StandardCrypto))
readKesSigningKey = undefined

-- | The bulk credentials file: a JSON array of operational
-- certificate\/VRF\/KES triples, decoded here into the same pieces the
-- individual options yield.
--
-- TODO @js: implement via cardano-keys, which is to hold the bulk file's
-- format alongside the decoders for the three envelopes it nests.
readBulkCredentials ::
  FilePath ->
  ExceptT
    String
    IO
    [ ( OCert.OCert StandardCrypto
      , VKey StakePool
      , VRF.SignKeyVRF (VRF StandardCrypto)
      , KES.UnsoundPureSignKeyKES (KES StandardCrypto)
      )
    ]
readBulkCredentials = undefined

--
-- Errors
--

-- | The credential options only make sense in complete sets, so a partial set
-- is reported by naming the option that would complete it.
missingOption :: String -> String
missingOption option =
  "To forge blocks, --" <> option <> " must also be specified"
