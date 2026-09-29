{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The forging credentials, as the consensus layer wants them.
--
-- Which files a tool was pointed at comes from @cardano-config@, as ordinary
-- node command-line options. Decoding them is done with the key types vendored
-- from @cardano-api@ under "Cardano.Api", and what is left for here is mapping
-- the key material onto 'ByronLeaderCredentials' and
-- 'ShelleyLeaderCredentials'.
module Cardano.Tools.Credentials
  ( LeaderCredentials (..)
  , readLeaderCredentials
  ) where

import qualified Cardano.Api.Any as Api
import qualified Cardano.Api.Key as Api
import qualified Cardano.Api.KeysByron as Api
import qualified Cardano.Api.KeysPraos as Api
import qualified Cardano.Api.OperationalCertificate as Api
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Chain.Delegation as Byron.Delegation
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Configuration.CliArgs as CLI
import Cardano.Crypto.KES (UnsoundPureSignKeyKES)
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Keys (KeyRole (StakePool), VKey, coerceKeyRole)
import Cardano.Prelude (canonicalDecodePretty)
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Control.Exception (IOException, try)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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

-- | Read the credential files named on the command line and map them onto the
-- consensus leader credentials.
--
-- As in the node, the Shelley-based credentials are the sum of what the
-- individual @--shelley-*@ options name and what the bulk credentials file
-- holds. Supplying none is not an error; it yields no forgers.
--
-- The Byron genesis is needed because a Byron delegation certificate has to be
-- issued by one of its genesis keys.
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
    (_, _, SNothing) ->
      throwE $ missingOption "shelley-kes-key or --shelley-kes-agent-socket"
    (SJust certFile, SJust vrfFile, SJust kesSource) -> do
      opCert <- readTextEnvelope Api.AsOperationalCertificate certFile
      Api.VrfSigningKey vrfSignKey <-
        readTextEnvelope (Api.AsSigningKey Api.AsVrfKey) vrfFile
      credentialsSource <- case kesSource of
        -- The unsound variant: the KES signing key sits in a file on disk
        -- instead of never leaving a KES agent's memory.
        CLI.KESKeyFilePath kesFile -> do
          kesSignKey <-
            readTextEnvelope (Api.AsSigningKey Api.AsUnsoundPureKesKey) kesFile
          checkOpCertKesKey (certFile <> " with " <> kesFile) opCert kesSignKey
          pure $ PraosCredentialsUnsound (opCertOf opCert) (kesSignKeyOf kesSignKey)
        CLI.KESAgentSocketPath socketPath ->
          pure $ PraosCredentialsAgent socketPath
      pure [mkShelleyCredentials (coldVerKeyOf opCert) vrfSignKey credentialsSource]

-- | The Shelley-based credentials in the bulk credentials file, which holds any
-- number of them. A bulk file only ever carries KES signing keys, never a KES
-- agent's socket.
readShelleyBulk ::
  CLI.Credentials -> ExceptT String IO [ShelleyLeaderCredentials StandardCrypto]
readShelleyBulk creds = case CLI.bulkCredentialsFile creds of
  SNothing -> pure []
  SJust file -> do
    entries <- readBulkCredentialsFile file
    traverse (fromBulkEntry file) (zip [0 :: Int ..] entries)
 where
  fromBulkEntry file (index, (teCert, teVrf, teKes)) = do
    -- Which entry of the file it was, because that is all that distinguishes
    -- one pair in a bulk file from the next.
    let source = file <> ", entry " <> show index
    opCert <- decodeTextEnvelope source Api.AsOperationalCertificate teCert
    Api.VrfSigningKey vrfSignKey <-
      decodeTextEnvelope source (Api.AsSigningKey Api.AsVrfKey) teVrf
    kesSignKey <-
      decodeTextEnvelope source (Api.AsSigningKey Api.AsUnsoundPureKesKey) teKes
    checkOpCertKesKey source opCert kesSignKey
    pure $
      mkShelleyCredentials
        (coldVerKeyOf opCert)
        vrfSignKey
        (PraosCredentialsUnsound (opCertOf opCert) (kesSignKeyOf kesSignKey))

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
-- Reading the credential files
--

-- | Read a text envelope: the operational certificate and the VRF and KES
-- signing keys are all stored as one.
readTextEnvelope ::
  Api.HasTextEnvelope a => Api.AsType a -> FilePath -> ExceptT String IO a
readTextEnvelope asType path =
  ExceptT $ first Api.displayError <$> Api.readFileTextEnvelope asType path

-- | Decode a text envelope that was read as part of a larger file.
decodeTextEnvelope ::
  Api.HasTextEnvelope a =>
  -- | Where the envelope came from, for the error message.
  String ->
  Api.AsType a ->
  Api.TextEnvelope ->
  ExceptT String IO a
decodeTextEnvelope source asType =
  except
    . first (Api.displayError . Api.FileError source)
    . Api.deserialiseFromTextEnvelope asType

-- | The bulk credentials file: a JSON list of triples of text envelopes, each
-- holding an operational certificate, a VRF signing key and a KES signing key,
-- in that order.
readBulkCredentialsFile ::
  FilePath ->
  ExceptT String IO [(Api.TextEnvelope, Api.TextEnvelope, Api.TextEnvelope)]
readBulkCredentialsFile path = do
  content <- readFileBytes path
  except . first (\err -> path <> ": " <> err) $ Aeson.eitherDecodeStrict' content

-- | The Byron signing key, which is not a text envelope: the file is the raw
-- CBOR of a legacy Byron @XPrv@.
readByronSigningKey :: FilePath -> ExceptT String IO Byron.Crypto.SigningKey
readByronSigningKey path = do
  content <- readFileBytes path
  case Api.deserialiseFromRawBytes (Api.AsSigningKey Api.AsByronKey) content of
    Nothing -> throwE $ "Byron signing key deserialisation error in: " <> path
    Just (Api.ByronSigningKey signingKey) -> pure signingKey

-- | The Byron delegation certificate, which is neither a text envelope nor
-- CBOR: the file is canonical JSON, like the Byron genesis itself.
readByronDelegationCertificate ::
  FilePath -> ExceptT String IO Byron.Delegation.Certificate
readByronDelegationCertificate path = do
  content <- readFileBytes path
  except . first renderError $ canonicalDecodePretty (LBS.fromStrict content)
 where
  renderError err = "Canonical decode failure in " <> path <> ": " <> Text.unpack err

readFileBytes :: FilePath -> ExceptT String IO BS.ByteString
readFileBytes path =
  ExceptT $ first renderError <$> try (BS.readFile path)
 where
  renderError :: IOException -> String
  renderError err = "Error reading " <> path <> ": " <> show err

-- | Check that an operational certificate authorises the KES key it was handed
-- alongside.
--
-- It matters: a certificate paired with a KES key it does not name forges
-- blocks the certificate does not authorise, which the network rejects while
-- the tool reports nothing.
checkOpCertKesKey ::
  -- | Where the two came from, for the error message.
  String ->
  Api.OperationalCertificate ->
  Api.SigningKey Api.UnsoundPureKesKey ->
  ExceptT String IO ()
checkOpCertKesKey source opCert kesSignKey
  | suppliedKesKeyHash == certifiedKesKeyHash = pure ()
  | otherwise =
      throwE $
        source
          <> ": the KES key does not match the one named by the operational certificate"
 where
  certifiedKesKeyHash = Api.verificationKeyHash (Api.getHotKey opCert)
  suppliedKesKeyHash = Api.verificationKeyHash (Api.getVerificationKey kesSignKey)

--
-- Mapping onto the consensus types
--

-- | The consensus operational certificate a vendored one wraps.
opCertOf :: Api.OperationalCertificate -> OCert.OCert StandardCrypto
opCertOf (Api.OperationalCertificate opCert _) = opCert

-- | The stake pool cold verification key an operational certificate names,
-- which the file carries alongside the certificate itself.
coldVerKeyOf :: Api.OperationalCertificate -> VKey StakePool
coldVerKeyOf (Api.OperationalCertificate _ (Api.StakePoolVerificationKey coldVerKey)) =
  coldVerKey

-- | The consensus KES signing key a vendored one wraps.
kesSignKeyOf ::
  Api.SigningKey Api.UnsoundPureKesKey ->
  UnsoundPureSignKeyKES (KES StandardCrypto)
kesSignKeyOf (Api.KesSigningKey kesSignKey) = kesSignKey

--
-- Errors
--

-- | The credential options only make sense in complete sets, so a partial set
-- is reported by naming the option that would complete it.
missingOption :: String -> String
missingOption option =
  "To forge blocks, --" <> option <> " must also be specified"
