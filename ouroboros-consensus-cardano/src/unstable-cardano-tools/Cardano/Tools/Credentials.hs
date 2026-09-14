{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The forging credentials, as the consensus layer wants them.
--
-- Which files a tool was pointed at comes from @cardano-config@, as ordinary
-- node command-line options, and decoding them is @cardano-keys@'s job. Left
-- for here is the step that produces consensus types: mapping the key material
-- onto 'ByronLeaderCredentials' and 'ShelleyLeaderCredentials'.
module Cardano.Tools.Credentials
  ( LeaderCredentials (..)
  , readLeaderCredentials
  ) where

import qualified Cardano.Chain.Delegation as Byron.Delegation
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Configuration.CliArgs as CLI
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Keys as Keys
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Keys (KeyRole (StakePool), VKey, coerceKeyRole)
import Cardano.Protocol.Crypto (StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
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
import Prettyprinter (Doc, pretty)

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
    (_, _, SNothing) -> throwE $ missingOption "shelley-kes-key"
    (SJust certFile, SJust vrfFile, SJust kesSource) -> do
      opCert <- readTextEnvelope certFile
      Keys.VrfSigningKey vrfSignKey <- readTextEnvelope vrfFile
      credentialsSource <- case kesSource of
        -- The unsound variant: the KES signing key sits in a file on disk
        -- instead of never leaving a KES agent's memory.
        CLI.KESKeyFilePath kesFile -> do
          kesSignKey <- readTextEnvelope kesFile
          checkOpCertKesKey (certFile <> " with " <> kesFile) opCert kesSignKey
          pure $
            PraosCredentialsUnsound
              (opCertOf opCert)
              (Keys.unsoundPureKesSigningKey kesSignKey)
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
    entries <-
      ExceptT $
        first (renderKeyFileError Keys.renderTextEnvelopeError)
          <$> Keys.readBulkCredentialsFile file
    traverse (fromBulkEntry file) (zip [0 :: Int ..] entries)
 where
  fromBulkEntry file (index, (opCert, Keys.VrfSigningKey vrfSignKey, kesSignKey)) = do
    -- Which entry of the file it was, because that is all that distinguishes
    -- one pair in a bulk file from the next.
    checkOpCertKesKey (file <> ", entry " <> show index) opCert kesSignKey
    pure $
      mkShelleyCredentials
        (coldVerKeyOf opCert)
        vrfSignKey
        ( PraosCredentialsUnsound
            (opCertOf opCert)
            (Keys.unsoundPureKesSigningKey kesSignKey)
        )

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
-- signing keys are all stored as one. Which type the file has to hold follows
-- from the type the caller wants back.
readTextEnvelope :: Keys.HasTextEnvelope a => FilePath -> ExceptT String IO a
readTextEnvelope path =
  ExceptT $
    first (renderKeyFileError Keys.renderTextEnvelopeError)
      <$> Keys.readFileTextEnvelope path

-- | The Byron signing key, which is not a text envelope: the file is the raw
-- CBOR of a legacy Byron @XPrv@.
readByronSigningKey :: FilePath -> ExceptT String IO Byron.Crypto.SigningKey
readByronSigningKey path = do
  Keys.ByronSigningKey signingKey <-
    ExceptT $
      first (renderKeyFileError Keys.renderSerialiseAsRawBytesError)
        <$> Keys.readByronSigningKeyFile path
  pure signingKey

-- | The Byron delegation certificate, which is neither a text envelope nor
-- CBOR: the file is canonical JSON, like the Byron genesis itself.
readByronDelegationCertificate ::
  FilePath -> ExceptT String IO Byron.Delegation.Certificate
readByronDelegationCertificate path =
  ExceptT $
    first (renderKeyFileError pretty)
      <$> Keys.readByronDelegationCertificateFile path

-- | Check that an operational certificate authorises the KES key it was handed
-- alongside.
--
-- @cardano-keys@ leaves this to the caller. It matters: a certificate paired
-- with a KES key it does not name forges blocks the certificate does not
-- authorise, which the network rejects while the tool reports nothing.
checkOpCertKesKey ::
  -- | Where the two came from, for the error message.
  String ->
  Keys.OperationalCertificate ->
  Keys.SigningKey Keys.KesKey ->
  ExceptT String IO ()
checkOpCertKesKey source opCert kesSignKey =
  except . first renderMismatch $ Keys.checkKesKeyMatchesOpCert opCert kesSignKey
 where
  renderMismatch mismatch =
    source <> ": " <> Keys.docToString (Keys.renderKesKeyMismatch mismatch)

-- | Report one of @cardano-keys@' file errors as a message. It renders errors
-- as @prettyprinter@ documents and hands out the payload renderer separately,
-- hence the argument.
renderKeyFileError :: (e -> Doc ann) -> Keys.FileError e -> String
renderKeyFileError renderPayload =
  Keys.docToString . Keys.renderFileError renderPayload

--
-- Mapping onto the consensus types
--

-- | The consensus operational certificate a @cardano-keys@ one wraps.
opCertOf :: Keys.OperationalCertificate -> OCert.OCert StandardCrypto
opCertOf (Keys.OperationalCertificate opCert _) = opCert

-- | The stake pool cold verification key an operational certificate names,
-- which the file carries alongside the certificate itself.
coldVerKeyOf :: Keys.OperationalCertificate -> VKey StakePool
coldVerKeyOf (Keys.OperationalCertificate _ (Keys.StakePoolVerificationKey coldVerKey)) =
  coldVerKey

--
-- Errors
--

-- | The credential options only make sense in complete sets, so a partial set
-- is reported by naming the option that would complete it.
missingOption :: String -> String
missingOption option =
  "To forge blocks, --" <> option <> " must also be specified"
