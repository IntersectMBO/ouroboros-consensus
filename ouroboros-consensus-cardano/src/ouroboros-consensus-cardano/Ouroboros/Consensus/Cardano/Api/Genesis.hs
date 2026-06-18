{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading genesis files and forging credentials for assembling a Cardano
-- protocol, without depending on @cardano-api@ \/ @cardano-node@.
--
-- This is the minimal subset of @cardano-node@'s @Cardano.Node.Protocol.*@
-- modules that the db-tools actually use: a polymorphic genesis-file reader
-- (shared by all the Shelley-based eras), the Byron genesis reader, the Byron
-- and Praos leader-credential loaders and 'genesisHashToPraosNonce'.
module Ouroboros.Consensus.Cardano.Api.Genesis
  ( -- * Genesis and credential file locations
    GenesisFile (..)
  , GenesisHash (..)
  , ProtocolFilepaths (..)

    -- * Reading genesis files
  , genesisHashToPraosNonce
  , readByronGenesis
  , readGenesisAny

    -- * Reading leader credentials
  , readByronLeaderCredentials
  , readShelleyLeaderCredentials

    -- * Errors
  , ByronProtocolInstantiationError (..)
  , GenesisReadError (..)
  , PraosLeaderCredentialsError (..)
  ) where

import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.UTxO as Byron.UTxO
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Prelude (canonicalDecodePretty)
import Cardano.Protocol.Crypto (StandardCrypto)
import Control.Exception (IOException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra
  ( bimapExceptT
  , firstExceptT
  , handleIOExceptT
  , hoistEither
  , hoistMaybe
  , left
  , newExceptT
  )
import Data.Aeson (FromJSON (..), ToJSON, Value (String), eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Ouroboros.Consensus.Byron.Node
  ( ByronLeaderCredentials
  , ByronLeaderCredentialsError
  , mkByronLeaderCredentials
  )
import Ouroboros.Consensus.Cardano.Api.Keys
import Ouroboros.Consensus.Cardano.Api.Serialise
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosCanBeLeader (..)
  , PraosCredentialsSource (..)
  )
import Ouroboros.Consensus.Shelley.Node
  ( Nonce (..)
  , ShelleyLeaderCredentials (..)
  )

-- ----------------------------------------------------------------------------
-- Genesis and credential file locations
--

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Types.hs

newtype GenesisFile = GenesisFile
  {unGenesisFile :: FilePath}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid =
    fail $
      "Parsing of GenesisFile failed due to type mismatch. "
        <> "Encountered: "
        <> show invalid

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 BS.ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data ProtocolFilepaths
  = ProtocolFilepaths
  { byronCertFile :: !(Maybe FilePath)
  , byronKeyFile :: !(Maybe FilePath)
  , shelleyKESFile :: !(Maybe FilePath)
  , shelleyVRFFile :: !(Maybe FilePath)
  , shelleyCertFile :: !(Maybe FilePath)
  , shelleyBulkCredsFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Reading Shelley-based genesis files
--

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol/Shelley.hs

genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

-- | Read and decode a genesis file in any of the Shelley-based eras (Shelley,
-- Alonzo, Conway, Dijkstra), checking the hash against an optional expected
-- value. The era is selected by the 'FromJSON' instance demanded at the call
-- site.
readGenesisAny ::
  FromJSON genesis =>
  GenesisFile ->
  Maybe GenesisHash ->
  ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mbExpectedGenesisHash = do
  content <-
    handleIOExceptT (GenesisReadFileError file) $
      BS.readFile file
  let genesisHash = GenesisHash (Crypto.hashWith id content)
  checkExpectedGenesisHash genesisHash
  genesis <-
    firstExceptT (GenesisDecodeError file) $
      hoistEither $
        eitherDecodeStrict' content
  return (genesis, genesisHash)
 where
  checkExpectedGenesisHash ::
    GenesisHash ->
    ExceptT GenesisReadError IO ()
  checkExpectedGenesisHash actual =
    case mbExpectedGenesisHash of
      Just expected
        | actual /= expected ->
            left (GenesisHashMismatch actual expected)
      _ -> return ()

readShelleyLeaderCredentials ::
  Maybe ProtocolFilepaths ->
  ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readShelleyLeaderCredentials Nothing = return []
readShelleyLeaderCredentials (Just pfp) =
  -- The set of credentials is a sum total of what comes from the CLI,
  -- as well as what's in the bulk credentials file.
  (<>)
    <$> readLeaderCredentialsSingleton pfp
    <*> readLeaderCredentialsBulk pfp

readLeaderCredentialsSingleton ::
  ProtocolFilepaths ->
  ExceptT
    PraosLeaderCredentialsError
    IO
    [ShelleyLeaderCredentials StandardCrypto]
-- It's OK to supply none of the files on the CLI
readLeaderCredentialsSingleton
  ProtocolFilepaths
    { shelleyCertFile = Nothing
    , shelleyVRFFile = Nothing
    , shelleyKESFile = Nothing
    } = pure []
-- Or to supply all of the files
readLeaderCredentialsSingleton
  ProtocolFilepaths
    { shelleyCertFile = Just opCertFile
    , shelleyVRFFile = Just vrfFile
    , shelleyKESFile = Just kesFile
    } = do
    vrfSKey <-
      firstExceptT PraosFileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile)

    (opCert, kesSKey) <- opCertKesKeyCheck kesFile opCertFile

    return [mkPraosLeaderCredentials opCert vrfSKey kesSKey]

-- But not OK to supply some of the files without the others.
readLeaderCredentialsSingleton ProtocolFilepaths{shelleyCertFile = Nothing} =
  left OCertNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths{shelleyVRFFile = Nothing} =
  left VRFKeyNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths{shelleyKESFile = Nothing} =
  left KESKeyNotSpecified

opCertKesKeyCheck ::
  -- | KES key
  FilePath ->
  -- | Operational certificate
  FilePath ->
  ExceptT PraosLeaderCredentialsError IO (OperationalCertificate, SigningKey UnsoundPureKesKey)
opCertKesKeyCheck kesFile certFile = do
  opCert <-
    firstExceptT PraosFileError (newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile)
  kesSKey <-
    firstExceptT
      PraosFileError
      (newExceptT $ readFileTextEnvelope (AsSigningKey AsUnsoundPureKesKey) kesFile)
  let opCertSpecifiedKesKeyhash = verificationKeyHash $ getHotKey opCert
      suppliedKesKeyHash = verificationKeyHash $ getVerificationKey kesSKey
  -- Specified KES key in operational certificate should match the one
  -- supplied to the node.
  if suppliedKesKeyHash /= opCertSpecifiedKesKeyhash
    then left $ MismatchedKesKey kesFile certFile
    else return (opCert, kesSKey)

data ShelleyCredentials
  = ShelleyCredentials
  { scCert :: (TextEnvelope, FilePath)
  , scVrf :: (TextEnvelope, FilePath)
  , scKes :: (TextEnvelope, FilePath)
  }

readLeaderCredentialsBulk ::
  ProtocolFilepaths ->
  ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readLeaderCredentialsBulk ProtocolFilepaths{shelleyBulkCredsFile = mfp} =
  mapM parseShelleyCredentials =<< readBulkFile mfp
 where
  parseShelleyCredentials ::
    ShelleyCredentials ->
    ExceptT PraosLeaderCredentialsError IO (ShelleyLeaderCredentials StandardCrypto)
  parseShelleyCredentials ShelleyCredentials{scCert, scVrf, scKes} =
    mkPraosLeaderCredentials
      <$> parseEnvelope AsOperationalCertificate scCert
      <*> parseEnvelope (AsSigningKey AsVrfKey) scVrf
      <*> parseEnvelope (AsSigningKey AsUnsoundPureKesKey) scKes

  readBulkFile ::
    Maybe FilePath ->
    ExceptT PraosLeaderCredentialsError IO [ShelleyCredentials]
  readBulkFile Nothing = pure []
  readBulkFile (Just fp) = do
    content <-
      handleIOExceptT (CredentialsReadError fp) $
        BS.readFile fp
    envelopes <-
      firstExceptT (EnvelopeParseError fp) $
        hoistEither $
          eitherDecodeStrict' content
    pure $ uncurry mkCredentials <$> zip [0 ..] envelopes
   where
    mkCredentials ::
      Int ->
      (TextEnvelope, TextEnvelope, TextEnvelope) ->
      ShelleyCredentials
    mkCredentials ix (teCert, teVrf, teKes) =
      let loc ty = fp <> "." <> show ix <> ty
       in ShelleyCredentials
            (teCert, loc "cert")
            (teVrf, loc "vrf")
            (teKes, loc "kes")

mkPraosLeaderCredentials ::
  OperationalCertificate ->
  SigningKey VrfKey ->
  SigningKey UnsoundPureKesKey ->
  ShelleyLeaderCredentials StandardCrypto
mkPraosLeaderCredentials
  (OperationalCertificate opcert (StakePoolVerificationKey vkey))
  (VrfSigningKey vrfKey)
  (KesSigningKey kesKey) =
    ShelleyLeaderCredentials
      { shelleyLeaderCredentialsCanBeLeader =
          PraosCanBeLeader
            { praosCanBeLeaderColdVerKey = coerceKeyRole vkey
            , praosCanBeLeaderSignKeyVRF = vrfKey
            , praosCanBeLeaderCredentialsSource = PraosCredentialsUnsound opcert kesKey
            }
      , shelleyLeaderCredentialsLabel = "Shelley"
      }

parseEnvelope ::
  HasTextEnvelope a =>
  AsType a ->
  (TextEnvelope, FilePath) ->
  ExceptT PraosLeaderCredentialsError IO a
parseEnvelope as (te, loc) =
  firstExceptT (PraosFileError . FileError loc)
    . hoistEither
    $ deserialiseFromTextEnvelope as te

-- ----------------------------------------------------------------------------
-- Reading the Byron genesis and credentials
--

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol/Byron.hs

readByronGenesis ::
  GenesisFile ->
  Maybe GenesisHash ->
  RequiresNetworkMagic ->
  ExceptT
    ByronProtocolInstantiationError
    IO
    Byron.Genesis.Config
readByronGenesis (GenesisFile file) mbExpectedGenesisHash ncReqNetworkMagic = do
  (genesisData, genesisHash) <-
    firstExceptT (ByronGenesisReadError file) $
      Byron.Genesis.readGenesisData file
  checkExpectedGenesisHash genesisHash
  return
    Byron.Genesis.Config
      { Byron.Genesis.configGenesisData = genesisData
      , Byron.Genesis.configGenesisHash = genesisHash
      , Byron.Genesis.configReqNetMagic = ncReqNetworkMagic
      , Byron.Genesis.configUTxOConfiguration = Byron.UTxO.defaultUTxOConfiguration
      -- TODO: add config support for the UTxOConfiguration if needed
      }
 where
  checkExpectedGenesisHash ::
    Byron.Genesis.GenesisHash ->
    ExceptT ByronProtocolInstantiationError IO ()
  checkExpectedGenesisHash actual' =
    case mbExpectedGenesisHash of
      Just expected
        | actual /= expected ->
            left (ByronGenesisHashMismatch actual expected)
       where
        actual = fromByronGenesisHash actual'
      _ -> return ()

  fromByronGenesisHash :: Byron.Genesis.GenesisHash -> GenesisHash
  fromByronGenesisHash (Byron.Genesis.GenesisHash h) =
    GenesisHash
      . fromMaybe impossible
      . Crypto.hashFromBytes
      . Byron.Crypto.hashToBytes
      $ h
   where
    impossible =
      error "fromByronGenesisHash: old and new crypto libs disagree on hash size"

readByronLeaderCredentials ::
  Byron.Genesis.Config ->
  Maybe ProtocolFilepaths ->
  ExceptT
    ByronProtocolInstantiationError
    IO
    (Maybe ByronLeaderCredentials)
readByronLeaderCredentials _ Nothing = return Nothing
readByronLeaderCredentials
  genesisConfig
  ( Just
      ProtocolFilepaths
        { byronCertFile
        , byronKeyFile
        }
    ) =
    case (byronCertFile, byronKeyFile) of
      (Nothing, Nothing) -> pure Nothing
      (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
      (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
      (Just delegCertFile, Just signingKeyFile) -> do
        signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
        delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
        ByronSigningKey signingKey <-
          hoistMaybe (SigningKeyDeserialiseFailure signingKeyFile) $
            deserialiseFromRawBytes (AsSigningKey AsByronKey) $
              LB.toStrict signingKeyFileBytes
        delegCert <-
          firstExceptT (CanonicalDecodeFailure delegCertFile)
            . hoistEither
            $ canonicalDecodePretty delegCertFileBytes

        bimapExceptT CredentialsError Just
          . hoistEither
          $ mkByronLeaderCredentials genesisConfig signingKey delegCert "Byron"

-- ----------------------------------------------------------------------------
-- Errors
--

data GenesisReadError
  = GenesisReadFileError !FilePath !IOException
  | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
  | GenesisDecodeError !FilePath !String
  deriving Show

instance Error GenesisReadError where
  displayError (GenesisReadFileError fp err) =
    "There was an error reading the genesis file: "
      <> fp
      <> " Error: "
      <> show err
  displayError (GenesisHashMismatch actual expected) =
    "Wrong genesis file: the actual hash is "
      <> show actual
      <> ", but the expected genesis hash given in the node "
      <> "configuration file is "
      <> show expected
  displayError (GenesisDecodeError fp err) =
    "There was an error parsing the genesis file: "
      <> fp
      <> " Error: "
      <> show err

data PraosLeaderCredentialsError
  = CredentialsReadError !FilePath !IOException
  | EnvelopeParseError !FilePath !String
  | PraosFileError !(FileError TextEnvelopeError)
  | OCertNotSpecified
  | VRFKeyNotSpecified
  | KESKeyNotSpecified
  | MismatchedKesKey
      FilePath
      -- ^ KES signing key
      FilePath
      -- ^ Operational certificate
  deriving Show

instance Error PraosLeaderCredentialsError where
  displayError (CredentialsReadError fp err) =
    "There was an error reading a credentials file: "
      <> fp
      <> " Error: "
      <> show err
  displayError (EnvelopeParseError fp err) =
    "There was an error parsing a credentials envelope: "
      <> fp
      <> " Error: "
      <> show err
  displayError (PraosFileError fileErr) = displayError fileErr
  displayError (MismatchedKesKey kesFp certFp) =
    "The KES key provided at: "
      <> show kesFp
      <> " does not match the KES key specified in the operational certificate at: "
      <> show certFp
  displayError OCertNotSpecified = missingFlagMessage "shelley-operational-certificate"
  displayError VRFKeyNotSpecified = missingFlagMessage "shelley-vrf-key"
  displayError KESKeyNotSpecified = missingFlagMessage "shelley-kes-key"

missingFlagMessage :: String -> String
missingFlagMessage flag =
  "To create blocks, the --" <> flag <> " must also be specified"

data ByronProtocolInstantiationError
  = CanonicalDecodeFailure !FilePath !Text
  | ByronGenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
  | DelegationCertificateFilepathNotSpecified
  | ByronGenesisReadError !FilePath !Byron.Genesis.GenesisDataError
  | CredentialsError !ByronLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath
  | SigningKeyFilepathNotSpecified
  deriving Show

instance Error ByronProtocolInstantiationError where
  displayError (CanonicalDecodeFailure fp failure) =
    "Canonical decode failure in "
      <> fp
      <> " Canonical failure: "
      <> Text.unpack failure
  displayError (ByronGenesisHashMismatch actual expected) =
    "Wrong Byron genesis file: the actual hash is "
      <> show actual
      <> ", but the expected Byron genesis hash given in the node configuration "
      <> "file is "
      <> show expected
  displayError DelegationCertificateFilepathNotSpecified =
    "Delegation certificate filepath not specified"
  displayError (ByronGenesisReadError fp err) =
    "There was an error parsing the genesis file: "
      <> fp
      <> " Error: "
      <> show err
  -- TODO: Implement ByronLeaderCredentialsError render function in ouroboros-network
  displayError (CredentialsError byronLeaderCredentialsError) =
    "Byron leader credentials error: " <> show byronLeaderCredentialsError
  displayError (SigningKeyDeserialiseFailure fp) =
    "Signing key deserialisation error in: " <> fp
  displayError SigningKeyFilepathNotSpecified =
    "Signing key filepath not specified"
