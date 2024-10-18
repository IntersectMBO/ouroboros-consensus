{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Ouroboros.Consensus.Protocol.Praos.Header where

import Cardano.Crypto.DSIGN (
    DSIGNAlgorithm (SignKeyDSIGN, genKeyDSIGN),
    Ed25519DSIGN,
    deriveVerKeyDSIGN,
 )
import Cardano.Crypto.Hash (Blake2b_256, Hash, hash)
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.KES.Class (genKeyKES)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.VRF (deriveVerKeyVRF)
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Crypto.VRF.Praos as VRF
import Cardano.Ledger.BaseTypes (
    Nonce (..),
    ProtVer (..),
    Version,
    natVersion,
 )
import Cardano.Ledger.Binary (MaxVersion, decCBOR, decodeFull', decodeFullAnnotator, serialize')
import Cardano.Ledger.Keys (VKey (..), signedDSIGN)
import Cardano.Protocol.TPraos.BHeader (
    HashHeader (..),
    PrevHash (..),
 )
import Cardano.Protocol.TPraos.OCert (
    KESPeriod (..),
    OCert (..),
    OCertSignable (..),
 )
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64)
import Ouroboros.Consensus.Protocol.Praos.Header (
    Header,
    HeaderBody (..),
    pattern Header,
 )
import Ouroboros.Consensus.Protocol.Praos.VRF (mkInputVRF)
import Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)
import Test.QuickCheck (Gen, arbitrary, choose, frequency, generate, getPositive, shrinkList, sized, vectorOf)

-- * Test Vectors

generateSamples :: IO Sample
generateSamples = generate genSample

-- FIXME: Should be defined according to some Era
testVersion :: Version
testVersion = natVersion @MaxVersion

data Sample = Sample
    { context :: !GeneratorContext
    , headers :: ![MutatedHeader]
    }
    deriving (Show, Eq)

instance Json.ToJSON Sample where
    toJSON Sample{context, headers} =
        Json.object
            [ "context" .= context
            , "headers" .= headers
            ]

instance Json.FromJSON Sample where
    parseJSON = Json.withObject "Sample" $ \obj -> do
        context <- obj .: "context"
        headers <- obj .: "headers"
        pure Sample{..}

genSample :: Gen Sample
genSample = do
    context <- genContext
    headers <- sized $ \n -> vectorOf n $ genMutatedHeader context
    pure $ Sample{context, headers}

genMutatedHeader :: GeneratorContext -> Gen MutatedHeader
genMutatedHeader context = do
    mutation <- genMutation
    header <- genHeader context
    mutated <- mutate context header mutation
    pure $ MutatedHeader{header = mutated, mutation}

shrinkSample :: Sample -> [Sample]
shrinkSample Sample{context, headers} =
    [ Sample{context, headers = headers'}
    | headers' <- shrinkList (const []) headers
    ]

mutate :: GeneratorContext -> Header StandardCrypto -> Mutation -> Gen (Header StandardCrypto)
mutate context header = \case
    NoMutation -> pure header
    MutateKESKey -> do
        let Header body _ = header
        newKESSignKey <- newKESSigningKey <$> gen32Bytes
        KESPeriod kesPeriod <- genValidKESPeriod (hbSlotNo body) praosSlotsPerKESPeriod
        let sig' = KES.signKES () kesPeriod body newKESSignKey
        pure $ Header body (KES.SignedKES sig')
    MutateColdKey -> do
        let Header body _ = header
        newColdSignKey <- genKeyDSIGN . mkSeedFromBytes <$> gen32Bytes
        (hbOCert, KESPeriod kesPeriod) <- genCert (hbSlotNo body) context{coldSignKey = newColdSignKey}
        let newBody = body{hbOCert}
        let sig' = KES.signKES () kesPeriod newBody kesSignKey
        pure $ Header newBody (KES.SignedKES sig')
  where
    GeneratorContext{praosSlotsPerKESPeriod, kesSignKey} = context

data Mutation = NoMutation | MutateKESKey | MutateColdKey
    deriving (Eq, Show)

instance Json.ToJSON Mutation where
    toJSON = \case
        NoMutation -> "NoMutation"
        MutateKESKey -> "MutateKESKey"
        MutateColdKey -> "MutateColdKey"

instance Json.FromJSON Mutation where
    parseJSON = \case
        "NoMutation" -> pure NoMutation
        "MutateKESKey" -> pure MutateKESKey
        "MutateColdKey" -> pure MutateColdKey
        _ -> fail "Invalid mutation"

expectedError :: Mutation -> String
expectedError = \case
    NoMutation -> "No error"
    MutateKESKey -> "InvalidKesSignatureOCERT"
    MutateColdKey -> "InvalidSignatureOCERT"

genMutation :: Gen Mutation
genMutation =
    frequency
        [ (2, pure NoMutation)
        , (1, pure MutateKESKey)
        , (1, pure MutateColdKey)
        ]

data MutatedHeader = MutatedHeader
    { header :: !(Header StandardCrypto)
    , mutation :: !Mutation
    }
    deriving (Show, Eq)

instance Json.ToJSON MutatedHeader where
    toJSON MutatedHeader{header, mutation} =
        Json.object
            [ "header" .= cborHeader
            , "mutation" .= mutation
            ]
      where
        cborHeader = decodeUtf8 . Base64.encode $ serialize' testVersion header

instance Json.FromJSON MutatedHeader where
    parseJSON = Json.withObject "MutatedHeader" $ \obj -> do
        cborHeader <- obj .: "header"
        mutation <- obj .: "mutation"
        header <- parseHeader cborHeader
        pure MutatedHeader{header, mutation}
      where
        parseHeader cborHeader = do
            let headerBytes = Base64.decodeLenient (encodeUtf8 cborHeader)
            either (fail . show) pure $ decodeFullAnnotator @(Header StandardCrypto) testVersion "Header" decCBOR $ LBS.fromStrict headerBytes

-- * Generators
type KESKey = KES.SignKeyKES (KES.Sum6KES Ed25519DSIGN Blake2b_256)

newVRFSigningKey :: ByteString -> (VRF.SignKeyVRF VRF.PraosVRF, VRF.VerKeyVRF VRF.PraosVRF)
newVRFSigningKey = VRF.genKeyPairVRF . mkSeedFromBytes

newKESSigningKey :: ByteString -> KESKey
newKESSigningKey = genKeyKES . mkSeedFromBytes

data GeneratorContext = GeneratorContext
    { praosSlotsPerKESPeriod :: !Word64
    , kesSignKey :: !KESKey
    , coldSignKey :: !(SignKeyDSIGN Ed25519DSIGN)
    , vrfSignKey :: !(VRF.SignKeyVRF VRF.PraosVRF)
    , nonce :: !Nonce
    }
    deriving (Show)

instance Eq GeneratorContext where
    a == b =
        praosSlotsPerKESPeriod a == praosSlotsPerKESPeriod b
            && serialize' testVersion (kesSignKey a) == serialize' testVersion (kesSignKey b)
            && coldSignKey a == coldSignKey b
            && vrfSignKey a == vrfSignKey b
            && nonce a == nonce b

instance Json.ToJSON GeneratorContext where
    toJSON GeneratorContext{..} =
        Json.object
            [ "praosSlotsPerKESPeriod" .= praosSlotsPerKESPeriod
            , "kesSignKey" .= cborKesSignKey
            , "coldSignKey" .= cborColdSignKey
            , "vrfSignKey" .= cborVrfSignKey
            , "nonce" .= cborNonce
            ]
      where
        cborKesSignKey = decodeUtf8 . Base64.encode $ serialize' testVersion kesSignKey
        cborColdSignKey = decodeUtf8 . Base64.encode $ serialize' testVersion coldSignKey
        cborVrfSignKey = decodeUtf8 . Base64.encode $ serialize' testVersion vrfSignKey
        cborNonce = decodeUtf8 . Base64.encode $ serialize' testVersion nonce

instance Json.FromJSON GeneratorContext where
    parseJSON = Json.withObject "GeneratorContext" $ \obj -> do
        praosSlotsPerKESPeriod <- obj .: "praosSlotsPerKESPeriod"
        cborKesSignKey <- obj .: "kesSignKey"
        cborColdSignKey <- obj .: "coldSignKey"
        cborVrfSignKey <- obj .: "vrfSignKey"
        cborNonce <- obj .: "nonce"
        kesSignKey <- parseKey cborKesSignKey
        coldSignKey <- parseKey cborColdSignKey
        vrfSignKey <- parseKey cborVrfSignKey
        nonce <- parseKey cborNonce
        pure GeneratorContext{..}
      where
        parseKey cborKey = do
            let keyBytes = Base64.decodeLenient (encodeUtf8 cborKey)
            either (fail . show) pure $ decodeFull' testVersion keyBytes

genContext :: Gen GeneratorContext
genContext = do
    praosSlotsPerKESPeriod <- choose (100, 10000)
    kesSignKey <- newKESSigningKey <$> gen32Bytes
    coldSignKey <- genKeyDSIGN . mkSeedFromBytes <$> gen32Bytes
    vrfSignKey <- fst <$> newVRFSigningKey <$> gen32Bytes
    nonce <- Nonce <$> genHash
    pure $ GeneratorContext{..}

{- | Generate a well-formed header

The header is signed with the KES key, and all the signing keys
generated for the purpose of producing the header are returned.
-}
genHeader :: GeneratorContext -> Gen (Header StandardCrypto)
genHeader context = do
    (body, KESPeriod kesPeriod) <- genHeaderBody context
    let sign = KES.SignedKES $ KES.signKES () kesPeriod body kesSignKey
    pure $ (Header body sign)
  where
    GeneratorContext{kesSignKey} = context

genHeaderBody :: GeneratorContext -> Gen (HeaderBody StandardCrypto, KESPeriod)
genHeaderBody context = do
    hbBlockNo <- BlockNo <$> arbitrary
    hbSlotNo <- SlotNo . getPositive <$> arbitrary
    hbPrev <- BlockHash . HashHeader <$> genHash
    let hbVk = VKey $ deriveVerKeyDSIGN coldSignKey
    let rho' = mkInputVRF hbSlotNo nonce
        hbVrfRes = VRF.evalCertified () rho' vrfSignKey
        hbVrfVk = deriveVerKeyVRF vrfSignKey
    hbBodySize <- choose (1000, 90000)
    hbBodyHash <- genHash
    (hbOCert, kesPeriod) <- genCert hbSlotNo context
    let hbProtVer = protocolVersionZero
        headerBody = HeaderBody{..}
    pure $ (headerBody, kesPeriod)
  where
    GeneratorContext{coldSignKey, vrfSignKey, nonce} = context

protocolVersionZero :: ProtVer
protocolVersionZero = ProtVer versionZero 0
  where
    versionZero :: Version
    versionZero = natVersion @0

genCert :: SlotNo -> GeneratorContext -> Gen (OCert StandardCrypto, KESPeriod)
genCert slotNo context = do
    let ocertVkHot = KES.deriveVerKeyKES kesSignKey
    ocertN <- arbitrary
    ocertKESPeriod <- genValidKESPeriod slotNo praosSlotsPerKESPeriod
    let ocertSigma = signedDSIGN @StandardCrypto coldSignKey (OCertSignable ocertVkHot ocertN ocertKESPeriod)
    pure (OCert{..}, ocertKESPeriod)
  where
    GeneratorContext{kesSignKey, praosSlotsPerKESPeriod, coldSignKey} = context

genValidKESPeriod :: SlotNo -> Word64 -> Gen KESPeriod
genValidKESPeriod slotNo praosSlotsPerKESPeriod =
    pure $ KESPeriod $ fromIntegral $ unSlotNo slotNo `div` praosSlotsPerKESPeriod

genHash :: Gen (Hash Blake2b_256 a)
genHash = coerce . hash <$> gen32Bytes

gen32Bytes :: Gen ByteString
gen32Bytes = BS.pack <$> vectorOf 32 arbitrary
