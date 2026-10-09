{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Praos, and the pieces every protocol built on Praos shares.
--
-- Each such protocol is its own @proto@ type with its own instances, so that
-- none of Praos's rules are reused by accident. The data types, however, are
-- shared: they carry a @proto@ parameter, so that there is one definition of
-- each Praos concept regardless of which extensions the protocol enables. The
-- functions suffixed @PolyPraos@ are the method implementations every such
-- protocol delegates to.
--
-- @Poly@ means "many": a @PolyPraos@ type or function handles multiple
-- extensions of/overlays on Praos, the ones used for Cardano. And the
-- @PolyPraos@ functions are indeed /polymorphic/ in the @proto@ tyvar.
module Ouroboros.Consensus.Protocol.Praos
  ( AnnouncedBy (..)
  , PolyPraosCrypto
  , PolyPraosState (..)
  , PolyPraosValidationErr (..)
  , ConsensusConfig (..)
  , LeiosOnly (..)
  , Praos
  , PraosCannotForge (..)
  , PraosCrypto
  , PraosFields (..)
  , PraosIsLeader (..)
  , PraosLedgerView
  , PraosParams (..)
  , PraosState
  , PraosToSign (..)
  , PraosValidateView
  , PraosValidationErr
  , SerialisePraosState
  , Ticked (..)
  , checkIsLeaderPolyPraos
  , forgePraosFields
  , getOpCertCountersPolyPraos
  , getPraosNoncesPolyPraos
  , leiosContextFreeHeaderChecks
  , praosCheckCanForge
  , reupdateChainDepStatePolyPraos
  , updateChainDepStatePolyPraos
  , tickChainDepStatePolyPraos
  , validateKESSignature
  , validateVRFSignature

    -- * For testing purposes
  , doValidateKESSignature
  , doValidateVRFSignature
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (ActiveSlotCoeff, Nonce, StrictMaybe (..), (⭒))
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import qualified Cardano.Ledger.Chain as SL
import Cardano.Ledger.Core (fromEraCBOR, toEraCBOR)
import Cardano.Ledger.Hashes (HASH, extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Keys
  ( DSIGN
  , KeyHash
  , VKey (VKey)
  , coerceKeyRole
  , hashKey
  )
import qualified Cardano.Ledger.Keys as SL
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Slot (Duration (Duration), (+*))
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (Crypto, KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.Praos.VRF
  ( InputVRF
  , mkInputVRF
  , vrfLeaderValue
  , vrfNonceValue
  )
import qualified Cardano.Protocol.TPraos.API as SL
import Cardano.Protocol.TPraos.BlockHeader
  ( BoundedNatural (bvValue)
  , checkLeaderNatValue
  , prevHashToNonce
  )
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (KESPeriod)
  , OCert (OCert)
  , OCertSignable
  )
import qualified Cardano.Protocol.TPraos.OCert as OCert
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import Cardano.Slotting.EpochInfo
  ( EpochInfo
  , epochInfoEpoch
  , epochInfoFirst
  , epochInfoSlotLength
  , hoistEpochInfo
  )
import Cardano.Slotting.Slot
  ( EpochNo (EpochNo)
  , SlotNo (SlotNo)
  , WithOrigin
  , unSlotNo
  )
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (decode, encode))
import Control.Exception (throw)
import Control.Monad (unless, when)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.Leios.Types as Leios
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos.Orphans ()
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Signed (Signed)
import Ouroboros.Consensus.Protocol.TPraos
  ( ConsensusConfig (TPraosConfig, tpraosEpochInfo, tpraosParams)
  , TPraos
  , TPraosState (tpraosStateChainDepState, tpraosStateLastSlot)
  )
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.CBOR (decodeStrictMaybe, encodeStrictMaybe)
import Ouroboros.Consensus.Util.Versioned
  ( VersionDecoder (Decode)
  , decodeVersion
  , encodeVersion
  )

-- | Praos with no extensions.
type Praos :: Type -> Type
data Praos c

type instance ShelleyProtocolHeader (Praos c) = PraosCodec.Header c

-- | Praos is not Leios, so it holds the left alternative.
newtype instance LeiosOnly (Praos c) a b = PraosLacksLeios a
  deriving (Eq, Generic, Show)

deriving anyclass instance NoThunks a => NoThunks (LeiosOnly (Praos c) a b)

instance Functor (LeiosOnly (Praos c) a) where
  fmap _ (PraosLacksLeios a) = PraosLacksLeios a

instance () ~ a => Applicative (LeiosOnly (Praos c) a) where
  pure _ = PraosLacksLeios ()
  PraosLacksLeios () <*> PraosLacksLeios () = PraosLacksLeios ()

instance Foldable (LeiosOnly (Praos c) a) where
  foldMap _ (PraosLacksLeios _) = mempty

instance Traversable (LeiosOnly (Praos c) a) where
  traverse _ (PraosLacksLeios a) = pure (PraosLacksLeios a)

instance TypeSwitch (LeiosOnly (Praos c)) where
  typeSwitchL = PraosLacksLeios (PraosLacksLeios ())
  typeSwitchR = PraosLacksLeios ()

-- | What a protocol needs of its crypto: the Praos essentials, plus signing
-- whichever header body it is the protocol for.
class
  ( Crypto c
  , DSIGN.Signable DSIGN (OCertSignable c)
  , VRF.Signable (VRF c) InputVRF
  , KES.Signable (KES c) (Signed (ShelleyProtocolHeader proto))
  ) =>
  PolyPraosCrypto proto c

instance PolyPraosCrypto (Praos StandardCrypto) StandardCrypto

class (Crypto c, PolyPraosCrypto (Praos c) c) => PraosCrypto c

instance PraosCrypto StandardCrypto

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

data PraosFields c toSign = PraosFields
  { praosSignature :: KES.SignedKES (KES c) toSign
  , praosToSign :: toSign
  }
  deriving Generic

deriving instance
  (NoThunks toSign, Crypto c) =>
  NoThunks (PraosFields c toSign)

deriving instance
  (Show toSign, Crypto c) =>
  Show (PraosFields c toSign)

-- | Fields arising from praos execution which must be included in
-- the block signature.
data PraosToSign c = PraosToSign
  { praosToSignIssuerVK :: SL.VKey SL.BlockIssuer
  -- ^ Verification key for the issuer of this block.
  , praosToSignVrfVK :: VRF.VerKeyVRF (VRF c)
  , praosToSignVrfRes :: VRF.CertifiedVRF (VRF c) InputVRF
  -- ^ Verifiable random value. This is used both to prove the issuer is
  -- eligible to issue a block, and to contribute to the evolving nonce.
  , praosToSignOCert :: OCert.OCert c
  -- ^ Lightweight delegation certificate mapping the cold (DSIGN) key to
  -- the online KES key.
  }
  deriving Generic

instance Crypto c => NoThunks (PraosToSign c)

deriving instance Crypto c => Show (PraosToSign c)

forgePraosFields ::
  ( Crypto c
  , KES.Signable (KES c) toSign
  , Monad m
  ) =>
  HotKey c m ->
  PraosCanBeLeader c ->
  PraosIsLeader c ->
  (PraosToSign c -> toSign) ->
  m (PraosFields c toSign)
forgePraosFields
  hotKey
  PraosCanBeLeader
    { praosCanBeLeaderColdVerKey
    , praosCanBeLeaderSignKeyVRF
    }
  PraosIsLeader{praosIsLeaderVrfRes}
  mkToSign = do
    ocert <- HotKey.getOCert hotKey
    let signedFields =
          PraosToSign
            { praosToSignIssuerVK = praosCanBeLeaderColdVerKey
            , praosToSignVrfVK = VRF.deriveVerKeyVRF praosCanBeLeaderSignKeyVRF
            , praosToSignVrfRes = praosIsLeaderVrfRes
            , praosToSignOCert = ocert
            }
        toSign = mkToSign signedFields
    signature <- HotKey.sign hotKey toSign
    return
      PraosFields
        { praosSignature = signature
        , praosToSign = toSign
        }

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Praos parameters that are node independent
data PraosParams = PraosParams
  { praosSlotsPerKESPeriod :: !Word64
  -- ^ See 'Globals.slotsPerKESPeriod'.
  , praosLeaderF :: !SL.ActiveSlotCoeff
  -- ^ Active slots coefficient. This parameter represents the proportion
  -- of slots in which blocks should be issued. This can be interpreted as
  -- the probability that a party holding all the stake will be elected as
  -- leader for a given slot.
  , praosSecurityParam :: !SecurityParam
  -- ^ See 'Globals.securityParameter'.
  , praosMaxKESEvo :: !Word64
  -- ^ Maximum number of KES iterations, see 'Globals.maxKESEvo'.
  , praosMaxMajorPV :: !MaxMajorProtVer
  -- ^ All blocks invalid after this protocol version, see
  -- 'Globals.maxMajorPV'.
  , praosRandomnessStabilisationWindow :: !Word64
  -- ^ The number of slots before the start of an epoch where the
  -- corresponding epoch nonce is snapshotted. This has to be at least one
  -- stability window such that the nonce is stable at the beginning of the
  -- epoch. Ouroboros Genesis requires this to be even larger, see
  -- 'SL.computeRandomnessStabilisationWindow'.
  }
  deriving (Generic, NoThunks)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
newtype PraosIsLeader c = PraosIsLeader
  { praosIsLeaderVrfRes :: VRF.CertifiedVRF (VRF c) InputVRF
  }
  deriving Generic

instance Crypto c => NoThunks (PraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams :: !PraosParams
  , praosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
  -- it's useful for this record to be EpochInfo and one other thing,
  -- because the one other thing can then be used as the
  -- PartialConsensConfig in the HFC instance.
  }
  deriving Generic

instance Crypto c => NoThunks (ConsensusConfig (Praos c))

instance HasMaxMajorProtVer (Praos c) where
  protoMaxMajorPV = praosMaxMajorPV . praosParams

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

-- | Praos consensus state.
--
-- We track the last slot and the counters for operational certificates, as well
-- as a series of nonces which get updated in different ways over the course of
-- an epoch.
data PolyPraosState proto = PraosState
  { praosStateLastSlot :: !(WithOrigin SlotNo)
  , praosStateOCertCounters :: !(Map (KeyHash SL.BlockIssuer) Word64)
  -- ^ Operation Certificate counters
  , praosStateEvolvingNonce :: !Nonce
  -- ^ Evolving nonce
  , praosStateCandidateNonce :: !Nonce
  -- ^ Candidate nonce
  , praosStateEpochNonce :: !Nonce
  -- ^ Epoch nonce
  , praosStatePreviousEpochNonce :: !Nonce
  -- ^ Previous epoch nonce
  , praosStateLabNonce :: !Nonce
  -- ^ Nonce constructed from the hash of the previous block
  , praosStateLastEpochBlockNonce :: !Nonce
  -- ^ Nonce corresponding to the LAB nonce of the last block of the previous
  -- epoch
  , praosStateLeiosAnnouncement ::
      !(LeiosOnly proto () (StrictMaybe AnnouncedBy))
  -- ^ The announcement carried by the most recently applied header, if any.
  -- A header with no announcement clears it.
  }
  deriving Generic

type PraosState c = PolyPraosState (Praos c)

type PraosLedgerView c = Views.PolyPraosLedgerView (Praos c)

type PraosValidateView c = Views.PolyPraosValidateView (Praos c) c

deriving instance
  Show (LeiosOnly proto () (StrictMaybe AnnouncedBy)) => Show (PolyPraosState proto)

deriving instance
  Eq (LeiosOnly proto () (StrictMaybe AnnouncedBy)) => Eq (PolyPraosState proto)

instance
  ( Typeable proto
  , NoThunks (LeiosOnly proto () (StrictMaybe AnnouncedBy))
  ) =>
  NoThunks (PolyPraosState proto)

-- | An endorser block announcement, and the issuer of the header that carried
-- it.
--
-- With that header's slot ('praosStateLastSlot') the issuer identifies the
-- election.
data AnnouncedBy = MkAnnouncedBy
  { announcedByIssuer :: !(KeyHash SL.BlockIssuer)
  , announcedEb :: !EbReferencesAnnouncement
  }
  deriving (Generic, Show, Eq, NoThunks)

encodeAnnouncedBy :: AnnouncedBy -> CBOR.Encoding
encodeAnnouncedBy (MkAnnouncedBy issuer (EbReferencesAnnouncement h sz)) =
  CBOR.encodeListLen 3 <> toCBOR issuer <> toCBOR (extractHash h) <> toCBOR sz

decodeAnnouncedBy :: CBOR.Decoder s AnnouncedBy
decodeAnnouncedBy = do
  enforceSize "AnnouncedBy" 3
  MkAnnouncedBy
    <$> fromCBOR
    <*> (EbReferencesAnnouncement . unsafeMakeSafeHash <$> fromCBOR <*> fromCBOR)

instance SerialisePraosState proto => ToCBOR (PolyPraosState proto) where
  toCBOR = encode

instance SerialisePraosState proto => FromCBOR (PolyPraosState proto) where
  fromCBOR = decode

-- | What encoding 'PolyPraosState' needs of its protocol.
--
-- 'LeiosOnly' decides which fields are written, and which format version is
-- written: 0 for 'Praos', 1 for 'Praos2'. The two protocols' versions are
-- separate namespaces: the HFC's era index precedes them, so the codec is
-- already chosen when the version is read.
type SerialisePraosState proto =
  ( Typeable proto
  , Applicative (LeiosOnly proto ())
  , Traversable (LeiosOnly proto ())
  )

instance SerialisePraosState proto => Serialise (PolyPraosState proto) where
  encode
    PraosState
      { praosStateLastSlot
      , praosStateOCertCounters
      , praosStateEvolvingNonce
      , praosStateCandidateNonce
      , praosStateEpochNonce
      , praosStatePreviousEpochNonce
      , praosStateLabNonce
      , praosStateLastEpochBlockNonce
      , praosStateLeiosAnnouncement
      } =
      encodeVersion version $
        mconcat
          [ CBOR.encodeListLen (8 + nLeiosFields)
          , toCBOR praosStateLastSlot
          , toCBOR praosStateOCertCounters
          , toEraCBOR @ShelleyEra praosStateEvolvingNonce
          , toEraCBOR @ShelleyEra praosStateCandidateNonce
          , toEraCBOR @ShelleyEra praosStateEpochNonce
          , toEraCBOR @ShelleyEra praosStatePreviousEpochNonce
          , toEraCBOR @ShelleyEra praosStateLabNonce
          , toEraCBOR @ShelleyEra praosStateLastEpochBlockNonce
          , foldMap (encodeStrictMaybe encodeAnnouncedBy) praosStateLeiosAnnouncement
          ]
     where
      nLeiosFields = foldr (\() _ -> 1) 0 (pure_LeiosOnly @proto ())
      version = foldr (\() _ -> 1) 0 (pure_LeiosOnly @proto ())

  decode =
    decodeVersion
      [(version, Decode decodePraosState)]
   where
    version = foldr (\() _ -> 1) 0 (pure_LeiosOnly @proto ())
    nLeiosFields = foldr (\() _ -> 1) 0 (pure_LeiosOnly @proto ())

    decodePraosState :: CBOR.Decoder s (PolyPraosState proto)
    decodePraosState = do
      enforceSize "PraosState" (8 + nLeiosFields)
      PraosState
        <$> fromCBOR
        <*> fromCBOR
        <*> fromEraCBOR @ShelleyEra
        <*> fromEraCBOR @ShelleyEra
        <*> fromEraCBOR @ShelleyEra
        <*> fromEraCBOR @ShelleyEra
        <*> fromEraCBOR @ShelleyEra
        <*> fromEraCBOR @ShelleyEra
        <*> traverse
          (\() -> decodeStrictMaybe decodeAnnouncedBy)
          (pure_LeiosOnly @proto ())

data instance Ticked (PolyPraosState proto) = TickedPraosState
  { tickedPraosStateChainDepState :: PolyPraosState proto
  , tickedPraosStateLedgerView :: Views.PolyPraosLedgerView proto
  }

-- | Errors which we might encounter
data PolyPraosValidationErr proto c
  = VRFKeyUnknown
      !(KeyHash SL.StakePool) -- unknown VRF keyhash (not registered)
  | VRFKeyWrongVRFKey
      !(KeyHash SL.StakePool) -- KeyHash of block issuer
      !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c))) -- VRF KeyHash registered with stake pool
      !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c))) -- VRF KeyHash from Header
  | VRFKeyBadProof
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF c) InputVRF) -- VRF calculated nonce value
  | VRFLeaderValueTooBig Natural Rational ActiveSlotCoeff
  | KESBeforeStartOCERT
      !KESPeriod -- OCert Start KES Period
      !KESPeriod -- Current KES Period
  | KESAfterEndOCERT
      !KESPeriod -- Current KES Period
      !KESPeriod -- OCert Start KES Period
      !Word64 -- Max KES Key Evolutions
  | CounterTooSmallOCERT
      !Word64 -- last KES counter used
      !Word64 -- current KES counter
  | -- | The KES counter has been incremented by more than 1
    CounterOverIncrementedOCERT
      !Word64 -- last KES counter used
      !Word64 -- current KES counter
  | InvalidSignatureOCERT
      !Word64 -- OCert counter
      !KESPeriod -- OCert KES period
      !String -- DSIGN error message
  | InvalidKesSignatureOCERT
      !Word -- current KES Period
      !Word -- KES start period
      !Word -- expected KES evolutions
      !Word64 -- max KES evolutions
      !String -- error message given by Consensus Layer
  | NoCounterForKeyHashOCERT
      !(KeyHash SL.BlockIssuer) -- stake pool key hash
  | -- | The header sets its cert bit, but its predecessor announced no endorser
    -- block, so there is nothing for the certificate to certify.
    LeiosCertWithoutAnnouncement
      !(LeiosOnly proto Void ())
  | -- | The header sets its cert bit too soon after its predecessor's
    -- announcement: the announcement, voting and diffusion periods have not all
    -- elapsed.
    LeiosCertTooYoung
      !(LeiosOnly proto Void ())
      !SlotNo -- Slot of the announcing block
      !SlotNo -- Slot of this header
      !SlotNo -- Earliest slot in which this header could have certified
  | -- | The header announces an endorser block larger than the protocol
    -- parameters allow.
    LeiosEbTooBig
      !(LeiosOnly proto Void ())
      !Word32 -- Announced size
      !Word32 -- Maximum size
  deriving Generic

type PraosValidationErr c = PolyPraosValidationErr (Praos c) c

deriving instance
  (Crypto c, Eq (LeiosOnly proto Void ())) =>
  Eq (PolyPraosValidationErr proto c)

deriving instance
  (Typeable proto, Crypto c, NoThunks (LeiosOnly proto Void ())) =>
  NoThunks (PolyPraosValidationErr proto c)

deriving instance
  (Crypto c, Show (LeiosOnly proto Void ())) =>
  Show (PolyPraosValidationErr proto c)

instance ChainDepStateSupportsPeras (PolyPraosState proto) where
  getEpochNonce = praosStateEpochNonce

instance ChainDepStateSupportsPeras (Ticked (PolyPraosState proto)) where
  getEpochNonce = praosStateEpochNonce . tickedPraosStateChainDepState

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  type ChainDepState (Praos c) = PolyPraosState (Praos c)
  type IsLeader (Praos c) = PraosIsLeader c
  type CanBeLeader (Praos c) = PraosCanBeLeader c
  type TiebreakerView (Praos c) = PraosTiebreakerView c
  type LedgerView (Praos c) = Views.PolyPraosLedgerView (Praos c)
  type ValidationErr (Praos c) = PolyPraosValidationErr (Praos c) c
  type ValidateView (Praos c) = Views.PolyPraosValidateView (Praos c) c

  protocolSecurityParam = praosSecurityParam . praosParams

  checkIsLeader = checkIsLeaderPolyPraos

  tickChainDepState = tickChainDepStatePolyPraos

  updateChainDepState = updateChainDepStatePolyPraos

  reupdateChainDepState = reupdateChainDepStatePolyPraos

-- | 'checkIsLeader' for every Praos.
checkIsLeaderPolyPraos ::
  forall proto c.
  PolyPraosCrypto proto c =>
  ConsensusConfig (Praos c) ->
  PraosCanBeLeader c ->
  SlotNo ->
  Ticked (PolyPraosState proto) ->
  Maybe (PraosIsLeader c)
checkIsLeaderPolyPraos
  cfg
  PraosCanBeLeader
    { praosCanBeLeaderSignKeyVRF
    , praosCanBeLeaderColdVerKey
    }
  slot
  cs =
    if meetsLeaderThreshold cfg lv (SL.coerceKeyRole vkhCold) rho
      then
        Just
          PraosIsLeader
            { praosIsLeaderVrfRes = coerce rho
            }
      else Nothing
   where
    chainState = tickedPraosStateChainDepState cs
    lv = tickedPraosStateLedgerView cs
    eta0 = praosStateEpochNonce chainState
    vkhCold = SL.hashKey praosCanBeLeaderColdVerKey
    rho' = mkInputVRF slot eta0

    rho = VRF.evalCertified () rho' praosCanBeLeaderSignKeyVRF

-- | 'tickChainDepState' for every Praos.
--
-- Updating the chain dependent state for Praos.
--
-- If we are not in a new epoch, then nothing happens. If we are in a new
-- epoch, we do three things:
-- - Store the existing current epoch nonce as the "previous epoch" nonce.
--   This is needed to validate Peras certificates when they appear in blocks.
-- - Update the epoch nonce to the combination of the candidate nonce and the
--   nonce derived from the last block of the previous epoch.
-- - Update the "last block of previous epoch" nonce to the nonce derived
--   from the last applied block.
tickChainDepStatePolyPraos ::
  ConsensusConfig (Praos c) ->
  Views.PolyPraosLedgerView proto ->
  SlotNo ->
  PolyPraosState proto ->
  Ticked (PolyPraosState proto)
tickChainDepStatePolyPraos
  PraosConfig{praosEpochInfo}
  lv
  slot
  st =
    TickedPraosState
      { tickedPraosStateChainDepState = st'
      , tickedPraosStateLedgerView = lv
      }
   where
    newEpoch =
      isNewEpoch
        (History.toPureEpochInfo praosEpochInfo)
        (praosStateLastSlot st)
        slot
    st' =
      if newEpoch
        then
          st
            { praosStateEpochNonce =
                praosStateCandidateNonce st
                  ⭒ praosStateLastEpochBlockNonce st
            , praosStatePreviousEpochNonce =
                praosStateEpochNonce st
            , praosStateLastEpochBlockNonce =
                praosStateLabNonce st
            }
        else st

-- | 'updateChainDepState' for every Praos.
--
-- Validate and update the chain dependent state as a result of processing a
-- new header.
--
-- This consists of:
-- - Validate the VRF checks
-- - Validate the KES checks
-- - Call 'reupdateChainDepState'
updateChainDepStatePolyPraos ::
  ( PolyPraosCrypto proto c
  , Applicative (LeiosOnly proto ())
  , Foldable (LeiosOnly proto ())
  , TypeSwitch (LeiosOnly proto)
  ) =>
  ConsensusConfig (Praos c) ->
  Views.PolyPraosValidateView proto c ->
  SlotNo ->
  Ticked (PolyPraosState proto) ->
  Except (PolyPraosValidationErr proto c) (PolyPraosState proto)
updateChainDepStatePolyPraos
  cfg@( PraosConfig
          PraosParams{praosLeaderF}
          _
        )
  b
  slot
  tcs = do
    -- The Leios header checks are cheap, so they run first.
    leiosHeaderChecks cfg lv b slot cs
    -- First, we check the KES signature, which validates that the issuer is
    -- in fact who they say they are.
    validateKESSignature cfg lv (praosStateOCertCounters cs) b
    -- Then we examing the VRF proof, which confirms that they have the
    -- right to issue in this slot.
    validateVRFSignature (praosStateEpochNonce cs) lv praosLeaderF b
    -- Finally, we apply the changes from this header to the chain state.
    pure $ reupdateChainDepStatePolyPraos cfg b slot tcs
   where
    lv = tickedPraosStateLedgerView tcs
    cs = tickedPraosStateChainDepState tcs

-- | 'reupdateChainDepState' for every Praos.
--
-- Re-update the chain dependent state as a result of processing a header.
--
-- This consists of:
-- - Update the last applied block hash.
-- - Update the evolving and (potentially) candidate nonces based on the
--   position in the epoch.
-- - Update the operational certificate counter.
-- - Record the header's announcement, if any, replacing the previous one.
reupdateChainDepStatePolyPraos ::
  forall proto c.
  Functor (LeiosOnly proto ()) =>
  ConsensusConfig (Praos c) ->
  Views.PolyPraosValidateView proto c ->
  SlotNo ->
  Ticked (PolyPraosState proto) ->
  PolyPraosState proto
reupdateChainDepStatePolyPraos
  _cfg@( PraosConfig
           PraosParams{praosRandomnessStabilisationWindow}
           ei
         )
  b
  slot
  tcs =
    cs
      { praosStateLastSlot = NotOrigin slot
      , praosStateLabNonce = prevHashToNonce (Views.hvPrevHash b)
      , praosStateEvolvingNonce = newEvolvingNonce
      , praosStateCandidateNonce =
          if slot +* Duration praosRandomnessStabilisationWindow < firstSlotNextEpoch
            then newEvolvingNonce
            else praosStateCandidateNonce cs
      , praosStateOCertCounters =
          Map.insert hk n $ praosStateOCertCounters cs
      , praosStateLeiosAnnouncement =
          fmap
            (\(_containsCert, mbAnn) -> MkAnnouncedBy hk <$> mbAnn)
            (Views.hvLeios b)
      }
   where
    epochInfoWithErr =
      hoistEpochInfo
        (either throw pure . runExcept)
        ei
    firstSlotNextEpoch = runIdentity $ do
      EpochNo currentEpochNo <- epochInfoEpoch epochInfoWithErr slot
      let nextEpoch = EpochNo $ currentEpochNo + 1
      epochInfoFirst epochInfoWithErr nextEpoch
    cs = tickedPraosStateChainDepState tcs
    eta = vrfNonceValue (Proxy @c) $ Views.hvVrfRes b
    newEvolvingNonce = praosStateEvolvingNonce cs ⭒ eta
    OCert _ n _ _ = Views.hvOCert b
    hk = hashKey $ Views.hvVK b

-- | The Leios header checks that read only the header and the ledger view.
--
-- Sound out of context: the bound they check is forecast for the header's own
-- slot, so any path that validates the header reads the same value.
--
-- They run only for protocols with Leios, which are the ones that fill
-- 'typeSwitchR'.
leiosContextFreeHeaderChecks ::
  ( Applicative (LeiosOnly proto ())
  , Foldable (LeiosOnly proto ())
  , TypeSwitch (LeiosOnly proto)
  ) =>
  Views.PolyPraosLedgerView proto ->
  Views.PolyPraosValidateView proto c ->
  Except (PolyPraosValidationErr proto c) ()
leiosContextFreeHeaderChecks lv b =
  traverse_ check $
    (,,) <$> typeSwitchR <*> Views.hvLeios b <*> Views.plvMaxEbBodySize lv
 where
  check (err, (_containsCert, mbAnn), maxEbBodySize) =
    case mbAnn of
      SNothing -> pure ()
      SJust ann -> do
        let announced = ebReferencesAnnouncementSize ann
        when (announced > maxEbBodySize) $
          throwError $
            LeiosEbTooBig err announced maxEbBodySize

-- | The Leios-specific checks on a header, called by 'updateChainDepState'.
--
-- 'leiosContextFreeHeaderChecks' plus the one check that needs the header's
-- immediate predecessor: a block may not certify an announcement younger than
-- the certification gap, and only the predecessor's state says which
-- announcement that is.
leiosHeaderChecks ::
  ( Applicative (LeiosOnly proto ())
  , Foldable (LeiosOnly proto ())
  , TypeSwitch (LeiosOnly proto)
  ) =>
  ConsensusConfig (Praos c) ->
  Views.PolyPraosLedgerView proto ->
  Views.PolyPraosValidateView proto c ->
  SlotNo ->
  PolyPraosState proto ->
  Except (PolyPraosValidationErr proto c) ()
leiosHeaderChecks PraosConfig{praosEpochInfo} lv b slot cs = do
  leiosContextFreeHeaderChecks lv b
  traverse_ check $
    (,,,,,)
      <$> typeSwitchR
      <*> Views.hvLeios b
      <*> Views.plvAnnouncementPeriodLength lv
      <*> Views.plvVotePeriodLength lv
      <*> Views.plvDiffusionPeriodLength lv
      <*> praosStateLeiosAnnouncement cs
 where
  check
    ( err
      , (containsCert, _mbAnn)
      , announcementPeriod
      , votePeriod
      , diffusionPeriod
      , announcedByPredecessor
      ) =
      when containsCert $
        case (announcedByPredecessor, praosStateLastSlot cs) of
          (SJust{}, NotOrigin announcingSlot) -> do
            let earliestAllowed =
                  Leios.minCertificationSlot
                    ( runIdentity $
                        epochInfoSlotLength
                          (History.toPureEpochInfo praosEpochInfo)
                          slot
                    )
                    announcementPeriod
                    votePeriod
                    diffusionPeriod
                    announcingSlot
            when (slot < earliestAllowed) $
              throwError $
                LeiosCertTooYoung err announcingSlot slot earliestAllowed
          -- A state that announced an endorser block has necessarily applied a
          -- header, so 'Origin' is the same situation as announcing nothing.
          _ ->
            throwError $
              LeiosCertWithoutAnnouncement err

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
  forall proto c.
  ConsensusConfig (Praos c) ->
  Views.PolyPraosLedgerView proto ->
  SL.KeyHash SL.StakePool ->
  VRF.CertifiedVRF (VRF c) InputVRF ->
  Bool
meetsLeaderThreshold
  PraosConfig{praosParams}
  Views.PraosLedgerView{Views.plvPoolDistr}
  keyHash
  rho =
    checkLeaderNatValue
      (vrfLeaderValue (Proxy @c) rho)
      r
      (praosLeaderF praosParams)
   where
    SL.PoolDistr poolDistr _totalActiveStake = plvPoolDistr
    r =
      maybe 0 SL.individualPoolStake $
        Map.lookup keyHash poolDistr

validateVRFSignature ::
  forall proto c.
  PolyPraosCrypto proto c =>
  Nonce ->
  Views.PolyPraosLedgerView proto ->
  ActiveSlotCoeff ->
  Views.PolyPraosValidateView proto c ->
  Except (PolyPraosValidationErr proto c) ()
validateVRFSignature eta0 (Views.plvPoolDistr -> SL.PoolDistr pd _) =
  doValidateVRFSignature eta0 pd

-- NOTE: this function is much easier to test than 'validateVRFSignature' because we don't need
-- to construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateVRFSignature ::
  forall proto c.
  PolyPraosCrypto proto c =>
  Nonce ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  ActiveSlotCoeff ->
  Views.PolyPraosValidateView proto c ->
  Except (PolyPraosValidationErr proto c) ()
doValidateVRFSignature eta0 pd f b = do
  case Map.lookup hk pd of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (SL.IndividualPoolStake{SL.individualPoolStake = sigma, SL.individualPoolStakeVrf = vrfHK}) -> do
      let vrfHKStake = SL.fromVRFVerKeyHash vrfHK
          vrfHKBlock = VRF.hashVerKeyVRF vrfK
      vrfHKStake
        == vrfHKBlock
          ?! VRFKeyWrongVRFKey hk vrfHKStake vrfHKBlock
      VRF.verifyCertified
        ()
        vrfK
        (mkInputVRF slot eta0)
        vrfCert
        ?! VRFKeyBadProof slot eta0 vrfCert
      checkLeaderNatValue vrfLeaderVal sigma f
        ?! VRFLeaderValueTooBig (bvValue vrfLeaderVal) sigma f
 where
  hk = coerceKeyRole . hashKey . Views.hvVK $ b
  vrfK = Views.hvVrfVK b
  vrfCert = Views.hvVrfRes b
  vrfLeaderVal = vrfLeaderValue (Proxy @c) vrfCert
  slot = Views.hvSlotNo b

validateKESSignature ::
  PolyPraosCrypto proto c =>
  ConsensusConfig (Praos c) ->
  Views.PolyPraosLedgerView proto ->
  Map (KeyHash SL.BlockIssuer) Word64 ->
  Views.PolyPraosValidateView proto c ->
  Except (PolyPraosValidationErr proto c) ()
validateKESSignature
  _cfg@( PraosConfig
           PraosParams{praosMaxKESEvo, praosSlotsPerKESPeriod}
           _ei
         )
  Views.PraosLedgerView{Views.plvPoolDistr = SL.PoolDistr plvPoolDistr _totalActiveStake}
  ocertCounters =
    doValidateKESSignature praosMaxKESEvo praosSlotsPerKESPeriod plvPoolDistr ocertCounters

-- NOTE: This function is much easier to test than 'validateKESSignature' because we don't need to
-- construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateKESSignature ::
  PolyPraosCrypto proto c =>
  Word64 ->
  Word64 ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  Map (KeyHash SL.BlockIssuer) Word64 ->
  Views.PolyPraosValidateView proto c ->
  Except (PolyPraosValidationErr proto c) ()
doValidateKESSignature praosMaxKESEvo praosSlotsPerKESPeriod stakeDistribution ocertCounters b =
  do
    c0 <= kp ?! KESBeforeStartOCERT c0 kp
    kp_ < c0_ + fromIntegral praosMaxKESEvo ?! KESAfterEndOCERT kp c0 praosMaxKESEvo

    let t = if kp_ >= c0_ then kp_ - c0_ else 0
    -- this is required to prevent an arithmetic underflow, in the case of kp_ <
    -- c0_ we get the above `KESBeforeStartOCERT` failure in the transition.

    DSIGN.verifySignedDSIGN () vkcold (OCert.ocertToSignable oc) tau
      ?!: InvalidSignatureOCERT n c0
    KES.verifySignedKES () vk_hot t (Views.hvSigned b) (Views.hvSignature b)
      ?!: InvalidKesSignatureOCERT kp_ c0_ t praosMaxKESEvo

    case currentIssueNo of
      Nothing -> do
        throwError $ NoCounterForKeyHashOCERT hk
      Just m -> do
        m <= n ?! CounterTooSmallOCERT m n
        n <= m + 1 ?! CounterOverIncrementedOCERT m n
 where
  oc@(OCert vk_hot n c0@(KESPeriod c0_) tau) = Views.hvOCert b
  (VKey vkcold) = Views.hvVK b
  SlotNo s = Views.hvSlotNo b
  hk = hashKey $ Views.hvVK b
  kp@(KESPeriod kp_) =
    if praosSlotsPerKESPeriod == 0
      then error "kesPeriod: slots per KES period was set to zero"
      else KESPeriod . fromIntegral $ s `div` praosSlotsPerKESPeriod

  currentIssueNo :: Maybe Word64
  currentIssueNo
    | r@Just{} <- Map.lookup hk ocertCounters =
        r
    | Map.member (coerceKeyRole hk) stakeDistribution =
        Just 0
    | otherwise =
        Nothing

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data PraosCannotForge c
  = -- | The KES key in our operational certificate can't be used because the
    -- current (wall clock) period is before the start period of the key.
    -- current KES period.
    --
    -- Note: the opposite case, i.e., the wall clock period being after the
    -- end period of the key, is caught when trying to update the key in
    -- 'updateForgeState'.
    PraosCannotForgeKeyNotUsableYet
      -- | Current KES period according to the wallclock slot, i.e., the KES
      -- period in which we want to use the key.
      !OCert.KESPeriod
      -- | Start KES period of the KES key.
      !OCert.KESPeriod
  deriving Generic

deriving instance Crypto c => Show (PraosCannotForge c)

praosCheckCanForge ::
  ConsensusConfig (Praos c) ->
  SlotNo ->
  HotKey.KESInfo ->
  Either (PraosCannotForge c) ()
praosCheckCanForge
  PraosConfig{praosParams}
  curSlot
  kesInfo
    | let startPeriod = HotKey.kesStartPeriod kesInfo
    , startPeriod > wallclockPeriod =
        throwError $ PraosCannotForgeKeyNotUsableYet wallclockPeriod startPeriod
    | otherwise =
        return ()
   where
    -- The current wallclock KES period
    wallclockPeriod :: OCert.KESPeriod
    wallclockPeriod =
      OCert.KESPeriod $
        fromIntegral $
          unSlotNo curSlot `div` praosSlotsPerKESPeriod praosParams

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance PraosCrypto c => PraosProtocolSupportsNode (Praos c) where
  type PraosProtocolSupportsNodeCrypto (Praos c) = c

  getPraosNonces _prx = getPraosNoncesPolyPraos

  getOpCertCounters _prx = getOpCertCountersPolyPraos

-- | 'getPraosNonces' for every Praos.
getPraosNoncesPolyPraos :: PolyPraosState proto -> PraosNonces
getPraosNoncesPolyPraos cdst =
  PraosNonces
    { candidateNonce = praosStateCandidateNonce
    , epochNonce = praosStateEpochNonce
    , evolvingNonce = praosStateEvolvingNonce
    , labNonce = praosStateLabNonce
    , previousLabNonce = praosStateLastEpochBlockNonce
    }
 where
  PraosState
    { praosStateCandidateNonce
    , praosStateEpochNonce
    , praosStateEvolvingNonce
    , praosStateLabNonce
    , praosStateLastEpochBlockNonce
    } = cdst

-- | 'getOpCertCounters' for every Praos.
getOpCertCountersPolyPraos ::
  PolyPraosState proto -> Map (KeyHash SL.BlockIssuer) Word64
getOpCertCountersPolyPraos cdst =
  praosStateOCertCounters
 where
  PraosState
    { praosStateOCertCounters
    } = cdst

{-------------------------------------------------------------------------------
  Translation from transitional Praos
-------------------------------------------------------------------------------}

-- | We can translate between TPraos and Praos, provided:
--
-- - They share the same HASH algorithm
-- - They share the same ADDRHASH algorithm
-- - They share the same DSIGN verification keys
-- - They share the same VRF verification keys
instance TranslateProto (TPraos c) (Praos c) where
  translateLedgerView _ SL.TPraosLedgerView{SL.tplvPoolDistr, SL.tplvChainChecks} =
    Views.PraosLedgerView
      { Views.plvPoolDistr = tplvPoolDistr
      , Views.plvMaxHeaderSize = SL.ccMaxBHSize tplvChainChecks
      , Views.plvMaxBodySize = SL.ccMaxBBSize tplvChainChecks
      , Views.plvProtocolVersion = SL.ccProtocolVersion tplvChainChecks
      , Views.plvCommittee = PraosLacksLeios ()
      , Views.plvQuorumStakeThreshold = PraosLacksLeios ()
      , Views.plvAnnouncementPeriodLength = PraosLacksLeios ()
      , Views.plvVotePeriodLength = PraosLacksLeios ()
      , Views.plvDiffusionPeriodLength = PraosLacksLeios ()
      , Views.plvMaxEbBodySize = PraosLacksLeios ()
      , Views.plvMaxEbTxsSize = PraosLacksLeios ()
      }

  translateChainDepState _ tpState =
    PraosState
      { praosStateLastSlot = tpraosStateLastSlot tpState
      , praosStateOCertCounters = Map.mapKeysMonotonic coerce certCounters
      , praosStateEvolvingNonce = evolvingNonce
      , praosStateCandidateNonce = candidateNonce
      , praosStateEpochNonce = epochNonce
      , praosStatePreviousEpochNonce = epochNonce -- same as current epoch nonce
      , praosStateLabNonce = csLabNonce
      , praosStateLastEpochBlockNonce = SL.ticknStatePrevHashNonce csTickn
      , praosStateLeiosAnnouncement = PraosLacksLeios ()
      }
   where
    SL.ChainDepState{SL.csProtocol, SL.csTickn, SL.csLabNonce} =
      tpraosStateChainDepState tpState
    SL.PrtclState certCounters evolvingNonce candidateNonce =
      csProtocol
    epochNonce = SL.ticknStateEpochNonce csTickn

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

-- | Check value and raise error if it is false.
(?!) :: Bool -> e -> Except e ()
a ?! b = unless a $ throwError b

infix 1 ?!

(?!:) :: Either e1 a -> (e1 -> e2) -> Except e2 ()
(Right _) ?!: _ = pure ()
(Left e1) ?!: f = throwError $ f e1

infix 1 ?!:

instance Views.ForecastsLeios (Praos c) era where
  forecastToPolyPraosLedgerView (f :: SL.Forecast t era) =
    Views.PraosLedgerView
      { Views.plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
      , Views.plvMaxHeaderSize = ccMaxBHSize cc
      , Views.plvMaxBodySize = ccMaxBBSize cc
      , Views.plvProtocolVersion = ccProtocolVersion cc
      , Views.plvCommittee = PraosLacksLeios ()
      , Views.plvQuorumStakeThreshold = PraosLacksLeios ()
      , Views.plvAnnouncementPeriodLength = PraosLacksLeios ()
      , Views.plvVotePeriodLength = PraosLacksLeios ()
      , Views.plvDiffusionPeriodLength = PraosLacksLeios ()
      , Views.plvMaxEbBodySize = PraosLacksLeios ()
      , Views.plvMaxEbTxsSize = PraosLacksLeios ()
      }
   where
    cc = SL.forecastChainChecks @t @era f
