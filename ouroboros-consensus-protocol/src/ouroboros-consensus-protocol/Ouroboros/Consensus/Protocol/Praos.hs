{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Protocol.Praos
  ( AnnouncedBy (..)
  , ConsensusConfig (..)
  , Praos
  , PraosCannotForge (..)
  , PraosCrypto
  , PraosFields (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  , PraosState (..)
  , PraosToSign (..)
  , PraosValidationErr (..)
  , PraosWithLeiosValidationErr (..)
  , PraosWithLeios
  , PraosWithLeiosState (..)
  , Ticked (..)
  , forgePraosFields
  , praosCheckCanForge

    -- * For testing purposes
  , doValidateKESSignature
  , doValidateKESSignatureWorker
  , WhetherToUpperBoundOCERT (..)
  , doValidateVRFSignature
  ) where

import Cardano.Binary (Decoder, FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff
  , Nonce
  , StrictMaybe (..)
  , (⭒)
  )
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Chain as SL
import Cardano.Ledger.Core (fromEraCBOR, toEraCBOR)
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys
  ( DSIGN
  , KeyHash
  , VKey (VKey)
  , coerceKeyRole
  , hashKey
  )
import qualified Cardano.Ledger.Keys as SL
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (Duration (Duration), (+*))
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (Crypto, KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
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
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (decode, encode))
import Control.Exception (throw)
import Control.Monad (unless, when)
import Control.Monad.Except (Except, runExcept, throwError, withExcept)
import Data.Coerce (coerce)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoTypes
  ( EbAnnouncement
  , decodeEbAnnouncement
  , ebAnnouncementSize
  , encodeEbAnnouncement
  , minCertificationSlot
  )
import qualified LeiosDemoTypes as Leios
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Protocol.Praos.Common
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos
  ( ConsensusConfig (TPraosConfig, tpraosEpochInfo, tpraosParams)
  , TPraos
  , TPraosState (tpraosStateChainDepState, tpraosStateLastSlot)
  )
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.CBOR
  ( decodeNullStrictMaybe
  , encodeNullStrictMaybe
  )
import Ouroboros.Consensus.Util.Versioned
  ( VersionDecoder (Decode)
  , decodeVersion
  , encodeVersion
  )

-- | The base Praos protocol.
data Praos c

-- | Praos with the Leios extension.
--
-- A separate type rather than an index on 'Praos'. The two share their
-- configuration, their nonce handling, their leader check and both signature
-- checks --- as ordinary functions, the way 'TPraos' and 'Praos' already share
-- code. What they do not share is the chain-dep state's extra field, the header
-- body that gets signed, and the Leios header checks; a single definition
-- covering both needed a singleton to case on and two dictionary-summoning
-- helpers before GHC would accept it.
data PraosWithLeios c

class
  ( Crypto c
  , DSIGN.Signable DSIGN (OCertSignable c)
  , KES.Signable (KES c) (LeiosCodec.HeaderBody c)
  , KES.Signable (KES c) (PraosCodec.HeaderBody c)
  , VRF.Signable (VRF c) InputVRF
  ) =>
  PraosCrypto c

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
  (NoThunks toSign, PraosCrypto c) =>
  NoThunks (PraosFields c toSign)

deriving instance
  (Show toSign, PraosCrypto c) =>
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

instance PraosCrypto c => NoThunks (PraosToSign c)

deriving instance PraosCrypto c => Show (PraosToSign c)

forgePraosFields ::
  ( PraosCrypto c
  , KES.Signable (KES c) toSign
  , Monad m
  ) =>
  HotKey c m ->
  CanBeLeader (Praos c) ->
  IsLeader (Praos c) ->
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

instance PraosCrypto c => NoThunks (PraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams :: !PraosParams
  , praosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
  -- it's useful for this record to be EpochInfo and one other thing,
  -- because the one other thing can then be used as the
  -- PartialConsensConfig in the HFC instance.
  }
  deriving Generic

-- | The Leios extension configures nothing of its own, so it reuses Praos's
-- configuration whole.
newtype instance ConsensusConfig (PraosWithLeios c) = PraosWithLeiosConfig
  { praosConfigOfLeios :: ConsensusConfig (Praos c)
  }
  deriving Generic

instance PraosCrypto c => NoThunks (ConsensusConfig (Praos c))

instance PraosCrypto c => NoThunks (ConsensusConfig (PraosWithLeios c))

instance HasMaxMajorProtVer (Praos c) where
  protoMaxMajorPV = praosMaxMajorPV . praosParams

instance HasMaxMajorProtVer (PraosWithLeios c) where
  protoMaxMajorPV = protoMaxMajorPV . praosConfigOfLeios

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

-- | Praos consensus state.
--
-- We track the last slot and the counters for operational certificates, as well
-- as a series of nonces which get updated in different ways over the course of
-- an epoch.
data PraosState = PraosState
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
  }
  deriving (Generic, Show, Eq)

-- | 'PraosState' plus what the Leios extension tracks.
data PraosWithLeiosState = PraosWithLeiosState
  { pwlsPraos :: !PraosState
  , pwlsLeiosAnnouncement :: !(StrictMaybe AnnouncedBy)
  -- ^ The Leios 'EbAnnouncement' from the most recently applied header on
  -- this chain — overwritten on every header tick (so a header with no
  -- announcement clears the field). The 'ResolveLeiosBlock' instance for
  -- the Dijkstra-era CardanoBlock reads this to look up the EB closure
  -- that a certifying block's 'LeiosCert' refers to; only the
  -- immediately-previous announcement is ever certified.
  }
  deriving (Generic, Show, Eq)

-- | An EB announcement, and the issuer of the header that carried it.
--
-- With that header's slot ('praosStateLastSlot') the issuer gives the
-- election. A CertRB is validated against its predecessor's chain-dep state,
-- so this is what lets the arrival of a CertRB /header/ name the election it
-- claims a certificate for --- which is how a peer claiming two different
-- ones for a single election is caught (see
-- @LeiosDemoLogic.checkMsgRollForwardForLeiosOffers@).
data AnnouncedBy = MkAnnouncedBy
  { announcedByIssuer :: !(KeyHash SL.BlockIssuer)
  , announcedEb :: !EbAnnouncement
  }
  deriving (Generic, Show, Eq, NoThunks)

encodeAnnouncedBy :: AnnouncedBy -> CBOR.Encoding
encodeAnnouncedBy (MkAnnouncedBy issuer ann) =
  CBOR.encodeListLen 2 <> toCBOR issuer <> encodeEbAnnouncement ann

decodeAnnouncedBy :: Decoder s AnnouncedBy
decodeAnnouncedBy = do
  enforceSize "AnnouncedBy" 2
  MkAnnouncedBy <$> fromCBOR <*> decodeEbAnnouncement

instance NoThunks PraosState

instance NoThunks PraosWithLeiosState

instance ToCBOR PraosState where
  toCBOR = encode

instance FromCBOR PraosState where
  fromCBOR = decode

instance ToCBOR PraosWithLeiosState where
  toCBOR = encode

instance FromCBOR PraosWithLeiosState where
  fromCBOR = decode

-- | The eight fields of 'PraosState', in order.
--
-- 'PraosWithLeiosState' writes these and then its own, which is the whole of
-- how the two formats relate.
encodePraosStateFields :: PraosState -> [CBOR.Encoding]
encodePraosStateFields
  PraosState
    { praosStateLastSlot
    , praosStateOCertCounters
    , praosStateEvolvingNonce
    , praosStateCandidateNonce
    , praosStateEpochNonce
    , praosStatePreviousEpochNonce
    , praosStateLabNonce
    , praosStateLastEpochBlockNonce
    } =
    [ toCBOR praosStateLastSlot
    , toCBOR praosStateOCertCounters
    , toEraCBOR @ShelleyEra praosStateEvolvingNonce
    , toEraCBOR @ShelleyEra praosStateCandidateNonce
    , toEraCBOR @ShelleyEra praosStateEpochNonce
    , toEraCBOR @ShelleyEra praosStatePreviousEpochNonce
    , toEraCBOR @ShelleyEra praosStateLabNonce
    , toEraCBOR @ShelleyEra praosStateLastEpochBlockNonce
    ]

decodePraosStateFields :: Decoder s PraosState
decodePraosStateFields =
  PraosState
    <$> fromCBOR
    <*> fromCBOR
    <*> fromEraCBOR @ShelleyEra
    <*> fromEraCBOR @ShelleyEra
    <*> fromEraCBOR @ShelleyEra
    <*> fromEraCBOR @ShelleyEra
    <*> fromEraCBOR @ShelleyEra
    <*> fromEraCBOR @ShelleyEra

-- | Mainnet's format, unchanged: version 0, eight fields.
instance Serialise PraosState where
  encode st =
    encodeVersion 0 $ mconcat $ CBOR.encodeListLen 8 : encodePraosStateFields st

  decode = decodeVersion [(0, Decode dec)]
   where
    dec :: forall s. Decoder s PraosState
    dec = enforceSize "PraosState" 8 >> decodePraosStateFields

-- | A format of its own, which merely also starts counting: version 1 is what
-- the code before this split wrote for Dijkstra -- back when Dijkstra was
-- paired with 'Praos' -- with the same fields in the same order. A node already
-- running the Leios prototype can therefore still decode the chain-dep state in
-- the snapshots it has on disk.
instance Serialise PraosWithLeiosState where
  encode (PraosWithLeiosState base ann) =
    encodeVersion 1 $
      mconcat $
        CBOR.encodeListLen 9
          : encodePraosStateFields base
            <> [encodeNullStrictMaybe encodeAnnouncedBy ann]

  decode = decodeVersion [(1, Decode dec)]
   where
    dec :: forall s. Decoder s PraosWithLeiosState
    dec = do
      enforceSize "PraosWithLeiosState" 9
      PraosWithLeiosState
        <$> decodePraosStateFields
        <*> decodeNullStrictMaybe decodeAnnouncedBy

data instance Ticked PraosState = TickedPraosState
  { tickedPraosStateChainDepState :: PraosState
  , tickedPraosStateLedgerView :: Views.PraosLedgerView
  }

data instance Ticked PraosWithLeiosState = TickedPraosWithLeiosState
  { tickedPraosWithLeiosStateChainDepState :: PraosWithLeiosState
  , tickedPraosWithLeiosStateLedgerView :: Views.PraosWithLeiosLedgerView
  }

-----

-- | Errors which we might encounter
data PraosValidationErr c
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
  deriving Generic

deriving instance PraosCrypto c => Eq (PraosValidationErr c)

deriving instance PraosCrypto c => NoThunks (PraosValidationErr c)

deriving instance PraosCrypto c => Show (PraosValidationErr c)

-- | A header fails either the base protocol's checks or Leios's own, and
-- nothing checks both at once, so this is a sum rather than one flat type with
-- a Leios constructor the base protocol could never throw.
data PraosWithLeiosValidationErr c
  = PraosErr !(PraosValidationErr c)
  | LeiosErr !Leios.LeiosHeaderErr
  deriving Generic

deriving instance PraosCrypto c => Eq (PraosWithLeiosValidationErr c)

deriving instance PraosCrypto c => NoThunks (PraosWithLeiosValidationErr c)

deriving instance PraosCrypto c => Show (PraosWithLeiosValidationErr c)

{-------------------------------------------------------------------------------
  The one part both protocols share

  Everything else 'PraosWithLeios' needs from 'Praos' it gets by calling the
  'Praos' method; only the methods that take a 'ValidateView' cannot delegate,
  because the two protocols sign different header bodies. This is the body of
  'reupdateChainDepState', lifted out for exactly that reason.
-------------------------------------------------------------------------------}

-- | The six chain-dep-state fields a header updates.
--
-- @body@ is whatever the protocol signs; nothing here reads it.
reupdatePraosState ::
  forall body c.
  PraosParams ->
  EpochInfo (Except History.PastHorizonException) ->
  Views.HeaderView body c ->
  SlotNo ->
  PraosState ->
  PraosState
reupdatePraosState
  PraosParams{praosRandomnessStabilisationWindow}
  ei
  b
  slot
  cs =
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
    eta = vrfNonceValue (Proxy @c) $ Views.hvVrfRes b
    newEvolvingNonce = praosStateEvolvingNonce cs ⭒ eta
    OCert _ n _ _ = Views.hvOCert b
    hk = hashKey $ Views.hvVK b

-- | The base protocol's ticked state, as it sits inside this one's.
--
-- What lets 'PraosWithLeios' hand its state to a 'Praos' method.
basePraosTicked :: Ticked PraosWithLeiosState -> Ticked PraosState
basePraosTicked tcs =
  TickedPraosState
    { tickedPraosStateChainDepState =
        pwlsPraos (tickedPraosWithLeiosStateChainDepState tcs)
    , tickedPraosStateLedgerView =
        Views.pwlvBase (tickedPraosWithLeiosStateLedgerView tcs)
    }

{-------------------------------------------------------------------------------
  The two instances
-------------------------------------------------------------------------------}

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  type ChainDepState (Praos c) = PraosState
  type IsLeader (Praos c) = PraosIsLeader c
  type CanBeLeader (Praos c) = PraosCanBeLeader c
  type TiebreakerView (Praos c) = PraosTiebreakerView c
  type LedgerView (Praos c) = Views.PraosLedgerView
  type ValidationErr (Praos c) = PraosValidationErr c
  type ValidateView (Praos c) = Views.PraosHeaderView c

  protocolSecurityParam = praosSecurityParam . praosParams

  checkIsLeader
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
  tickChainDepState
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

  -- Validate and update the chain dependent state as a result of processing a
  -- new header.
  --
  -- This consists of:
  -- - Validate the VRF checks
  -- - Validate the KES checks
  -- - Call 'reupdateChainDepState'
  --
  updateChainDepState
    cfg@( PraosConfig
            PraosParams{praosLeaderF}
            _
          )
    b
    slot
    tcs = do
      -- First, we check the KES signature, which validates that the issuer is
      -- in fact who they say they are.
      validateKESSignature cfg lv (praosStateOCertCounters cs) b
      -- Then we examing the VRF proof, which confirms that they have the
      -- right to issue in this slot.
      validateVRFSignature (praosStateEpochNonce cs) lv praosLeaderF b
      -- Finally, we apply the changes from this header to the chain state.
      pure $ reupdateChainDepState cfg b slot tcs
     where
      lv = tickedPraosStateLedgerView tcs
      cs = tickedPraosStateChainDepState tcs

  -- Re-update the chain dependent state as a result of processing a header.
  --
  -- This consists of:
  -- - Update the last applied block hash.
  -- - Update the evolving and (potentially) candidate nonces based on the
  --   position in the epoch.
  -- - Update the operational certificate counter.
  reupdateChainDepState PraosConfig{praosParams, praosEpochInfo} b slot tcs =
    reupdatePraosState
      praosParams
      praosEpochInfo
      b
      slot
      (tickedPraosStateChainDepState tcs)

-- | Praos with Leios reuses the base protocol wherever the method does not
-- take a 'ValidateView': those it simply calls, handing over the base state it
-- carries. 'updateChainDepState' and 'reupdateChainDepState' cannot, because
-- the two protocols sign different header bodies, so they call the shared
-- checks directly.
instance PraosCrypto c => ConsensusProtocol (PraosWithLeios c) where
  type ChainDepState (PraosWithLeios c) = PraosWithLeiosState
  type IsLeader (PraosWithLeios c) = PraosIsLeader c
  type CanBeLeader (PraosWithLeios c) = PraosCanBeLeader c
  type TiebreakerView (PraosWithLeios c) = PraosTiebreakerView c
  type LedgerView (PraosWithLeios c) = Views.PraosWithLeiosLedgerView
  type ValidationErr (PraosWithLeios c) = PraosWithLeiosValidationErr c
  type ValidateView (PraosWithLeios c) = Views.LeiosHeaderView c

  protocolSecurityParam = protocolSecurityParam @(Praos c) . praosConfigOfLeios

  checkIsLeader cfg cbl slot tcs =
    checkIsLeader @(Praos c) (praosConfigOfLeios cfg) cbl slot (basePraosTicked tcs)

  tickChainDepState cfg lv slot st =
    TickedPraosWithLeiosState
      { tickedPraosWithLeiosStateChainDepState =
          st
            { pwlsPraos =
                tickedPraosStateChainDepState $
                  tickChainDepState @(Praos c)
                    (praosConfigOfLeios cfg)
                    (Views.pwlvBase lv)
                    slot
                    (pwlsPraos st)
            }
      , tickedPraosWithLeiosStateLedgerView = lv
      }

  updateChainDepState cfg b slot tcs = do
    -- The Leios header checks. Cheap, so they run before the signature
    -- checks.
    --
    -- NB cert/txs exclusivity is not among these: it is a property of the
    -- body, and 'blockMatchesHeader' already enforces it where the body is
    -- in hand. Nor is the EB closure's size: the announcement carries only
    -- one size, and it is the body's.
    withExcept LeiosErr $
      leiosHeaderChecks baseCfg (Views.pwlvLeios lv) b slot cs
    withExcept PraosErr $ do
      validateKESSignature baseCfg baseLv (praosStateOCertCounters baseCs) baseB
      validateVRFSignature (praosStateEpochNonce baseCs) baseLv praosLeaderF baseB
    pure $ reupdateChainDepState cfg b slot tcs
   where
    baseCfg@(PraosConfig PraosParams{praosLeaderF} _) = praosConfigOfLeios cfg
    lv = tickedPraosWithLeiosStateLedgerView tcs
    baseLv = Views.pwlvBase lv
    cs = tickedPraosWithLeiosStateChainDepState tcs
    baseCs = pwlsPraos cs
    baseB = Views.lhvBase b

  reupdateChainDepState cfg b slot tcs =
    PraosWithLeiosState
      { pwlsPraos =
          reupdatePraosState
            praosParams
            praosEpochInfo
            (Views.lhvBase b)
            slot
            (pwlsPraos cs)
      , -- Overwritten on every header, so a header with no announcement
        -- clears the field.
        pwlsLeiosAnnouncement =
          MkAnnouncedBy (hashKey (Views.hvVK (Views.lhvBase b)))
            <$> Views.lhvAnnouncement b
      }
   where
    PraosConfig{praosParams, praosEpochInfo} = praosConfigOfLeios cfg
    cs = tickedPraosWithLeiosStateChainDepState tcs

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
  forall c.
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
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
  forall body c.
  PraosCrypto c =>
  Nonce ->
  Views.PraosLedgerView ->
  ActiveSlotCoeff ->
  Views.HeaderView body c ->
  Except (PraosValidationErr c) ()
validateVRFSignature eta0 (Views.plvPoolDistr -> SL.PoolDistr pd _) =
  doValidateVRFSignature eta0 pd

-- NOTE: this function is much easier to test than 'validateVRFSignature' because we don't need
-- to construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateVRFSignature ::
  forall body c.
  PraosCrypto c =>
  Nonce ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  ActiveSlotCoeff ->
  Views.HeaderView body c ->
  Except (PraosValidationErr c) ()
doValidateVRFSignature eta0 pd f b = do
  case Map.lookup hk pd of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (SL.IndividualPoolStake sigma _totalPoolStake vrfHK _leiosKey) -> do
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

-- | The Leios-specific checks on a header, called by 'updateChainDepState'
--
-- No dispatch: this runs only for 'PraosWithLeios', so the Leios fields of the
-- header view, the ledger view and the chain-dep state are simply there.
leiosHeaderChecks ::
  forall c.
  ConsensusConfig (Praos c) ->
  Views.LeiosLedgerView ->
  Views.LeiosHeaderView c ->
  SlotNo ->
  PraosWithLeiosState ->
  -- | Only the Leios errors; the caller tags them
  Except Leios.LeiosHeaderErr ()
leiosHeaderChecks PraosConfig{praosEpochInfo} llv b slot cs = do
  -- Note that the genesis state doesn't announce an EB.
  when (Views.lhvContainsCert b) $
    case (pwlsLeiosAnnouncement cs, praosStateLastSlot (pwlsPraos cs)) of
      (SJust{}, NotOrigin announcingSlot) -> do
        let earliestAllowed =
              minCertificationSlot
                ( runIdentity $
                    epochInfoSlotLength
                      (History.toPureEpochInfo praosEpochInfo)
                      slot
                )
                (Views.llvAnnouncementPeriodLength llv)
                (Views.llvVotePeriodLength llv)
                (Views.llvDiffusionPeriodLength llv)
                announcingSlot
        when (slot < earliestAllowed) $
          throwError $
            Leios.LeiosCertTooYoung announcingSlot slot earliestAllowed
      -- A state that announced an EB has necessarily applied a header, so
      -- 'Origin' is the same situation as announcing nothing.
      _ -> throwError Leios.LeiosCertWithoutAnnouncement

  case Views.lhvAnnouncement b of
    SNothing -> pure ()
    SJust ann -> do
      let announced = ebAnnouncementSize ann
          maximum' = Views.llvMaxEbBodySize llv
      when (announced > maximum') $
        throwError $
          Leios.LeiosEbTooBig announced maximum'

validateKESSignature ::
  (PraosCrypto c, KES.Signable (KES c) body) =>
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  Map (KeyHash SL.BlockIssuer) Word64 ->
  Views.HeaderView body c ->
  Except (PraosValidationErr c) ()
validateKESSignature
  _cfg@( PraosConfig
           PraosParams{praosMaxKESEvo, praosSlotsPerKESPeriod}
           _ei
         )
  Views.PraosLedgerView{Views.plvPoolDistr = SL.PoolDistr lvPoolDistr _totalActiveStake}
  ocertCounters =
    doValidateKESSignature praosMaxKESEvo praosSlotsPerKESPeriod lvPoolDistr ocertCounters

-- | Whether 'doValidateKESSignatureWorker' enforces the OCERT counter's /upper/
-- bound, i.e. rejects (with 'CounterOverIncrementedOCERT') a counter more than
-- one greater than the one we have recorded for this issuer.
--
-- Normal header validation enforces it ('UpperBoundOCERT'). Validating a
-- relayed Leios announcement out-of-context against a (possibly lagging)
-- immutable tip legitimately sees counters that have run ahead of our recorded
-- view, so that path skips it ('DoNotUpperBoundOCERT'). The counter's /lower/
-- bound ('CounterTooSmallOCERT', a revoked key) is enforced either way.
data WhetherToUpperBoundOCERT
  = UpperBoundOCERT
  | DoNotUpperBoundOCERT
  deriving (Eq, Show)

-- NOTE: This function is much easier to test than 'validateKESSignature' because we don't need to
-- construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateKESSignature ::
  (PraosCrypto c, KES.Signable (KES c) body) =>
  Word64 ->
  Word64 ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  Map (KeyHash SL.BlockIssuer) Word64 ->
  Views.HeaderView body c ->
  Except (PraosValidationErr c) ()
doValidateKESSignature = doValidateKESSignatureWorker UpperBoundOCERT

-- | The worker underlying 'doValidateKESSignature', parameterized by whether to
-- enforce the OCERT counter's upper bound (see 'WhetherToUpperBoundOCERT').
doValidateKESSignatureWorker ::
  forall body c.
  (PraosCrypto c, KES.Signable (KES c) body) =>
  WhetherToUpperBoundOCERT ->
  Word64 ->
  Word64 ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  Map (KeyHash SL.BlockIssuer) Word64 ->
  Views.HeaderView body c ->
  Except (PraosValidationErr c) ()
doValidateKESSignatureWorker whetherToUpperBound praosMaxKESEvo praosSlotsPerKESPeriod stakeDistribution ocertCounters b =
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
        case whetherToUpperBound of
          UpperBoundOCERT -> n <= m + 1 ?! CounterOverIncrementedOCERT m n
          DoNotUpperBoundOCERT -> pure ()
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

deriving instance PraosCrypto c => Show (PraosCannotForge c)

praosCheckCanForge ::
  -- | Slots per KES period; see 'configSlotsPerKESPeriod'
  Word64 ->
  SlotNo ->
  HotKey.KESInfo ->
  Either (PraosCannotForge c) ()
praosCheckCanForge
  slotsPerKESPeriod
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
          unSlotNo curSlot `div` slotsPerKESPeriod

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance PraosCrypto c => PraosProtocolSupportsNode (Praos c) where
  type PraosProtocolSupportsNodeCrypto (Praos c) = c

  getPraosNonces _prx cdst =
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

  getOpCertCounters _prx cdst =
    praosStateOCertCounters
   where
    PraosState
      { praosStateOCertCounters
      } = cdst

instance PraosCrypto c => PraosProtocolSupportsNode (PraosWithLeios c) where
  type PraosProtocolSupportsNodeCrypto (PraosWithLeios c) = c

  getPraosNonces _prx = getPraosNonces (Proxy @(Praos c)) . pwlsPraos

  getOpCertCounters _prx = getOpCertCounters (Proxy @(Praos c)) . pwlsPraos

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
      }
   where
    SL.ChainDepState{SL.csProtocol, SL.csTickn, SL.csLabNonce} =
      tpraosStateChainDepState tpState
    SL.PrtclState certCounters evolvingNonce candidateNonce =
      csProtocol
    epochNonce = SL.ticknStateEpochNonce csTickn

-- | Composed out of the two translations either side of it, rather than
-- repeating the projections.
instance TranslateProto (TPraos c) (PraosWithLeios c) where
  translateLedgerView _ =
    translateLedgerView (Proxy @(Praos c, PraosWithLeios c))
      . translateLedgerView (Proxy @(TPraos c, Praos c))

  translateChainDepState _ =
    translateChainDepState (Proxy @(Praos c, PraosWithLeios c))
      . translateChainDepState (Proxy @(TPraos c, Praos c))

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

-- | Crossing from the base protocol into the Leios extension.
--
-- Everything carries over unchanged; only the Leios announcement has to be
-- introduced, and it starts empty, since no header of the extension we are
-- leaving could have carried one.
instance TranslateProto (Praos c) (PraosWithLeios c) where
  -- The Leios data has to be conjured from a state that has none; see
  -- 'Views.initialLeiosLedgerView'. Everything else carries over by being the
  -- very same record.
  translateLedgerView _ lv =
    Views.PraosWithLeiosLedgerView
      { Views.pwlvBase = lv
      , Views.pwlvLeios = Views.initialLeiosLedgerView
      }

  -- The announcement starts empty, since no header of the protocol we are
  -- leaving could have carried one.
  translateChainDepState _ st =
    PraosWithLeiosState
      { pwlsPraos = st
      , pwlsLeiosAnnouncement = SNothing
      }
