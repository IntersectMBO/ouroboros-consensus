{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , Praos
  , PraosCannotForge (..)
  , PraosCrypto
  , PraosFields (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  , PraosState (..)
  , PraosToSign (..)
  , PraosValidationErr (..)
  , Ticked (..)
  , forgePraosFields
  , praosCheckCanForge

    -- * For testing purposes
  , doValidateKESSignature
  , doValidateVRFSignature
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (ActiveSlotCoeff, Nonce, (⭒))
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Chain as SL
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys
  ( DSIGN
  , KeyHash
  , KeyRole (BlockIssuer)
  , VKey (VKey)
  , coerceKeyRole
  , hashKey
  )
import qualified Cardano.Ledger.Keys as SL
import Cardano.Ledger.Slot (Duration (Duration), (+*))
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (Crypto, KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.API as SL
import Cardano.Protocol.TPraos.BHeader
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
import Control.Monad (unless)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Coerce (coerce)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import Ouroboros.Consensus.Protocol.Praos.VRF
  ( InputVRF
  , mkInputVRF
  , vrfLeaderValue
  , vrfNonceValue
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos
  ( ConsensusConfig (TPraosConfig, tpraosEpochInfo, tpraosParams)
  , TPraos
  , TPraosState (tpraosStateChainDepState, tpraosStateLastSlot)
  )
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.Versioned
  ( VersionDecoder (Decode)
  , decodeVersion
  , encodeVersion
  )

data Praos c

class
  ( Crypto c
  , DSIGN.Signable DSIGN (OCertSignable c)
  , KES.Signable (KES c) (HeaderBody c)
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
  { praosToSignIssuerVK :: SL.VKey 'SL.BlockIssuer
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
    , praosCanBeLeaderOpCert
    }
  PraosIsLeader{praosIsLeaderVrfRes}
  mkToSign = do
    signature <- HotKey.sign hotKey toSign
    return
      PraosFields
        { praosSignature = signature
        , praosToSign = toSign
        }
   where
    toSign = mkToSign signedFields

    signedFields =
      PraosToSign
        { praosToSignIssuerVK = praosCanBeLeaderColdVerKey
        , praosToSignVrfVK = VRF.deriveVerKeyVRF praosCanBeLeaderSignKeyVRF
        , praosToSignVrfRes = praosIsLeaderVrfRes
        , praosToSignOCert = praosCanBeLeaderOpCert
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

instance PraosCrypto c => NoThunks (ConsensusConfig (Praos c))

type PraosValidateView c = Views.HeaderView c

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
  , praosStateOCertCounters :: !(Map (KeyHash 'BlockIssuer) Word64)
  -- ^ Operation Certificate counters
  , praosStateEvolvingNonce :: !Nonce
  -- ^ Evolving nonce
  , praosStateCandidateNonce :: !Nonce
  -- ^ Candidate nonce
  , praosStateEpochNonce :: !Nonce
  -- ^ Epoch nonce
  , praosStateLabNonce :: !Nonce
  -- ^ Nonce constructed from the hash of the previous block
  , praosStateLastEpochBlockNonce :: !Nonce
  -- ^ Nonce corresponding to the LAB nonce of the last block of the previous
  -- epoch
  }
  deriving (Generic, Show, Eq)

instance NoThunks PraosState

instance ToCBOR PraosState where
  toCBOR = encode

instance FromCBOR PraosState where
  fromCBOR = decode

instance Serialise PraosState where
  encode
    PraosState
      { praosStateLastSlot
      , praosStateOCertCounters
      , praosStateEvolvingNonce
      , praosStateCandidateNonce
      , praosStateEpochNonce
      , praosStateLabNonce
      , praosStateLastEpochBlockNonce
      } =
      encodeVersion 0 $
        mconcat
          [ CBOR.encodeListLen 7
          , toCBOR praosStateLastSlot
          , toCBOR praosStateOCertCounters
          , toCBOR praosStateEvolvingNonce
          , toCBOR praosStateCandidateNonce
          , toCBOR praosStateEpochNonce
          , toCBOR praosStateLabNonce
          , toCBOR praosStateLastEpochBlockNonce
          ]

  decode =
    decodeVersion
      [(0, Decode decodePraosState)]
   where
    decodePraosState = do
      enforceSize "PraosState" 7
      PraosState
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR

data instance Ticked PraosState = TickedPraosState
  { tickedPraosStateChainDepState :: PraosState
  , tickedPraosStateLedgerView :: Views.LedgerView
  }

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
      !(KeyHash 'BlockIssuer) -- stake pool key hash
  deriving Generic

deriving instance PraosCrypto c => Eq (PraosValidationErr c)

deriving instance PraosCrypto c => NoThunks (PraosValidationErr c)

deriving instance PraosCrypto c => Show (PraosValidationErr c)

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  type ChainDepState (Praos c) = PraosState
  type IsLeader (Praos c) = PraosIsLeader c
  type CanBeLeader (Praos c) = PraosCanBeLeader c
  type SelectView (Praos c) = PraosChainSelectView c
  type LedgerView (Praos c) = Views.LedgerView
  type ValidationErr (Praos c) = PraosValidationErr c
  type ValidateView (Praos c) = PraosValidateView c

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
  -- epoch, we do two things:
  -- - Update the epoch nonce to the combination of the candidate nonce and the
  --   nonce derived from the last block of the previous epoch.
  -- - Update the "last block of previous epoch" nonce to the nonce derived from
  --   the last applied block.
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
              , praosStateLastEpochBlockNonce = praosStateLabNonce st
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
  reupdateChainDepState
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

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
  forall c.
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  SL.KeyHash 'SL.StakePool ->
  VRF.CertifiedVRF (VRF c) InputVRF ->
  Bool
meetsLeaderThreshold
  PraosConfig{praosParams}
  Views.LedgerView{Views.lvPoolDistr}
  keyHash
  rho =
    checkLeaderNatValue
      (vrfLeaderValue (Proxy @c) rho)
      r
      (praosLeaderF praosParams)
   where
    SL.PoolDistr poolDistr _totalActiveStake = lvPoolDistr
    r =
      maybe 0 SL.individualPoolStake $
        Map.lookup keyHash poolDistr

validateVRFSignature ::
  forall c.
  PraosCrypto c =>
  Nonce ->
  Views.LedgerView ->
  ActiveSlotCoeff ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
validateVRFSignature eta0 (Views.lvPoolDistr -> SL.PoolDistr pd _) =
  doValidateVRFSignature eta0 pd

-- NOTE: this function is much easier to test than 'validateVRFSignature' because we don't need
-- to construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateVRFSignature ::
  forall c.
  PraosCrypto c =>
  Nonce ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  ActiveSlotCoeff ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
doValidateVRFSignature eta0 pd f b = do
  case Map.lookup hk pd of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (SL.IndividualPoolStake sigma _totalPoolStake vrfHK) -> do
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
  PraosCrypto c =>
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  Map (KeyHash 'BlockIssuer) Word64 ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
validateKESSignature
  _cfg@( PraosConfig
           PraosParams{praosMaxKESEvo, praosSlotsPerKESPeriod}
           _ei
         )
  Views.LedgerView{Views.lvPoolDistr = SL.PoolDistr lvPoolDistr _totalActiveStake}
  ocertCounters =
    doValidateKESSignature praosMaxKESEvo praosSlotsPerKESPeriod lvPoolDistr ocertCounters

-- NOTE: This function is much easier to test than 'validateKESSignature' because we don't need to
-- construct a 'PraosConfig' nor 'LedgerView' to test it.
doValidateKESSignature ::
  PraosCrypto c =>
  Word64 ->
  Word64 ->
  Map (KeyHash SL.StakePool) SL.IndividualPoolStake ->
  Map (KeyHash BlockIssuer) Word64 ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
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
    | Map.member hk ocertCounters = Map.lookup hk ocertCounters
    | Set.member (coerceKeyRole hk) (Map.keysSet stakeDistribution) =
        Just 0
    | otherwise = Nothing

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
  translateLedgerView _ SL.LedgerView{SL.lvPoolDistr, SL.lvChainChecks} =
    Views.LedgerView
      { Views.lvPoolDistr = lvPoolDistr
      , Views.lvMaxHeaderSize = SL.ccMaxBHSize lvChainChecks
      , Views.lvMaxBodySize = SL.ccMaxBBSize lvChainChecks
      , Views.lvProtocolVersion = SL.ccProtocolVersion lvChainChecks
      }

  translateChainDepState _ tpState =
    PraosState
      { praosStateLastSlot = tpraosStateLastSlot tpState
      , praosStateOCertCounters = Map.mapKeysMonotonic coerce certCounters
      , praosStateEvolvingNonce = evolvingNonce
      , praosStateCandidateNonce = candidateNonce
      , praosStateEpochNonce = SL.ticknStateEpochNonce csTickn
      , praosStateLabNonce = csLabNonce
      , praosStateLastEpochBlockNonce = SL.ticknStatePrevHashNonce csTickn
      }
   where
    SL.ChainDepState{SL.csProtocol, SL.csTickn, SL.csLabNonce} =
      tpraosStateChainDepState tpState
    SL.PrtclState certCounters evolvingNonce candidateNonce =
      csProtocol

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
