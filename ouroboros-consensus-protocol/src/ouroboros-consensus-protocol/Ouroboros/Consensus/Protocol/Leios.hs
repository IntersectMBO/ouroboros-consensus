{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Leios consensus protocol, a variant of Praos that uses the Leios block
-- header type (with two additional fields for Leios-specific functionality).
module Ouroboros.Consensus.Protocol.Leios
  ( Leios
  , LeiosCrypto
  , ConsensusConfig (..)
  , LeiosHeaderView (..)
  ) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (ActiveSlotCoeff, Nonce, (⭒))
import Cardano.Ledger.Keys
  ( KeyHash
  , KeyRole (BlockIssuer)
  , VKey (VKey)
  , coerceKeyRole
  , hashKey
  )
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Slot (Duration (Duration), (+*))
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Cardano.Protocol.Praos.VRF
  ( InputVRF
  , mkInputVRF
  , vrfLeaderValue
  , vrfNonceValue
  )
import Cardano.Protocol.TPraos.BlockHeader
  ( BoundedNatural (bvValue)
  , PrevHash
  , checkLeaderNatValue
  , prevHashToNonce
  )
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (KESPeriod)
  , OCert (OCert)
  )
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Cardano.Slotting.EpochInfo
  ( EpochInfo
  , epochInfoEpoch
  , epochInfoFirst
  , hoistEpochInfo
  )
import Cardano.Slotting.Slot (EpochNo (EpochNo), SlotNo (SlotNo))
import Control.Exception (throw)
import Control.Monad (unless)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Coerce (coerce)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Protocol.Praos
  ( Praos
  , PraosCrypto
  , PraosIsLeader (..)
  , PraosParams (..)
  , PraosState (..)
  , PraosValidationErr (..)
  , Ticked (TickedPraosState, tickedPraosStateChainDepState, tickedPraosStateLedgerView)
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( HasMaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , PraosNonces (..)
  , PraosProtocolSupportsNode (..)
  , PraosTiebreakerView
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views

-- | The Leios protocol phantom type.
--
-- Leios uses the same consensus machinery as Praos, but with a different block
-- header type ('Leios.Header c') that has two additional fields compared to the
-- Praos header: @hbBlockBodyContainsLeiosCert@ and @hbEbAnnouncement@.
data Leios c

-- | Crypto class for the Leios protocol.
--
-- This is analogous to 'PraosCrypto' but additionally requires that the KES
-- scheme can sign 'Leios.HeaderBody c' (which has 12 fields vs Praos's 10).
class
  ( PraosCrypto c
  , KES.Signable (KES c) (Leios.HeaderBody c)
  ) =>
  LeiosCrypto c

instance LeiosCrypto StandardCrypto

-- | Static configuration for the Leios protocol.
--
-- Reuses 'PraosParams' and epoch info, analogous to 'PraosConfig'.
data instance ConsensusConfig (Leios c) = LeiosConfig
  { leiosPraosParams :: !PraosParams
  , leiosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
  }
  deriving Generic

instance LeiosCrypto c => NoThunks (ConsensusConfig (Leios c))

-- | View of the Leios block header required by the Leios consensus protocol.
--
-- Analogous to 'Views.HeaderView' for Praos, but uses 'Leios.HeaderBody c'
-- as the signed body type (and the corresponding KES signature type).
data LeiosHeaderView c = LeiosHeaderView
  { lhvPrevHash :: !PrevHash
  -- ^ Hash of the previous block
  , lhvVK :: !(VKey BlockIssuer)
  -- ^ Verification key of block issuer
  , lhvVrfVK :: !(VRF.VerKeyVRF (VRF c))
  -- ^ VRF verification key for block issuer
  , lhvVrfRes :: !(VRF.CertifiedVRF (VRF c) InputVRF)
  -- ^ VRF result
  , lhvOCert :: !(OCert.OCert c)
  -- ^ Operational certificate
  , lhvSlotNo :: !SlotNo
  -- ^ Slot
  , lhvSigned :: !(Leios.HeaderBody c)
  -- ^ Header body which must be signed
  , lhvSignature :: !(KES.SignedKES (KES c) (Leios.HeaderBody c))
  -- ^ KES signature of the header body
  }

instance HasMaxMajorProtVer (Leios c) where
  protoMaxMajorPV = praosMaxMajorPV . leiosPraosParams

-- | The Leios protocol uses the same ticked chain dep state as Praos
-- ('TickedPraosState'), since both use 'PraosState' as 'ChainDepState'.
instance LeiosCrypto c => ConsensusProtocol (Leios c) where
  type ChainDepState (Leios c) = PraosState
  type IsLeader (Leios c) = PraosIsLeader c
  type CanBeLeader (Leios c) = PraosCanBeLeader c
  type TiebreakerView (Leios c) = PraosTiebreakerView c
  type LedgerView (Leios c) = Views.PraosLedgerView
  type ValidationErr (Leios c) = PraosValidationErr c
  type ValidateView (Leios c) = LeiosHeaderView c

  protocolSecurityParam = praosSecurityParam . leiosPraosParams

  checkIsLeader
    LeiosConfig{leiosPraosParams = PraosParams{praosLeaderF}}
    PraosCanBeLeader
      { praosCanBeLeaderSignKeyVRF
      , praosCanBeLeaderColdVerKey
      }
    slot
    cs =
      if checkLeaderNatValue
        (vrfLeaderValue (Proxy @c) rho)
        r
        praosLeaderF
        then Just PraosIsLeader{praosIsLeaderVrfRes = coerce rho}
        else Nothing
     where
      chainState = tickedPraosStateChainDepState cs
      lv = tickedPraosStateLedgerView cs
      eta0 = praosStateEpochNonce chainState
      vkhCold = SL.hashKey praosCanBeLeaderColdVerKey
      rho' = mkInputVRF slot eta0
      rho = VRF.evalCertified () rho' praosCanBeLeaderSignKeyVRF
      SL.PoolDistr poolDistr _totalActiveStake = Views.plvPoolDistr lv
      r =
        maybe 0 SL.individualPoolStake $
          Map.lookup (SL.coerceKeyRole vkhCold) poolDistr

  tickChainDepState
    LeiosConfig{leiosEpochInfo}
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
          (History.toPureEpochInfo leiosEpochInfo)
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

  updateChainDepState
    cfg@( LeiosConfig
            PraosParams{praosLeaderF}
            _
          )
    b
    slot
    tcs = do
      validateLeiosKESSignature cfg lv (praosStateOCertCounters cs) b
      validateLeiosVRFSignature (praosStateEpochNonce cs) lv praosLeaderF b
      pure $ reupdateChainDepState cfg b slot tcs
     where
      lv = tickedPraosStateLedgerView tcs
      cs = tickedPraosStateChainDepState tcs

  reupdateChainDepState
    _cfg@( LeiosConfig
             PraosParams{praosRandomnessStabilisationWindow}
             ei
           )
    b
    slot
    tcs =
      cs
        { praosStateLastSlot = NotOrigin slot
        , praosStateLabNonce = prevHashToNonce (lhvPrevHash b)
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
      eta = vrfNonceValue (Proxy @c) $ lhvVrfRes b
      newEvolvingNonce = praosStateEvolvingNonce cs ⭒ eta
      OCert _ n _ _ = lhvOCert b
      hk = hashKey $ lhvVK b

{-------------------------------------------------------------------------------
  TranslateProto
-------------------------------------------------------------------------------}

-- | Translation from Praos to Leios: both use 'PraosState' as 'ChainDepState'
-- and 'PraosLedgerView' as 'LedgerView', so translation is trivial.
instance TranslateProto (Praos c) (Leios c) where
  translateLedgerView _ = id
  translateChainDepState _ = id

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

-- | Leios uses 'PraosState' as its 'ChainDepState', so the implementation
-- is identical to the 'Praos' instance.
instance LeiosCrypto c => PraosProtocolSupportsNode (Leios c) where
  type PraosProtocolSupportsNodeCrypto (Leios c) = c

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
  Validation helpers (analogous to Praos, but for LeiosHeaderView)
-------------------------------------------------------------------------------}

validateLeiosVRFSignature ::
  forall c.
  LeiosCrypto c =>
  Nonce ->
  Views.PraosLedgerView ->
  ActiveSlotCoeff ->
  LeiosHeaderView c ->
  Except (PraosValidationErr c) ()
validateLeiosVRFSignature eta0 (Views.plvPoolDistr -> SL.PoolDistr pd _) f b = do
  case Map.lookup hk pd of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (SL.IndividualPoolStake sigma _totalPoolStake vrfHK _blsKey) -> do
      let vrfHKStake = SL.fromVRFVerKeyHash vrfHK
          vrfHKBlock = VRF.hashVerKeyVRF vrfK
      vrfHKStake
        == vrfHKBlock
          ?! VRFKeyWrongVRFKey hk vrfHKStake vrfHKBlock
      VRF.verifyCertified () vrfK (mkInputVRF slot eta0) vrfCert
        ?! VRFKeyBadProof slot eta0 vrfCert
      checkLeaderNatValue vrfLeaderVal sigma f
        ?! VRFLeaderValueTooBig (bvValue vrfLeaderVal) sigma f
 where
  hk = coerceKeyRole . hashKey . lhvVK $ b
  vrfK = lhvVrfVK b
  vrfCert = lhvVrfRes b
  vrfLeaderVal = vrfLeaderValue (Proxy @c) vrfCert
  slot = lhvSlotNo b

validateLeiosKESSignature ::
  forall c.
  LeiosCrypto c =>
  ConsensusConfig (Leios c) ->
  Views.PraosLedgerView ->
  Map (KeyHash BlockIssuer) Word64 ->
  LeiosHeaderView c ->
  Except (PraosValidationErr c) ()
validateLeiosKESSignature
  LeiosConfig{leiosPraosParams = PraosParams{praosMaxKESEvo, praosSlotsPerKESPeriod}}
  Views.PraosLedgerView{Views.plvPoolDistr = SL.PoolDistr plvPoolDistr _totalActiveStake}
  ocertCounters
  b = do
    c0 <= kp ?! KESBeforeStartOCERT c0 kp
    kp_ < c0_ + fromIntegral praosMaxKESEvo ?! KESAfterEndOCERT kp c0 praosMaxKESEvo

    let t = if kp_ >= c0_ then kp_ - c0_ else 0

    DSIGN.verifySignedDSIGN () vkcold (OCert.ocertToSignable oc) tau
      ?!: InvalidSignatureOCERT n c0
    KES.verifySignedKES () vk_hot t (lhvSigned b) (lhvSignature b)
      ?!: InvalidKesSignatureOCERT kp_ c0_ t praosMaxKESEvo

    case currentIssueNo of
      Nothing -> throwError $ NoCounterForKeyHashOCERT hk
      Just m -> do
        m <= n ?! CounterTooSmallOCERT m n
        n <= m + 1 ?! CounterOverIncrementedOCERT m n
   where
    oc@(OCert vk_hot n c0@(KESPeriod c0_) tau) = lhvOCert b
    (VKey vkcold) = lhvVK b
    SlotNo s = lhvSlotNo b
    hk = hashKey $ lhvVK b
    kp@(KESPeriod kp_) =
      if praosSlotsPerKESPeriod == 0
        then error "kesPeriod: slots per KES period was set to zero"
        else KESPeriod . fromIntegral $ s `div` praosSlotsPerKESPeriod

    currentIssueNo :: Maybe Word64
    currentIssueNo
      | r@Just{} <- Map.lookup hk ocertCounters = r
      | Map.member (coerceKeyRole hk) plvPoolDistr = Just 0
      | otherwise = Nothing

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

(?!) :: Bool -> e -> Except e ()
a ?! b = unless a $ throwError b

infix 1 ?!

(?!:) :: Either e1 a -> (e1 -> e2) -> Except e2 ()
(Right _) ?!: _ = pure ()
(Left e1) ?!: f = throwError $ f e1

infix 1 ?!:
