{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Protocol.TPraos
  ( MaxMajorProtVer (..)
  , TPraos
  , TPraosFields (..)
  , TPraosIsLeader (..)
  , TPraosParams (..)
  , TPraosState (..)
  , TPraosToSign (..)
  , TPraosValidateView
  , forgeTPraosFields
  , mkShelleyGlobals
  , mkTPraosParams

    -- * Crypto
  , SL.PraosCrypto
  , StandardCrypto

    -- * CannotForge
  , TPraosCannotForge (..)
  , tpraosCheckCanForge

    -- * Type instances
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.BaseTypes as SL (ActiveSlotCoeff, Seed)
import Cardano.Ledger.BaseTypes.NonZero (nonZeroOr, unNonZero)
import Cardano.Ledger.Hashes (HASH)
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Cardano.Protocol.TPraos.Rules.Overlay as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Time (SystemStart (..))
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..))
import Control.Monad.Except
  ( Except
  , runExcept
  , throwError
  , withExceptT
  )
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (pack)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Ledger.Util
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.CBOR
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields
  { tpraosSignature :: KES.SignedKES (KES c) toSign
  , tpraosToSign :: toSign
  }
  deriving Generic

deriving instance
  (NoThunks toSign, SL.PraosCrypto c) =>
  NoThunks (TPraosFields c toSign)
deriving instance
  (Show toSign, SL.PraosCrypto c) =>
  Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign
  { tpraosToSignIssuerVK :: SL.VKey 'SL.BlockIssuer
  -- ^ Verification key for the issuer of this block.
  --
  -- Note that unlike in Classic/BFT where we have a key for the genesis
  -- delegate on whose behalf we are issuing this block, this key
  -- corresponds to the stake pool/core node actually forging the block.
  , tpraosToSignVrfVK :: VRF.VerKeyVRF (VRF c)
  , tpraosToSignEta :: VRF.CertifiedVRF (VRF c) SL.Nonce
  -- ^ Verifiable result containing the updated nonce value.
  , tpraosToSignLeader :: VRF.CertifiedVRF (VRF c) Natural
  -- ^ Verifiable proof of the leader value, used to determine whether the
  -- node has the right to issue a block in this slot.
  --
  -- We include a value here even for blocks forged under the BFT
  -- schedule. It is not required that such a value be verifiable (though
  -- by default it will be verifiably correct, but unused.)
  , tpraosToSignOCert :: SL.OCert c
  -- ^ Lightweight delegation certificate mapping the cold (DSIGN) key to
  -- the online KES key.
  }
  deriving Generic

instance SL.PraosCrypto c => NoThunks (TPraosToSign c)
deriving instance SL.PraosCrypto c => Show (TPraosToSign c)

forgeTPraosFields ::
  ( SL.PraosCrypto c
  , KES.Signable (KES c) toSign
  , Monad m
  ) =>
  HotKey c m ->
  CanBeLeader (TPraos c) ->
  IsLeader (TPraos c) ->
  (TPraosToSign c -> toSign) ->
  m (TPraosFields c toSign)
forgeTPraosFields hotKey PraosCanBeLeader{..} TPraosIsLeader{..} mkToSign = do
  signature <- HotKey.sign hotKey toSign
  return
    TPraosFields
      { tpraosSignature = signature
      , tpraosToSign = toSign
      }
 where
  toSign = mkToSign signedFields

  signedFields =
    TPraosToSign
      { tpraosToSignIssuerVK = praosCanBeLeaderColdVerKey
      , tpraosToSignVrfVK = VRF.deriveVerKeyVRF praosCanBeLeaderSignKeyVRF
      , tpraosToSignEta = tpraosIsLeaderEta
      , tpraosToSignLeader = tpraosIsLeaderProof
      , tpraosToSignOCert = praosCanBeLeaderOpCert
      }

-- | Because we are using the executable spec, rather than implementing the
-- protocol directly here, we have a fixed header type rather than an
-- abstraction. So our validate view is fixed to this.
type TPraosValidateView c = SL.BHeader c

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams
  { tpraosSlotsPerKESPeriod :: !Word64
  -- ^ See 'Globals.slotsPerKESPeriod'.
  , tpraosLeaderF :: !SL.ActiveSlotCoeff
  -- ^ Active slots coefficient. This parameter represents the proportion
  -- of slots in which blocks should be issued. This can be interpreted as
  -- the probability that a party holding all the stake will be elected as
  -- leader for a given slot.
  , tpraosSecurityParam :: !SecurityParam
  -- ^ See 'Globals.securityParameter'.
  , tpraosMaxKESEvo :: !Word64
  -- ^ Maximum number of KES iterations, see 'Globals.maxKESEvo'.
  , tpraosQuorum :: !Word64
  -- ^ Quorum for update system votes and MIR certificates, see
  -- 'Globals.quorum'.
  , tpraosMaxMajorPV :: !MaxMajorProtVer
  -- ^ All blocks invalid after this protocol version, see
  -- 'Globals.maxMajorPV'.
  , tpraosMaxLovelaceSupply :: !Word64
  -- ^ Maximum number of lovelace in the system, see
  -- 'Globals.maxLovelaceSupply'.
  , tpraosNetworkId :: !SL.Network
  -- ^ Testnet or mainnet?
  , tpraosInitialNonce :: !SL.Nonce
  -- ^ Initial nonce used for the TPraos protocol state. Typically this is
  -- derived from the hash of the Shelley genesis config JSON file, but
  -- different values may be used for testing purposes.
  --
  -- NOTE: this is only used when translating the Byron 'ChainDepState' to
  -- the Shelley 'ChainDepState', at which point we'll need access to the
  -- initial nonce at runtime. TODO #2326.
  , tpraosSystemStart :: !SystemStart
  -- ^ The system start, as projected from the chain's genesis block.
  }
  deriving (Generic, NoThunks)

mkTPraosParams ::
  MaxMajorProtVer ->
  -- | Initial nonce
  SL.Nonce ->
  SL.ShelleyGenesis ->
  TPraosParams
mkTPraosParams maxMajorPV initialNonce genesis =
  TPraosParams
    { tpraosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
    , tpraosLeaderF = SL.sgActiveSlotCoeff genesis
    , tpraosMaxKESEvo = SL.sgMaxKESEvolutions genesis
    , tpraosQuorum = SL.sgUpdateQuorum genesis
    , tpraosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesis
    , tpraosNetworkId = SL.sgNetworkId genesis
    , tpraosSecurityParam = securityParam
    , tpraosMaxMajorPV = maxMajorPV
    , tpraosInitialNonce = initialNonce
    , tpraosSystemStart = systemStart
    }
 where
  securityParam = SecurityParam $ SL.sgSecurityParam genesis
  systemStart = SystemStart $ SL.sgSystemStart genesis

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosIsLeader c = TPraosIsLeader
  { tpraosIsLeaderEta :: VRF.CertifiedVRF (VRF c) SL.Nonce
  , tpraosIsLeaderProof :: VRF.CertifiedVRF (VRF c) Natural
  , tpraosIsLeaderGenVRFHash :: Maybe (Hash.Hash HASH (VRF.VerKeyVRF (VRF c)))
  -- ^ When in the overlay schedule (otherwise 'Nothing'), return the hash
  -- of the VRF verification key in the overlay schedule
  }
  deriving Generic

instance SL.PraosCrypto c => NoThunks (TPraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (TPraos c) = TPraosConfig
  { tpraosParams :: !TPraosParams
  , tpraosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
  -- it's useful for this record to be EpochInfo and one other thing,
  -- because the one other thing can then be used as the
  -- PartialConsensConfig in the HFC instance.
  }
  deriving Generic

instance SL.PraosCrypto c => NoThunks (ConsensusConfig (TPraos c))

instance HasMaxMajorProtVer (TPraos c) where
  protoMaxMajorPV = tpraosMaxMajorPV . tpraosParams

-- | Transitional Praos consensus state.
--
-- In addition to the 'ChainDepState' provided by the ledger, we track the slot
-- number of the last applied header.
data TPraosState = TPraosState
  { tpraosStateLastSlot :: !(WithOrigin SlotNo)
  , tpraosStateChainDepState :: !SL.ChainDepState
  }
  deriving (Generic, Show, Eq)

instance NoThunks TPraosState

-- | Version 0 supported rollback, removed in #2575.
serialisationFormatVersion1 :: VersionNumber
serialisationFormatVersion1 = 1

instance ToCBOR TPraosState where
  toCBOR = encode

instance FromCBOR TPraosState where
  fromCBOR = decode

instance Serialise TPraosState where
  encode (TPraosState slot chainDepState) =
    encodeVersion serialisationFormatVersion1 $
      mconcat
        [ CBOR.encodeListLen 2
        , encodeWithOrigin toCBOR slot
        , toCBOR chainDepState
        ]

  decode =
    decodeVersion
      [(serialisationFormatVersion1, Decode decodeTPraosState1)]
   where
    decodeTPraosState1 = do
      enforceSize "TPraosState" 2
      TPraosState <$> decodeWithOrigin fromCBOR <*> fromCBOR

data instance Ticked TPraosState = TickedChainDepState
  { tickedTPraosStateChainDepState :: SL.ChainDepState
  , tickedTPraosStateLedgerView :: SL.LedgerView
  }

instance SL.PraosCrypto c => ConsensusProtocol (TPraos c) where
  type ChainDepState (TPraos c) = TPraosState
  type IsLeader (TPraos c) = TPraosIsLeader c
  type CanBeLeader (TPraos c) = PraosCanBeLeader c
  type TiebreakerView (TPraos c) = PraosTiebreakerView c
  type LedgerView (TPraos c) = SL.LedgerView
  type ValidationErr (TPraos c) = SL.ChainTransitionError c
  type ValidateView (TPraos c) = TPraosValidateView c

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  checkIsLeader cfg PraosCanBeLeader{..} slot cs = do
    -- First, check whether we're in the overlay schedule
    case SL.lookupInOverlaySchedule firstSlot gkeys d asc slot of
      -- Slot isn't in the overlay schedule, so we're in Praos
      Nothing
        | meetsLeaderThreshold cfg lv (SL.coerceKeyRole vkhCold) y ->
            Just
              TPraosIsLeader
                { tpraosIsLeaderEta = coerce rho
                , tpraosIsLeaderProof = coerce y
                , tpraosIsLeaderGenVRFHash = Nothing
                }
        | otherwise ->
            Nothing
      -- This is a non-active slot; nobody may produce a block
      Just SL.NonActiveSlot -> Nothing
      -- The given genesis key has authority to produce a block in this
      -- slot. Check whether we're its delegate.
      Just (SL.ActiveSlot gkhash) -> case Map.lookup gkhash dlgMap of
        Nothing ->
          error "unknown genesis key in overlay schedule"
        Just (SL.GenDelegPair dlgHash genDlgVRFHash)
          | SL.coerceKeyRole dlgHash == vkhCold ->
              Just
                TPraosIsLeader
                  { tpraosIsLeaderEta = coerce rho
                  , -- Note that this leader value is not checked for slots in
                    -- the overlay schedule, so we could set it to whatever we
                    -- want. We evaluate it as normal for simplicity's sake.
                    tpraosIsLeaderProof = coerce y
                  , tpraosIsLeaderGenVRFHash = Just $ SL.fromVRFVerKeyHash genDlgVRFHash
                  }
          | otherwise ->
              Nothing
   where
    chainState = tickedTPraosStateChainDepState cs
    lv = tickedTPraosStateLedgerView cs
    d = SL.lvD lv
    asc = tpraosLeaderF $ tpraosParams cfg
    firstSlot =
      firstSlotOfEpochOfSlot
        (History.toPureEpochInfo $ tpraosEpochInfo cfg)
        slot
    gkeys = Map.keysSet dlgMap
    eta0 = SL.ticknStateEpochNonce $ SL.csTickn chainState
    vkhCold = SL.hashKey praosCanBeLeaderColdVerKey
    rho' = SL.mkSeed SL.seedEta slot eta0
    y' = SL.mkSeed SL.seedL slot eta0

    rho = VRF.evalCertified () rho' praosCanBeLeaderSignKeyVRF
    y = VRF.evalCertified () y' praosCanBeLeaderSignKeyVRF

    SL.GenDelegs dlgMap = SL.lvGenDelegs lv

  tickChainDepState
    cfg@TPraosConfig{..}
    lv
    slot
    (TPraosState lastSlot st) =
      TickedChainDepState
        { tickedTPraosStateChainDepState = st'
        , tickedTPraosStateLedgerView = lv
        }
     where
      st' =
        SL.tickChainDepState
          (mkShelleyGlobals cfg)
          lv
          ( isNewEpoch
              (History.toPureEpochInfo tpraosEpochInfo)
              lastSlot
              slot
          )
          st

  updateChainDepState cfg b slot cs =
    TPraosState (NotOrigin slot)
      <$> SL.updateChainDepState
        (mkShelleyGlobals cfg)
        (tickedTPraosStateLedgerView cs)
        b
        (tickedTPraosStateChainDepState cs)

  reupdateChainDepState cfg b slot cs =
    TPraosState (NotOrigin slot) $
      SL.reupdateChainDepState
        (mkShelleyGlobals cfg)
        (tickedTPraosStateLedgerView cs)
        b
        (tickedTPraosStateChainDepState cs)

mkShelleyGlobals :: ConsensusConfig (TPraos c) -> SL.Globals
mkShelleyGlobals TPraosConfig{..} =
  SL.Globals
    { epochInfo =
        hoistEpochInfo
          (runExcept . withExceptT (T.pack . show))
          tpraosEpochInfo
    , slotsPerKESPeriod = tpraosSlotsPerKESPeriod
    , stabilityWindow = SL.computeStabilityWindow k tpraosLeaderF
    , randomnessStabilisationWindow = SL.computeRandomnessStabilisationWindow k tpraosLeaderF
    , securityParameter = nonZeroOr k $ error "The security parameter cannot be zero."
    , maxKESEvo = tpraosMaxKESEvo
    , quorum = tpraosQuorum
    , maxLovelaceSupply = tpraosMaxLovelaceSupply
    , activeSlotCoeff = tpraosLeaderF
    , networkId = tpraosNetworkId
    , systemStart = tpraosSystemStart
    }
 where
  k = unNonZero $ maxRollbacks tpraosSecurityParam
  TPraosParams{..} = tpraosParams

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
  forall c.
  SL.PraosCrypto c =>
  ConsensusConfig (TPraos c) ->
  LedgerView (TPraos c) ->
  SL.KeyHash 'SL.StakePool ->
  VRF.CertifiedVRF (VRF c) SL.Seed ->
  Bool
meetsLeaderThreshold
  TPraosConfig{tpraosParams}
  SL.LedgerView{lvPoolDistr}
  keyHash
  certNat =
    SL.checkLeaderValue
      (VRF.certifiedOutput certNat)
      r
      (tpraosLeaderF tpraosParams)
   where
    SL.PoolDistr poolDistr _totalActiveStake = lvPoolDistr
    r =
      maybe 0 SL.individualPoolStake $
        Map.lookup keyHash poolDistr

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data TPraosCannotForge c
  = -- | The KES key in our operational certificate can't be used because the
    -- current (wall clock) period is before the start period of the key.
    -- current KES period.
    --
    -- Note: the opposite case, i.e., the wall clock period being after the
    -- end period of the key, is caught when trying to update the key in
    -- 'updateForgeState'.
    TPraosCannotForgeKeyNotUsableYet
      -- | Current KES period according to the wallclock slot, i.e., the KES
      -- period in which we want to use the key.
      !Absolute.KESPeriod
      -- | Start KES period of the KES key.
      !Absolute.KESPeriod
  | -- | We are a genesis delegate, but our VRF key (second argument) does not
    -- match the registered key for that delegate (first argument).
    TPraosCannotForgeWrongVRF
      !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c)))
      !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c)))
  deriving Generic

deriving instance SL.PraosCrypto c => Show (TPraosCannotForge c)

tpraosCheckCanForge ::
  ConsensusConfig (TPraos c) ->
  -- | Precomputed hash of the VRF verification key
  Hash.Hash HASH (VRF.VerKeyVRF (VRF c)) ->
  SlotNo ->
  IsLeader (TPraos c) ->
  HotKey.KESInfo ->
  Either (TPraosCannotForge c) ()
tpraosCheckCanForge
  TPraosConfig{tpraosParams}
  forgingVRFHash
  curSlot
  TPraosIsLeader{tpraosIsLeaderGenVRFHash}
  kesInfo
    | let startPeriod = HotKey.kesStartPeriod kesInfo
    , startPeriod > wallclockPeriod =
        throwError $ TPraosCannotForgeKeyNotUsableYet wallclockPeriod startPeriod
    | Just genVRFHash <- tpraosIsLeaderGenVRFHash
    , genVRFHash /= forgingVRFHash =
        throwError $ TPraosCannotForgeWrongVRF genVRFHash forgingVRFHash
    | otherwise =
        return ()
   where
    -- The current wallclock KES period
    wallclockPeriod :: Absolute.KESPeriod
    wallclockPeriod =
      Absolute.KESPeriod $
        fromIntegral $
          unSlotNo curSlot `div` tpraosSlotsPerKESPeriod tpraosParams

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance SL.PraosCrypto c => PraosProtocolSupportsNode (TPraos c) where
  type PraosProtocolSupportsNodeCrypto (TPraos c) = c

  getPraosNonces _prx cdst =
    PraosNonces
      { candidateNonce
      , epochNonce = ticknStateEpochNonce
      , evolvingNonce
      , labNonce = csLabNonce
      , previousLabNonce = ticknStatePrevHashNonce
      }
   where
    TPraosState{tpraosStateChainDepState} = cdst
    SL.ChainDepState
      { SL.csLabNonce
      , SL.csProtocol
      , SL.csTickn
      } = tpraosStateChainDepState
    SL.PrtclState
      _opcertCounters
      evolvingNonce
      candidateNonce =
        csProtocol
    SL.TicknState
      { ticknStateEpochNonce
      , ticknStatePrevHashNonce
      } = csTickn

  getOpCertCounters _prx cdst = opcertCounters
   where
    TPraosState{tpraosStateChainDepState} = cdst
    SL.ChainDepState
      { SL.csProtocol
      } = tpraosStateChainDepState
    SL.PrtclState
      opcertCounters
      _evolvingNonce
      _candidateNonce =
        csProtocol

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense toSign, SL.PraosCrypto c) => Condense (TPraosFields c toSign) where
  condense = condense . tpraosToSign
