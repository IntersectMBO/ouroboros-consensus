{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( BaseHeaderBody
  , BaseHeaderView (..)
  , BasePraosLedgerView (..)
  , ForecastsLeios (..)
  , LeiosLedgerView (..)
  , PraosLedgerView
  , initialLeiosLedgerView
  , forecastToBasePraosLedgerView
  , extendHeaderBodyWithLeios
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes
  ( Milliseconds32 (..)
  , ProtVer
  , StrictMaybe
  , UnitInterval
  )
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (LeiosCommittee, emptyLeiosCommittee)
import Cardano.Protocol.Crypto (KES, VRF)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32)
import LeiosDemoTypes (EbAnnouncement)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Protocol.Praos.Common

{-------------------------------------------------------------------------------
  The upstream header types, per extension
-------------------------------------------------------------------------------}

-- | The upstream @cardano-protocol@ header body this extension uses.
--
-- Its owners define one per protocol, wholly separately, and future extensions
-- of Praos (eg Ouroboros Phalanx) will presumably each bring another. This is
-- the KES-signed object, which is why it appears in 'BaseHeaderView' as-is
-- rather than being projected field by field.
type BaseHeaderBody :: PraosExtension -> Type -> Type
type family BaseHeaderBody pext :: Type -> Type where
  BaseHeaderBody PextNone = PraosCodec.HeaderBody
  BaseHeaderBody PextLeios = LeiosCodec.HeaderBody

-- | The Leios header body is the Praos one plus the two Leios fields, so
-- whoever builds one builds the Praos body first and hands it here.
extendHeaderBodyWithLeios ::
  PraosCodec.HeaderBody c ->
  -- | Whether the block body carries a Leios certificate
  Bool ->
  StrictMaybe LeiosCodec.EbAnnouncement ->
  LeiosCodec.HeaderBody c
extendHeaderBodyWithLeios pb containsCert ann =
  LeiosCodec.HeaderBody
    { LeiosCodec.hbBlockNo = PraosCodec.hbBlockNo pb
    , LeiosCodec.hbSlotNo = PraosCodec.hbSlotNo pb
    , LeiosCodec.hbPrev = PraosCodec.hbPrev pb
    , LeiosCodec.hbVk = PraosCodec.hbVk pb
    , LeiosCodec.hbVrfVk = PraosCodec.hbVrfVk pb
    , LeiosCodec.hbVrfRes = PraosCodec.hbVrfRes pb
    , LeiosCodec.hbBodySize = PraosCodec.hbBodySize pb
    , LeiosCodec.hbBodyHash = PraosCodec.hbBodyHash pb
    , LeiosCodec.hbOCert = PraosCodec.hbOCert pb
    , LeiosCodec.hbProtVer = PraosCodec.hbProtVer pb
    , LeiosCodec.hbBlockBodyContainsLeiosCert = containsCert
    , LeiosCodec.hbEbAnnouncement = ann
    }

{-------------------------------------------------------------------------------
  Header view
-------------------------------------------------------------------------------}

type BaseHeaderView :: PraosExtension -> Type -> Type

-- | View of the block header required by the Praos protocol.
data BaseHeaderView pext crypto = HeaderView
  { hvPrevHash :: !PrevHash
  -- ^ Hash of the previous block
  , hvVK :: !(VKey BlockIssuer)
  -- ^ verification key of block issuer
  , hvVrfVK :: !(VerKeyVRF (VRF crypto))
  -- ^ VRF verification key for block issuer
  , hvVrfRes :: !(CertifiedVRF (VRF crypto) InputVRF)
  -- ^ VRF result
  , hvOCert :: !(OCert crypto)
  -- ^ operational certificate
  , hvSlotNo :: !SlotNo
  -- ^ Slot
  , hvLeios ::
      !( StrictMaybeLeios
           (PraosExtensionHasLeios pext)
           (Bool, StrictMaybe EbAnnouncement)
       )
  -- ^ The Leios payload: whether this block's body carries a certificate (ie
  -- whether it is a CertRB), and the endorser block this header announces.
  --
  -- Statically absent unless the extension has Leios, since the header checks
  -- that read it only exist there.
  , hvSigned :: !(BaseHeaderBody pext crypto)
  -- ^ Header which must be signed
  , hvSignature :: !(SignedKES (KES crypto) (BaseHeaderBody pext crypto))
  -- ^ KES Signature of the header
  }

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

type BasePraosLedgerView :: PraosExtension -> Type

-- | View of the ledger required by the Praos protocol.
data BasePraosLedgerView pext = PraosLedgerView
  { plvPoolDistr :: SL.PoolDistr
  -- ^ Stake distribution
  , plvMaxHeaderSize :: !Word16
  -- ^ Maximum header size
  , plvMaxBodySize :: !Word32
  -- ^ Maximum block body size
  , plvProtocolVersion :: !ProtVer
  -- ^ Current protocol version
  , plvLeios :: !(StrictMaybeLeios (PraosExtensionHasLeios pext) LeiosLedgerView)
  -- ^ The Leios data, statically absent unless the extension has Leios.
  --
  -- One field rather than one per datum, so the committee and the parameters
  -- cannot disagree about whether Leios is enabled.
  }

deriving instance Show (BasePraosLedgerView pext)

type PraosLedgerView = BasePraosLedgerView PextNone

-- | The Leios part of 'BasePraosLedgerView'.
--
-- Only what the protocol itself checks. The other Leios parameters bound an
-- endorser block's contents, which is validated with a real ledger state in
-- hand, so doesn't need to be forecasted.
data LeiosLedgerView = LeiosLedgerView
  { llvCommittee :: !LeiosCommittee
  -- ^ Who may vote this epoch, and with what weight
  , llvQuorumStakeThreshold :: !UnitInterval
  -- ^ Weight a certificate must accumulate
  , llvAnnouncementPeriodLength :: !Milliseconds32
  , llvVotePeriodLength :: !Milliseconds32
  , llvDiffusionPeriodLength :: !Milliseconds32
  -- ^ The three periods that determine how long after its announcement an
  -- endorser block may be certified. Kept as durations, since converting to a
  -- count of slots needs the slot length, which only the consensus config has.
  , llvMaxEbBodySize :: !Word32
  -- ^ Maximum total size of an endorser block itself (/not/ the closure)
  }
  deriving (Eq, Show)

-- | The Leios view of a ledger state that seats no committee.
--
-- Nothing can be certified against it: the committee is empty and the quorum is
-- the entire weight. This is the truth rather than a placeholder, both before
-- the Leios era and during its first epochs, until a snapshot seated by the new
-- era's rules rotates in.
--
-- The remaining parameters have no counterpart in such a state, so they admit
-- anything. They only bound an endorser block, whose contents are checked again
-- against the era's parameters proper, whereas an over-permissive committee
-- would be unsound.
initialLeiosLedgerView :: LeiosLedgerView
initialLeiosLedgerView =
  LeiosLedgerView
    { llvCommittee = emptyLeiosCommittee
    , llvQuorumStakeThreshold = maxBound
    , llvAnnouncementPeriodLength = Milliseconds32 0
    , llvVotePeriodLength = Milliseconds32 0
    , llvDiffusionPeriodLength = Milliseconds32 1000000000
    , llvMaxEbBodySize = 0
    }

type ForecastsLeios :: PraosExtension -> Type -> Constraint

-- | The Leios part of an era's forecast, as this extension sees it.
--
-- Reading that part needs 'SL.DijkstraEraForecast', which the extensions
-- without Leios must not demand of their eras. 'KnownPraosExtension' cannot
-- serve here: refining @pext@ says nothing about @era@, and it is an @era@
-- dictionary that is missing.
class ForecastsLeios pext era where
  forecastToLeiosPart ::
    proxy pext ->
    SL.Forecast t era ->
    StrictMaybeLeios (PraosExtensionHasLeios pext) LeiosLedgerView

instance ForecastsLeios PextNone era where
  forecastToLeiosPart _ _ = SNothingLeios

instance SL.DijkstraEraForecast era => ForecastsLeios PextLeios era where
  forecastToLeiosPart _ = SJustLeios . forecastToLeiosLedgerView

-- | Build a 'BasePraosLedgerView' from a ledger 'SL.Forecast'
forecastToBasePraosLedgerView ::
  forall pext t era.
  (ForecastsLeios pext era, SL.EraForecast era) =>
  SL.Forecast t era ->
  BasePraosLedgerView pext
forecastToBasePraosLedgerView f =
  PraosLedgerView
    { plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
    , plvMaxHeaderSize = ccMaxBHSize cc
    , plvMaxBodySize = ccMaxBBSize cc
    , plvProtocolVersion = ccProtocolVersion cc
    , plvLeios = forecastToLeiosPart (Proxy @pext) f
    }
 where
  cc = SL.forecastChainChecks @t @era f

forecastToLeiosLedgerView ::
  forall t era.
  SL.DijkstraEraForecast era =>
  SL.Forecast t era ->
  LeiosLedgerView
forecastToLeiosLedgerView f =
  LeiosLedgerView
    { llvCommittee = f ^. SL.leiosCommitteeForecastL @era @t
    , llvQuorumStakeThreshold = f ^. SL.leiosQuorumStakeThresholdForecastL @era @t
    , llvAnnouncementPeriodLength = f ^. SL.leiosAnnouncementPeriodLengthForecastL @era @t
    , llvVotePeriodLength = f ^. SL.leiosVotePeriodLengthForecastL @era @t
    , llvDiffusionPeriodLength = f ^. SL.leiosDiffusionPeriodLengthForecastL @era @t
    , llvMaxEbBodySize = f ^. SL.maxEndorserBlockReferencesSizeForecastL @era @t
    }
