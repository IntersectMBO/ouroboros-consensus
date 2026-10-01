{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( HeaderView (..)
  , PraosHeaderView
  , LeiosHeaderView (..)
  , PraosLedgerView (..)
  , PraosWithLeiosLedgerView (..)
  , LeiosLedgerView (..)
  , initialLeiosLedgerView
  , forecastToPraosLedgerView
  , forecastToPraosWithLeiosLedgerView
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
import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
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
import Data.Word (Word16, Word32)
import LeiosDemoTypes (EbAnnouncement)
import Lens.Micro ((^.))

{-------------------------------------------------------------------------------
  The upstream header types
-------------------------------------------------------------------------------}

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

-- | View of the block header required by the Praos protocol.
--
-- Parameterised by the KES-signed body, because that is the only thing the
-- Praos extensions actually disagree about: every field below is a projection
-- each extension's header offers, and @body@ is the object the signature
-- covers. Carrying it as an ordinary type parameter is what lets the shared
-- signature checks ask for a @Signable (KES c) body@ dictionary in the normal
-- way.
data HeaderView body c = HeaderView
  { hvPrevHash :: !PrevHash
  -- ^ Hash of the previous block
  , hvVK :: !(VKey BlockIssuer)
  -- ^ verification key of block issuer
  , hvVrfVK :: !(VerKeyVRF (VRF c))
  -- ^ VRF verification key for block issuer
  , hvVrfRes :: !(CertifiedVRF (VRF c) InputVRF)
  -- ^ VRF result
  , hvOCert :: !(OCert c)
  -- ^ operational certificate
  , hvSlotNo :: !SlotNo
  -- ^ Slot
  , hvSigned :: !body
  -- ^ Header body which must be signed
  , hvSignature :: !(SignedKES (KES c) body)
  -- ^ KES signature of the header body
  }

type PraosHeaderView c = HeaderView (PraosCodec.HeaderBody c) c

-- | A 'HeaderView' over the Leios header body, plus the two Leios fields.
--
-- The Leios fields are plain rather than optional: this view exists only for
-- 'Ouroboros.Consensus.Protocol.Praos.PraosWithLeios', whose headers always
-- carry them.
data LeiosHeaderView c = LeiosHeaderView
  { lhvBase :: !(HeaderView (LeiosCodec.HeaderBody c) c)
  , lhvContainsCert :: !Bool
  -- ^ Whether this block's body carries a certificate (ie whether it is a
  -- CertRB)
  , lhvAnnouncement :: !(StrictMaybe EbAnnouncement)
  -- ^ The endorser block this header announces
  }

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

-- | View of the ledger required by the Praos protocol.
data PraosLedgerView = PraosLedgerView
  { plvPoolDistr :: SL.PoolDistr
  -- ^ Stake distribution
  , plvMaxHeaderSize :: !Word16
  -- ^ Maximum header size
  , plvMaxBodySize :: !Word32
  -- ^ Maximum block body size
  , plvProtocolVersion :: !ProtVer
  -- ^ Current protocol version
  }
  deriving Show

-- | View of the ledger required by Praos with Leios: the Praos one plus the
-- Leios one, since the Leios header checks need both.
data PraosWithLeiosLedgerView = PraosWithLeiosLedgerView
  { pwlvBase :: !PraosLedgerView
  , pwlvLeios :: !LeiosLedgerView
  }
  deriving Show

-- | The Leios part of 'PraosWithLeiosLedgerView'.
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
-- era's rules rotates in. And so the other parameter values don't actually
-- matter.
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

forecastToPraosLedgerView ::
  forall t era.
  SL.EraForecast era =>
  SL.Forecast t era ->
  PraosLedgerView
forecastToPraosLedgerView f =
  PraosLedgerView
    { plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
    , plvMaxHeaderSize = ccMaxBHSize cc
    , plvMaxBodySize = ccMaxBBSize cc
    , plvProtocolVersion = ccProtocolVersion cc
    }
 where
  cc = SL.forecastChainChecks @t @era f

-- | Unlike 'forecastToPraosLedgerView' this needs
-- 'Dijkstra.DijkstraEraForecast', which is the whole reason it is a separate
-- function: the extensions without Leios must not demand that of their eras.
forecastToPraosWithLeiosLedgerView ::
  forall t era.
  Dijkstra.DijkstraEraForecast era =>
  SL.Forecast t era ->
  PraosWithLeiosLedgerView
forecastToPraosWithLeiosLedgerView f =
  PraosWithLeiosLedgerView
    { pwlvBase = forecastToPraosLedgerView @t @era f
    , pwlvLeios = forecastToLeiosLedgerView @t @era f
    }

forecastToLeiosLedgerView ::
  forall t era.
  Dijkstra.DijkstraEraForecast era =>
  SL.Forecast t era ->
  LeiosLedgerView
forecastToLeiosLedgerView f =
  LeiosLedgerView
    { llvCommittee = f ^. Dijkstra.leiosCommitteeForecastL @era @t
    , llvQuorumStakeThreshold = f ^. Dijkstra.leiosQuorumStakeThresholdForecastL @era @t
    , llvAnnouncementPeriodLength = f ^. Dijkstra.leiosAnnouncementPeriodLengthForecastL @era @t
    , llvVotePeriodLength = f ^. Dijkstra.leiosVotePeriodLengthForecastL @era @t
    , llvDiffusionPeriodLength = f ^. Dijkstra.leiosDiffusionPeriodLengthForecastL @era @t
    , llvMaxEbBodySize = f ^. Dijkstra.maxEndorserBlockReferencesSizeForecastL @era @t
    }
