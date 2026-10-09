{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( PolyPraosLedgerView (..)
  , PolyPraosValidateView (..)
  , ForecastsLeios (..)
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes
  ( Milliseconds32
  , ProtVer
  , StrictMaybe
  , UnitInterval
  )
import Cardano.Ledger.Block (EbReferencesAnnouncement)
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (LeiosCommittee)
import Cardano.Protocol.Crypto (KES, VRF)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32)
import Ouroboros.Consensus.Protocol.Praos.Common (LeiosOnly, ShelleyProtocolHeader)
import Ouroboros.Consensus.Protocol.Signed (Signed)

{-------------------------------------------------------------------------------
  Header view
-------------------------------------------------------------------------------}

-- | View of the block header required by the Praos protocol.
type PolyPraosValidateView :: Type -> Type -> Type
data PolyPraosValidateView proto crypto = HeaderView
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
  , hvLeios :: !(LeiosOnly proto () (Bool, StrictMaybe EbReferencesAnnouncement))
  -- ^ Whether this block's body carries a Leios certificate, and the endorser
  -- block this header announces
  , hvSigned :: !(Signed (ShelleyProtocolHeader proto))
  -- ^ Header which must be signed
  , hvSignature :: !(SignedKES (KES crypto) (Signed (ShelleyProtocolHeader proto)))
  -- ^ KES Signature of the header
  }

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

-- | View of the ledger required by the Praos protocol.
type PolyPraosLedgerView :: Type -> Type
data PolyPraosLedgerView proto = PraosLedgerView
  { plvPoolDistr :: SL.PoolDistr
  -- ^ Stake distribution
  , plvMaxHeaderSize :: !Word16
  -- ^ Maximum header size
  , plvMaxBodySize :: !Word32
  -- ^ Maximum block body size
  , plvProtocolVersion :: !ProtVer
  -- ^ Current protocol version
  , plvCommittee :: !(LeiosOnly proto () LeiosCommittee)
  -- ^ Who may vote this epoch, and with what weight
  , plvQuorumStakeThreshold :: !(LeiosOnly proto () UnitInterval)
  -- ^ Weight a certificate must accumulate
  , plvAnnouncementPeriodLength :: !(LeiosOnly proto () Milliseconds32)
  , plvVotePeriodLength :: !(LeiosOnly proto () Milliseconds32)
  , plvDiffusionPeriodLength :: !(LeiosOnly proto () Milliseconds32)
  -- ^ The three periods that determine how long after its announcement an
  -- endorser block may be certified. Kept as durations, since converting to a
  -- count of slots needs the slot length, which only the consensus config has.
  , plvMaxEbBodySize :: !(LeiosOnly proto () Word32)
  -- ^ Maximum size of an endorser block itself, not its closure
  , plvMaxEbTxsSize :: !(LeiosOnly proto () Word32)
  -- ^ Maximum total size of the transactions an endorser block references
  }

deriving instance
  ( Show (LeiosOnly proto () LeiosCommittee)
  , Show (LeiosOnly proto () UnitInterval)
  , Show (LeiosOnly proto () Milliseconds32)
  , Show (LeiosOnly proto () Word32)
  ) =>
  Show (PolyPraosLedgerView proto)

-- | How a protocol reads an era's forecast.
--
-- A method rather than one shared function because only the protocols with
-- Leios may demand more of their era than 'SL.EraForecast', and knowing
-- @proto@ alone cannot supply that @era@ dictionary.
type ForecastsLeios :: Type -> Type -> Constraint
class ForecastsLeios proto era where
  forecastToPolyPraosLedgerView ::
    SL.EraForecast era => SL.Forecast t era -> PolyPraosLedgerView proto
