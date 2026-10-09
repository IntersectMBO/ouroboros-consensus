{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( BasePraosValidateView (..)
  , BasePraosLedgerView (..)
  , ForecastsLeios (..)
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
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (LeiosCommittee)
import Cardano.Protocol.Crypto (KES, VRF)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32)
import LeiosDemoTypes (EbAnnouncement)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Signed (Signed)

{-------------------------------------------------------------------------------
  The upstream header types, per protocol
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

type BasePraosValidateView :: Type -> Type -> Type

-- | View of the block header required by the Praos protocol.
data BasePraosValidateView proto crypto = HeaderView
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
  , hvLeios :: !(EitherLeiosF proto () (Bool, StrictMaybe EbAnnouncement))
  -- ^ The Leios payload: whether this block's body carries a certificate (ie
  -- whether it is a CertRB), and the endorser block this header announces.
  , hvSigned :: !(Signed (ShelleyProtocolHeader proto))
  -- ^ Header which must be signed
  , hvSignature :: !(SignedKES (KES crypto) (Signed (ShelleyProtocolHeader proto)))
  -- ^ KES Signature of the header
  }

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

type BasePraosLedgerView :: Type -> Type

-- | View of the ledger required by the Praos protocol.
data BasePraosLedgerView proto = PraosLedgerView
  { plvPoolDistr :: SL.PoolDistr
  -- ^ Stake distribution
  , plvMaxHeaderSize :: !Word16
  -- ^ Maximum header size
  , plvMaxBodySize :: !Word32
  -- ^ Maximum block body size
  , plvProtocolVersion :: !ProtVer
  -- ^ Current protocol version
  , plvCommittee :: !(EitherLeiosF proto () LeiosCommittee)
  -- ^ Who may vote this epoch, and with what weight
  , plvQuorumStakeThreshold :: !(EitherLeiosF proto () UnitInterval)
  -- ^ Weight a certificate must accumulate
  , plvAnnouncementPeriodLength :: !(EitherLeiosF proto () Milliseconds32)
  , plvVotePeriodLength :: !(EitherLeiosF proto () Milliseconds32)
  , plvDiffusionPeriodLength :: !(EitherLeiosF proto () Milliseconds32)
  -- ^ The three periods that determine how long after its announcement an
  -- endorser block may be certified. Kept as durations, since converting to a
  -- count of slots needs the slot length, which only the consensus config has.
  , plvMaxEbBodySize :: !(EitherLeiosF proto () Word32)
  -- ^ Maximum total size of an endorser block itself (/not/ the closure)
  , plvMaxEbTxsSize :: !(EitherLeiosF proto () Word32)
  -- ^ Maximum total size of the transactions an endorser block references
  }

deriving instance
  ( Show (EitherLeiosF proto () LeiosCommittee)
  , Show (EitherLeiosF proto () UnitInterval)
  , Show (EitherLeiosF proto () Milliseconds32)
  , Show (EitherLeiosF proto () Word32)
  ) =>
  Show (BasePraosLedgerView proto)

type ForecastsLeios :: Type -> Type -> Constraint

-- | How a protocol reads an era's forecast.
--
-- A method rather than one shared function because only the protocols with
-- Leios may demand 'Dijkstra.DijkstraEraForecast' of their era, and knowing
-- @proto@ alone cannot supply that @era@ dictionary.
class ForecastsLeios proto era where
  forecastToBasePraosLedgerView ::
    SL.EraForecast era => SL.Forecast t era -> BasePraosLedgerView proto
