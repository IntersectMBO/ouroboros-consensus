{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( PolyPraosLedgerView (..)
  , PolyPraosValidateView (..)
  , ForecastToPolyPraosLedgerView (..)
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes
  ( ProtVer
  , StrictMaybe
  )
import Cardano.Ledger.Block (EbReferencesAnnouncement)
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (KES, VRF)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView)
import Ouroboros.Consensus.Protocol.Praos.Common (ShelleyProtocolHeader, WhenLeios)
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
  , hvLeios :: !(WhenLeios proto (Bool, StrictMaybe EbReferencesAnnouncement))
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
  , plvLeios :: !(WhenLeios proto LeiosLedgerView)
  -- ^ The committee, quorum threshold, periods and endorser block size bounds
  -- that Leios needs, present only when the protocol has Leios
  }

deriving instance
  Show (WhenLeios proto LeiosLedgerView) =>
  Show (PolyPraosLedgerView proto)

-- | How a protocol reads an era's forecast.
--
-- A method rather than one shared function because only the protocols with
-- Leios may demand more of their era than 'SL.EraForecast', and knowing
-- @proto@ alone cannot supply that @era@ dictionary.
--
-- TODO Indexing on @era@ leaks a ledger-era, hence 'ShelleyBlock'-level, concept
-- into this protocol-level module: a protocol should not need to know which era
-- it runs in. It is a compromise, since the forecast it reads is itself indexed
-- by era. We might instead move this class, and any like it, down into the
-- 'ShelleyBlock' layer, where protocol and era are paired anyway.
type ForecastToPolyPraosLedgerView :: Type -> Type -> Constraint
class ForecastToPolyPraosLedgerView proto era where
  forecastToPolyPraosLedgerView ::
    SL.EraForecast era => SL.Forecast t era -> PolyPraosLedgerView proto
