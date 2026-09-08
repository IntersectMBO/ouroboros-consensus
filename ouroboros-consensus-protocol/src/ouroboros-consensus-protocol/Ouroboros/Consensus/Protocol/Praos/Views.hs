{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( BaseHeaderBody
  , BaseHeaderView (..)
  , PraosLedgerView (..)
  , forecastToPraosLedgerView
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe)
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (KES, VRF)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Kind (Type)
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

-----

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

-- | Build a 'PraosLedgerView' from a ledger 'EraForecast'
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
