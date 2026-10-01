{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( HeaderView' (..)
  , HeaderView
  , PraosLedgerView (..)
  , forecastToPraosLedgerView
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (KES, VRF)
import Cardano.Protocol.Praos.BlockHeader (HeaderBody)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Word (Word16, Word32)
import Lens.Micro ((^.))

-- | The 'HeaderView'' of Praos, which signs the Praos header body.
type HeaderView crypto = HeaderView' (HeaderBody crypto) crypto

-- | View of the block header required by the Praos protocol, parameterised by
-- the body its signature covers.
data HeaderView' body crypto = HeaderView'
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
  , hvSigned :: !body
  -- ^ Header which must be signed
  , hvSignature :: !(SignedKES (KES crypto) body)
  -- ^ KES Signature of the header
  }

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
