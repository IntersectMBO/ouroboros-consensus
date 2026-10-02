{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( HeaderView (..)
  , PraosHeaderView
  , LeiosHeaderView
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
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Cardano.Protocol.Praos.BlockHeader (HeaderBody)
import Cardano.Protocol.Praos.VRF (InputVRF)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Word (Word16, Word32)
import Lens.Micro ((^.))

-- | View of the block header required by the Praos protocol.
--
-- Parameterised by the KES-signed header body. Every other field is a
-- projection that any Praos-family header offers; the signed body is the one
-- thing an extension of Praos changes, because extending the header changes
-- what the signature covers. Carrying it as an ordinary type parameter is what
-- lets the signature check below ask for a @Signable (KES crypto) body@
-- dictionary in the normal way, for whichever body the protocol in hand signs.
data HeaderView body crypto = HeaderView
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
  -- ^ Header body which must be signed
  , hvSignature :: !(SignedKES (KES crypto) body)
  -- ^ KES Signature of the header body
  }

-- | The 'HeaderView' of the base Praos protocol, which signs the Praos header
-- body.
type PraosHeaderView crypto = HeaderView (HeaderBody crypto) crypto

-- | The 'HeaderView' of Praos with Leios, which signs the Leios header body:
-- the Praos fields plus the two Leios ones.
--
-- Those two are carried but not projected: nothing in the signature or
-- envelope checks reads them.
--
-- TODO Project them, for the checks that do read them: an endorser block's
-- certification window and its size.
type LeiosHeaderView crypto = HeaderView (LeiosCodec.HeaderBody crypto) crypto

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
