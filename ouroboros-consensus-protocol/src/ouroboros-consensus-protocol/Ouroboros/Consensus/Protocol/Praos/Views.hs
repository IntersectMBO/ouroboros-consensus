{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Protocol.Praos.Views
  ( HeaderView (..)
  , LedgerView (..)
  ) where

import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (KES, VRF)
import Cardano.Protocol.TPraos.BHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Slot (SlotNo)
import Data.Word (Word16, Word32)
import Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | View of the block header required by the Praos protocol.
data HeaderView crypto = HeaderView
  { hvPrevHash :: !PrevHash
  -- ^ Hash of the previous block
  , hvVK :: !(VKey 'BlockIssuer)
  -- ^ verification key of block issuer
  , hvVrfVK :: !(VerKeyVRF (VRF crypto))
  -- ^ VRF verification key for block issuer
  , hvVrfRes :: !(CertifiedVRF (VRF crypto) InputVRF)
  -- ^ VRF result
  , hvOCert :: !(OCert crypto)
  -- ^ operational certificate
  , hvSlotNo :: !SlotNo
  -- ^ Slot
  , hvSigned :: !(HeaderBody crypto)
  -- ^ Header which must be signed
  , hvSignature :: !(SignedKES (KES crypto) (HeaderBody crypto))
  -- ^ KES Signature of the header
  }

data LedgerView = LedgerView
  { lvPoolDistr :: SL.PoolDistr
  -- ^ Stake distribution
  , lvMaxHeaderSize :: !Word16
  -- ^ Maximum header size
  , lvMaxBodySize :: !Word32
  -- ^ Maximum block body size
  , lvProtocolVersion :: !ProtVer
  -- ^ Current protocol version
  }
  deriving Show
