{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Protocol.Praos.Views (
    HeaderView (..)
  , LedgerView (..)
  ) where

import           Cardano.Crypto.KES (SignedKES)
import           Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import           Cardano.Ledger.BaseTypes (ProtVer)
import           Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Protocol.Crypto (KES, VRF)
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Protocol.TPraos.OCert (OCert)
import           Cardano.Slotting.Slot (SlotNo)
import           Data.Word (Word16, Word32)
import           Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | View of the block header required by the Praos protocol.
data HeaderView crypto = HeaderView
  { -- | Hash of the previous block
    hvPrevHash  :: !PrevHash,
    -- | verification key of block issuer
    hvVK        :: !(VKey 'BlockIssuer),
    -- | VRF verification key for block issuer
    hvVrfVK     :: !(VerKeyVRF (VRF crypto)),
    -- | VRF result
    hvVrfRes    :: !(CertifiedVRF (VRF crypto) InputVRF),
    -- | operational certificate
    hvOCert     :: !(OCert crypto),
    -- | Slot
    hvSlotNo    :: !SlotNo,
    -- | Header which must be signed
    hvSigned    :: !(HeaderBody crypto),
    -- | KES Signature of the header
    hvSignature :: !(SignedKES (KES crypto) (HeaderBody crypto))
  }

data LedgerView = LedgerView
  { -- | Stake distribution
    lvPoolDistr       :: SL.PoolDistr,
    -- | Maximum header size
    lvMaxHeaderSize   :: !Word16,
    -- | Maximum block body size
    lvMaxBodySize     :: !Word32,
    -- | Current protocol version
    lvProtocolVersion :: !ProtVer
  }
  deriving (Show)
