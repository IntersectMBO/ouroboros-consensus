{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | What a consensus protocol says about the Leios announcement a header may
-- carry.
module Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , decodeLeiosAnnouncement
  , encodeLeiosAnnouncement
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , OCINStaleness (..)
  , defaultGetLeiosLedgerView
  , defaultValidateLeiosAnnouncement
  , defaultValidateViewLeiosAnnouncement
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Plain (decodeRecordNamed)
import Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad.Except (Except)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Leios.Types (BytesSize, EbHash, decodeEbHash, encodeEbHash)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Ticked (Ticked)

-- | The announcement a header carries, as its protocol view gives it.
data LeiosAnnouncement = MkLeiosAnnouncement
  { announcementElectee :: !(KeyHash BlockIssuer)
  -- ^ the hash of the elected pool's cold verification key; with the header's
  -- slot, this identifies the election
  , announcementEbHash :: !EbHash
  , announcementEbSize :: !BytesSize
  -- ^ the announced endorser block's size
  --
  -- TODO Nothing reads it yet. A certifying ranking block's ChainSync
  -- @MsgRollForward@ will be interpreted as an offer of the endorser block it
  -- certifies, of this size, and Leios voting will confirm that the size is
  -- accurate.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

encodeLeiosAnnouncement :: LeiosAnnouncement -> CBOR.Encoding
encodeLeiosAnnouncement (MkLeiosAnnouncement electee ebHash ebSize) =
  CBOR.encodeListLen 3 <> toCBOR electee <> encodeEbHash ebHash <> CBOR.encodeWord32 ebSize

decodeLeiosAnnouncement :: CBOR.Decoder s LeiosAnnouncement
decodeLeiosAnnouncement =
  decodeRecordNamed (T.pack "LeiosAnnouncement") (const 3) $
    MkLeiosAnnouncement <$> fromCBOR <*> decodeEbHash <*> CBOR.decodeWord32

-- | The Leios parameters a ledger view carries.
--
-- TODO Once this record holds all of @PolyPraosLedgerView@'s Leios fields,
-- @PolyPraosLedgerView@ could contain it as a single @WhenLeios@ field instead
-- of carrying each Leios parameter separately.
data LeiosLedgerView = MkLeiosLedgerView
  { llvMaxEbBodySize :: !BytesSize
  -- ^ the largest endorser block the ledger admits
  , llvAnnouncementPeriod :: !NominalDiffTime
  -- ^ L_hdr, the announcement period
  , llvVotePeriod :: !NominalDiffTime
  -- ^ L_vote, the voting period
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | Whether an announcement's operational certificate issue number (OCIN) is
-- at least the one our immutable tip records for its issuer.
--
-- A stale one is accepted from the peer but neither processed nor relayed; see
-- 'validateLeiosAnnouncement'.
data OCINStaleness = FreshOCIN | StaleOCIN
  deriving stock (Eq, Show)

class ConsensusProtocol p => LeiosProtocol p where
  -- | The announcement carried by the header with this view, if any.
  validateViewLeiosAnnouncement :: proxy p -> ValidateView p -> Maybe LeiosAnnouncement

  -- | The protocol-level validation of a relayed announcement's header, done
  -- out of context: against the immutable tip's chain-dependent state ticked to
  -- the header's slot, rather than against the header's predecessor.
  --
  -- Since that state may lag, a check our lag could spuriously fail must not
  -- reject; a counter that is merely behind instead yields 'StaleOCIN'.
  --
  -- Only called for a view that carries an announcement.
  validateLeiosAnnouncement ::
    ConsensusConfig p ->
    ValidateView p ->
    SlotNo ->
    Ticked (ChainDepState p) ->
    Except (ValidationErr p) OCINStaleness

  -- | The Leios parameters this ledger view carries, or 'Nothing' if the
  -- protocol has no Leios.
  getLeiosLedgerView :: proxy p -> LedgerView p -> Maybe LeiosLedgerView

-- | 'validateViewLeiosAnnouncement' for a protocol whose headers never carry an
-- announcement.
defaultValidateViewLeiosAnnouncement :: proxy p -> ValidateView p -> Maybe LeiosAnnouncement
defaultValidateViewLeiosAnnouncement _proxy _view = Nothing

-- | 'validateLeiosAnnouncement' for a protocol whose headers never carry an
-- announcement, which is therefore never called.
defaultValidateLeiosAnnouncement ::
  ConsensusConfig p ->
  ValidateView p ->
  SlotNo ->
  Ticked (ChainDepState p) ->
  Except (ValidationErr p) OCINStaleness
defaultValidateLeiosAnnouncement _cfg _view _slot _st = pure FreshOCIN

-- | 'getLeiosLedgerView' for a protocol that has no Leios.
defaultGetLeiosLedgerView :: proxy p -> LedgerView p -> Maybe LeiosLedgerView
defaultGetLeiosLedgerView _proxy _view = Nothing
