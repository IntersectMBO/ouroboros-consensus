{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | What a consensus protocol says about the Leios announcement a header may
-- carry.
module Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , decodeLeiosAnnouncement
  , encodeLeiosAnnouncement
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , OCINStaleness (..)
  , defaultGetContainsLeiosCert
  , defaultGetLeiosLedgerView
  , defaultValidateLeiosAnnouncement
  , defaultGetLeiosAnnouncement
  , trivialLeiosLedgerView
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.BaseTypes (Milliseconds32, UnitInterval)
import Cardano.Ledger.Binary.Plain (decodeRecordNamed)
import Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import Cardano.Ledger.State (LeiosCommittee)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad.Except (Except)
import Data.Maybe.Strict (StrictMaybe, maybeToStrictMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, SlotNo)
import Ouroboros.Consensus.Leios.Types (BytesSize, EndorserBlockHash)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Ticked (Ticked)

-- | The announcement a header carries, as its protocol view gives it.
data LeiosAnnouncement = MkLeiosAnnouncement
  { announcementElectee :: !(KeyHash BlockIssuer)
  -- ^ the hash of the elected pool's cold verification key; with the header's
  -- slot, this identifies the election
  , announcementEbHash :: !EndorserBlockHash
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
  CBOR.encodeListLen 3 <> toCBOR electee <> toCBOR ebHash <> CBOR.encodeWord32 ebSize

decodeLeiosAnnouncement :: CBOR.Decoder s LeiosAnnouncement
decodeLeiosAnnouncement =
  decodeRecordNamed (T.pack "LeiosAnnouncement") (const 3) $
    MkLeiosAnnouncement <$> fromCBOR <*> fromCBOR <*> CBOR.decodeWord32

-- | The Leios parameters a ledger view carries.
--
-- The periods are durations, as the protocol parameters define them; they need
-- not be whole numbers of slots.
data LeiosLedgerView = MkLeiosLedgerView
  { llvCommittee :: !LeiosCommittee
  -- ^ who may vote this epoch, and with what weight
  , llvQuorumStakeThreshold :: !UnitInterval
  -- ^ the weight a certificate must accumulate
  , llvAnnouncementPeriod :: !Milliseconds32
  -- ^ L_hdr, the announcement period
  , llvVotePeriod :: !Milliseconds32
  -- ^ L_vote, the voting period
  , llvDiffusionPeriod :: !Milliseconds32
  -- ^ L_diff, the diffusion period
  , llvMaxEbBodySize :: !BytesSize
  -- ^ the largest endorser block the ledger admits, not counting its
  -- transactions
  , llvMaxEbTxsSize :: !BytesSize
  -- ^ the largest total size of the transactions an endorser block references
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
  getLeiosAnnouncement :: proxy p -> ValidateView p -> Maybe LeiosAnnouncement

  -- | Whether the block whose header has this view contains a Leios
  -- certificate, which certifies the endorser block its predecessor announced.
  getContainsLeiosCert :: proxy p -> ValidateView p -> Bool

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

-- | 'getLeiosAnnouncement' for a protocol whose headers never carry an
-- announcement.
defaultGetLeiosAnnouncement :: proxy p -> ValidateView p -> Maybe LeiosAnnouncement
defaultGetLeiosAnnouncement _proxy _view = Nothing

-- | 'getContainsLeiosCert' for a protocol whose blocks never contain a Leios
-- certificate.
defaultGetContainsLeiosCert :: proxy p -> ValidateView p -> Bool
defaultGetContainsLeiosCert _proxy _view = False

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

-- | The Leios part of the ledger view of a block whose ledger view is trivial,
-- and so the same at every slot
--
-- A trivial ledger view does not rule out Leios: the protocol may fix its Leios
-- parameters, which 'getLeiosLedgerView' would give.
trivialLeiosLedgerView ::
  forall blk proxy.
  (LeiosProtocol (BlockProtocol blk), LedgerView (BlockProtocol blk) ~ ()) =>
  proxy blk ->
  StrictMaybe LeiosLedgerView
trivialLeiosLedgerView _proxy =
  maybeToStrictMaybe $ getLeiosLedgerView (Proxy @(BlockProtocol blk)) ()
