{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | What a consensus protocol says about the Leios announcement a header may
-- carry.
module Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosProtocol (..)
  , OCINStaleness (..)
  , defaultValidateLeiosAnnouncement
  , defaultValidateViewLeiosAnnouncement
  ) where

import Control.Monad.Except (Except)
import Data.ByteString.Short (ShortByteString)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Leios.Types (BytesSize, EbHash)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Ticked (Ticked)

-- | The announcement a header carries, as its protocol view gives it.
data LeiosAnnouncement = MkLeiosAnnouncement
  { announcementElectee :: !ShortByteString
  -- ^ the hash of the elected pool's cold key; with the header's slot, this
  -- identifies the election. It is untyped bytes because the class is shared by
  -- every protocol, whose electees differ: Praos's is a 28-byte key hash, a test
  -- block's whatever identity it chooses.
  , announcementEbHash :: !EbHash
  , announcementEbSize :: !BytesSize
  -- ^ the announced endorser block's size, which nothing trusts
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
