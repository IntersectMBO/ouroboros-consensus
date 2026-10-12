{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What one LeiosNotify client remembers about its upstream peer, and how it
-- judges that peer's replies against it.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( ExnLeiosNotifyInvalidAnnouncement (..)
  , LeiosNotifyPeerState (..)
  , emptyLeiosNotifyPeerState
  , onLeiosNotifyAnnouncement
  , pruneLeiosNotifyPeerState
  ) where

import Control.Monad.Except (runExceptT)
import Control.Tracer (nullTracer)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemTime)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Leios.Protocol (LeiosProtocol)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Validate
import Ouroboros.Consensus.Util.IOLike (Exception, IOLike, throwIO)

-- | What one LeiosNotify client remembers about its upstream peer.
--
-- Only an announcement adds to it, and every entry is for an election at or
-- after 'lnpsPruneSlot'; the node prunes it as its immutable tip advances.
data LeiosNotifyPeerState blk = MkLeiosNotifyPeerState
  { lnpsPruneSlot :: !SlotNo
  -- ^ The slot the state has been pruned up to.
  , lnpsAnnouncements :: !(Announcements.AnnouncementsState (AnnouncingHeader blk))
  -- ^ What this peer has announced, for dedup and equivocation counting.
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "LeiosNotifyPeerState" (LeiosNotifyPeerState blk)

emptyLeiosNotifyPeerState :: LeiosNotifyPeerState blk
emptyLeiosNotifyPeerState =
  MkLeiosNotifyPeerState
    { lnpsPruneSlot = SlotNo 0
    , lnpsAnnouncements = Announcements.emptyAnnouncementsState
    }

-- | Forget everything about elections before the given slot, which is the
-- immutable tip's.
pruneLeiosNotifyPeerState :: SlotNo -> LeiosNotifyPeerState blk -> LeiosNotifyPeerState blk
pruneLeiosNotifyPeerState immTipSlot st
  | lnpsPruneSlot st < immTipSlot =
      MkLeiosNotifyPeerState
        { lnpsPruneSlot = immTipSlot
        , lnpsAnnouncements = Announcements.pruneAnnouncementsState immTipSlot (lnpsAnnouncements st)
        }
  | otherwise = st

-- | Thrown when a peer's 'MsgLeiosBlockAnnouncement' is one an honest peer
-- would not send; the ensuing thread death disconnects it.
data ExnLeiosNotifyInvalidAnnouncement blk
  = -- | The header carries no announcement, so it should not have been relayed
    -- as one.
    ExnLeiosNotifyAnnouncementMissing
  | ExnLeiosNotifyInvalidAnnouncement !(Announcements.ErrAnnouncement (AnnouncementInvalidity blk))
  deriving Show

instance Typeable blk => Exception (ExnLeiosNotifyInvalidAnnouncement blk)

-- | Judge a peer's 'MsgLeiosBlockAnnouncement' against what it has announced
-- before, throwing if it is one an honest peer would not send.
--
-- Nothing else in the node processes the announcement yet.
onLeiosNotifyAnnouncement ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  SystemTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  TopLevelConfig blk ->
  -- | the immutable tip's ledger state
  ExtLedgerState blk EmptyMK ->
  LeiosNotifyPeerState blk ->
  Header blk ->
  m (LeiosNotifyPeerState blk)
onLeiosNotifyAnnouncement systemTime futureCheck cfg immLedger st hdr = do
  anc <- case mkAnnouncingHeader cfg hdr of
    Nothing -> throwIO (ExnLeiosNotifyAnnouncementMissing @blk)
    Just x -> pure x
  res <-
    runExceptT $
      Announcements.onAnnouncement
        nullTracer
        ancElId
        (announcementValidity systemTime futureCheck cfg immLedger)
        (\_anc _validated -> pure ())
        (lnpsAnnouncements st)
        anc
  case res of
    Left err -> throwIO (ExnLeiosNotifyInvalidAnnouncement err :: ExnLeiosNotifyInvalidAnnouncement blk)
    Right announcements -> pure st{lnpsAnnouncements = announcements}
