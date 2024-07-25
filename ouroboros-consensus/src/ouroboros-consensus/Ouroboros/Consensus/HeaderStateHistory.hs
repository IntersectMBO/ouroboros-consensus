{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | HeaderState history
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.HeaderStateHistory (HeaderStateHistory)
-- > import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
module Ouroboros.Consensus.HeaderStateHistory (
    HeaderStateHistory (..)
  , cast
  , current
  , rewind
  , trim
    -- * 'HeaderStateWithTime'
  , HeaderStateWithTime (..)
  , castHeaderStateWithTime
  , mkHeaderStateWithTime
  , mkHeaderStateWithTimeFromSummary
    -- * Validation
  , validateHeader
    -- * Support for tests
  , fromChain
  ) where

import           Control.Monad.Except (Except)
import           Data.Coerce (Coercible)
import qualified Data.List.NonEmpty as NE
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (RelativeTime)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import           Ouroboros.Consensus.HardFork.History (Summary)
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import           Ouroboros.Consensus.HeaderValidation hiding (validateHeader)
import qualified Ouroboros.Consensus.HeaderValidation as HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.CallStack (HasCallStack)
import           Ouroboros.Network.AnchoredSeq (Anchorable, AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain

-- | Maintain a history of 'HeaderStateWithTime's.
newtype HeaderStateHistory blk = HeaderStateHistory {
      unHeaderStateHistory ::
           AnchoredSeq
             (WithOrigin SlotNo)
             (HeaderStateWithTime blk)
             (HeaderStateWithTime blk)
    }
  deriving (Generic)

deriving stock instance (BlockSupportsProtocol blk, HasAnnTip blk)
                      => Eq (HeaderStateHistory blk)
deriving stock instance (BlockSupportsProtocol blk, HasAnnTip blk)
                      => Show (HeaderStateHistory blk)
deriving newtype instance (BlockSupportsProtocol blk, HasAnnTip blk)
                        => NoThunks (HeaderStateHistory blk)

current :: HeaderStateHistory blk -> HeaderStateWithTime blk
current = either id id . AS.head . unHeaderStateHistory

-- | Append a 'HeaderState' to the history.
append :: HeaderStateWithTime blk -> HeaderStateHistory blk -> HeaderStateHistory blk
append h (HeaderStateHistory history) = HeaderStateHistory (history :> h)

-- | Trim the 'HeaderStateHistory' to the given size, dropping the oldest
-- snapshots. The anchor will be shifted accordingly.
--
-- Note that we do not include the anchor in the size. For example, trimming to
-- 0 results in no snapshots but still an anchor. Trimming to 1 results in 1
-- snapshot and an anchor.
trim :: Int -> HeaderStateHistory blk -> HeaderStateHistory blk
trim n (HeaderStateHistory history) =
    HeaderStateHistory (AS.anchorNewest (fromIntegral n) history)

cast ::
     ( Coercible (ChainDepState (BlockProtocol blk ))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => HeaderStateHistory blk -> HeaderStateHistory blk'
cast (HeaderStateHistory history) =
      HeaderStateHistory
    $ AS.bimap castHeaderStateWithTime castHeaderStateWithTime history

-- | \( O\(n\) \). Rewind the header state history
--
-- We also return the oldest 'HeaderStateWithTime' that was rewound, if any.
--
-- NOTE: we don't distinguish headers of regular blocks from headers of EBBs.
-- Whenever we use \"header\" it can be either. In practice, EBB headers do not
-- affect the 'ChainDepState', but they /do/ affect the 'AnnTip'.
--
-- PRECONDITION: the point to rewind to must correspond to a header (or
-- 'GenesisPoint') that was previously applied to the header state history.
--
-- Rewinding the header state history is intended to be used when switching to a
-- fork, longer or equally long to the chain to which the current header state
-- corresponds. So each rewinding should be followed by rolling forward (using
-- 'headerStateHistoryPush') at least as many blocks that we have rewound.
--
-- Note that repeatedly rewinding a header state history does not make it
-- possible to rewind it all the way to genesis (this would mean that the whole
-- historical header state is accumulated or derivable from the current header
-- state history). For example, rewinding a header state by @i@ blocks and then
-- rewinding that header state again by @j@ where @i + j > k@ is not possible
-- and will yield 'Nothing'.
rewind ::
     forall blk. (HasAnnTip blk)
  => Point blk
  -> HeaderStateHistory blk
  -> Maybe (HeaderStateHistory blk, Maybe (HeaderStateWithTime blk))
rewind p (HeaderStateHistory history) = do
    (prefix, suffix) <- AS.splitAfterMeasure
      (pointSlot p)
      ((== p) . headerStatePoint . hswtHeaderState . either id id)
      history
    let oldestRewound = case suffix of
          AS.Empty _   -> Nothing
          hswt AS.:< _ -> Just hswt
    pure (HeaderStateHistory prefix, oldestRewound)


{-------------------------------------------------------------------------------
  HeaderStateWithTime
-------------------------------------------------------------------------------}

-- | A 'HeaderState' together with the 'RelativeTime' corresponding to the tip
-- slot of the state. For a state at 'Origin', we use the same time as for slot
-- 0.
data HeaderStateWithTime blk = HeaderStateWithTime {
    hswtHeaderState :: !(HeaderState blk)
  , hswtSlotTime    :: !RelativeTime
  }
  deriving stock (Generic)

deriving stock instance (BlockSupportsProtocol blk, HasAnnTip blk)
                      => Eq (HeaderStateWithTime blk)
deriving stock instance (BlockSupportsProtocol blk, HasAnnTip blk)
                      => Show (HeaderStateWithTime blk)
deriving anyclass instance (BlockSupportsProtocol blk, HasAnnTip blk)
                         => NoThunks (HeaderStateWithTime blk)

instance Anchorable (WithOrigin SlotNo) (HeaderStateWithTime blk) (HeaderStateWithTime blk) where
  asAnchor = id
  getAnchorMeasure _ = fmap annTipSlotNo . headerStateTip . hswtHeaderState

castHeaderStateWithTime ::
     ( Coercible (ChainDepState (BlockProtocol blk ))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => HeaderStateWithTime blk -> HeaderStateWithTime blk'
castHeaderStateWithTime hswt = HeaderStateWithTime {
      hswtHeaderState = castHeaderState $ hswtHeaderState hswt
    , hswtSlotTime    = hswtSlotTime hswt
    }

mkHeaderStateWithTimeFromSummary ::
     (HasCallStack, HasAnnTip blk)
  => Summary (HardForkIndices blk)
     -- ^ Must be able to convert the tip slot of the 'HeaderState' to a time.
  -> HeaderState blk
  -> HeaderStateWithTime blk
mkHeaderStateWithTimeFromSummary summary hst =
    HeaderStateWithTime {
        hswtHeaderState = hst
      , hswtSlotTime    = slotTime
      }
  where
    (slotTime, _) = Qry.runQueryPure qry summary
    qry           = Qry.slotToWallclock slot
    slot          = fromWithOrigin 0 $ pointSlot $ headerStatePoint hst

mkHeaderStateWithTime ::
     (HasCallStack, HasHardForkHistory blk, HasAnnTip blk)
  => LedgerConfig blk
  -> ExtLedgerState blk
  -> HeaderStateWithTime blk
mkHeaderStateWithTime lcfg (ExtLedgerState lst hst) =
    mkHeaderStateWithTimeFromSummary summary hst
  where
    -- A summary can always translate the tip slot of the ledger state it was
    -- created from.
    summary = hardForkSummary lcfg lst

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Variation on 'HeaderValidation.validateHeader' that maintains a
-- 'HeaderStateHistory'.
--
-- This is used only in the chain sync client for header-only validation.
--
-- Note: this function does not trim the 'HeaderStateHistory'.
validateHeader ::
     forall blk. (BlockSupportsProtocol blk, ValidateEnvelope blk)
  => TopLevelConfig blk
  -> LedgerView (BlockProtocol blk)
  -> Header blk
  -> RelativeTime
     -- ^ The time of the slot of the header.
  -> HeaderStateHistory blk
  -> Except (HeaderError blk) (HeaderStateHistory blk)
validateHeader cfg lv hdr slotTime history = do
    st' <- HeaderValidation.validateHeader cfg lv hdr st
    return $ append (HeaderStateWithTime st' slotTime) history
  where
    st :: Ticked (HeaderState blk)
    st = tickHeaderState
           (configConsensus cfg)
           lv
           (blockSlot hdr)
           (hswtHeaderState $ current history)

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Create a 'HeaderStateHistory' corresponding to the blocks in the given
-- 'Chain'.
--
-- PRECONDITION: the blocks in the chain are valid.
fromChain ::
     forall blk.
     ( ApplyBlock (ExtLedgerState blk) blk
     , HasHardForkHistory blk
     , HasAnnTip blk
     )
  => TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger state
  -> Chain blk
  -> HeaderStateHistory blk
fromChain cfg initState chain =
    HeaderStateHistory (AS.fromOldestFirst anchorSnapshot snapshots)
  where
    anchorSnapshot NE.:| snapshots =
          fmap (mkHeaderStateWithTime (configLedger cfg))
        . NE.scanl
            (flip (tickThenReapply (ExtLedgerCfg cfg)))
            initState
        . Chain.toOldestFirst
        $ chain

