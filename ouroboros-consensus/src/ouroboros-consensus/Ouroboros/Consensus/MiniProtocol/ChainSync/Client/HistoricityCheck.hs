{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck (
    -- * Interface
    HistoricalChainSyncMessage (..)
  , HistoricityCheck (..)
  , HistoricityCutoff (..)
  , HistoricityException (..)
    -- * Real implementation
  , mkCheck
  , noCheck
  ) where

import           Control.Exception (Exception)
import           Control.Monad (when)
import           Control.Monad.Except (throwError)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Typeable (eqT)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (RelativeTime,
                     SystemTime (..), diffRelTime)
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateWithTime (..))
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip,
                     headerStatePoint)
import           Ouroboros.Consensus.Node.GsmState (GsmState (..))

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

-- | Interface for the ChainSync client for deciding whether @MsgRollBackward@s
-- and @MsgAwaitReply@s are historical.
data HistoricityCheck m blk = HistoricityCheck {
    -- | Determine whether the received message is historical. Depending on the
    -- first argument, the second argument is:
    --
    --  * 'HistoricalMsgRollBackward': The oldest state that was rolled back.
    --    (Note that rollbacks of depth zero are hence never historical.).
    --
    --  * 'HistoricalMsgAwaitReply': The state corresponding to the tip of the
    --    candidate fragment when @MsgAwaitReply@ was sent.
    judgeMessageHistoricity ::
         HistoricalChainSyncMessage
      -> HeaderStateWithTime blk
      -> m (Either HistoricityException ())
  }

-- | ChainSync historicity checks are performed for @MsgRollBackward@s and
-- @MsgAwaitReply@s, see 'HistoricityCheck'.
data HistoricalChainSyncMessage =
    HistoricalMsgRollBackward
  | HistoricalMsgAwaitReply
  deriving stock (Show, Eq)

data HistoricityException =
  -- | We received a @MsgRollBackward@ or a @MsgAwaitReply@ while their
  -- candidate chain was too old for it to plausibly have been sent by an honest
  -- caught-up peer.
  --
  -- INVARIANT: @'historicityCutoff' < 'arrivalTime' `diffRelTime` 'slotTime'@
  forall blk. HasHeader blk => HistoricityException {
      historicalMessage :: HistoricalChainSyncMessage
      -- | Depending on 'historicalMessage':
      --
      --  * 'HistoricalMsgRollBackward': The oldest header that was rewound.
      --
      --  * 'HistoricalMsgAwaitReply': The tip of the candidate fragment.
    , historicalPoint   :: !(Point blk)
      -- | The time corresponding to the slot of 'historicalPoint'.
    , slotTime          :: !RelativeTime
      -- | When the offending 'historicalMessage' was received.
    , arrivalTime       :: !RelativeTime
    , historicityCutoff :: !HistoricityCutoff
    }
  deriving anyclass (Exception)

deriving stock instance Show HistoricityException

instance Eq HistoricityException where
  (==)
    (HistoricityException l0 (l1 :: Point l) l2 l3 l4)
    (HistoricityException r0 (r1 :: Point r) r2 r3 r4)
    = case eqT @l @r of
        Nothing   -> False
        Just Refl -> (l0, l1, l2, l3, l4) == (r0, r1, r2, r3, r4)

-- ^ The maximum age of a @MsgRollBackward@ or @MsgAwaitReply@ at arrival time,
-- constraining the age of the oldest rewound header or the tip of the candidate
-- fragment, respectively.
--
-- This should be set to at least the maximum duration (across all eras) of a
-- stability window (the number of slots in which at least @k@ blocks are
-- guaranteed to arise).
--
-- For example, on Cardano mainnet today, the Praos Chain Growth property
-- implies that @3k/f@ (=129600) slots (=36 hours) will contain at least @k@
-- (=2160) blocks. (Byron has a smaller stability window, namely @2k@ (=24 hours
-- as the Byron slot length is 20s). Thus a peer rolling back a header that is
-- older than 36 hours or signals that it doesn't have more headers is either
-- violating the maximum rollback or else isn't a caught-up node. Either way, a
-- syncing node should not be connected to that peer.
newtype HistoricityCutoff = HistoricityCutoff {
    getHistoricityCutoff :: NominalDiffTime
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Real implmementation
-------------------------------------------------------------------------------}

-- | Do not perform any historicity checks. This is useful when we only sync
-- from trusted peers (Praos mode) or when the impact of historical messages is
-- already mitigated by other means (for example indirectly by the Limit on
-- Patience in the case of Genesis /without/ ChainSync Jumping).
noCheck :: Applicative m => HistoricityCheck m blk
noCheck = HistoricityCheck {
      judgeMessageHistoricity = \_msg _hswt -> pure $ Right ()
    }

-- | Deny all rollbacks that rewind blocks older than
-- 'HistoricityCutoff' upon arrival.
mkCheck ::
     forall m blk.
     ( Monad m
     , HasHeader blk
     , HasAnnTip blk
     )
  => SystemTime m
  -> m GsmState
     -- ^ Get the current 'GsmState'.
     --
     -- This is used to disable the historicity check when we are caught up. The
     -- rationale is extra resilience against disconnects between honest nodes
     -- in disaster scenarios with very low chain density.
  -> HistoricityCutoff
  -> HistoricityCheck m blk
mkCheck systemTime getCurrentGsmState cshc = HistoricityCheck {
      judgeMessageHistoricity = \msg hswt -> getCurrentGsmState >>= \case
          PreSyncing -> judgeRollback msg hswt
          Syncing    -> judgeRollback msg hswt
          CaughtUp   -> pure $ Right ()
    }
  where
    HistoricityCutoff historicityCutoff = cshc

    judgeRollback ::
         HistoricalChainSyncMessage
      -> HeaderStateWithTime blk
      -> m (Either HistoricityException ())
    judgeRollback msg (HeaderStateWithTime headerState slotTime) = do
        arrivalTime <- systemTimeCurrent systemTime
        let actualRollbackAge = arrivalTime `diffRelTime` slotTime
        pure $ when (historicityCutoff < actualRollbackAge) $
          throwError HistoricityException {
              historicalMessage = msg
            , historicalPoint   = headerStatePoint headerState
            , slotTime
            , arrivalTime
            , historicityCutoff = cshc
            }
