{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricalRollbacks (
    -- * Interface
    HistoricalRollbackCheck (..)
  , HistoricalRollbackException (..)
  , MaxRollbackAge (..)
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
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Node.GsmState (GsmState (..))

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

-- | Interface for the ChainSync client for deciding whether rollbacks are
-- historical.
data HistoricalRollbackCheck m blk = HistoricalRollbackCheck {
    -- | Determine whether the received @MsgRollBackward@ to the given point
    -- meets the historical rollback criteria.
    --
    -- PRECONDITION: The given 'LedgerState' must be able to translate the slot
    -- of the given 'Point' to a wallclock time.
    onRollBackward ::
         LedgerConfig blk
      -> LedgerState blk
      -> Point blk
      -> m (Either HistoricalRollbackException ())
  }

data HistoricalRollbackException =
  -- | We received a rollback to a point that was too old to plausibly have been
  -- sent by an honest peer.
  --
  -- INVARIANT: @'maxRollbackAge' < 'arrivalTime' `diffRelTime` 'slotTime'@
  forall blk. HasHeader blk => HistoricalRollbackException {
      rollbackPoint  :: !(Point blk)
      -- | The time corresponding to the onset of the slot of the rollback
      -- point.
    , slotTime       :: !RelativeTime
      -- | When the @MsgRollBackward@ to the given point was received.
    , arrivalTime    :: !RelativeTime
      -- | The maximum age of a rollback at arrival time.
    , maxRollbackAge :: !MaxRollbackAge
    }
  deriving anyclass (Exception)

deriving stock instance Show HistoricalRollbackException

instance Eq HistoricalRollbackException where
  (==)
    (HistoricalRollbackException (l0 :: Point l) l1 l2 l3)
    (HistoricalRollbackException (r0 :: Point r) r1 r2 r3)
    = case eqT @l @r of
        Nothing   -> False
        Just Refl -> (l0, l1, l2, l3) == (r0, r1, r2, r3)

-- ^ The maximum age of a rollback at arrival time.
--
-- This should be set to at least the maximum duration (across all eras) of a
-- stability window (the number of slots in which at least @k@ blocks are
-- guaranteed to arise).
--
-- For example, on Cardano mainnet today, the Praos Chain Growth property
-- implies that @3k/f@ (=129600) slots (=36 hours) will contain at least @k@
-- (=2160) blocks. (Byron has a smaller stability window, namely @2k@ (=24 hours
-- as the Byron slot length is 20s). Thus a peer rolling back a header that is
-- older than 36 hours is either violating the maximum rollback or else isn't a
-- caught-up node. Either way, a syncing node should not be connected to that
-- peer.
newtype MaxRollbackAge = MaxRollbackAge {
    getMaxRollbackAge :: NominalDiffTime
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Real implmementation
-------------------------------------------------------------------------------}

-- | Do not perform any historicity checks. This is useful when we only sync
-- from trusted peers (Praos mode) or when the impact of historical rollbacks is
-- already mitigated by other means (for example indirectly by the Limit on
-- Patience in the case of Genesis /without/ ChainSync Jumping).
noCheck :: Applicative m => HistoricalRollbackCheck m blk
noCheck = HistoricalRollbackCheck {
      onRollBackward = \_lcfg _lst _rollbackPoint -> pure $ Right ()
    }

-- | Deny all rollbacks that are older than the given 'NominalDiffTime' upon
-- arrival.
mkCheck ::
     forall m blk.
     ( Monad m
     , HasHeader blk
     , HasHardForkHistory blk
     )
  => SystemTime m
  -> m GsmState
     -- ^ Get the current 'GsmState'.
     --
     -- This is used to disable the historical rollback check when we are caught
     -- up. The rationale is extra resilience against disconnects between honest
     -- nodes in disaster scenarios with very low chain density.
  -> MaxRollbackAge
  -> HistoricalRollbackCheck m blk
mkCheck systemTime getCurrentGsmState maxRollbackAge = HistoricalRollbackCheck {
      onRollBackward = \lcfg lst rollbackPoint -> getCurrentGsmState >>= \case
          PreSyncing -> judgeRollback lcfg lst rollbackPoint
          Syncing    -> judgeRollback lcfg lst rollbackPoint
          CaughtUp   -> pure $ Right ()
    }
  where
    judgeRollback lcfg lst rollbackPoint = do
        arrivalTime <- systemTimeCurrent systemTime
        let actualRollbackAge = arrivalTime `diffRelTime` slotTime
        pure $ when (getMaxRollbackAge maxRollbackAge < actualRollbackAge) $
          throwError HistoricalRollbackException {
              rollbackPoint
            , slotTime
            , arrivalTime
            , maxRollbackAge
            }
      where
        -- Use slot 0 in case we are rolling back to Genesis.
        rollbackSlot  = fromWithOrigin 0 $ pointSlot rollbackPoint
        qry           = Qry.slotToWallclock rollbackSlot
        hfSummary     = hardForkSummary lcfg lst
        -- This can't fail as per the precondition on 'onRollBackward'.
        (slotTime, _) = Qry.runQueryPure qry hfSummary
