{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck (
    -- * Interface
    HeaderInFutureCheck (..)
  , SomeHeaderInFutureCheck (..)
    -- * Real Implementation
  , HeaderArrivalException (..)
  , realHeaderInFutureCheck
  ) where

import           Control.Exception (Exception)
import           Control.Monad (unless, when)
import           Control.Monad.Class.MonadTimer.SI (MonadDelay, threadDelay)
import           Control.Monad.Except (Except, liftEither, throwError)
import           Data.Proxy (Proxy (Proxy))
import           Data.Time.Clock (NominalDiffTime)
import           Data.Type.Equality ((:~:) (Refl))
import           Data.Typeable (eqT)
import           Ouroboros.Consensus.Block.Abstract (Header)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint,
                     headerRealPoint, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (RelativeTime, SystemTime, diffRelTime, systemTimeCurrent)
import           Ouroboros.Consensus.Fragment.InFuture (ClockSkew, unClockSkew)
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory,
                     hardForkSummary)
import           Ouroboros.Consensus.HardFork.History (PastHorizonException)
import           Ouroboros.Consensus.HardFork.History.Qry (runQuery,
                     slotToWallclock)
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import           Ouroboros.Consensus.Util.Time (nominalDelay)
import           Ouroboros.Network.Block (HasHeader)

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

data SomeHeaderInFutureCheck m blk = forall arrival judgment.
    SomeHeaderInFutureCheck (HeaderInFutureCheck m blk arrival judgment)

-- | The interface a ChainSync client needs in order to check the arrival time
-- of headers.
--
-- Instead of alphabetical, the fields are in the order in which the ChainSync
-- client logic will invoke them for each header.
data HeaderInFutureCheck m blk arrival judgment = HeaderInFutureCheck {
    proxyArrival :: Proxy arrival
  ,
    -- | This is ideally called _immediately_ upon the header arriving.
    recordHeaderArrival :: Header blk -> m arrival
  ,
    -- | Judge what to do about the header's arrival time.
    --
    -- Note that this may be called after a delay, hence @arrival@ contains at
    -- least the arrival time.
    --
    -- In particular, such a delay might be caused by waiting for the
    -- intersection with the local selection to change after this function
    -- returns 'Ouroboros.Consensus.HardFork.HistoryPastHorizon'.
    judgeHeaderArrival ::
         LedgerConfig blk
      -> LedgerState blk
      -> arrival
      -> Except PastHorizonException judgment
  ,
    -- | Enact the judgment.
    --
    -- On success, return the slot time of the header; otherwise, an exception
    -- should be raised.
    handleHeaderArrival ::
         judgment
      -> m (Except HeaderArrivalException RelativeTime)
  }

{-------------------------------------------------------------------------------
  Real implmementation
-------------------------------------------------------------------------------}

data HeaderArrivalException =
  -- | The header arrived so early that its issuer either minted it before
  -- their clock reached its slot onset or else the difference between their
  -- clock and ours is more severe than we're configured to tolerate.
  --
  -- INVARIANT: @'tolerableClockSkew' < negate 'ageUponArrival'@
  forall blk. HasHeader blk => FarFutureHeaderException {
      ageUponArrival     :: !NominalDiffTime
    ,
      arrivedPoint       :: !(RealPoint blk)
    ,
      arrivalTime        :: !RelativeTime
    ,
      tolerableClockSkew :: !NominalDiffTime
    }

deriving instance Show HeaderArrivalException

instance Exception HeaderArrivalException

instance Eq HeaderArrivalException where
  (==)
    (FarFutureHeaderException l0 (l1 :: RealPoint l) l2 l3)
    (FarFutureHeaderException r0 (r1 :: RealPoint r) r2 r3)
    = case eqT @l @r of
        Nothing   -> False
        Just Refl -> (l0, l1, l2, l3) == (r0, r1, r2, r3)

realHeaderInFutureCheck ::
  ( HasHeader blk
  , HasHeader (Header blk)
  , HasHardForkHistory blk
  , MonadDelay m
  )
  => ClockSkew -> SystemTime m -> SomeHeaderInFutureCheck m blk
realHeaderInFutureCheck skew systemTime =
    SomeHeaderInFutureCheck
  $ HeaderInFutureCheck {
    proxyArrival        = Proxy
  , recordHeaderArrival = \hdr -> do
        (,) (headerRealPoint hdr) <$> systemTimeCurrent systemTime
  , judgeHeaderArrival = \lcfg lst (p, arrivalTime_) -> do
        let qry       = slotToWallclock (realPointSlot p)
            hfSummary = hardForkSummary lcfg lst
              -- TODO cache this in the KnownIntersectionState? Or even in the
              -- LedgerDB?
        (onset, _slotLength) <- liftEither $ runQuery qry hfSummary
        pure (p, arrivalTime_, onset)
  , handleHeaderArrival = \(p, arrivalTime_, onset) -> do
        let ageUponArrival_ = arrivalTime_ `diffRelTime` onset
            tooEarly        = unClockSkew skew < negate ageUponArrival_
              -- TODO leap seconds?

        -- this delay is the simple part of Ouroboros Chronos
        unless tooEarly $ do
            now <- systemTimeCurrent systemTime
            let ageNow         = now `diffRelTime` onset
                syntheticDelay = negate ageNow
            when (0 < syntheticDelay) $ do   -- note https://github.com/input-output-hk/io-sim/issues/129
                threadDelay $ nominalDelay syntheticDelay   -- TODO leap seconds?

        pure $ do
          when tooEarly $ throwError FarFutureHeaderException {
                ageUponArrival     = ageUponArrival_
              , arrivedPoint       = p
              , arrivalTime        = arrivalTime_
              , tolerableClockSkew = unClockSkew skew
              }
          -- no exception if within skew
          pure onset
  }
