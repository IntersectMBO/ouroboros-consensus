{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Peras.Time
  ( -- * Time-resolution context
    TimeResolutionContext (..)
  , TimeResolutionContextHandle (..)
  , mkTimeResolutionContextHandle

    -- * Errors
  , TimeResolutionError (..)

    -- * Time resolution info types
  , SlotToTimeInfo (..)
  , TimeToSlotInfo (..)
  , EpochToSlotInfo (..)
  , SlotToEpochInfo (..)
  , PerasRoundToSlotInfo (..)
  , SlotToPerasRoundInfo (..)
  , EpochToPerasRoundInfo (..)
  , PerasRoundToEpochInfo (..)

    -- * Smart accessors for time resolution info types
  , ttsiSlotNo
  , ttsiSlotStartTime
  , ttsiSlotEndTime
  , ttsiSlotLength
  , steiEpochNo
  , steiEpochStartSlot
  , steiEpochEndSlot
  , steiEpochSize
  , steiIsNewEpoch
  , stpriPerasRoundNo
  , stpriPerasRoundStartSlot
  , stpriPerasRoundEndSlot
  , stpriPerasRoundLength
  , stpriIsNewPerasRound
  , prteiEpochNo
  , prteiEpochStartPerasRound
  , prteiEpochEndPerasRound
  , prteiEpochSizeInPerasRounds
  , prteiIsNewEpoch

    -- * Time resolution within a context
  , runQueryInContext
  , resolveTimeToSlotInfo
  , resolveSlotToTimeInfo
  , resolveSlotToEpochInfo
  , resolveEpochToSlotInfo
  , resolveSlotToPerasRoundInfo
  , resolvePerasRoundToSlotInfo
  , resolveEpochToPerasRoundInfo
  , resolvePerasRoundToEpochInfo

    -- * Time resolution through a context handle
  , withHandle
  , resolveTimeToSlotInfoWithHandle
  , resolveSlotToTimeInfoWithHandle
  , resolveSlotToEpochInfoWithHandle
  , resolveEpochToSlotInfoWithHandle
  , resolveSlotToPerasRoundInfoWithHandle
  , resolvePerasRoundToSlotInfoWithHandle
  , resolveEpochToPerasRoundInfoWithHandle
  , resolvePerasRoundToEpochInfoWithHandle
  ) where

import Cardano.Prelude (Bifunctor (bimap))
import Cardano.Slotting.Time (addRelativeTime, getSlotLength)
import Control.Exception.Base (Exception)
import Data.Time (NominalDiffTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( EpochNo
  , EpochSize (..)
  , PerasRoundNo (..)
  , SlotNo
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SlotLength
  )
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (hardForkSummary))
import Ouroboros.Consensus.HardFork.History.EraParams (PerasEnabled, fromPerasEnabled)
import Ouroboros.Consensus.HardFork.History.Qry
  ( PastHorizonException
  , Qry
  , epochToSlot
  , perasRoundNoToSlot
  , runQuery
  , slotToEpoch
  , slotToEpoch'
  , slotToPerasRoundNo
  , slotToWallclock
  , wallclockToSlot
  )
import Ouroboros.Consensus.HardFork.History.Util (addSlots)
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Peras.Params (PerasRoundLength (..))
import Ouroboros.Consensus.Util.IOLike
  ( MonadSTM (..)
  , MonadThrow
  , throwSTM
  )

data TimeResolutionContext blk where
  TimeResolutionContext :: forall blk mk. LedgerConfig blk -> LedgerState blk mk -> TimeResolutionContext blk

newtype TimeResolutionContextHandle m blk = TimeResolutionContextHandle (STM m (TimeResolutionContext blk))

mkTimeResolutionContextHandle ::
  MonadSTM m => LedgerConfig blk -> STM m (LedgerState blk mk) -> TimeResolutionContextHandle m blk
mkTimeResolutionContextHandle cfg mkState = TimeResolutionContextHandle $ TimeResolutionContext cfg <$> mkState

data TimeResolutionError
  = TimeResolutionErrorPastHorizon PastHorizonException
  | TimeResolutionPerasNotEnabled
  deriving (Show, Exception)

-- | Absorb a 'NoPerasEnabled' result into a 'TimeResolutionPerasNotEnabled'
-- error.
absorbNoPerasEnabled :: PerasEnabled a -> Either TimeResolutionError a
absorbNoPerasEnabled = fromPerasEnabled (Left TimeResolutionPerasNotEnabled) . fmap Right

data SlotToTimeInfo = SlotToTimeInfo
  { sttiSlotNo :: SlotNo
  , sttiSlotStartTime :: RelativeTime
  , sttiSlotEndTime :: RelativeTime
  , sttiSlotLength :: SlotLength
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data TimeToSlotInfo = TimeToSlotInfo
  { ttsiSlotToTimeInfo :: SlotToTimeInfo
  , ttsiTimeSpentInSlot :: NominalDiffTime
  , ttsiTimeLeftInSlot :: NominalDiffTime
  , ttsiCurrentTime :: RelativeTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

ttsiSlotNo :: TimeToSlotInfo -> SlotNo
ttsiSlotNo = sttiSlotNo . ttsiSlotToTimeInfo
ttsiSlotStartTime :: TimeToSlotInfo -> RelativeTime
ttsiSlotStartTime = sttiSlotStartTime . ttsiSlotToTimeInfo
ttsiSlotEndTime :: TimeToSlotInfo -> RelativeTime
ttsiSlotEndTime = sttiSlotEndTime . ttsiSlotToTimeInfo
ttsiSlotLength :: TimeToSlotInfo -> SlotLength
ttsiSlotLength = sttiSlotLength . ttsiSlotToTimeInfo

data EpochToSlotInfo = EpochToSlotInfo
  { etsiEpochNo :: EpochNo
  , etsiEpochStartSlot :: SlotNo
  , etsiEpochEndSlot :: SlotNo
  , etsiEpochSize :: EpochSize
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data SlotToEpochInfo = SlotToEpochInfo
  { steiEpochToSlotInfo :: EpochToSlotInfo
  , steiCurrentSlot :: SlotNo
  , steiSlotsSpentInEpoch :: Word64
  , steiSlotsLeftInEpoch :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

steiEpochNo :: SlotToEpochInfo -> EpochNo
steiEpochNo = etsiEpochNo . steiEpochToSlotInfo
steiEpochStartSlot :: SlotToEpochInfo -> SlotNo
steiEpochStartSlot = etsiEpochStartSlot . steiEpochToSlotInfo
steiEpochEndSlot :: SlotToEpochInfo -> SlotNo
steiEpochEndSlot = etsiEpochEndSlot . steiEpochToSlotInfo
steiEpochSize :: SlotToEpochInfo -> EpochSize
steiEpochSize = etsiEpochSize . steiEpochToSlotInfo

steiIsNewEpoch :: SlotToEpochInfo -> Bool
steiIsNewEpoch stei = steiSlotsSpentInEpoch stei == 0

data PerasRoundToSlotInfo = PerasRoundToSlotInfo
  { prtsiPerasRoundNo :: PerasRoundNo
  , prtsiPerasRoundStartSlot :: SlotNo
  , prtsiPerasRoundEndSlot :: SlotNo
  , prtsiPerasRoundLength :: PerasRoundLength
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data SlotToPerasRoundInfo = SlotToPerasRoundInfo
  { stpriPerasRoundToSlotInfo :: PerasRoundToSlotInfo
  , stpriCurrentSlot :: SlotNo
  , stpriSlotsSpentInPerasRound :: Word64
  , stpriSlotsLeftInPerasRound :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

stpriPerasRoundNo :: SlotToPerasRoundInfo -> PerasRoundNo
stpriPerasRoundNo = prtsiPerasRoundNo . stpriPerasRoundToSlotInfo
stpriPerasRoundStartSlot :: SlotToPerasRoundInfo -> SlotNo
stpriPerasRoundStartSlot = prtsiPerasRoundStartSlot . stpriPerasRoundToSlotInfo
stpriPerasRoundEndSlot :: SlotToPerasRoundInfo -> SlotNo
stpriPerasRoundEndSlot = prtsiPerasRoundEndSlot . stpriPerasRoundToSlotInfo
stpriPerasRoundLength :: SlotToPerasRoundInfo -> PerasRoundLength
stpriPerasRoundLength = prtsiPerasRoundLength . stpriPerasRoundToSlotInfo

stpriIsNewPerasRound :: SlotToPerasRoundInfo -> Bool
stpriIsNewPerasRound stpri = stpriSlotsSpentInPerasRound stpri == 0

data EpochToPerasRoundInfo = EpochToPerasRoundInfo
  { etpriEpochNo :: EpochNo
  , etpriEpochStartPerasRound :: PerasRoundNo
  , etpriEpochEndPerasRound :: PerasRoundNo
  , etpriEpochSizeInPerasRounds :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data PerasRoundToEpochInfo = PerasRoundToEpochInfo
  { prteiEpochToPerasRoundInfo :: EpochToPerasRoundInfo
  , prteiCurrentPerasRound :: PerasRoundNo
  , prteiPerasRoundsSpentInEpoch :: Word64
  , prteiPerasRoundsLeftInEpoch :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

prteiEpochNo :: PerasRoundToEpochInfo -> EpochNo
prteiEpochNo = etpriEpochNo . prteiEpochToPerasRoundInfo
prteiEpochStartPerasRound :: PerasRoundToEpochInfo -> PerasRoundNo
prteiEpochStartPerasRound = etpriEpochStartPerasRound . prteiEpochToPerasRoundInfo
prteiEpochEndPerasRound :: PerasRoundToEpochInfo -> PerasRoundNo
prteiEpochEndPerasRound = etpriEpochEndPerasRound . prteiEpochToPerasRoundInfo
prteiEpochSizeInPerasRounds :: PerasRoundToEpochInfo -> Word64
prteiEpochSizeInPerasRounds = etpriEpochSizeInPerasRounds . prteiEpochToPerasRoundInfo

prteiIsNewEpoch :: PerasRoundToEpochInfo -> Bool
prteiIsNewEpoch prtei = prteiPerasRoundsSpentInEpoch prtei == 0

runQueryInContext ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  Qry a ->
  Either TimeResolutionError a
runQueryInContext (TimeResolutionContext cfg state) qry =
  bimap TimeResolutionErrorPastHorizon id $ runQuery qry (hardForkSummary cfg state)

-- | Get the 'TimeToSlotInfo' for a given 'RelativeTime'.
resolveTimeToSlotInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  RelativeTime ->
  Either TimeResolutionError TimeToSlotInfo
resolveTimeToSlotInfo ctx absTime =
  runQueryInContext ctx (wallclockToSlot absTime) >>= \(absSlot, timeSpentInSlot, timeLeftInSlot) -> do
    (startTime, slotLength) <- runQueryInContext ctx (slotToWallclock absSlot)
    pure $
      TimeToSlotInfo
        { ttsiSlotToTimeInfo =
            SlotToTimeInfo
              { sttiSlotNo = absSlot
              , sttiSlotStartTime = startTime
              , sttiSlotEndTime = addRelativeTime timeLeftInSlot absTime
              , sttiSlotLength = slotLength
              }
        , ttsiTimeSpentInSlot = timeSpentInSlot
        , ttsiTimeLeftInSlot = timeLeftInSlot
        , ttsiCurrentTime = absTime
        }

-- | Get the 'SlotToTimeInfo' for the initial 'SlotNo'.
resolveSlotToTimeInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError SlotToTimeInfo
resolveSlotToTimeInfo ctx absSlot =
  runQueryInContext ctx (slotToWallclock absSlot) >>= \(startTime, slotLength) ->
    pure $
      SlotToTimeInfo
        { sttiSlotNo = absSlot
        , sttiSlotStartTime = startTime
        , sttiSlotEndTime = addRelativeTime (getSlotLength slotLength) startTime
        , sttiSlotLength = slotLength
        }

-- | Get the 'SlotToEpochInfo' for a given 'SlotNo'.
resolveSlotToEpochInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError SlotToEpochInfo
resolveSlotToEpochInfo ctx absSlot =
  runQueryInContext ctx (slotToEpoch absSlot) >>= \(absEpoch, slotsSpentInEpoch, slotsLeftInEpoch) -> do
    (startSlot, epochSize) <- runQueryInContext ctx (epochToSlot absEpoch)
    pure $
      SlotToEpochInfo
        { steiEpochToSlotInfo =
            EpochToSlotInfo
              { etsiEpochNo = absEpoch
              , etsiEpochStartSlot = startSlot
              , etsiEpochEndSlot = addSlots (unEpochSize epochSize) startSlot
              , etsiEpochSize = epochSize
              }
        , steiCurrentSlot = absSlot
        , steiSlotsSpentInEpoch = slotsSpentInEpoch
        , steiSlotsLeftInEpoch = slotsLeftInEpoch
        }

-- | Get the 'EpochToSlotInfo' for a given 'EpochNo'.
resolveEpochToSlotInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  EpochNo ->
  Either TimeResolutionError EpochToSlotInfo
resolveEpochToSlotInfo ctx absEpoch =
  runQueryInContext ctx (epochToSlot absEpoch) >>= \(startSlot, epochSize) ->
    pure $
      EpochToSlotInfo
        { etsiEpochNo = absEpoch
        , etsiEpochStartSlot = startSlot
        , etsiEpochEndSlot = addSlots (unEpochSize epochSize) startSlot
        , etsiEpochSize = epochSize
        }

-- | Get the 'SlotToPerasRoundInfo' for a given 'SlotNo'.
resolveSlotToPerasRoundInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError SlotToPerasRoundInfo
resolveSlotToPerasRoundInfo ctx absSlot =
  runQueryInContext ctx (slotToPerasRoundNo absSlot)
    >>= absorbNoPerasEnabled
    >>= \(absPerasRoundNo, slotsSpentInPerasRound, slotsLeftInPerasRound) -> do
      (startSlot, roundLength) <-
        runQueryInContext ctx (perasRoundNoToSlot absPerasRoundNo) >>= absorbNoPerasEnabled
      pure $
        SlotToPerasRoundInfo
          { stpriPerasRoundToSlotInfo =
              PerasRoundToSlotInfo
                { prtsiPerasRoundNo = absPerasRoundNo
                , prtsiPerasRoundStartSlot = startSlot
                , prtsiPerasRoundEndSlot = addSlots (unPerasRoundLength roundLength) startSlot
                , prtsiPerasRoundLength = roundLength
                }
          , stpriCurrentSlot = absSlot
          , stpriSlotsSpentInPerasRound = slotsSpentInPerasRound
          , stpriSlotsLeftInPerasRound = slotsLeftInPerasRound
          }

-- | Get the 'PerasRoundToSlotInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToSlotInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  PerasRoundNo ->
  Either TimeResolutionError PerasRoundToSlotInfo
resolvePerasRoundToSlotInfo ctx perasRoundNo =
  runQueryInContext ctx (perasRoundNoToSlot perasRoundNo)
    >>= absorbNoPerasEnabled
    >>= \(startSlot, roundLength) ->
      pure $
        PerasRoundToSlotInfo
          { prtsiPerasRoundNo = perasRoundNo
          , prtsiPerasRoundStartSlot = startSlot
          , prtsiPerasRoundEndSlot = addSlots (unPerasRoundLength roundLength) startSlot
          , prtsiPerasRoundLength = roundLength
          }

-- | Get the 'EpochToPerasRoundInfo' for a given 'EpochNo'.
resolveEpochToPerasRoundInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  EpochNo ->
  Either TimeResolutionError EpochToPerasRoundInfo
resolveEpochToPerasRoundInfo ctx absEpoch =
  runQueryInContext ctx (epochToSlot absEpoch) >>= \(epochStartSlot, epochSize) -> do
    let epochEndSlot = addSlots (unEpochSize epochSize) epochStartSlot
    (epochStartPerasRound, _, _) <-
      runQueryInContext ctx (slotToPerasRoundNo epochStartSlot) >>= absorbNoPerasEnabled
    (epochEndPerasRound, _, _) <-
      runQueryInContext ctx (slotToPerasRoundNo epochEndSlot) >>= absorbNoPerasEnabled
    pure $
      EpochToPerasRoundInfo
        { etpriEpochNo = absEpoch
        , etpriEpochStartPerasRound = epochStartPerasRound
        , etpriEpochEndPerasRound = epochEndPerasRound
        , etpriEpochSizeInPerasRounds =
            -- TODO: check if EndPerasRound is inclusive or exclusive. If exclusive, we need to add 1 here.
            -- Or maybe we want to do a division of epoch size (in slots) by round size (in slots) instead?
            unPerasRoundNo epochEndPerasRound - unPerasRoundNo epochStartPerasRound
        }

-- | Get the 'PerasRoundToEpochInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToEpochInfo ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  PerasRoundNo ->
  Either TimeResolutionError PerasRoundToEpochInfo
resolvePerasRoundToEpochInfo ctx perasRoundNo =
  runQueryInContext ctx (perasRoundNoToSlot perasRoundNo)
    >>= absorbNoPerasEnabled
    >>= \(roundStartSlot, _) -> do
      (absEpoch, _) <- runQueryInContext ctx (slotToEpoch' roundStartSlot)
      (epochStartSlot, epochSize) <- runQueryInContext ctx (epochToSlot absEpoch)
      let epochEndSlot = addSlots (unEpochSize epochSize) epochStartSlot
      (epochStartPerasRound, _, _) <-
        runQueryInContext ctx (slotToPerasRoundNo epochStartSlot) >>= absorbNoPerasEnabled
      (epochEndPerasRound, _, _) <-
        runQueryInContext ctx (slotToPerasRoundNo epochEndSlot) >>= absorbNoPerasEnabled
      pure $
        PerasRoundToEpochInfo
          { prteiEpochToPerasRoundInfo =
              EpochToPerasRoundInfo
                { etpriEpochNo = absEpoch
                , etpriEpochStartPerasRound = epochStartPerasRound
                , etpriEpochEndPerasRound = epochEndPerasRound
                , etpriEpochSizeInPerasRounds =
                    -- TODO: check if EndPerasRound is inclusive or exclusive. If exclusive, we need to add 1 here.
                    -- Or maybe we want to do a division of epoch size (in slots) by round size (in slots) instead?
                    unPerasRoundNo epochEndPerasRound - unPerasRoundNo epochStartPerasRound
                }
          , prteiCurrentPerasRound = perasRoundNo
          , prteiPerasRoundsSpentInEpoch =
              unPerasRoundNo perasRoundNo - unPerasRoundNo epochStartPerasRound
          , prteiPerasRoundsLeftInEpoch =
              unPerasRoundNo epochEndPerasRound - unPerasRoundNo perasRoundNo
          }

-- -------------------------------------------------------------------------------
-- Time resolution through a context handle
-- -------------------------------------------------------------------------------

-- | Run a pure context resolver against a 'TimeResolutionContextHandle', reading
-- the current context from the handle and throwing any 'TimeResolutionError' in
-- 'STM'.
withHandle ::
  (MonadSTM m, MonadThrow (STM m)) =>
  (TimeResolutionContext blk -> a -> Either TimeResolutionError b) ->
  TimeResolutionContextHandle m blk ->
  a ->
  STM m b
withHandle f (TimeResolutionContextHandle getContext) x = do
  ctx <- getContext
  either throwSTM pure (f ctx x)

-- | Get the 'TimeToSlotInfo' for a given 'RelativeTime'.
resolveTimeToSlotInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  RelativeTime ->
  STM m TimeToSlotInfo
resolveTimeToSlotInfoWithHandle = withHandle resolveTimeToSlotInfo

-- | Get the 'SlotToTimeInfo' for the initial 'SlotNo'.
resolveSlotToTimeInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m SlotToTimeInfo
resolveSlotToTimeInfoWithHandle = withHandle resolveSlotToTimeInfo

-- | Get the 'SlotToEpochInfo' for a given 'SlotNo'.
resolveSlotToEpochInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m SlotToEpochInfo
resolveSlotToEpochInfoWithHandle = withHandle resolveSlotToEpochInfo

-- | Get the 'EpochToSlotInfo' for a given 'EpochNo'.
resolveEpochToSlotInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  EpochNo ->
  STM m EpochToSlotInfo
resolveEpochToSlotInfoWithHandle = withHandle resolveEpochToSlotInfo

-- | Get the 'SlotToPerasRoundInfo' for a given 'SlotNo'.
resolveSlotToPerasRoundInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m SlotToPerasRoundInfo
resolveSlotToPerasRoundInfoWithHandle = withHandle resolveSlotToPerasRoundInfo

-- | Get the 'PerasRoundToSlotInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToSlotInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  PerasRoundNo ->
  STM m PerasRoundToSlotInfo
resolvePerasRoundToSlotInfoWithHandle = withHandle resolvePerasRoundToSlotInfo

-- | Get the 'EpochToPerasRoundInfo' for a given 'EpochNo'.
resolveEpochToPerasRoundInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  EpochNo ->
  STM m EpochToPerasRoundInfo
resolveEpochToPerasRoundInfoWithHandle = withHandle resolveEpochToPerasRoundInfo

-- | Get the 'PerasRoundToEpochInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToEpochInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk) =>
  TimeResolutionContextHandle m blk ->
  PerasRoundNo ->
  STM m PerasRoundToEpochInfo
resolvePerasRoundToEpochInfoWithHandle = withHandle resolvePerasRoundToEpochInfo
