{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , EraIndexed (eraIndexedToNS)
  , forgetEraIndex

    -- * Smart accessors for time resolution info types
  , ttsiSlotNo
  , ttsiSlotStartTime
  , ttsiSlotEndTime
  , ttsiSlotLength
  , steiEpochNo
  , steiEpochStartSlot
  , steiEpochEndSlot
  , steiEpochSize
  , steiIsFirstSlotOfEpoch
  , stpriPerasRoundNo
  , stpriPerasRoundStartSlot
  , stpriPerasRoundEndSlot
  , stpriPerasRoundLength
  , stpriIsFirstSlotOfPerasRound
  , prteiEpochNo
  , prteiEpochStartPerasRound
  , prteiEpochEndPerasRound
  , prteiEpochSizeInPerasRounds
  , prteiIsFirstPerasRoundOfEpoch

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
import Data.Functor.Product (Product (..))
import Data.SOP (HSequence (..), K (..), Top, (:.:) (Comp))
import Data.SOP.Classes (HCollapse (..), hmap)
import Data.SOP.Constraint (All)
import Data.SOP.Match (matchNS)
import Data.SOP.Strict.NS (NS)
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
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (HardForkIndices, hardForkSummary))
import qualified Ouroboros.Consensus.HardFork.History as HF
import Ouroboros.Consensus.HardFork.History.Qry
  ( PastHorizonException
  , Qry
  , epochToSlot
  , perasRoundNoToSlot
  , runQueryNS
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
  = TimeResolutionErrorPastHorizon !PastHorizonException
  | TimeResolutionPerasNotEnabled
  | TimeResolutionEraMismatch
  deriving (Show, Exception)

-- | Absorb a 'NoPerasEnabled' result into a 'TimeResolutionPerasNotEnabled'
-- error.
absorbNoPerasEnabled ::
  All Top (HardForkIndices blk) =>
  Either TimeResolutionError (EraIndexed blk (HF.PerasEnabled a)) ->
  Either TimeResolutionError (EraIndexed blk a)
absorbNoPerasEnabled res = do
  EraIndexed ns <- res
  fmap EraIndexed $
    hsequence' $
      hmap
        ( \(K pea) -> Comp $ case pea of
            HF.PerasEnabled a -> Right (K a)
            HF.NoPerasEnabled -> Left TimeResolutionPerasNotEnabled
        )
        ns

data SlotToTimeInfo = SlotToTimeInfo
  { sttiSlotNo :: !SlotNo
  , sttiSlotStartTime :: !RelativeTime
  , sttiSlotEndTime :: !RelativeTime
  , sttiSlotLength :: !SlotLength
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data TimeToSlotInfo = TimeToSlotInfo
  { ttsiSlotToTimeInfo :: !SlotToTimeInfo
  , ttsiTimeSpentInSlot :: !NominalDiffTime
  , ttsiTimeLeftInSlot :: !NominalDiffTime
  , ttsiCurrentTime :: !RelativeTime
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
  { etsiEpochNo :: !EpochNo
  , etsiEpochStartSlot :: !SlotNo
  , etsiEpochEndSlot :: !SlotNo
  , etsiEpochSize :: !EpochSize
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data SlotToEpochInfo = SlotToEpochInfo
  { steiEpochToSlotInfo :: !EpochToSlotInfo
  , steiCurrentSlot :: !SlotNo
  , steiSlotsSpentInEpoch :: !Word64
  , steiSlotsLeftInEpoch :: !Word64
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

-- TODO: check with consensus team if that is resilient enough to detect epoch
-- boundaries
steiIsFirstSlotOfEpoch :: SlotToEpochInfo -> Bool
steiIsFirstSlotOfEpoch stei = steiSlotsSpentInEpoch stei == 0

data PerasRoundToSlotInfo = PerasRoundToSlotInfo
  { prtsiPerasRoundNo :: !PerasRoundNo
  , prtsiPerasRoundStartSlot :: !SlotNo
  , prtsiPerasRoundEndSlot :: !SlotNo
  , prtsiPerasRoundLength :: !PerasRoundLength
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data SlotToPerasRoundInfo = SlotToPerasRoundInfo
  { stpriPerasRoundToSlotInfo :: !PerasRoundToSlotInfo
  , stpriCurrentSlot :: !SlotNo
  , stpriSlotsSpentInPerasRound :: !Word64
  , stpriSlotsLeftInPerasRound :: !Word64
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

stpriIsFirstSlotOfPerasRound :: SlotToPerasRoundInfo -> Bool
stpriIsFirstSlotOfPerasRound stpri = stpriSlotsSpentInPerasRound stpri == 0

data EpochToPerasRoundInfo = EpochToPerasRoundInfo
  { etpriEpochNo :: !EpochNo
  , etpriEpochStartPerasRound :: !PerasRoundNo
  , etpriEpochEndPerasRound :: !PerasRoundNo
  , etpriEpochSizeInPerasRounds :: !Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

data PerasRoundToEpochInfo = PerasRoundToEpochInfo
  { prteiEpochToPerasRoundInfo :: !EpochToPerasRoundInfo
  , prteiCurrentPerasRound :: !PerasRoundNo
  , prteiPerasRoundsSpentInEpoch :: !Word64
  , prteiPerasRoundsLeftInEpoch :: !Word64
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

prteiIsFirstPerasRoundOfEpoch :: PerasRoundToEpochInfo -> Bool
prteiIsFirstPerasRoundOfEpoch prtei = prteiPerasRoundsSpentInEpoch prtei == 0

newtype EraIndexed blk a = EraIndexed {eraIndexedToNS :: NS (K a) (HardForkIndices blk)}

bindAndEnsureSameEra ::
  All Top (HardForkIndices blk) =>
  Either TimeResolutionError (EraIndexed blk a) ->
  (a -> Either TimeResolutionError (EraIndexed blk b)) ->
  Either TimeResolutionError (EraIndexed blk b)
m `bindAndEnsureSameEra` f = do
  EraIndexed ns <- m
  fmap EraIndexed $
    hsequence' $
      hmap
        ( \(K a) -> Comp $
            do
              let n = f a
              EraIndexed ns' <- n
              case matchNS ns ns' of
                Left _ -> Left TimeResolutionEraMismatch
                Right nsPair -> pure . K $ hcollapse $ hmap (\(Pair _ (K b)) -> (K b)) nsPair
        )
        ns

forgetEraIndex :: All Top (HardForkIndices blk) => EraIndexed blk a -> a
forgetEraIndex (EraIndexed ns) = hcollapse ns

runQueryInContext ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  Qry a ->
  Either TimeResolutionError (EraIndexed blk a)
runQueryInContext (TimeResolutionContext cfg state) qry =
  bimap TimeResolutionErrorPastHorizon EraIndexed $ runQueryNS qry (hardForkSummary cfg state)

withEraIndexedResult ::
  All Top (HardForkIndices blk) =>
  Either e (EraIndexed blk a) -> (a -> b) -> Either e (EraIndexed blk b)
withEraIndexedResult res f = fmap (\(EraIndexed ns) -> EraIndexed $ hmap (\(K a) -> K (f a)) ns) res

-- | Get the 'TimeToSlotInfo' for a given 'RelativeTime'.
resolveTimeToSlotInfo ::
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  RelativeTime ->
  Either TimeResolutionError (EraIndexed blk TimeToSlotInfo)
resolveTimeToSlotInfo ctx absTime =
  runQueryInContext ctx (wallclockToSlot absTime) `bindAndEnsureSameEra` \(absSlot, timeSpentInSlot, timeLeftInSlot) ->
    runQueryInContext ctx (slotToWallclock absSlot) `withEraIndexedResult` \(startTime, slotLength) ->
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
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError (EraIndexed blk SlotToTimeInfo)
resolveSlotToTimeInfo ctx absSlot =
  runQueryInContext ctx (slotToWallclock absSlot) `withEraIndexedResult` \(startTime, slotLength) ->
    SlotToTimeInfo
      { sttiSlotNo = absSlot
      , sttiSlotStartTime = startTime
      , sttiSlotEndTime = addRelativeTime (getSlotLength slotLength) startTime
      , sttiSlotLength = slotLength
      }

-- | Get the 'SlotToEpochInfo' for a given 'SlotNo'.
resolveSlotToEpochInfo ::
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError (EraIndexed blk SlotToEpochInfo)
resolveSlotToEpochInfo ctx absSlot =
  runQueryInContext ctx (slotToEpoch absSlot) `bindAndEnsureSameEra` \(absEpoch, slotsSpentInEpoch, slotsLeftInEpoch) ->
    runQueryInContext ctx (epochToSlot absEpoch) `withEraIndexedResult` \(startSlot, epochSize) ->
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
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  EpochNo ->
  Either TimeResolutionError (EraIndexed blk EpochToSlotInfo)
resolveEpochToSlotInfo ctx absEpoch =
  runQueryInContext ctx (epochToSlot absEpoch) `withEraIndexedResult` \(startSlot, epochSize) ->
    EpochToSlotInfo
      { etsiEpochNo = absEpoch
      , etsiEpochStartSlot = startSlot
      , etsiEpochEndSlot = addSlots (unEpochSize epochSize) startSlot
      , etsiEpochSize = epochSize
      }

-- | Get the 'SlotToPerasRoundInfo' for a given 'SlotNo'.
resolveSlotToPerasRoundInfo ::
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  SlotNo ->
  Either TimeResolutionError (EraIndexed blk SlotToPerasRoundInfo)
resolveSlotToPerasRoundInfo ctx absSlot =
  absorbNoPerasEnabled (runQueryInContext ctx (slotToPerasRoundNo absSlot))
    `bindAndEnsureSameEra` \(absPerasRoundNo, slotsSpentInPerasRound, slotsLeftInPerasRound) ->
      absorbNoPerasEnabled (runQueryInContext ctx (perasRoundNoToSlot absPerasRoundNo))
        `withEraIndexedResult` \(startSlot, roundLength) ->
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
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  PerasRoundNo ->
  Either TimeResolutionError (EraIndexed blk PerasRoundToSlotInfo)
resolvePerasRoundToSlotInfo ctx perasRoundNo =
  absorbNoPerasEnabled (runQueryInContext ctx (perasRoundNoToSlot perasRoundNo))
    `withEraIndexedResult` \(startSlot, roundLength) ->
      PerasRoundToSlotInfo
        { prtsiPerasRoundNo = perasRoundNo
        , prtsiPerasRoundStartSlot = startSlot
        , prtsiPerasRoundEndSlot = addSlots (unPerasRoundLength roundLength) startSlot
        , prtsiPerasRoundLength = roundLength
        }

-- | Get the 'EpochToPerasRoundInfo' for a given 'EpochNo'.
resolveEpochToPerasRoundInfo ::
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  EpochNo ->
  Either TimeResolutionError (EraIndexed blk EpochToPerasRoundInfo)
resolveEpochToPerasRoundInfo ctx absEpoch =
  runQueryInContext ctx (epochToSlot absEpoch) `bindAndEnsureSameEra` \(epochStartSlot, epochSize) ->
    let epochEndSlot = addSlots (unEpochSize epochSize) epochStartSlot
     in absorbNoPerasEnabled (runQueryInContext ctx (slotToPerasRoundNo epochStartSlot)) `bindAndEnsureSameEra` \(epochStartPerasRound, _, _) ->
          absorbNoPerasEnabled (runQueryInContext ctx (slotToPerasRoundNo epochEndSlot)) `withEraIndexedResult` \(epochEndPerasRound, _, _) ->
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
  (HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContext blk ->
  PerasRoundNo ->
  Either TimeResolutionError (EraIndexed blk PerasRoundToEpochInfo)
resolvePerasRoundToEpochInfo ctx perasRoundNo =
  absorbNoPerasEnabled (runQueryInContext ctx (perasRoundNoToSlot perasRoundNo)) `bindAndEnsureSameEra` \(roundStartSlot, _) ->
    runQueryInContext ctx (slotToEpoch' roundStartSlot) `bindAndEnsureSameEra` \(absEpoch, _) ->
      runQueryInContext ctx (epochToSlot absEpoch) `bindAndEnsureSameEra` \(epochStartSlot, epochSize) ->
        let epochEndSlot = addSlots (unEpochSize epochSize) epochStartSlot
         in absorbNoPerasEnabled (runQueryInContext ctx (slotToPerasRoundNo epochStartSlot)) `bindAndEnsureSameEra` \(epochStartPerasRound, _, _) ->
              absorbNoPerasEnabled (runQueryInContext ctx (slotToPerasRoundNo epochEndSlot)) `withEraIndexedResult` \(epochEndPerasRound, _, _) ->
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
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  RelativeTime ->
  STM m (EraIndexed blk TimeToSlotInfo)
resolveTimeToSlotInfoWithHandle = withHandle resolveTimeToSlotInfo

-- | Get the 'SlotToTimeInfo' for the initial 'SlotNo'.
resolveSlotToTimeInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m (EraIndexed blk SlotToTimeInfo)
resolveSlotToTimeInfoWithHandle = withHandle resolveSlotToTimeInfo

-- | Get the 'SlotToEpochInfo' for a given 'SlotNo'.
resolveSlotToEpochInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m (EraIndexed blk SlotToEpochInfo)
resolveSlotToEpochInfoWithHandle = withHandle resolveSlotToEpochInfo

-- | Get the 'EpochToSlotInfo' for a given 'EpochNo'.
resolveEpochToSlotInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  EpochNo ->
  STM m (EraIndexed blk EpochToSlotInfo)
resolveEpochToSlotInfoWithHandle = withHandle resolveEpochToSlotInfo

-- | Get the 'SlotToPerasRoundInfo' for a given 'SlotNo'.
resolveSlotToPerasRoundInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  SlotNo ->
  STM m (EraIndexed blk SlotToPerasRoundInfo)
resolveSlotToPerasRoundInfoWithHandle = withHandle resolveSlotToPerasRoundInfo

-- | Get the 'PerasRoundToSlotInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToSlotInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  PerasRoundNo ->
  STM m (EraIndexed blk PerasRoundToSlotInfo)
resolvePerasRoundToSlotInfoWithHandle = withHandle resolvePerasRoundToSlotInfo

-- | Get the 'EpochToPerasRoundInfo' for a given 'EpochNo'.
resolveEpochToPerasRoundInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  EpochNo ->
  STM m (EraIndexed blk EpochToPerasRoundInfo)
resolveEpochToPerasRoundInfoWithHandle = withHandle resolveEpochToPerasRoundInfo

-- | Get the 'PerasRoundToEpochInfo' for a given 'PerasRoundNo'.
resolvePerasRoundToEpochInfoWithHandle ::
  (MonadSTM m, MonadThrow (STM m), HasHardForkHistory blk, All Top (HardForkIndices blk)) =>
  TimeResolutionContextHandle m blk ->
  PerasRoundNo ->
  STM m (EraIndexed blk PerasRoundToEpochInfo)
resolvePerasRoundToEpochInfoWithHandle = withHandle resolvePerasRoundToEpochInfo
