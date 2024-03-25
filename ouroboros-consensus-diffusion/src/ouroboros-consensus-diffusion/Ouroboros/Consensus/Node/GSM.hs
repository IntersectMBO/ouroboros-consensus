{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | The Genesis State Machine decides whether the node is caught-up or not.
module Ouroboros.Consensus.Node.GSM (
    CandidateVersusSelection (..)
  , DurationFromNow (..)
  , GsmEntryPoints (..)
  , GsmNodeKernelArgs (..)
  , GsmView (..)
  , MarkerFileView (..)
  , WrapDurationUntilTooOld (..)
    -- * Auxiliaries
  , TraceGsmEvent (..)
  , initializationLedgerJudgement
    -- * Constructors
  , realDurationUntilTooOld
  , realGsmEntryPoints
  , realMarkerFileView
  ) where

import qualified Cardano.Slotting.Slot as Slot
import qualified Control.Concurrent.Class.MonadSTM.TVar as LazySTM
import           Control.Monad (forever, join, unless)
import           Control.Monad.Class.MonadSTM (MonadSTM, STM, atomically, check)
import           Control.Monad.Class.MonadThrow (MonadThrow)
import           Control.Monad.Class.MonadTimer (threadDelay)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import           Control.Tracer (Tracer, traceWith)
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Time (NominalDiffTime)
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Clock
import qualified Ouroboros.Consensus.HardFork.Abstract as HardFork
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Ledger.Basics as L
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import           Ouroboros.Consensus.Util.NormalForm.StrictTVar (StrictTVar)
import qualified Ouroboros.Consensus.Util.NormalForm.StrictTVar as StrictSTM
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (LedgerStateJudgement (..))
import           System.FS.API (HasFS, createDirectoryIfMissing, doesFileExist,
                     removeFile, withFile)
import           System.FS.API.Types (AllowExisting (..), FsPath, OpenMode (..),
                     mkFsPath)
import           System.Random (StdGen, uniformR)
import Data.Semigroup (All(..))

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

data DurationFromNow =
    After !NominalDiffTime
    -- ^ INVARIANT positive
  |
    Already
    -- ^ This value represents all non-positive durations, ie events from the
    -- past
  deriving (Eq, Show)

data CandidateVersusSelection =
    CandidateDoesNotIntersect
    -- ^ The GSM assumes that this is ephemeral
    --
    -- For example, the ChainSync client will either disconnect from the peer
    -- or update the candidate to one that is not stale. It's also technically
    -- possible that the selection is stale, which the ChainDB would also
    -- resolve as soon as possible.
  |
    WhetherCandidateIsBetter !Bool
    -- ^ Whether the candidate is better than the selection
  deriving (Eq, Show)

data GsmView m upstreamPeer selection state = GsmView {
    antiThunderingHerd        :: Maybe StdGen
    -- ^ An initial seed used to randomly increase 'minCaughtUpDuration' by up
    -- to 15% every transition from OnlyBootstrap to CaughtUp, in order to
    -- avoid a thundering herd phenemenon.
    --
    -- 'Nothing' should only be used for testing.
  ,
    candidateOverSelection    ::
        selection -> state -> CandidateVersusSelection
  ,
    peerIsIdle                ::
        upstreamPeer -> state -> STM m Bool
  ,
    durationUntilTooOld       :: Maybe (selection -> m DurationFromNow)
    -- ^ How long from now until the selection will be so old that the node
    -- should exit the @CaughtUp@ state
    --
    -- 'Nothing' means the selection can never become too old.
  ,
    equivalent                :: selection -> selection -> Bool
    -- ^ Whether the two selections are equivalent for the purpose of the
    -- Genesis State Machine
  ,
    getChainSyncStates        ::
        STM m (Map.Map upstreamPeer (StrictTVar m state))
    -- ^ The latest candidates from the upstream ChainSync peers
  ,
    getCurrentSelection       :: STM m selection
    -- ^ The node's current selection
  ,
    minCaughtUpDuration       :: NominalDiffTime
    -- ^ How long the node must stay in CaughtUp after transitioning to it from
    -- OnlyBootstrap, regardless of the selection's age. This prevents the
    -- whole network from thrashing between CaughtUp and OnlyBootstrap if
    -- there's an outage in block production.
    --
    -- See also 'antiThunderingHerd'.
  ,
    setCaughtUpPersistentMark :: Bool -> m ()
    -- ^ EG touch/delete the marker file on disk
  ,
    writeLedgerStateJudgement :: LedgerStateJudgement -> m ()
    -- ^ EG update the TVar that the Diffusion Layer monitors
  }

-- | The two proper GSM states for boot strap peers
--
-- See the @BootstrapPeersIER.md@ document for their specification.
--
-- See 'initializationLedgerJudgement' for the @Initializing@ pseudo-state.
data GsmEntryPoints m = GsmEntryPoints {
    enterCaughtUp      :: forall neverTerminates. m neverTerminates
    -- ^ ASSUMPTION the marker file is present on disk, a la
    -- @'setCaughtUpPersistentMark' True@
    --
    -- Thus this can be invoked at node start up after determining the marker
    -- file is present (and the tip is still not stale)
  ,
    enterOnlyBootstrap :: forall neverTerminates. m neverTerminates
    -- ^ ASSUMPTION the marker file is absent on disk, a la
    -- @'setCaughtUpPersistentMark' False@
    --
    -- Thus this can be invoked at node start up after determining the marker
    -- file is absent.
  }

-----

-- | Determine the initial 'LedgerStateJudgment'
--
-- Also initializes the persistent marker file.
initializationLedgerJudgement ::
     ( L.GetTip (L.LedgerState blk)
     , Monad m
     )
  => m (L.LedgerState blk)
  -> Maybe (WrapDurationUntilTooOld m blk)
     -- ^ 'Nothing' if @blk@ has no age limit
  -> MarkerFileView m
  -> m LedgerStateJudgement
initializationLedgerJudgement
    getCurrentLedger
    mbDurationUntilTooOld
    markerFileView
  = do
    wasCaughtUp <- hasMarkerFile markerFileView
    if not wasCaughtUp then pure TooOld else do
        case mbDurationUntilTooOld of
            Nothing -> return YoungEnough
            Just wd -> do
                sno <- L.getTipSlot <$> getCurrentLedger
                getDurationUntilTooOld wd sno >>= \case
                    After{}     -> return YoungEnough
                    Already     -> do
                        removeMarkerFile markerFileView
                        return TooOld

{-------------------------------------------------------------------------------
  A real implementation
-------------------------------------------------------------------------------}

-- | The actual GSM logic for boot strap peers
--
-- See the @BootstrapPeersIER.md@ document for the specification of this logic.
realGsmEntryPoints :: forall m upstreamPeer selection tracedSelection candidate.
     ( SI.MonadDelay m
     , SI.MonadTimer m
     )
  => (selection -> tracedSelection, Tracer m (TraceGsmEvent tracedSelection))
  -> GsmView m upstreamPeer selection candidate
  -> GsmEntryPoints m
realGsmEntryPoints tracerArgs gsmView = GsmEntryPoints {
    enterCaughtUp
  ,
    enterOnlyBootstrap
  }
  where
    (cnvSelection, tracer) = tracerArgs

    GsmView {
        antiThunderingHerd
      ,
        candidateOverSelection
      ,
        peerIsIdle
      ,
        durationUntilTooOld
      ,
        equivalent
      ,
        getChainSyncStates
      ,
        getCurrentSelection
      ,
        minCaughtUpDuration
      ,
        setCaughtUpPersistentMark
      ,
        writeLedgerStateJudgement
      } = gsmView

    enterCaughtUp :: forall neverTerminates. m neverTerminates
    enterCaughtUp  = enterCaughtUp' antiThunderingHerd

    enterOnlyBootstrap :: forall neverTerminates. m neverTerminates
    enterOnlyBootstrap  = enterOnlyBootstrap' antiThunderingHerd

    enterCaughtUp' :: forall neverTerminates. Maybe StdGen -> m neverTerminates
    enterCaughtUp' g = do
        (g', ev) <- blockWhileCaughtUp g

        setCaughtUpPersistentMark False
        writeLedgerStateJudgement TooOld
        traceWith tracer ev

        enterOnlyBootstrap' g'

    enterOnlyBootstrap' :: Maybe StdGen -> forall neverTerminates. m neverTerminates
    enterOnlyBootstrap' g = do
        ev <- blockUntilCaughtUp

        writeLedgerStateJudgement YoungEnough
        setCaughtUpPersistentMark True
        traceWith tracer ev

        -- When transitioning from OnlyBootstrap to CaughtUp, the node will
        -- remain in CaughtUp for at least 'minCaughtUpDuration', regardless of
        -- the selection's age.
        SI.threadDelay $ realToFrac minCaughtUpDuration

        enterCaughtUp' g

    blockWhileCaughtUp ::
         Maybe StdGen
      -> m (Maybe StdGen, TraceGsmEvent tracedSelection)
    blockWhileCaughtUp g = do
        -- Randomly add up to 5min.
        --
        -- Under the ideal circumstances, nodes have perfectly synchronized
        -- clocks. However, if there's a block production outage, that means
        -- /all/ nodes will switch back to the bootstrap peers
        -- /simultaneously/, incurring a thundering herd of requests on that
        -- relatively small population. This random change will spread that
        -- load out.
        --
        -- TODO should the Diffusion Layer do this? IE the node /promptly/
        -- switches to OnlyBootstrap, but then the Diffusion Layer introces a
        -- delay before reaching out to the bootstrap peers?
        let (bonus, g') = case g of
                Nothing -> (0, Nothing)   -- it's disabled in some tests
                Just x  ->
                    let (seconds, !g'') =
                            uniformR (0, 300 :: Int) x
                    in
                    (fromIntegral seconds, Just g'')

        ev <- atomically getCurrentSelection >>= blockWhileCaughtUpHelper bonus

        pure (g', ev)

    blockWhileCaughtUpHelper ::
         SI.DiffTime
      -> selection
      -> m (TraceGsmEvent tracedSelection)
    blockWhileCaughtUpHelper bonus selection = do
        let tracedSelection = cnvSelection selection

            computeDuration :: m (Maybe DurationFromNow)
            computeDuration = mapM ($ selection) durationUntilTooOld
        computeDuration >>= \case
            Nothing          -> forever $ threadDelay maxBound
            Just Already     -> do   -- it's already too old
                pure $ GsmEventLeaveCaughtUp tracedSelection Already
            Just (After dur) -> do
                varTimeoutExpired <- SI.registerDelay (realToFrac dur + bonus)

                -- If the selection changes before the timeout expires, loop to
                -- setup a new timeout for the new tip.
                --
                -- Otherwise the timeout expired before the selection changed
                -- (or they both happened after the previous attempt of this
                -- STM transaction), so the node is no longer in @CaughtUp@.
                join $ atomically $ do
                    expired <- LazySTM.readTVar varTimeoutExpired
                    let ev = GsmEventLeaveCaughtUp tracedSelection (After dur)
                    if expired then pure (pure ev) else do
                        selection' <- getCurrentSelection
                        check $ not $ equivalent selection selection'
                        pure $ blockWhileCaughtUpHelper bonus selection'

    blockUntilCaughtUp :: m (TraceGsmEvent tracedSelection)
    blockUntilCaughtUp = atomically $ do
        -- STAGE 1: all ChainSync clients report no subsequent headers
        varsState <- getChainSyncStates
        states    <- traverse StrictSTM.readTVar varsState
        allIdle   <- foldMap All <$> traverse (uncurry peerIsIdle) (Map.toList states)
        check $
             not (Map.null states)
          && getAll allIdle

        -- STAGE 2: no candidate is better than the node's current
        -- selection
        --
        -- For the Bootstrap State Machine, it's fine to completely ignore
        -- block diffusion pipelining here, because all bootstrap peers will
        -- /promptly/ rollback the tentative header if its block body turns out
        -- to be invalid (aka /trap header/). Thus the node will stay in
        -- CaughtUp slighty longer, until the system is no longer pipelining a
        -- block; general Praos reasoning ensures that won't take particularly
        -- long.
        selection  <- getCurrentSelection
        candidates <- traverse StrictSTM.readTVar varsState
        let ok candidate =
                WhetherCandidateIsBetter False
             == candidateOverSelection selection candidate
        check $ all ok candidates

        pure $ GsmEventEnterCaughtUp
            (Map.size states)
            (cnvSelection selection)

        -- STAGE 3: the previous stages weren't so slow that the idler
        -- set/candidate set/individual candidates changed
        --
        -- At this point, the STM scheduler will automatically retry this
        -- transaction if and only if any of the TVars are no longer
        -- pointer-equal to what was read above. That outcome is unlikely as
        -- long as there are not a huge number of peers; as Simon Marlow wrote,
        -- "Never read an unbounded number of TVars in a single transaction
        -- because the O(n) performance of readTVar then gives O(n*n) for the
        -- whole transaction."
        --
        -- (NSF: I peeked at ghc/rts/STM.c today. The thing being counted by
        -- the O(n*n) notation in the quote above is iterations of a C for loop
        -- that reads a C array. The transaction log is a linked list of
        -- chunks, each a 16 element array. So the 4 node kernel tvars + one
        -- tvar for each of the first 12 peers fill up the first chunk, and
        -- then there's a new chunk for each group of 16 peers beyond that. For
        -- example, 44 peers would exactly fill 3 chunks. Thus, each readTVar
        -- pages in at most 4 VM pages for the number of peers we're
        -- anticipating. And then the STM validation at the end touches them
        -- all one last time. Summary: seems likely to be fast enough.)

data TraceGsmEvent selection =
    GsmEventEnterCaughtUp !Int !selection
    -- ^ how many peers and the current selection
  |
    GsmEventLeaveCaughtUp !selection !DurationFromNow
    -- ^ the current selection and its age
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  A helper for constructing a real 'GsmView'
-------------------------------------------------------------------------------}

newtype WrapDurationUntilTooOld m blk = DurationUntilTooOld {
    getDurationUntilTooOld :: Slot.WithOrigin Slot.SlotNo -> m DurationFromNow
  }

-- | The real system's 'durationUntilTooOld'
realDurationUntilTooOld ::
     ( HardFork.HasHardForkHistory blk
     , MonadSTM m
     )
  => L.LedgerConfig blk
  -> STM m (L.LedgerState blk)
  -> NominalDiffTime
     -- ^ If the volatile tip is older than this, then the node will exit the
     -- @CaughtUp@ state.
     --
     -- Eg 'Ouroboros.Consensus.Node.llrnMaxCaughtUpAge'
     --
     -- WARNING This function returns 'Already' if the wall clock is beyond the
     -- current ledger state's translation horizon; that may be confusing if an
     -- unexpectedly large 'NominalDiffTime' is given here (eg 1 one week).
  -> Clock.SystemTime m
  -> m (WrapDurationUntilTooOld m blk)
realDurationUntilTooOld lcfg getLedgerState maxCaughtUpAge systemTime = do
    runner <-
        HardFork.runWithCachedSummary
      $ HardFork.hardForkSummary lcfg <$> getLedgerState
    pure $ DurationUntilTooOld $ \woSlot -> do
        now <- Clock.systemTimeCurrent systemTime
        case woSlot of
            Slot.Origin  -> pure $ toDur now $ Clock.RelativeTime 0
            Slot.At slot -> do
                let qry = Qry.slotToWallclock slot
                atomically $ HardFork.cachedRunQuery runner qry <&> \case
                    Left Qry.PastHorizon{}  -> Already
                    Right (onset, _slotLen) -> toDur now onset
  where
    toDur
        (Clock.RelativeTime now)
        (Clock.getRelativeTime -> (+ maxCaughtUpAge) -> limit)
      = if limit <= now then Already else After (limit - now)

{-------------------------------------------------------------------------------
  A helper for constructing a real 'GsmView'

  TODO should these operations properly be part of the ChainDB?
-------------------------------------------------------------------------------}

-- | A view on the GSM's /Caught-Up persistent marker/ file
--
-- These comments constrain the result of 'realMarkerFile'; mock views in
-- testing are free to be different.
data MarkerFileView m = MarkerFileView {
    hasMarkerFile    :: m Bool
  ,
    -- | Remove the marker file
    --
    -- Will throw an 'FsResourceDoesNotExist' error when it does not exist.
    removeMarkerFile :: m ()
  ,
    -- | Create the marker file
    --
    -- Idempotent.
    touchMarkerFile  :: m ()
  }

-- | The real system's 'MarkerFileView'
--
-- The strict 'ChainDB' argument is unused, but its existence ensures there's
-- only one process using this file system.
realMarkerFileView ::
     MonadThrow m
  => ChainDB m blk
  -> HasFS m h
     -- ^ should be independent of other filesystems, eg @gsm/@
  -> MarkerFileView m
realMarkerFileView !_cdb hasFS =
    MarkerFileView {
        hasMarkerFile
      ,
        removeMarkerFile = removeFile hasFS markerFile
      ,
        touchMarkerFile = do
            createDirectoryIfMissing hasFS True (mkFsPath [])
            alreadyExists <- hasMarkerFile
            unless alreadyExists $
                withFile hasFS markerFile (WriteMode MustBeNew) $ \_h ->
                    return ()
      }
  where
    hasMarkerFile = doesFileExist hasFS markerFile

-- | The path to the GSM's /Caught-Up persistent marker/ inside its dedicated
-- 'HasFS'
--
-- If the file is present on node initialization, then the node was in the
-- @CaughtUp@ state when it shut down.
markerFile :: FsPath
markerFile = mkFsPath ["CaughtUpMarker"]

{-------------------------------------------------------------------------------
  A helper for the NodeKernel
-------------------------------------------------------------------------------}

-- | Arguments the NodeKernel has to take because of the GSM
data GsmNodeKernelArgs m blk = GsmNodeKernelArgs {
    gsmAntiThunderingHerd  :: StdGen
    -- ^ See 'antiThunderingHerd'
  ,
    gsmDurationUntilTooOld :: Maybe (WrapDurationUntilTooOld m blk)
    -- ^ See 'durationUntilTooOld'
  ,
    gsmMarkerFileView      :: MarkerFileView m
  ,
    gsmMinCaughtUpDuration :: NominalDiffTime
    -- ^ See 'minCaughtUpDuration'
  }
