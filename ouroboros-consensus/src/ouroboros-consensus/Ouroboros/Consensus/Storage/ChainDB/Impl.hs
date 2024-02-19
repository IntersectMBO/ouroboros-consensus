{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs (..)
  , SerialiseDiskConstraints
  , defaultArgs
  , openDB
  , withDB
    -- * Trace types
  , NewTipInfo (..)
  , TraceAddBlockEvent (..)
  , TraceCopyToImmutableDBEvent (..)
  , TraceEvent (..)
  , TraceFollowerEvent (..)
  , TraceGCEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceIteratorEvent (..)
  , TraceOpenEvent (..)
  , TracePipeliningEvent (..)
  , TraceValidationEvent (..)
    -- * Re-exported for convenience
  , Args.RelativeMountPoint (..)
  , ImmutableDB.ImmutableDbSerialiseConstraints
  , LedgerDB.LedgerDbSerialiseConstraints
  , VolatileDB.VolatileDbSerialiseConstraints
    -- * Internals for testing purposes
  , Internal (..)
  , openDBInternal
  ) where

import           Control.Monad (void, when)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Class (lift)
import           Control.Tracer
import           Data.Functor ((<&>))
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (StrictMaybe (..))
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (ChainDbArgs,
                     defaultArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Follower as Follower
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (WithTempRegistry,
                     allocate, runInnerWithTempRegistry, runWithTempRegistry,
                     withRegistry)
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Consensus.Util.TentativeState
                     (TentativeState (NoLastInvalidTentative))
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

withDB ::
     forall m blk a.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     , MonadBase m m
     )
  => Complete Args.ChainDbArgs m blk
  -> (ChainDB m blk -> m a)
  -> m a
withDB args = bracket (fst <$> openDBInternal args True) API.closeDB

openDB ::
     forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     , MonadBase m m
     )
  => Complete Args.ChainDbArgs m blk
  -> m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal ::
     forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     , HasCallStack
     , MonadBase m m
     )
  => Complete Args.ChainDbArgs m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = runWithTempRegistry $ do
    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningDB
    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningImmutableDB
    immutableDB <- ImmutableDB.openDB argsImmutableDb $ innerOpenCont ImmutableDB.closeDB
    immutableDbTipPoint <- lift $ atomically $ ImmutableDB.getTipPoint immutableDB
    let immutableDbTipChunk =
          chunkIndexOfPoint (ImmutableDB.immChunkInfo argsImmutableDb) immutableDbTipPoint
    lift $ traceWith tracer $
      TraceOpenEvent $
        OpenedImmutableDB immutableDbTipPoint immutableDbTipChunk

    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningVolatileDB
    (volatileDB, maxSlot) <- VolatileDB.openDB argsVolatileDb $ innerOpenCont (VolatileDB.closeDB . fst)
    (chainDB, testing, env) <- lift $ do
      traceWith tracer $ TraceOpenEvent (OpenedVolatileDB maxSlot)
      traceWith tracer $ TraceOpenEvent StartedOpeningLgrDB
      (lgrDB, replayed) <- LedgerDB.openDB
                            argsLgrDb
                            (ImmutableDB.streamAPI immutableDB)
                            immutableDbTipPoint
                            (Query.getAnyKnownBlock immutableDB volatileDB)
      traceWith tracer $ TraceOpenEvent OpenedLgrDB

      varInvalid      <- newTVarIO (WithFingerprint Map.empty (Fingerprint 0))
      varFutureBlocks <- newTVarIO Map.empty

      let initChainSelTracer = TraceInitChainSelEvent >$< tracer

      chain <- withRegistry $ \rr -> do
        traceWith initChainSelTracer StartedInitChainSelection
        chainAndLedger <- ChainSel.initialChainSelection
                            immutableDB
                            volatileDB
                            lgrDB
                            rr
                            initChainSelTracer
                            (Args.cdbsTopLevelConfig cdbSpecificArgs)
                            varInvalid
                            varFutureBlocks
                            (Args.cdbsCheckInFuture cdbSpecificArgs)
        traceWith initChainSelTracer InitalChainSelected

        let chain  = VF.validatedFragment chainAndLedger
            ledger = VF.validatedLedger   chainAndLedger

        atomically $ LedgerDB.forkerCommit ledger
        LedgerDB.forkerClose ledger

        pure chain

      LedgerDB.tryFlush lgrDB

      varChain           <- newTVarIO chain
      varTentativeState  <- newTVarIO NoLastInvalidTentative
      varTentativeHeader <- newTVarIO SNothing
      varIterators       <- newTVarIO Map.empty
      varFollowers       <- newTVarIO Map.empty
      varNextIteratorKey <- newTVarIO (IteratorKey 0)
      varNextFollowerKey <- newTVarIO (FollowerKey   0)
      varCopyLock        <- newSVar  ()
      varKillBgThreads   <- newTVarIO $ return ()
      blocksToAdd        <- newBlocksToAdd (Args.cdbsBlocksToAddSize cdbSpecificArgs)

      let env = CDB { cdbImmutableDB     = immutableDB
                    , cdbVolatileDB      = volatileDB
                    , cdbLedgerDB        = lgrDB
                    , cdbChain           = varChain
                    , cdbTentativeState  = varTentativeState
                    , cdbTentativeHeader = varTentativeHeader
                    , cdbIterators       = varIterators
                    , cdbFollowers       = varFollowers
                    , cdbTopLevelConfig  = Args.cdbsTopLevelConfig cdbSpecificArgs
                    , cdbInvalid         = varInvalid
                    , cdbNextIteratorKey = varNextIteratorKey
                    , cdbNextFollowerKey = varNextFollowerKey
                    , cdbCopyLock        = varCopyLock
                    , cdbTracer          = tracer
                    , cdbRegistry        = Args.cdbsRegistry cdbSpecificArgs
                    , cdbGcDelay         = Args.cdbsGcDelay cdbSpecificArgs
                    , cdbGcInterval      = Args.cdbsGcInterval cdbSpecificArgs
                    , cdbKillBgThreads   = varKillBgThreads
                    , cdbCheckInFuture   = Args.cdbsCheckInFuture cdbSpecificArgs
                    , cdbBlocksToAdd     = blocksToAdd
                    , cdbFutureBlocks    = varFutureBlocks
                    }
      h <- fmap CDBHandle $ newTVarIO $ ChainDbOpen env
      let chainDB = API.ChainDB
            { addBlockAsync            = getEnv2    h ChainSel.addBlockAsync
            , getCurrentChain          = getEnvSTM  h Query.getCurrentChain
            , getTipBlock              = getEnv     h Query.getTipBlock
            , getTipHeader             = getEnv     h Query.getTipHeader
            , getTipPoint              = getEnvSTM  h Query.getTipPoint
            , getBlockComponent        = getEnv2    h Query.getBlockComponent
            , getIsFetched             = getEnvSTM  h Query.getIsFetched
            , getIsValid               = getEnvSTM  h Query.getIsValid
            , getMaxSlotNo             = getEnvSTM  h Query.getMaxSlotNo
            , stream                   = Iterator.stream  h
            , newFollower              = Follower.newFollower h
            , getIsInvalidBlock        = getEnvSTM  h Query.getIsInvalidBlock
            , closeDB                  = closeDB h
            , isOpen                   = isOpen  h
            , getCurrentLedger         = getEnvSTM  h Query.getCurrentLedger
            , getImmutableLedger       = getEnvSTM  h Query.getImmutableLedger
            , getPastLedger            = getEnvSTM1 h Query.getPastLedger
            , getHeaderStateHistory    = getEnvSTM  h Query.getHeaderStateHistory
            , getReadOnlyForkerAtPoint = getEnv2    h Query.getReadOnlyForkerAtPoint
            , getLedgerTablesAtFor     = getEnv2    h Query.getLedgerTablesAtFor
            , getStatistics            = getEnv     h Query.getStatistics
            }
          testing = Internal
            { intCopyToImmutableDB = getEnv  h Background.copyToImmutableDB
            , intGarbageCollect    = getEnv1 h Background.garbageCollect
            , intTryTakeSnapshot   = getEnv  h $ \env' ->
                void $ LedgerDB.tryTakeSnapshot (cdbLedgerDB env') Nothing maxBound
            , intAddBlockRunner    = getEnv  h Background.addBlockRunner
            , intKillBgThreads     = varKillBgThreads
            }

      traceWith tracer $ TraceOpenEvent $ OpenedDB
        (castPoint $ AF.anchorPoint chain)
        (castPoint $ AF.headPoint   chain)

      when launchBgTasks $ Background.launchBgTasks env replayed

      return (chainDB, testing, env)

    _ <- lift $ allocate (Args.cdbsRegistry cdbSpecificArgs) (\_ -> return chainDB) API.closeDB

    return ((chainDB, testing), env)
  where
    tracer = Args.cdbsTracer cdbSpecificArgs
    Args.ChainDbArgs argsImmutableDb argsVolatileDb argsLgrDb cdbSpecificArgs = args

-- | We use 'runInnerWithTempRegistry' for the component databases.
innerOpenCont ::
     (IOLike m, HasCallStack)
  => (innerDB -> m ())
  -> WithTempRegistry st m (innerDB, st)
  -> WithTempRegistry (ChainDbEnv m blk) m innerDB
innerOpenCont closer m =
  runInnerWithTempRegistry
    (fmap (\(innerDB, st) -> (innerDB, st, innerDB)) m)
    ((True <$) . closer)
    (\_env _innerDB -> True)
      -- This check is degenerate because handles in @_env@ and the
      -- @_innerDB@ handle do not support an equality check; all of the
      -- identifying data is only in the handle's closure, not
      -- accessible because of our intentional encapsulation choices.

isOpen :: IOLike m => ChainDbHandle m blk -> STM m Bool
isOpen (CDBHandle varState) = readTVar varState <&> \case
    ChainDbClosed    -> False
    ChainDbOpen _env -> True

closeDB ::
     forall m blk.
     ( IOLike m
     , HasHeader (Header blk)
     , HasCallStack
     )
  => ChainDbHandle m blk -> m ()
closeDB (CDBHandle varState) = do
    traceMarkerIO "Closing ChainDB"
    mbOpenEnv <- atomically $ readTVar varState >>= \case
      -- Idempotent
      ChainDbClosed   -> return Nothing
      ChainDbOpen env -> do
        writeTVar varState ChainDbClosed
        return $ Just env

    -- Only when the ChainDB was open
    whenJust mbOpenEnv $ \cdb@CDB{..} -> do

      Follower.closeAllFollowers cdb
      Iterator.closeAllIterators cdb

      killBgThreads <- atomically $ readTVar cdbKillBgThreads
      killBgThreads

      ImmutableDB.closeDB cdbImmutableDB
      VolatileDB.closeDB cdbVolatileDB
      LedgerDB.closeDB cdbLedgerDB

      chain <- atomically $ readTVar cdbChain

      traceWith cdbTracer $ TraceOpenEvent $ ClosedDB
        (castPoint $ AF.anchorPoint chain)
        (castPoint $ AF.headPoint chain)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'chunkIndexOfSlot' to 'Point'
--
-- Returns 'firstChunkNo' in case of 'GenesisPoint'.
chunkIndexOfPoint :: ImmutableDB.ChunkInfo -> Point blk -> ImmutableDB.ChunkNo
chunkIndexOfPoint chunkInfo = \case
    GenesisPoint      -> ImmutableDB.firstChunkNo
    BlockPoint slot _ -> ImmutableDB.chunkIndexOfSlot chunkInfo slot
