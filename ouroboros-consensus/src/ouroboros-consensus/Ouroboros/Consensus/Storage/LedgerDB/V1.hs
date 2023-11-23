{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | This LedgerDB implementation mainly consists of two components:
--
-- - __The 'DbChangelog'__: in charge of managing the volatile data, i.e.
--     in-memory [ledger states]('LedgerState') and
--     [differences]("Data.Map.Diff.Strict").
--
-- - __The 'BackingStore'__: in charge of storing the
--     'Ouroboros.Consensus.Ledger.Tables.LedgerTable's at a particular
--     point in the chain. There are two implementations, see
--     "Ouroboros.Consensus.Storage.LedgerDB.BackingStore".
--
-- This image depicts the rough structure of a LedgerDB, where the
-- 'BackingStore' is represented as a database and the rest of the data
-- lives in the 'DbChangelog'. Notice how the differences are purposefully
-- located in between ledger states, making clear that they are the
-- difference between the previous and the next ledger state.
--
-- The 'BackingStore' contains the values at the Anchor of the database.
--
-- <<docs/haddocks/ledgerdb-structure.svg>>
--
-- == Consistency between the 'BackingStore' and the 'DbChangelog'
--
-- At every point in time, the sequence of differences in the 'DbChangelog'
-- carried by the 'LedgerDB' has to be
-- [/anchored/]("Ouroboros.Consensus.Storage.LedgerDB.DbChangelog#anchored")
-- at the 'BackingStore'. This is achieved by means of maintaining an
-- internal 'RAWLock' that makes sure that the 'DbChangelog' is only flushed
-- into the 'BackingStore' when no readers are making use of it.
--
-- == Flushing differences
--
-- In order to flush differences, a sufficient number of blocks must have
-- been applied since the last flush, dictated by the 'DiskPolicy', in
-- particular by 'FlushFrequency'. The flush is performed by first splitting
-- the 'DbChangelog', updating its mutable reference inside the LedgerDB
-- state and then pushing the /root measure/ of the immutable diffs into the
-- 'BackingStore' (with a write lock acquired).
--
-- == Snapshots
--
-- See [section below](#g:snapshots).
--
-- == __(image code)__
--
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/"
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/ledgerdb-structure.svg" defaultEnv (tikz ["positioning", "arrows", "shapes.geometric"]) "\\node at (-1,1) {States}; \
-- >>> \ \\node at (1,2) {Anchor}; \
-- >>> \ \\draw (-2.2,-0.2) -- (8.2,-0.2); \
-- >>> \ \\draw[<-] (2,1) -- (3,1); \
-- >>> \ \\draw[<-] (5,1) -- (6,1); \
-- >>> \ \\node[rectangle,draw,minimum width=2cm,minimum height=1cm] at (1,1) {}; \
-- >>> \ \\node[rectangle,draw,minimum width=2cm,minimum height=1cm] at (4,1) {}; \
-- >>> \ \\node[rectangle,draw,minimum width=2cm,minimum height=1cm] at (7,1) {}; \
-- >>> \ \\node[regular polygon, regular polygon sides=3, draw] at (2.5,-1) {}; \
-- >>> \ \\node[regular polygon, regular polygon sides=3, draw] at (5.5,-1) {}; \
-- >>> \ \\node at (-1,-1) {Diffs}; \
-- >>> \ \\node[cylinder, shape border rotate=90,draw,minimum width=1cm,label=below:Backing store] at (1, -1){}; \
-- >>> \ \\draw[<-,dashed] (1.6,-1) -- (2.1,-1); \
-- >>> \ \\draw[<-,dashed] (3,-1) -- (5.2, -1);\
-- >>> \ \\draw[<<-,dotted] (1,0.75) -- (1,-0.75);"
-- >>> :}
--
module Ouroboros.Consensus.Storage.LedgerDB.V1 (
    -- * LedgerDB API
    module Ouroboros.Consensus.Storage.LedgerDB.API
    -- * Opening a LedgerDB
  , openDB
    -- ** Arguments
  , FlushFrequency (..)
  , LedgerDBArgs (..)
  , QueryBatchSize (..)
  , defaultArgs
    -- * Exposed internals for testing purposes
  , TestInternals (..)
  , openDBInternal
  ) where

import           Codec.CBOR.Decoding
import           Codec.Serialise.Class
import           Control.Monad
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer
import           Data.Foldable
import           Data.Functor.Contravariant
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderStateHistory hiding (current)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog hiding
                     (ExceededRollback, ResolveBlock)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbCh
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Flush
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Init
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS
import           System.FS.API
import           System.FS.API.Types

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | Arguments required to initialize a LedgerDB.
data LedgerDBArgs f m blk = LedgerDBArgs {
      lgrDiskPolicy           :: DiskPolicy
    , lgrGenesis              :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrHasFS                :: SomeHasFS m
    , lgrTopLevelConfig       :: HKD f (TopLevelConfig blk)
    , lgrTracer               :: Tracer m (TraceLedgerDBEvent blk)
    , lgrBsTracer             :: Tracer m BackingStoreTraceByBackend
    , lgrBackingStoreSelector :: !(BackingStoreSelector m)
    , lgrFlushFrequency       :: FlushFrequency
    , lgrQueryBatchSize       :: QueryBatchSize
    }

-- | Default arguments
defaultArgs ::
     Applicative m
  => SomeHasFS m
  -> DiskPolicy
  -> BackingStoreSelector m
  -> FlushFrequency
  -> QueryBatchSize
  -> LedgerDBArgs Defaults m blk
defaultArgs lgrHasFS diskPolicy bss flushFreq qbatchSize = LedgerDBArgs {
      lgrDiskPolicy           = diskPolicy
    , lgrGenesis              = NoDefault
    , lgrHasFS
    , lgrTopLevelConfig       = NoDefault
    , lgrTracer               = nullTracer
    , lgrBsTracer             = nullTracer
    , lgrBackingStoreSelector = bss
    , lgrFlushFrequency       = flushFreq
    , lgrQueryBatchSize       = qbatchSize
    }

-- | The number of diffs in the immutable part of the chain that we have to see
-- before we flush the ledger state to disk. See 'onDiskShouldFlush'.
--
-- INVARIANT: Should be at least 0.
data FlushFrequency =
  -- | A default value, which is determined by a specific 'DiskPolicy'. See
    -- 'defaultDiskPolicy' as an example.
    DefaultFlushFrequency
    -- | A requested value: the number of diffs in the immutable part of the
    -- chain required before flushing.
  | RequestedFlushFrequency Word64
  deriving (Show, Eq, Generic)

defaultShouldFlush :: FlushFrequency -> (Word64 -> Bool)
defaultShouldFlush requestedFlushFrequency = case requestedFlushFrequency of
      RequestedFlushFrequency value -> (>= value)
      DefaultFlushFrequency         -> (>= 100)

-- | The /maximum/ number of keys to read in a backing store range query.
--
-- When performing a ledger state query that involves on-disk parts of the
-- ledger state, we might have to read ranges of key-value pair data (e.g.,
-- UTxO) from disk using backing store range queries. Instead of reading all
-- data in one go, we read it in batches. 'QueryBatchSize' determines the size
-- of these batches.
--
-- INVARIANT: Should be at least 1.
--
-- It is fine if the result of a range read contains less than this number of
-- keys, but it should never return more.
data QueryBatchSize =
    -- | A default value, which is determined by a specific 'DiskPolicy'. See
    -- 'defaultDiskPolicy' as an example.
    DefaultQueryBatchSize
    -- | A requested value: the number of keys to read from disk in each batch.
  | RequestedQueryBatchSize Word64
  deriving (Show, Eq, Generic)

defaultQueryBatchSize :: QueryBatchSize -> Word64
defaultQueryBatchSize requestedQueryBatchSize = case requestedQueryBatchSize of
    RequestedQueryBatchSize value -> value
    DefaultQueryBatchSize         -> 100_000

{-------------------------------------------------------------------------------
  Opening a LedgerDB
-------------------------------------------------------------------------------}

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks that
-- were replayed.
openDB :: forall m l blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LedgerDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
          , l ~ ExtLedgerState blk
          )
       => LedgerDBArgs Identity m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m BackingStoreTraceByBackend
       -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmutableDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (RealPoint blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> m (LedgerDB m l blk, Word64)
openDB args bsTracer replayTracer immutableDB getBlock =
    f <$> openDBInternal args bsTracer replayTracer immutableDB getBlock
  where f (ldb, replayCounter, _) = (ldb, replayCounter)

-- | Open the ledger DB and expose internals for testing purposes
openDBInternal :: forall m l blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LedgerDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
          , l ~ ExtLedgerState blk
          )
       => LedgerDBArgs Identity m blk
       -> Tracer m BackingStoreTraceByBackend
       -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
       -> ImmutableDB m blk
       -> (RealPoint blk -> m blk)
       -> m (LedgerDB m l blk, Word64, TestInternals m l blk)
openDBInternal args@LedgerDBArgs { lgrHasFS = SomeHasFS fs } bsTracer replayTracer immutableDB getBlock = do
    createDirectoryIfMissing fs True (mkFsPath [])
    (_initLog, db, replayCounter, lgrBackingStore) <-
          initialize
            bsTracer
            replayTracer
            lgrTracer
            lgrHasFS
            decodeExtLedgerState'
            decode
            (configDbChangelog lgrTopLevelConfig)
            (defaultShouldFlush lgrFlushFrequency)
            lgrGenesis
            (streamAPI immutableDB)
            lgrBackingStoreSelector
    -- When initializing the ledger DB from disk we:
    --
    -- - Look for the newest valid snapshot, say 'Lbs', which corresponds to the
    --   application of a block in the immutable DB, say 'b'.
    --
    -- - Push onto the ledger DB all the ledger states that result from applying
    --   blocks found in the on-disk immutable DB, starting from the successor
    --   of 'b'.
    --
    -- The anchor of 'LedgerDB' must be the oldest point we can rollback to. So
    -- if we follow the procedure described above (that 'initFromDisk'
    -- implements), the newest ledger state in 'db', say 'Lbn' corresponds to
    -- the most recent block in the immutable DB. If this block is in the
    -- immutable DB, it means that at some point it was part of a chain that was
    -- >k blocks long. Thus 'Lbn' is the oldest point we can roll back to.
    -- Therefore, we need to make the newest state (current) of the ledger DB
    -- the anchor.
    let dbPrunedToImmDBTip = onChangelog pruneToImmTipOnly db
    (varDB, prevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    flushLock <- mkLedgerDBLock
    let env = LedgerDBEnv {
                 ldbChangelog      = varDB
               , ldbBackingStore   = lgrBackingStore
               , ldbLock           = flushLock
               , ldbPrevApplied    = prevApplied
               , ldbDiskPolicy     = lgrDiskPolicy
               , ldbTracer         = lgrTracer
               , ldbCfg            = lgrTopLevelConfig
               , ldbHasFS          = lgrHasFS
               , ldbShouldFlush    = defaultShouldFlush lgrFlushFrequency
               , ldbQueryBatchSize = defaultQueryBatchSize lgrQueryBatchSize
               , ldbResolveBlock   = getBlock
               , ldbSecParam       = configSecurityParam lgrTopLevelConfig
               , ldbBsTracer       = lgrBsTracer
               }
    h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
    return (mkLedgerDB h, replayCounter, TestInternals)

  where
    LedgerDBArgs {
        lgrHasFS
      , lgrTracer
      , lgrDiskPolicy
      , lgrTopLevelConfig
      , lgrGenesis
      , lgrBackingStoreSelector
      , lgrFlushFrequency
      , lgrQueryBatchSize
      , lgrBsTracer
      } = args

    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk EmptyMK)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

{-------------------------------------------------------------------------------
  Implementing the LedgerDB API
-------------------------------------------------------------------------------}

mkLedgerDB ::
     ( IOLike m
     , HasCallStack
     , IsLedger l
     , StandardHash l, HasLedgerTables l
     , HeaderHash l ~ HeaderHash blk
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> LedgerDB m l blk
mkLedgerDB h = LedgerDB {
      getVolatileTip         = getEnvSTM  h getVolatileTip'
    , getImmutableTip        = getEnvSTM  h getImmutableTip'
    , getPastLedgerState     = getEnvSTM1 h getPastLedgerState'
    , getHeaderStateHistory  = getEnvSTM  h getHeaderStateHistory'
    , getForkerAtFromTip     = getEnv1 h getForkHandleAtFromTip'
    , getForker              = getEnv2 h getForkHandle'
    , getPrevApplied         = getEnvSTM  h getPrevApplied'
    , addPrevApplied         = getEnvSTM1 h addPrevApplied'
    , garbageCollect         = getEnvSTM1 h garbageCollect'
    , getResolveBlock        = getEnvSTM h getResolveBlock'
    , tryTakeSnapshot        = getEnv2 h tryTakeSnapshot'
    , tryFlush               = getEnv h tryFlush'
    }

getVolatileTip' ::
     (MonadSTM m, GetTip l)
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
getVolatileTip' = fmap (current . anchorlessChangelog) . readTVar . ldbChangelog

getImmutableTip' ::
     MonadSTM m
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
getImmutableTip' = fmap (anchor . anchorlessChangelog) . readTVar . ldbChangelog

getPastLedgerState' ::
     ( MonadSTM m , HasHeader blk, IsLedger l, StandardHash l
     , HasLedgerTables l, HeaderHash l ~ HeaderHash blk )
  => LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
getPastLedgerState' env point = getPastLedgerAt point . anchorlessChangelog <$> readTVar (ldbChangelog env)

getHeaderStateHistory' ::
     (MonadSTM m, l ~ ExtLedgerState blk)
  => LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
getHeaderStateHistory' env = toHeaderStateHistory . adcStates . anchorlessChangelog <$> readTVar (ldbChangelog env)
  where
    toHeaderStateHistory ::
         AnchoredSeq (WithOrigin SlotNo) (ExtLedgerState blk EmptyMK) (ExtLedgerState blk EmptyMK)
      -> HeaderStateHistory blk
    toHeaderStateHistory =
          HeaderStateHistory
        . AS.bimap headerState headerState

-- | Given a point (or @Left ()@ for the tip), acquire both a value handle and a
-- db changelog at the requested point. Holds a read lock while doing so.
getForkHandle' ::
     forall m l blk a b. (
       HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk
  -> StaticEither b () (Point blk)
  -> STM m a
     -- ^ STM operation that we want to run in the same atomic block as the
     -- acquisition of the LedgerDB
  -> m ( a
        , StaticEither b
          (Forker m l blk)
          (Either (Point blk) (Forker m l blk))
        )
getForkHandle' env pt stmAct =
    withReadLock lock $ do
    (a, ldb') <- atomically $ do
      (,) <$> stmAct <*> (anchorlessChangelog <$> readTVar dbvar)
    (a,) <$> case pt of
      StaticLeft () -> StaticLeft <$> acquire ldb'
      StaticRight actualPoint -> StaticRight <$>
        case rollback actualPoint ldb' of
          Nothing    -> pure $ Left $ castPoint $ getTip $ anchor ldb'
          Just ldb'' -> Right <$> acquire ldb''
 where
    dbvar = ldbChangelog env
    lock = ldbLock env
    bs = ldbBackingStore env

    acquire ::
         AnchorlessDbChangelog l
      -> m (Forker m l blk)
    acquire l = do
      vh <- bsValueHandle bs
      if bsvhAtSlot vh == adcLastFlushedSlot l
        then newForker env vh l
        else error ("Critical error: Value handles are created at "
                    <> show (bsvhAtSlot vh)
                    <> " while the db changelog is at "
                    <> show (adcLastFlushedSlot l)
                    <> ". There is either a race condition or a logic bug"
                    )

-- | Like 'getForkhandle', buts at the tip.
getForkHandleAtFromTip' ::
     forall m l blk. (IOLike m, GetTip l, HasLedgerTables l, LedgerSupportsProtocol blk, NoThunks (l EmptyMK))
  => LedgerDBEnv m l blk
  -> Word64
  -> m (Either ExceededRollback (Forker m l blk))
getForkHandleAtFromTip' env n =
    withReadLock (ldbLock env) $ do
      clog <- atomically $ anchorlessChangelog <$> readTVar (ldbChangelog env)
      case rollbackN n clog of
        Nothing ->
          return $ Left $ ExceededRollback {
              API.rollbackMaximum   = maxRollback clog
            , API.rollbackRequested = n
            }
        Just clog' -> Right <$> acquire clog'
  where
    acquire ::
         AnchorlessDbChangelog l
      -> m (Forker m l blk)
    acquire l = do
      vh <- bsValueHandle (ldbBackingStore env)
      if bsvhAtSlot vh == adcLastFlushedSlot l
        then newForker env vh l
        else error ("Critical error: Value handles are created at "
                    <> show (bsvhAtSlot vh)
                    <> " while the db changelog is at "
                    <> show (adcLastFlushedSlot l)
                    <> ". There is either a race condition or a logic bug"
                    )

getPrevApplied' :: MonadSTM m => LedgerDBEnv m l blk -> STM m (Set (RealPoint blk))
getPrevApplied' env = readTVar (ldbPrevApplied env)

addPrevApplied' ::
     forall m l blk. (MonadSTM m, StandardHash blk)
  => LedgerDBEnv m l blk -> [RealPoint blk] -> STM m ()
addPrevApplied' env hs0 = modifyTVar (ldbPrevApplied env) (addPoints hs0)
  where
    addPoints :: [RealPoint blk] -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollect' :: MonadSTM m => LedgerDBEnv m l blk -> SlotNo -> STM m ()
garbageCollect' env slotNo = modifyTVar (ldbPrevApplied env) $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

getResolveBlock' :: MonadSTM m => LedgerDBEnv m l blk -> STM m (ResolveBlock m blk)
getResolveBlock' = pure . ldbResolveBlock

tryTakeSnapshot' ::
     ( l ~ ExtLedgerState blk
     , IOLike m, LedgerDbSerialiseConstraints blk, LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk -> Maybe (Time, Time) -> Word64 -> m SnapCounters
tryTakeSnapshot' env mTime nrBlocks =
    if onDiskShouldTakeSnapshot (ldbDiskPolicy env) (uncurry (flip diffTime) <$> mTime) nrBlocks then do
      void $ takeSnapshot
                (ldbChangelog env)
                (ldbLock env)
                (configCodec $ ldbCfg env)
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                (ldbBackingStore env)
      void $ trimSnapshots
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                (ldbDiskPolicy env)
      (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
    else
      pure $ SnapCounters (fst <$> mTime) nrBlocks

--
-- If the DbChangelog in the LedgerDB can flush (based on the DiskPolicy
-- with which this LedgerDB was opened), flush differences to the backing
-- store. Note this acquires a write lock on the backing store.
tryFlush' ::
     (IOLike m, HasLedgerTables l, GetTip l)
  => LedgerDBEnv m l blk -> m ()
tryFlush' env = do
    ldb <- readTVarIO $ ldbChangelog env
    when (ldbShouldFlush env $ DbCh.flushableLength $ anchorlessChangelog ldb)
        (withWriteLock
          (ldbLock env)
          (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
        )
