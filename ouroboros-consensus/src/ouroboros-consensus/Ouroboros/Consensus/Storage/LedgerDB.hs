{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | The Ledger DB is responsible for the following tasks:
--
-- - __Maintaining the in-memory ledger state at the tip__: Maintaining the
--     ledger state corresponding to the current tip in memory. When we try to
--     extend our chain with a new block fitting onto our tip, the block must
--     first be validated using the right ledger state, i.e., the ledger state
--     corresponding to the tip. The current ledger state is needed for various
--     other purposes.
--
-- - __Maintaining the past \(k\) in-memory ledger states__: we might roll back
--     up to \(k\) blocks when switching to a more preferable fork. Consider the
--     example below:
--
--     <<docs/haddocks/Ouroboros/Consensus/Storage/LedgerDB/1.svg>>
--
--     Our current chain's tip is \(C_2\), but the fork containing blocks
--     \(F_1\), \(F_2\), and \(F_3\) is more preferable. We roll back our chain
--     to the intersection point of the two chains, \(I\), which must be not
--     more than \(k\) blocks back from our current tip. Next, we must validate
--     block \(F_1\) using the ledger state at block \(I\), after which we can
--     validate \(F_2\) using the resulting ledger state, and so on.
--
--     This means that we need access to all ledger states of the past \(k\)
--     blocks, i.e., the ledger states corresponding to the volatile part of the
--     current chain. Note that applying a block to a ledger state is not an
--     invertible operation, so it is not possible to simply /unapply/ \(C_1\)
--     and \(C_2\) to obtain \(I\).
--
--     Access to the last \(k\) ledger states is not only needed for validating
--     candidate chains, but also by the:
--
--     - __Local state query server__: To query any of the past \(k\) ledger
--       states.
--
--     - __Chain sync client__: To validate headers of a chain that intersects
--        with any of the past \(k\) blocks.
--
-- - __Provide 'Ouroboros.Consensus.Ledger.Tables.Basics.LedgerTable's at any of the last \(k\) ledger states__: In order to apply blocks or transactions
--     on top of ledger states, the LedgerDB must be able to provide the
--     appropriate ledger tables at any of those ledger states.
--
-- - __Storing on disk__: To obtain a ledger state for the current tip of the
--     chain, one has to apply /all blocks in the chain/ one-by-one to the
--     initial ledger state. When starting up the system with an on-disk chain
--     containing millions of blocks, all of them would have to be read from
--     disk and applied. This process can take hours, depending on the storage
--     and CPU speed, and is thus too costly to perform on each startup.
--
--     For this reason, a recent snapshot of the ledger state should be
--     periodically written to disk. Upon the next startup, that snapshot can be
--     read and used to restore the current ledger state, as well as the past
--     \(k\) ledger states.
--
-- - __Flushing 'LedgerTable' differences__: The running Consensus has to
--     periodically flush chunks of differences from the 'DbChangelog' to the
--     'BackingStore', so that memory is off-loaded to the backing store, and if
--     the backing store is an on-disk implementation, reduce the memory usage.
--
-- Note that whenever we say /ledger state/ we mean the @'ExtLedgerState' blk
-- mk@ type described in "Ouroboros.Consensus.Ledger.Basics".
--
-- See the implementation description in [Implementation](#g:implementation)
--
-- === __(image code)__
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/Ouroboros/Consensus/Storage/LedgerDB/"
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/Ouroboros/Consensus/Storage/LedgerDB/1.svg" defaultEnv (tikz ["positioning", "arrows"]) "\
-- >>> \ \\draw (0, 0) -- (50pt, 0) coordinate (I);\
-- >>> \  \\draw (I) -- ++(20pt,  20pt) coordinate (C1) -- ++(20pt, 0) coordinate (C2);\
-- >>> \  \\draw (I) -- ++(20pt, -20pt) coordinate (F1) -- ++(20pt, 0) coordinate (F2) -- ++(20pt, 0) coordinate (F3);\
-- >>> \  \\node at (I)  {$\\bullet$};\
-- >>> \  \\node at (C1) {$\\bullet$};\
-- >>> \  \\node at (C2) {$\\bullet$};\
-- >>> \  \\node at (F1) {$\\bullet$};\
-- >>> \  \\node at (F2) {$\\bullet$};\
-- >>> \  \\node at (F3) {$\\bullet$};\
-- >>> \  \\node at (I) [above left] {$I$};\
-- >>> \  \\node at (C1) [above] {$C_1$};\
-- >>> \  \\node at (C2) [above] {$C_2$};\
-- >>> \  \\node at (F1) [below] {$F_1$};\
-- >>> \  \\node at (F2) [below] {$F_2$};\
-- >>> \  \\node at (F3) [below] {$F_3$};\
-- >>> \  \\draw (60pt, 50pt) node {$\\overbrace{\\hspace{60pt}}$};\
-- >>> \  \\draw (60pt, 60pt) node[fill=white] {$k$};\
-- >>> \  \\draw [dashed] (30pt, -40pt) -- (30pt, 45pt);"
-- >>> :}
--
module Ouroboros.Consensus.Storage.LedgerDB (
    -- * The LedgerDB API
    LedgerDB (..)
    -- ** Views
  , LedgerDBView (..)
  , LedgerDBView'
  , closeLedgerDBView
    -- ** Supporting types
  , SnapCounters (..)
  , ValidateResult (..)
    -- ** Constraints for a LedgerDB
  , LedgerDbSerialiseConstraints
    -- * Implementation #implementation#
    --
    -- | The LedgerDB implementation mainly consists of two components:
    --
    -- - __The 'DbChangelog'__: in charge of managing the volatile data, both in-memory
    --     ledger states and differences.
    --
    -- - __The 'BackingStore'__: in charge of storing the 'LedgerTable's at a
    --     particular point in the chain. There are two implementations, see
    --     "Ouroboros.Consensus.Storage.LedgerDB.BackingStore".
    --
    -- == Consistency between the 'BackingStore' and the 'DbChangelog'
    --
    -- At every point in time, the sequence of differences in the 'DbChangelog'
    -- carried by the 'LedgerDB' has to be /anchored/ at the 'BackingStore'. This is
    -- achieved by means of maintaining an internal 'RAWLock' that makes sure that
    -- the 'DbChangelog' is only flushed into the 'BackingStore' when no readers are
    -- making use of it.
    --
    -- == Flushing differences
    --
    -- In order to flush differences, a sufficient number of blocks must have been
    -- applied since the last flush, dictated by the 'DiskPolicy', in particular by
    -- 'FlushFrequency'. The flush is performed by first splitting the
    -- 'DbChangelog', updating its mutable reference inside the LedgerDB state and
    -- then pushing the /root measure/ of the immutable diffs into the
    -- 'BackingStore' (with a write lock acquired).
    --
    -- == Snapshots
    --
    -- See [section below](#g:snapshots).
    --
    -- ** Opening a database
  , BackingStoreSelector (..)
  , InitLog (..)
  , openDB
    -- *** Configuration
  , DiskPolicy (..)
  , FlushFrequency (..)
  , LedgerDBArgs (..)
  , QueryBatchSize (..)
  , SnapshotInterval (..)
  , defaultArgs
  , defaultDiskPolicy
    -- ** LocalStateQuery miniprotocol support
    --
    -- | These functions are to be used when implementing
    -- 'BlockSupportsLedgerQuery'
  , ledgerDBViewRangeRead
  , ledgerDBViewRead
    -- * Tracing
  , BackingStoreTraceByBackend (..)
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceBackingStoreInitEvent (..)
  , TraceLedgerDBEvent (..)
  , TraceReplayEvent (..)
  , TraceSnapshotEvent (..)
  , decorateReplayTracerWithGoal
    -- * 🧪 Testing
  , DiffsToFlush (..)
  , LedgerDBLock (..)
  , LedgerDBState (..)
  , flushIntoBackingStore
  , getLedgerTablesAtFor'
  , initialize
  , mkLedgerDB
  , mkLedgerDBLock
    -- * Snapshots #snapshots#
    --
    -- This whole section is internal and exposed only for the benefit of tests
    -- and tools, no other components in consensus have any knowledge of ledger
    -- snapshots.
    --
    -- $snapshots
  , DiskSnapshot (..)
  , SnapshotFailure (..)
  , decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , encodeSnapshot
  , readSnapshot
  , snapshotToStatePath
  , snapshotToTablesPath
  , takeSnapshot
  , writeSnapshot
  ) where

import           Cardano.Slotting.Slot
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Class
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad (forM, void, when)
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans (lift)
import           Control.Tracer
import           Data.Foldable (foldl')
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, mapMaybe)
import           Data.Monoid (Sum (..))
import           Data.Ord (Down (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP (K (K))
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack hiding (prettyCallStack)
import           NoThunks.Class (OnlyCheckWhnf (..), OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.API as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbCh
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     decodeWithOrigin, readIncremental)
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as Lock
import           Ouroboros.Consensus.Util.Versioned
import           Ouroboros.Network.Block (Point (Point))
import           System.FS.API
import           System.FS.API.Lazy (hPut)
import           Text.Read (readMaybe)
import           Util.CallStack

{-------------------------------------------------------------------------------
  The LedgerDB API
-------------------------------------------------------------------------------}

-- | When validating a sequence of blocks, these are the possible outcomes.
data ValidateResult blk =
    ValidateSuccessful       (AnchorlessDbChangelog' blk)
  | ValidateLedgerError      (AnnLedgerError' blk)
  | ValidateExceededRollBack ExceededRollback

-- | Counters to keep track of when we made the last snapshot.
data SnapCounters = SnapCounters {
    -- | When was the last time we made a snapshot
    prevSnapshotTime      :: !(Maybe Time)
    -- | How many blocks have we processed since the last snapshot
  , ntBlocksSinceLastSnap :: !Word64
  }

-- | A view of the @LedgerDB@ in a particular instant, holding a value handle to
-- consult the backing store if needed.
data LedgerDBView m l = LedgerDBView {
    viewHandle         :: !(LedgerBackingStoreValueHandle m l)
  , viewChangelog      :: !(AnchorlessDbChangelog l)
    -- | See 'onDiskQueryBatchSize'.
  , viewQueryBatchSize :: !Word64
  }

closeLedgerDBView :: LedgerDBView m l -> m ()
closeLedgerDBView LedgerDBView {viewHandle} = bsvhClose viewHandle

type LedgerDBView' m blk = LedgerDBView m (ExtLedgerState blk)

type View m b blk =
  StaticEither b
   (LedgerDBView' m blk)
   (Either
     (Point blk)
     (LedgerDBView' m blk))

{-------------------------------------------------------------------------------
  The LedgerDB API
-------------------------------------------------------------------------------}

-- | The API of the LedgerDB component
data LedgerDB m blk = LedgerDB {
    -- | Set the current DbChangelog in the LedgerDB.
    setCurrent            :: AnchorlessDbChangelog' blk -> STM m ()
    -- | Get the current DbChangelog in the LedgerDB.
  , getCurrent            :: STM m (DbChangelog' blk)
    -- | Get the set of previously succesfully applied blocks.
  , getPrevApplied        :: STM m (Set (RealPoint blk))
    -- | Ask the backing store and DbChangelog to provide a table of values at
    -- the requested point.
  , getLedgerTablesAtFor  ::
         Point blk
      -> LedgerTables (ExtLedgerState blk) KeysMK
      -> m (Either
             (PointNotFound blk)
             (LedgerTables (ExtLedgerState blk) ValuesMK))
    -- | Acquire a ledger db read view at the requested point or at the tip. If
    -- the requested point doesn't exist it will return a @StaticRight (Left
    -- pt)@.
  , acquireLDBReadView   ::
         forall a b.
         StaticEither b () (Point blk)
      -> STM m a
#if __GLASGOW_HASKELL__ >= 902
         -- ^ STM operation that we want to run in the same atomic block as the
         -- acquisition of the LedgerDB
#endif
      -> m (a, View m b blk)
    -- | Apply a list of blocks on top of the given DbChangelog
  , validate              ::
         BackingStoreValueHandle' m blk
      -> AnchorlessDbChangelog' blk
#if __GLASGOW_HASKELL__ >= 902
         -- ^ This function must start validation using this DbChangelog as a
         -- starting point, and not whichever DbChangelog was there in the
         -- internal state when this function was called.
#endif
      -> BlockCache blk
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks to roll back
#endif
      -> (UpdateLedgerDbTraceEvent blk -> m ())
      -> [Header blk]
      -> m (ValidateResult blk)
    -- | Garbage collect references to old blocks that have been previously
    -- applied.
  , garbageCollect        :: SlotNo -> STM m ()
    -- | If the DbChangelog in the LedgerDB can flush (based on the DiskPolicy
    -- with which this LedgerDB was opened), flush differences to the backing
    -- store. Note this acquires a write lock on the backing store.
  , tryFlush              :: m ()

    -- | If the provided arguments indicate so (based on the DiskPolicy with
    -- which this LedgerDB was opened), take a snapshot and delete stale ones.
  , tryTakeSnapshot       ::
         Maybe (Time, Time)
#if __GLASGOW_HASKELL__ >= 902
         -- ^ If a snapshot has been taken already, the time at which it was
         -- taken and the current time.
#endif
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks have been processed since the last snapshot.
#endif
      -> m SnapCounters

    -- | Get statistics from the LedgerDB, in particular the number of entries
    -- in the ledger tables at the given point.
  , getStatistics ::
          forall b.
             StaticEither b ()         (Point blk)
       -> m (StaticEither b Statistics (Either (Point blk) Statistics))

  } deriving NoThunks via OnlyCheckWhnfNamed "LedgerDB" (LedgerDB m blk)

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}
-- | Serialization constraints required by the 'LedgerDB' to be properly
-- instantiated with a @blk@.
type LedgerDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , CanSerializeLedgerTables (LedgerState blk)
  )

{-------------------------------------------------------------------------------
  Opening a database
-------------------------------------------------------------------------------}

-- | Initialization log
--
-- The initialization log records which snapshots from disk were considered,
-- in which order, and why some snapshots were rejected. It is primarily useful
-- for monitoring purposes.
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks that
-- were replayed.
openDB :: forall m blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LedgerDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
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
       -> m (LedgerDB m blk, Word64)
openDB args@LedgerDBArgs { lgrHasFS = SomeHasFS fs } bsTracer replayTracer immutableDB getBlock = do
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
            lgrDiskPolicy
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
    let dbPrunedToImmDBTip = onChangelog DbCh.pruneToImmTipOnly db
    (varDB, prevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    flushLock <- mkLedgerDBLock
    let st = LedgerDBState {
                 ldbChangelog    = varDB
               , varPrevApplied  = prevApplied
               , ldbBackingStore = lgrBackingStore
               , ldbLock         = flushLock
               }
    ledgerDB <- mkLedgerDB st args getBlock
    return (ledgerDB, replayCounter)

  where
    LedgerDBArgs {
        lgrHasFS
      , lgrTracer
      , lgrDiskPolicy
      , lgrTopLevelConfig
      , lgrGenesis
      , lgrBackingStoreSelector
      } = args

    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk EmptyMK)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

-- | Construct a ledger db from the given state
mkLedgerDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , HasCallStack
  )
  => LedgerDBState m blk
  -> LedgerDBArgs Identity m blk
  -> DbCh.ResolveBlock m blk
  -> m (LedgerDB m blk)
mkLedgerDB st args getBlock = do
  h <- fmap LDBHandle $ newTVarIO $ LedgerDBOpen st
  return $ LedgerDB {
      getCurrent = getStateSTM h getCurrent'

    , getPrevApplied = getStateSTM h getPrevApplied'

    , getLedgerTablesAtFor = \pt k -> getState h $ \st' ->
        getLedgerTablesAtFor' pt k (ldbChangelog st') (ldbBackingStore st')

    , acquireLDBReadView = \se stm -> getState h $ \st' ->
        acquireLDBReadView'
          se
          (ldbChangelog st')
          (ldbLock st')
          (ldbBackingStore st')
          policy
          stm

    , garbageCollect = getStateSTM1 h $ \st' ->
        garbageCollectPrevApplied (varPrevApplied st')

    , setCurrent = getStateSTM1 h $ \st' ->
        setCurrent' (ldbChangelog st')

    , tryFlush = getState h $ \st' -> do
        ldb <- atomically $ getCurrent' st'
        when (onDiskShouldFlush policy $ DbCh.flushableLength $ anchorlessChangelog ldb)
           (withWriteLock
              (ldbLock st')
              (flushLedgerDB (ldbChangelog st') (ldbBackingStore st'))
           )

    , validate = \ldbh chg cache rb tr hs -> getState h $ \st' ->
        validate' (varPrevApplied st') getBlock cfg ldbh chg cache rb tr hs

    , tryTakeSnapshot = getState2 h $ \st' mTime nrBlocks ->
        if onDiskShouldTakeSnapshot policy (uncurry (flip diffTime) <$> mTime) nrBlocks then do
          void $ takeSnapshot
                   (ldbChangelog st')
                   (ldbLock st')
                   (configCodec cfg)
                   (LedgerDBSnapshotEvent >$< tracer)
                   fs
                   (ldbBackingStore st')
          void $ trimSnapshots
                   (LedgerDBSnapshotEvent >$< tracer)
                   fs
                   policy
          (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
        else
          pure $ SnapCounters (fst <$> mTime) nrBlocks
    , getStatistics = \se -> getState h $ \st' -> do
        dbv <- snd
           <$> acquireLDBReadView'
                 se
                 (ldbChangelog st')
                 (ldbLock st')
                 (ldbBackingStore st')
                 policy
                 (pure ())
        case se of
          StaticLeft{}  -> fmap StaticLeft . getStatistics' . fromStaticLeft $ dbv
          StaticRight{} ->
            let x :: Either (Point blk) (LedgerDBView' m blk)
                x = fromStaticRight dbv
            in StaticRight <$> either (pure . Left) (fmap Right . getStatistics') x
    }
  where
    LedgerDBArgs {
        lgrTopLevelConfig = cfg
      , lgrHasFS = fs
      , lgrTracer = tracer
      , lgrDiskPolicy = policy
      } = args

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized DB as well as the block reference corresponding to the snapshot
-- we found on disk (the latter primarily for testing/monitoring purposes).
--
-- We do /not/ catch any exceptions thrown during streaming; should any be
-- thrown, it is the responsibility of the 'ChainDB' to catch these
-- and trigger (further) validation. We only discard snapshots if
--
-- * We cannot deserialise them, or
-- * they are /ahead/ of the chain
--
-- It is possible that the Ledger DB will not be able to roll back @k@ blocks
-- after initialization if the chain has been truncated (data corruption).
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initialize ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , CanSerializeLedgerTables (LedgerState blk)
       , HasCallStack
       )
  => Tracer m BackingStoreTraceByBackend
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceLedgerDBEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DbChangelogCfg (ExtLedgerState blk)
  -> DiskPolicy
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk blk
  -> BackingStoreSelector m
  -> m (InitLog blk, DbChangelog' blk, Word64, BackingStore' m blk)
initialize bsTracer
           replayTracer
           tracer
           hasFS
           decLedger
           decHash
           cfg
           policy
           getGenesisLedger
           stream
           bss =
    listSnapshots hasFS >>= tryNewestFirst id
  where
    bsiTrace :: TraceBackingStoreInitEvent
    bsiTrace = case bss of
      InMemoryBackingStore    -> BackingStoreInitialisedInMemory
      LMDBBackingStore limits -> BackingStoreInitialisedLMDB limits

    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m ( InitLog   blk
                        , DbChangelog' blk
                        , Word64
                        , BackingStore' m blk
                        )
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      traceWith replayTracer ReplayFromGenesis
      genesisLedger <- getGenesisLedger
      let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          initDb        = DbCh.empty (forgetLedgerTables genesisLedger)
      backingStore <-
          newBackingStore bsTracer bss hasFS (projectLedgerTables genesisLedger)
      traceWith (BackingStoreInitEvent >$< tracer) bsiTrace
      eDB <- runExceptT $ replayStartingWith
                            replayTracer'
                            cfg
                            policy
                            backingStore
                            stream
                            initDb
      case eDB of
        Left err -> do
          bsClose backingStore
          error $ "Invariant violation: invalid immutable chain " <> show err
        Right (db, replayed) -> do
          return ( acc InitFromGenesis
                 , db
                 , replayed
                 , backingStore
                 )

    tryNewestFirst acc (s:ss) = do
      eExtLedgerSt <- runExceptT $ readSnapshot hasFS decLedger decHash s
      case eExtLedgerSt of
        Left err -> do
          when (diskSnapshotIsTemporary s) $
            deleteSnapshot hasFS s
          traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s . InitFailureRead $ err
          tryNewestFirst (acc . InitFailure s (InitFailureRead err)) ss
        Right extLedgerSt -> do
          let initialPoint =
                  withOrigin (Point Origin) annTipPoint
                . headerStateTip
                . headerState
                $ extLedgerSt
          case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
            Origin        -> do
              -- Delete the snapshot of the Genesis ledger state. It should have
              -- never existed.
              deleteSnapshot hasFS s
              traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ InitFailureGenesis
              tryNewestFirst (acc . InitFailure s InitFailureGenesis) []

            NotOrigin pt -> do
              backingStore <- restoreBackingStore bsTracer bss hasFS (snapshotToTablesPath s)
              traceWith (BackingStoreInitEvent >$< tracer) bsiTrace
              traceWith replayTracer $
                ReplayFromSnapshot s pt (ReplayStart initialPoint)
              let tracer' = decorateReplayTracerWithStart initialPoint replayTracer
                  initDb  = DbCh.empty extLedgerSt
              eDB <- runExceptT $ replayStartingWith
                                    tracer'
                                    cfg
                                    policy
                                    backingStore
                                    stream
                                    initDb
              case eDB of
                Left err -> do
                  traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ err
                  when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
                  bsClose backingStore
                  tryNewestFirst (acc . InitFailure s err) ss
                Right (db, replayed) -> do
                  return (acc (InitFromSnapshot s pt), db, replayed, backingStore)

-- | Replay all blocks in the Immutable database using the 'StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
--
-- NOTE: we do flush differences into the 'BackingStore' as we go, but we don't
-- take snapshots of the in-memory parts.
--
-- TODO: #4402 expose the flushing frequence as a configuration
replayStartingWith ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> DbChangelogCfg (ExtLedgerState blk)
  -> DiskPolicy
  -> BackingStore' m blk
  -> StreamAPI m blk blk
  -> DbChangelog' blk
  -> ExceptT (SnapshotFailure blk) m (DbChangelog' blk, Word64)
replayStartingWith tracer cfg policy backingStore stream initDb = do
    streamAll stream (castPoint (DbCh.tip $ anchorlessChangelog initDb))
        InitFailureTooRecent
        (initDb, 0)
        push
  where
    DiskPolicy { onDiskShouldFlush } = policy

    push :: blk
         -> (DbChangelog' blk, Word64)
         -> m (DbChangelog' blk, Word64)
    push blk (!db, !replayed) = do
        !db' <- onChangelogM (DbCh.applyThenPush cfg (DbCh.ReapplyVal blk) (DbCh.readKeySets backingStore)) db

        -- It's OK to flush without a lock here, since the `LedgerDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        db'' <-
          if onDiskShouldFlush (DbCh.flushableLength $ anchorlessChangelog db')
          then do
            let (toFlush, toKeep) = DbCh.splitForFlushing db'
            mapM_ (flushIntoBackingStore backingStore) toFlush
            pure toKeep
          else pure db'

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (dbChangelogCfg cfg))
                       (ledgerState (DbCh.current $ anchorlessChangelog db))
                       (ledgerState (DbCh.current $ anchorlessChangelog db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Length of time that has to pas after which a snapshot is taken.
data SnapshotInterval =
    DefaultSnapshotInterval
  | RequestedSnapshotInterval DiffTime
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Generic)

-- | On-disk policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data DiskPolicy = DiskPolicy {
      -- | How many snapshots do we want to keep on disk?
      --
      -- A higher number of on-disk snapshots is primarily a safe-guard against
      -- disk corruption: it trades disk space for reliability.
      --
      -- Examples:
      --
      -- * @0@: Delete the snapshot immediately after writing.
      --        Probably not a useful value :-D
      -- * @1@: Delete the previous snapshot immediately after writing the next
      --        Dangerous policy: if for some reason the deletion happens before
      --        the new snapshot is written entirely to disk (we don't @fsync@),
      --        we have no choice but to start at the genesis snapshot on the
      --        next startup.
      -- * @2@: Always keep 2 snapshots around. This means that when we write
      --        the next snapshot, we delete the oldest one, leaving the middle
      --        one available in case of truncation of the write. This is
      --        probably a sane value in most circumstances.
      onDiskNumSnapshots       :: Word

      -- | Should we write a snapshot of the ledger state to disk?
      --
      -- This function is passed two bits of information:
      --
      -- * The time since the last snapshot, or 'NoSnapshotTakenYet' if none was taken yet.
      --   Note that 'NoSnapshotTakenYet' merely means no snapshot had been taking yet
      --   since the node was started; it does not necessarily mean that none
      --   exist on disk.
      --
      -- * The distance in terms of blocks applied to the /oldest/ ledger
      --   snapshot in memory. During normal operation, this is the number of
      --   blocks written to the ImmutableDB since the last snapshot. On
      --   startup, it is computed by counting how many immutable blocks we had
      --   to reapply to get to the chain tip. This is useful, as it allows the
      --   policy to decide to take a snapshot /on node startup/ if a lot of
      --   blocks had to be replayed.
      --
      -- See also 'defaultDiskPolicy'
    , onDiskShouldTakeSnapshot :: Maybe DiffTime -> Word64 -> Bool

      -- | Based on the current length of the diff sequence in the
      -- 'DbChangelog', decide whether we should flush to the 'BackingStore'
      -- (note that we will always keep @k@ states, but we will keep more
      -- differences that have not yet been flushed to the disk).
      --
      -- Flushing means only applying part of the diffs to the backing store, in
      -- particular we /don't/ serialize the ledger state.
    , onDiskShouldFlush        :: Word64 -> Bool

      -- | Size of a batch of lookups used in range queries for ledger tables.
    , onDiskQueryBatchSize     :: Word64
    }
  deriving NoThunks via OnlyCheckWhnf DiskPolicy

-- | Default on-disk policy suitable to use with cardano-node
--
defaultDiskPolicy ::
     SecurityParam
  -> SnapshotInterval
  -> FlushFrequency
  -> QueryBatchSize
  -> DiskPolicy
defaultDiskPolicy
  (SecurityParam k)
  requestedInterval
  requestedFlushFrequency
  requestedQueryBatchSize =
    DiskPolicy {
        onDiskNumSnapshots
      , onDiskShouldTakeSnapshot
      , onDiskShouldFlush
      , onDiskQueryBatchSize
      }
  where
    onDiskNumSnapshots :: Word
    onDiskNumSnapshots = 2

    onDiskShouldTakeSnapshot ::
         Maybe DiffTime
      -> Word64
      -> Bool
    onDiskShouldTakeSnapshot Nothing blocksSinceLast =
      -- If users never leave their wallet running for long, this would mean
      -- that under some circumstances we would never take a snapshot
      -- So, on startup (when the 'time since the last snapshot' is `Nothing`),
      -- we take a snapshot as soon as there are @k@ blocks replayed.
      -- This means that even if users frequently shut down their wallet, we still
      -- take a snapshot roughly every @k@ blocks. It does mean the possibility of
      -- an extra unnecessary snapshot during syncing (if the node is restarted), but
      -- that is not a big deal.
      blocksSinceLast >= k

    onDiskShouldTakeSnapshot (Just timeSinceLast) blocksSinceLast =
         timeSinceLast >= snapshotInterval
      || substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast

    -- | We want to create a snapshot after a substantial amount of blocks were
    -- processed (hard-coded to 50k blocks). Given the fact that during bootstrap
    -- a fresh node will see a lot of blocks over a short period of time, we want
    -- to limit this condition to happen not more often then a fixed amount of
    -- time (here hard-coded to 6 minutes)
    substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast =
      let minBlocksBeforeSnapshot      = 50_000
          minTimeBeforeSnapshot        = 6 * secondsToDiffTime 60
      in    blocksSinceLast >= minBlocksBeforeSnapshot
         && timeSinceLast   >= minTimeBeforeSnapshot

    -- | Requested snapshot interval can be explicitly provided by the
    -- caller (RequestedSnapshotInterval) or the caller might request the default
    -- snapshot interval (DefaultSnapshotInterval). If the latter then the
    -- snapshot interval is defaulted to k * 2 seconds - when @k = 2160@ the interval
    -- defaults to 72 minutes.
    snapshotInterval = case requestedInterval of
      RequestedSnapshotInterval value -> value
      DefaultSnapshotInterval           -> secondsToDiffTime $ fromIntegral $ k * 2

    onDiskShouldFlush = case requestedFlushFrequency of
      RequestedFlushFrequency value -> (>= value)
      DefaultFlushFrequency         -> (>= 100)

    onDiskQueryBatchSize = case requestedQueryBatchSize of
      RequestedQueryBatchSize value -> value
      DefaultQueryBatchSize         -> 100_000

-- | Arguments required to initialize a LedgerDB.
data LedgerDBArgs f m blk = LedgerDBArgs {
      lgrDiskPolicy           :: DiskPolicy
    , lgrGenesis              :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrHasFS                :: SomeHasFS m
    , lgrTopLevelConfig       :: HKD f (TopLevelConfig blk)
    , lgrTraceLedger          :: Tracer m (DbCh.AnchorlessDbChangelog' blk)
    , lgrTracer               :: Tracer m (TraceLedgerDBEvent blk)
    , lgrBsTracer             :: Tracer m BackingStoreTraceByBackend
    , lgrBackingStoreSelector :: !(BackingStoreSelector m)
    }

-- | Default arguments
defaultArgs ::
     Applicative m
  => SomeHasFS m
  -> DiskPolicy
  -> BackingStoreSelector m
  -> LedgerDBArgs Defaults m blk
defaultArgs lgrHasFS diskPolicy bss = LedgerDBArgs {
      lgrDiskPolicy           = diskPolicy
    , lgrGenesis              = NoDefault
    , lgrHasFS
    , lgrTopLevelConfig       = NoDefault
    , lgrTraceLedger          = nullTracer
    , lgrTracer               = nullTracer
    , lgrBsTracer             = nullTracer
    , lgrBackingStoreSelector = bss
    }

{-------------------------------------------------------------------------------
  Internal: State
-------------------------------------------------------------------------------}

data LedgerDBState m blk = LedgerDBState {
   ldbChangelog    :: !(StrictTVar m (DbChangelog' blk))
   -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
   -- the current chain of the ChainDB.
 , ldbBackingStore :: !(BackingStore' m blk)
   -- ^ Handle to the ledger's backing store, containing the parts that grow too
   -- big for in-memory residency
 , ldbLock         :: !(LedgerDBLock m)
   -- ^ The flush lock to the 'BackingStore'. This lock is crucial when it
   -- comes to keeping the data in memory consistent with the data on-disk.
   --
   -- This lock should be held whenever we want to keep a consistent view of
   -- the backing store for some time. In particular we use this:
   --
   -- - when performing a query on the ledger state, we need to hold a
   --   'LocalStateQueryView' which, while live, must maintain a consistent view
   --   of the DB, and therefore we acquire a Read lock.
   --
   -- - when taking a snapshot of the ledger db, we need to prevent others
   --   from altering the backing store at the same time, thus we acquire a
   --   Write lock.
 , varPrevApplied  :: !(StrictTVar m (Set (RealPoint blk)))
   -- ^ INVARIANT: this set contains only points that are in the
   -- VolatileDB.
   --
   -- INVARIANT: all points on the current chain fragment are in this set.
   --
   -- The VolatileDB might contain invalid blocks, these will not be in
   -- this set.
   --
   -- When a garbage-collection is performed on the VolatileDB, the points
   -- of the blocks eligible for garbage-collection should be removed from
   -- this set.
 } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LedgerDBState m blk)

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
newtype LedgerDbError blk =
    -- | The ChainDB is closed.
    --
    -- This will be thrown when performing some operations on the LedgerDB.The
    -- 'CallStack' of the operation on the LedgerDB is included in the error.
    ClosedDBError PrettyCallStack
    deriving (Show)
    deriving anyclass (Exception)

newtype LedgerDBHandle m blk = LDBHandle (StrictTVar m (LedgerDBStateEnv m blk))

getState :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
       => LedgerDBHandle m blk
       -> (LedgerDBState m blk -> m r)
       -> m r
getState (LDBHandle varState) f = readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getState' for functions taking two arguments.
getState2 :: (IOLike m, HasCallStack, HasHeader blk)
        => LedgerDBHandle m blk
        -> (LedgerDBState m blk -> a -> b -> m r)
        -> a -> b -> m r
getState2 h f a b = getState h (\env -> f env a b)

-- | Variant of 'getState' that works in 'STM'.
getStateSTM :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
          => LedgerDBHandle m blk
          -> (LedgerDBState m blk -> STM m r)
          -> STM m r
getStateSTM (LDBHandle varState) f = readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getState1' that works in 'STM'.
getStateSTM1 ::
     forall m blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m blk
  -> (LedgerDBState m blk -> a -> STM m r)
  -> a -> STM m r
getStateSTM1 (LDBHandle varState) f a = readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

data LedgerDBStateEnv m blk
  = LedgerDBOpen   !(LedgerDBState m blk)
  | LedgerDBClosed
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Internal: Implementing the API
-------------------------------------------------------------------------------}

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent' ::
     forall blk m.
     ( MonadSTM m
     , IsLedger (LedgerState blk)
     , HasLedgerTables (LedgerState blk)
     )
  => StrictTVar m (DbChangelog' blk)
  -> AnchorlessDbChangelog' blk
  -> STM m ()
setCurrent' v dblog =
  modifyTVar v (\pruned ->
    let s = fromWithOrigin 0
          . pointSlot
          . getTip
          $ changelogLastFlushedState pruned
    in DbChangelog {
          changelogLastFlushedState = changelogLastFlushedState pruned
        , anchorlessChangelog       = AnchorlessDbChangelog {
              adcLastFlushedSlot = adcLastFlushedSlot $ anchorlessChangelog pruned
            , adcStates          = adcStates dblog
            , adcDiffs           =
                ltliftA2 (f s) (adcDiffs $ anchorlessChangelog pruned) (adcDiffs dblog)
            }
        })
  where
    f :: (Ord k, Eq v)
      => SlotNo
      -> SeqDiffMK k v
      -> SeqDiffMK k v
      -> SeqDiffMK k v
    f s (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) = SeqDiffMK $
      if DS.minSlot prunedSeq == DS.minSlot extendedSeq
      then extendedSeq
      else snd $ DS.splitAtSlot s extendedSeq


-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollectPrevApplied :: IOLike m
                          => StrictTVar m (Set (RealPoint blk))
                          -> SlotNo
                          -> STM m ()
garbageCollectPrevApplied prevApplied slotNo = modifyTVar prevApplied $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

validate' :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => StrictTVar m (Set (RealPoint blk))
         -> ResolveBlock m blk
         -> TopLevelConfig blk
         -> BackingStoreValueHandle' m blk
         -> AnchorlessDbChangelog' blk
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate' prevApplied
         resolve
         config
         ldbhandle
         changelog
         blockCache
         numRollbacks
         trace
         hdrs = do
    aps <- mkAps <$> readTVarIO prevApplied
    res <- fmap rewrap $ defaultResolveWithErrors resolve $
             switch
               (configDbChangelog config)
               numRollbacks
               (lift . lift . trace)
               aps
               (lift . lift . readKeySetsWith ldbhandle)
               changelog
    atomically $ modifyTVar prevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError' blk) (Either ExceededRollback (AnchorlessDbChangelog' blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l. l ~ ExtLedgerState blk
          => Set (RealPoint blk)
          -> [Ap n l blk ( ResolvesBlocks    n   blk
                         , ThrowsLedgerError n l blk
                         )]
    mkAps prev =
      [ case ( Set.member (headerRealPoint hdr) prev
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->          ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> Weaken $ ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> Weaken $ ApplyVal   blk
          (True,  Just blk) -> Weaken $ ReapplyVal blk
      | hdr <- hdrs
      ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= annLedgerErrRef e)

    addPoints :: [RealPoint blk]
              -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushLedgerDB :: (MonadSTM m, GetTip l, HasLedgerTables l)
              => StrictTVar m (DbChangelog l)
              -> LedgerBackingStore m l
              -> m ()
flushLedgerDB chlogVar bstore = do
  diffs <- atomically $ do
    ldb' <- readTVar chlogVar
    let (toFlush, toKeep) = splitForFlushing ldb'
    case toFlush of
      Nothing -> pure ()
      Just {} -> writeTVar chlogVar toKeep
    pure toFlush
  mapM_ (flushIntoBackingStore bstore) diffs

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The write lock must be held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> m ()
flushIntoBackingStore backingStore dblog =
  bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceLedgerDBEvent blk =
    LedgerDBSnapshotEvent (TraceSnapshotEvent blk)
  | BackingStoreInitEvent TraceBackingStoreInitEvent
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getPrevApplied' :: IOLike m => LedgerDBState m blk -> STM m (Set (RealPoint blk))
getPrevApplied' = readTVar . varPrevApplied

getCurrent' :: IOLike m => LedgerDBState m blk -> STM m (DbChangelog' blk)
getCurrent' = readTVar . ldbChangelog

-- | Read and forward the values up to the given point on the chain.
getLedgerTablesAtFor' ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HeaderHash blk ~ HeaderHash l
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  )
  => Point blk
  -> LedgerTables l KeysMK
  -> StrictTVar m (DbChangelog l)
  -> LedgerBackingStore m l
  -> m (Either
        (PointNotFound blk)
        (LedgerTables l ValuesMK))
getLedgerTablesAtFor' pt keys dbvar bstore = do
  lgrDb <- anchorlessChangelog <$> readTVarIO dbvar
  case rollback pt lgrDb of
    Nothing -> pure $ Left $ PointNotFound pt
    Just l  -> do
      eValues <-
        getLedgerTablesFor l keys (readKeySets bstore)
      case eValues of
        Right v -> pure $ Right v
        Left _  -> getLedgerTablesAtFor' pt keys dbvar bstore

-- | Given a point (or @Left ()@ for the tip), acquire both a value handle and a
-- db changelog at the requested point. Holds a read lock while doing so.
acquireLDBReadView' ::
     forall a b m blk.
     (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
  => StaticEither b () (Point blk)
  -> StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> BackingStore' m blk
  -> DiskPolicy
  -> STM m a
     -- ^ STM operation that we want to run in the same atomic block as the
     -- acquisition of the LedgerDB
  -> m ( a
       , StaticEither b
           (LedgerDBView' m blk)
           (Either
            (Point blk)
            (LedgerDBView' m blk))
       )
acquireLDBReadView' p dbvar lock bs policy stmAct =
  withReadLock lock $ do
    (a, ldb') <- atomically $ do
      (,) <$> stmAct <*> (anchorlessChangelog <$> readTVar dbvar)
    (a,) <$> case p of
      StaticLeft () -> StaticLeft <$> acquire ldb'
      StaticRight actualPoint -> StaticRight <$>
        case rollback actualPoint ldb' of
          Nothing    -> pure $ Left $ castPoint $ getTip $ anchor ldb'
          Just ldb'' -> Right <$> acquire ldb''
 where
   acquire ::
        AnchorlessDbChangelog' blk
     -> m (LedgerDBView' m blk)
   acquire l = do
     vh <- bsValueHandle bs
     if bsvhAtSlot vh == adcLastFlushedSlot l
       then pure $ LedgerDBView vh l (onDiskQueryBatchSize policy)
       else error ("Critical error: Value handles are created at "
                   <> show (bsvhAtSlot vh)
                   <> " while the db changelog is at "
                   <> show (adcLastFlushedSlot l)
                   <> ". There is either a race condition or a logic bug"
                  )

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
getStatistics' ::
     (Monad m, IsLedger l, HasLedgerTables l)
  => LedgerDBView m l
  -> m Statistics
getStatistics' (LedgerDBView lbsvh dblog _) = do
    Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat lbsvh
    if seqNo /= seqNo' then
      error $ show (seqNo, seqNo')
    else
      pure $ Statistics {
          sequenceNumber = getTipSlot $ K dblog
        , numEntries     = n + nInserts - nDeletes
        }
  where
    diffs = adcDiffs  dblog
    seqNo = adcLastFlushedSlot dblog

    nInserts = getSum
            $ ltcollapse
            $ ltmap (K2 . numInserts . getSeqDiffMK)
              diffs
    nDeletes = getSum
            $ ltcollapse
            $ ltmap (K2 . numDeletes . getSeqDiffMK)
              diffs

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal immTip = (($ ReplayGoal immTip) >$<)

-- | Add the block at which a replay started.
--
-- This allows to compute a "percentage complete" when tracing the events.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithStart start = (($ ReplayStart start) >$<)

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (RealPoint blk)
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  -- | We replayed the given block (reference) on the genesis snapshot during
  -- the initialisation of the LedgerDB. Used during ImmutableDB replay.
  | ReplayedBlock
        (RealPoint blk)   -- ^ the block being replayed
        [LedgerEvent blk]
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

{- $snapshots

  Snapshotting a ledger state means saving a copy of the in-memory part of the
  ledger state serialized as a file on disk, as well as flushing differences on
  the ledger tables between the last snapshotted ledger state and the one that
  we are snapshotting now and making a copy of that resulting on-disk state.

  == Startup

  During initialisation, the goal is to construct an initial 'LedgerDB' where
  the sequence of in-memory states is empty except for the ledger state at the
  anchor, which has to correspond to the immutable tip, i.e., the block at the
  tip of the Immutable DB.

  Ideally, we can construct the initial 'LedgerDB' from a snapshot of the ledger
  state that we wrote to disk. Remember that updating a ledger state with a
  block is not invertible: we can apply a block to a ledger state, but we cannot
  /unapply/ a block to a ledger state. This means the snapshot has to be at
  least as old as the anchor. A snapshot matching the anchor can be used as is.
  A snapshot older than the anchor can be used after reapplying the necessary
  blocks. A snapshot newer than the anchor can /not/ be used, as we cannot
  unapply blocks to get the ledger state corresponding to the anchor. This is
  the reason why we only take snapshots of an immutable ledger state, i.e., of
  the anchor of the 'DbChangelog' (or older).

  On startup, the node will:

  1. Find the latest snapshot which will be a directory identified by the slot
     number of the snapshot:

        > <cardano-node data dir>
        > ├── volatile
        > ├── immutable
        > └── ledger
        >     ├── <slotNumber1>
        >     │   ├── tables
        >     │   └── state
        >     ├── <slotNumber2>
        >     │   ├── tables
        >     │   └── state
        >     └── <slotNumber3>
        >         ├── tables
        >         └── state

  2. Depending on the snapshots found, there are two possibilities:

       - If there is no snapshot to load, create a new @'BackingStore'@ with the
         contents of the Genesis ledger tables and finish.

       - If there is a snapshot found, then deserialize (with @DecodeDisk@) the
         @state@ file, which contains a serialization of the in-memory part of
         the LedgerState, with empty tables (i.e. a @ExtLedgerState blk
         EmptyMK@). If deserialization fails, delete this snapshot and start
         again. If the snapshot is newer than the immutable tip, delete this
         snapshot and start again.

        In case we found an snapshot, we will overwrite (either literally
        overwriting it or using some feature from the specific backend used) the
        @BackingStore@ tables with the @tables@ file from said snapshot as it
        was left in whatever state it was when the node shut down.

  3. The deserialized ledger state will be then used as the anchor for the
     ledger database.

  4. Reapply the immutable blocks after the snapshot to obtain the ledger state
       at the immutable tip. The blocks to reapply are streamed from the Immutable
       DB, using an iterator.

       Note that we can /reapply/ these blocks, which is quicker than applying
       them, as the existence of a snapshot newer than these blocks proves (unless
       the on-disk database has been tampered with, but this is not an attack we
       intend to protect against, as this would mean the machine has already been
       compromised) that they have been successfully applied in the past.

  Reading and applying blocks is costly. Typically, very few blocks need to be
  reapplied in practice. However, there is one exception: when the serialisation
  format of the ledger state changes, all snapshots (written using the old
  serialisation format) will fail to deserialise, and all blocks starting from
  genesis will have to be reapplied.

  At this point, the node carries a @DbChangelog@ that is initialized and ready
  to be applied blocks on the volatile database.

  == Taking snapshots during normal operation

  Snapshots are taken by the @'copyAndSnapshotRunner'@ when the disk policy
  dictates to do so. Whenever the chain grows past @k@ blocks, said runner will
  copy the blocks which are more than @k@ blocks from the tip (i.e. the ones
  that must be considered immutable) to the immutable database and then:

  1. Every time we have processed a specific amount of blocks since the last
       flush (set by default to 100), perform a flush of differences in the DB up
       to the immutable db tip.

  2. If dictated by the disk policy, flush immediately all the differences up to
       the immutable db tip and serialize (using 'EncodeDisk') the DbChangelog
       in-memory ledger states anchor (@ExtLedgerState blk EmptyMK@).

       A directory is created named after the slot number of the ledger state
       being snapshotted, and the serialization from above is written into the
       @\<slotNumber\>/state@ file and the @BackingStore@ tables are copied into
       the @\<slotNumber\>/tables@ file.

  3. There is a maximum number of snapshots that should exist in the disk at any
     time, dictated by the @DiskPolicy@, so if needed, we will trim out old
     snapshots.

  == Flush during startup and snapshot at the end of startup

  Due to the nature of the database having to carry around all the differences
  between the last snapshotted state and the current tip, there is a need to
  flush when replaying the chain as otherwise, for example on a replay from
  genesis to the tip, we would carry millions of differences in memory.

  Because of this, when we are replaying blocks we will flush regularly. As the
  last snapshot that was taken lives in a @\<slotNumber\>/tables@ file, there is
  no risk of destroying it (overwriting tables at another earlier snapshot) by
  flushing. Only when we finish replaying blocks and start the background
  threads (and specifically the @copyAndSnapshotRunner@), we will take a
  snapshot of the current immutable database anchor as described above.

-}

data DiskSnapshot = DiskSnapshot {
      -- | Snapshots are numbered. We will try the snapshots with the highest
      -- number first.
      --
      -- When creating a snapshot, we use the slot number of the ledger state it
      -- corresponds to as the snapshot number. This gives an indication of how
      -- recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on the
      -- snapshot number matching the slot number of the corresponding ledger
      -- state. We only use the snapshots numbers to determine the order in
      -- which we try them.
      dsNumber :: Word64

      -- | Snapshots can optionally have a suffix, separated by the snapshot
      -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
      -- as metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Ord, Generic)

data SnapshotFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis
  deriving (Show, Eq, Generic)

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

-- | Named snapshot are permanent, they will never be deleted when trimming.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

-- | Read snapshot from disk
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK)
readSnapshot hasFS decLedger decHash = do
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToStatePath
  where
    decoder :: Decoder s (ExtLedgerState blk EmptyMK)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{listDirectory}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

-- | Try to take a snapshot of the /oldest ledger state/ in the ledger DB
--
-- We write the /oldest/ ledger state to disk because the intention is to only
-- write ledger states to disk that we know to be immutable. Primarily for
-- testing purposes, 'takeSnapshot' returns the block reference corresponding
-- to the snapshot that we wrote.
--
-- If a snapshot with the same number already exists on disk or if the tip is at
-- genesis, no snapshot is taken.
--
-- Note that an EBB can have the same slot number and thus snapshot number as
-- the block after it. This doesn't matter. The one block difference in the
-- ledger state doesn't warrant an additional snapshot. The number in the name
-- of the snapshot is only indicative, we don't rely on it being correct.
--
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
  forall m blk.
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  )
  => StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> BackingStore.BackingStore' m blk
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot ldbvar lock ccfg tracer hasFS ldbBackingStore =
  withReadLock lock $ do
    state <- changelogLastFlushedState <$> readTVarIO ldbvar
    case pointToWithOriginRealPoint (castPoint (getTip state)) of
      Origin ->
        return Nothing
      NotOrigin t -> do
        let number   = unSlotNo (realPointSlot t)
            snapshot = DiskSnapshot number Nothing
        diskSnapshots <- listSnapshots hasFS
        if List.any ((== number) . dsNumber) diskSnapshots then
          return Nothing
        else do
          writeSnapshot hasFS ldbBackingStore encodeExtLedgerState' snapshot state
          traceWith tracer $ TookSnapshot snapshot t
          return $ Just (snapshot, t)
  where
    encodeExtLedgerState' :: ExtLedgerState blk EmptyMK -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> BackingStore.BackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK
  -> m ()
writeSnapshot (SomeHasFS hasFS) backingStore encLedger snapshot cs = do
    createDirectory hasFS (snapshotToDirPath snapshot)
    withFile hasFS (snapshotToStatePath snapshot) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encoder cs)
    BackingStore.bsCopy
      backingStore
      (snapshotToTablesPath snapshot)
  where
    encoder :: ExtLedgerState blk EmptyMK -> Encoding
    encoder = encodeSnapshot encLedger

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceSnapshotEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{onDiskNumSnapshots} = do
    -- We only trim temporary snapshots
    diskSnapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) diskSnapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{removeDirectoryRecursive}) = removeDirectoryRecursive . snapshotToDirPath

snapshotToDirName :: DiskSnapshot -> String
snapshotToDirName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

-- | The path within the LedgerDB's filesystem to the snapshot's directory
snapshotToDirPath :: DiskSnapshot -> FsPath
snapshotToDirPath = mkFsPath . (:[]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the directory that contains a
-- snapshot's backing store
snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath = mkFsPath . (\x -> [x, "tables"]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the directory that contains the
-- backing store
_tablesPath :: FsPath
_tablesPath = mkFsPath ["tables"]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    number <- readMaybe prefix
    return $ DiskSnapshot number suffix'
  where
    (prefix, suffix) = break (== '_') fileName

    suffix' :: Maybe String
    suffix' = case suffix of
      ""      -> Nothing
      _ : str -> Just str

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
--
-- * The tip: @WithOrigin (RealPoint blk)@
--
-- * The chain length: @Word64@
--
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen

{-------------------------------------------------------------------------------
  LocalStateQuery support
-------------------------------------------------------------------------------}

-- | How to perform a read using a 'LedgerDBView'.
ledgerDBViewRead :: (Monad m, HasLedgerTables l)
                 => LedgerDBView m l
                 -> LedgerTables l KeysMK
                 -> m (LedgerTables l ValuesMK)
ledgerDBViewRead (LedgerDBView lvh ldb _) ks = do
          let rew = rewindTableKeySets ldb ks
          unfwd <- readKeySetsWith lvh rew
          case forwardTableKeySets ldb unfwd of
              Left _err -> error "impossible!"
              Right vs  -> pure vs

-- | How to perform a range read using a 'LedgerDBView'.
ledgerDBViewRangeRead :: (Monad m, HasLedgerTables l)
                      => LedgerDBView m l
                      -> RangeQuery (LedgerTables l KeysMK)
                      -> m (LedgerTables l ValuesMK)
ledgerDBViewRangeRead (LedgerDBView lvh ldb _) rq = do
    let -- Get the differences without the keys that are greater or equal
        -- than the maximum previously seen key.
        diffs =
          maybe
            id
            (ltliftA2 doDropLTE)
            (BackingStore.rqPrev rq)
            $ ltmap prj
            $ adcDiffs ldb
            -- (1) Ensure that we never delete everything read from disk (ie
            --     if our result is non-empty then it contains something read
            --     from disk).
            --
            -- (2) Also, read one additional key, which we will not include in
            --     the result but need in order to know which in-memory
            --     insertions to include.
        maxDeletes = ltcollapse $ ltmap (K2 . numDeletesDiffMK) diffs
        nrequested = 1 + max (BackingStore.rqCount rq) (1 + maxDeletes)

    values <- BackingStore.bsvhRangeRead lvh (rq{BackingStore.rqCount = nrequested})
    pure $ ltliftA2 (doFixupReadResult nrequested) diffs values
  where
    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

    -- Remove all diff elements that are <= to the greatest given key
    doDropLTE ::
         Ord k
      => KeysMK k v
      -> DiffMK k v
      -> DiffMK k v
    doDropLTE (KeysMK ks) (DiffMK ds) =
        DiffMK
      $ case Set.lookupMax ks of
          Nothing -> ds
          Just k  -> Diff.filterOnlyKey (> k) ds

    -- NOTE: this is counting the deletions wrt disk.
    numDeletesDiffMK :: DiffMK k v -> Int
    numDeletesDiffMK (DiffMK d) =
      getSum $ Diff.foldMapDelta (Sum . oneIfDel) d
      where
        oneIfDel x = case x of
          Diff.Delete _ -> 1
          Diff.Insert _ -> 0

    -- INVARIANT: nrequested > 0
    --
    -- (1) if we reached the end of the store, then simply yield the given diff
    --     applied to the given values
    -- (2) otherwise, the readset must be non-empty, since 'rqCount' is positive
    -- (3) remove the greatest read key
    -- (4) remove all diff elements that are >= the greatest read key
    -- (5) apply the remaining diff
    -- (6) (the greatest read key will be the first fetched if the yield of this
    --     result is next passed as 'rqPrev')
    --
    -- Note that if the in-memory changelog contains the greatest key, then
    -- we'll return that in step (1) above, in which case the next passed
    -- 'rqPrev' will contain it, which will cause 'doDropLTE' to result in an
    -- empty diff, which will result in an entirely empty range query result,
    -- which is the termination case.
    doFixupReadResult ::
         Ord k
      => Int
      -- ^ Number of requested keys from the backing store.
      -> DiffMK   k v
      -- ^ Differences that will be applied to the values read from the backing
      -- store.
      -> ValuesMK k v
      -- ^ Values read from the backing store. The number of values read should
      -- be at most @nrequested@.
      -> ValuesMK k v
    doFixupReadResult
      nrequested
      (DiffMK ds)
      (ValuesMK vs) =
        let includingAllKeys        =
              Diff.applyDiff vs ds
            definitelyNoMoreToFetch = Map.size vs < nrequested
        in
        ValuesMK
      $ case Map.maxViewWithKey vs of
          Nothing             ->
              if definitelyNoMoreToFetch
              then includingAllKeys
              else error $ "Size of values " <> show (Map.size vs) <> ", nrequested " <> show nrequested
          Just ((k, _v), vs') ->
            if definitelyNoMoreToFetch then includingAllKeys else
            Diff.applyDiff
              vs'
               (Diff.filterOnlyKey (< k) ds)

{-------------------------------------------------------------------------------
  Internal: LedgerDB lock
-------------------------------------------------------------------------------}

-- | A lock to prevent the LedgerDB (i.e. a 'DbChangelog') from getting out of
-- sync with the 'BackingStore'.
--
-- We rely on the capability of the @BackingStore@s of providing
-- 'BackingStoreValueHandles' that can be used to hold a persistent view of the
-- database as long as the handle is open. Assuming this functionality, the lock
-- is used in three ways:
--
-- - Read lock to acquire a value handle: we do this when acquiring a view of the
--   'LedgerDB' (which lives in a 'StrictTVar' at the 'ChainDB' level) and of
--   the 'BackingStore'. We momentarily acquire a read lock, consult the
--   transactional variable and also open a 'BackingStoreValueHandle'. This is
--   the case for ledger state queries and for the forging loop.
--
-- - Read lock to ensure two operations are in sync: in the above situation, we
--   relied on the 'BackingStoreValueHandle' functionality, but sometimes we
--   won't access the values through a value handle, and instead we might use
--   the LMDB environment (as it is the case for 'lmdbCopy'). In these cases, we
--   acquire a read lock until we ended the copy, so that writers are blocked
--   until this process is completed. This is the case when taking a snapshot.
--
-- - Write lock when flushing differences.
newtype LedgerDBLock m = LedgerDBLock (Lock.RAWLock m ())
  deriving newtype NoThunks

mkLedgerDBLock :: IOLike m => m (LedgerDBLock m)
mkLedgerDBLock = LedgerDBLock <$> Lock.new ()

-- | Acquire the ledger DB read lock and hold it while performing an action
withReadLock :: IOLike m => LedgerDBLock m -> m a -> m a
withReadLock (LedgerDBLock lock) m =
    Lock.withReadAccess lock (\() -> m)

-- | Acquire the ledger DB write lock and hold it while performing an action
withWriteLock :: IOLike m => LedgerDBLock m -> m a -> m a
withWriteLock (LedgerDBLock lock) m =
    Lock.withWriteAccess lock (\() -> (,) () <$> m)
