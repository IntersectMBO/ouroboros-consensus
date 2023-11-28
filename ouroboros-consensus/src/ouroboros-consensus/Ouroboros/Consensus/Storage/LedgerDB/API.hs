{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | The Ledger DB is responsible for the following tasks:
--
-- - __Maintaining the in-memory ledger state at the tip__: When we try to
--     extend our chain with a new block fitting onto our tip, the block must
--     first be validated using the right ledger state, i.e., the ledger state
--     corresponding to the tip.
--
-- - __Maintaining the past \(k\) in-memory ledger states__: we might roll back
--     up to \(k\) blocks when switching to a more preferable fork. Consider the
--     example below:
--
--     <<docs/haddocks/ledgerdb-switch.svg>>
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
-- - __Providing 'Ouroboros.Consensus.Ledger.Tables.Basics.LedgerTable's at any
--     of the last \(k\) ledger states__: To apply blocks or transactions on top
--     of ledger states, the LedgerDB must be able to provide the appropriate
--     ledger tables at any of those ledger states.
--
-- - __Storing snapshots on disk__: To obtain a ledger state for the current tip
--     of the chain, one has to apply /all blocks in the chain/ one-by-one to
--     the initial ledger state. When starting up the system with an on-disk
--     chain containing millions of blocks, all of them would have to be read
--     from disk and applied. This process can take hours, depending on the
--     storage and CPU speed, and is thus too costly to perform on each startup.
--
--     For this reason, a recent snapshot of the ledger state should be
--     periodically written to disk. Upon the next startup, that snapshot can be
--     read and used to restore the current ledger state, as well as the past
--     \(k\) ledger states.
--
-- - __Flushing 'LedgerTable' differences__: The running Consensus has to
--     periodically flush chunks of [differences]("Data.Map.Diff.Strict")
--     from the 'DbChangelog' to the 'BackingStore', so that memory is
--     off-loaded to the backing store, and if the backing store is an on-disk
--     implementation, reduce the memory usage.
--
-- Note that whenever we say /ledger state/ we mean the @'ExtLedgerState' blk
-- mk@ type described in "Ouroboros.Consensus.Ledger.Basics".
--
-- === __(image code)__
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/"
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/ledgerdb-switch.svg" defaultEnv (tikz ["positioning", "arrows"]) "\
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
module Ouroboros.Consensus.Storage.LedgerDB.API (
    -- * Main API
    LedgerDB (..)
    -- * Exceptions
  , LedgerDbError (..)
    -- * Forker
  , Forker (..)
  , PointNotFound (..)
  , RangeQuery (..)
  , Statistics (..)
  , getTipForker
  , getTipStatistics
  , readLedgerTablesAtFor
  , withTipForker
    -- * DiskPolicy (re-exports)
  , DiskPolicy (..)
  , SnapshotInterval (..)
  , defaultDiskPolicy
    -- * Snapshots
  , DiskSnapshot (..)
  , SnapCounters (..)
  , SnapshotFailure (..)
    -- * Validation
  , ExceededRollback (..)
  , ValidateResult (..)
  , validate
    -- ** Annotated ledger errors
  , AnnLedgerError (..)
    -- ** Finding blocks
  , ResolveBlock
  , ResolvesBlocks (..)
    -- * Tracing
  , TraceLedgerDBEvent (..)
    -- ** Replay events
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- ** Snapshot events
  , TraceSnapshotEvent (..)
    -- ** Validation events
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
  ) where

import           Control.Monad (void)
import           Control.Monad.Base
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Except (ExceptT, MonadError (throwError),
                     runExcept, runExceptT)
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans
import           Control.Tracer
import           Data.Functor.Contravariant
import           Data.Kind
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API.DiskPolicy
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | The core API of the LedgerDB component
type LedgerDB :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDB m l blk = LedgerDB {
    -- | Get the empty ledger state at the (volatile) tip of the LedgerDB.
    getVolatileTip         ::              STM m (l EmptyMK)
    -- | Get the empty ledger state at the immutable tip of the LedgerDB.
  , getImmutableTip        ::              STM m (l EmptyMK)
    -- | Get an empty ledger state at a requested point in the LedgerDB, if it
    -- exists.
  , getPastLedgerState     :: Point blk -> STM m (Maybe (l EmptyMK))
    -- | Get the header state history for all ledger states in the LedgerDB.
  , getHeaderStateHistory  ::
         (l ~ ExtLedgerState blk)
      => STM m (HeaderStateHistory blk)
    -- | Acquire a 'Forker' at a requested @n@ blocks back from the tip.
    --
    -- You could view this as a rollback of @n@ blocks, and acquiring the
    -- 'Forker' at that point.
  , getForkerAtFromTip :: Word64 -> m (Either ExceededRollback (Forker m l blk))
    -- | Acquire a 'Forker' at the requested point or at the tip. If the
    -- requested point doesn't exist it will return a @StaticRight (Left pt)@.
  , getForker  ::
         forall a b.
         StaticEither b () (Point blk)
      -> STM m a
#if __GLASGOW_HASKELL__ >= 902
         -- ^ STM operation that we want to run in the same atomic block as the
         -- acquisition of the LedgerDB
#endif
    -> m ( a
         , StaticEither b
            (Forker m l blk)
            (Either (Point blk) (Forker m l blk))
         )
    -- | Get the references to blocks that have previously been applied.
  , getPrevApplied :: STM m (Set (RealPoint blk))
    -- | Add new references to blocks that have been applied.
  , addPrevApplied :: [RealPoint blk] -> STM m ()
    -- | Garbage collect references to old blocks that have been previously
    -- applied.
  , garbageCollect :: SlotNo -> STM m ()
    -- | Get
  , getResolveBlock :: STM m (ResolveBlock m blk)
    -- | If the provided arguments indicate so (based on the DiskPolicy with
    -- which this LedgerDB was opened), take a snapshot and delete stale ones.
  , tryTakeSnapshot ::
         (l ~ ExtLedgerState blk)
      => Maybe (Time, Time)
#if __GLASGOW_HASKELL__ >= 902
         -- ^ If a snapshot has been taken already, the time at which it was
         -- taken and the current time.
#endif
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks have been processed since the last snapshot.
#endif
      -> m SnapCounters
    -- | Flush in-memory LedgerDB state to disk, if possible. This is a no-op
    -- for implementations that do not need an explicit flush function.
  , tryFlush :: m ()
  }

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
data LedgerDbError blk =
      -- | The LedgerDB is closed.
      --
      -- This will be thrown when performing some operations on the LedgerDB. The
      -- 'CallStack' of the operation on the LedgerDB is included in the error.
      ClosedDBError PrettyCallStack
      -- | A Forker is closed.
    | ClosedForkerError PrettyCallStack
    deriving (Show)
    deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point the LedgerDB, which can be advanced to
-- evaluate forks in the chain.
type Forker :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Forker m l blk = Forker {
    -- | Close the current forker (idempotent).
    --
    -- Other functions on forkers should throw a 'ClosedForkError' once the
    -- forker is closed.
    --
    -- Note: always use this functions before the forker is forgotten!
    -- Otherwise, cleanup of (on-disk) state might not be prompt or guaranteed.
    forkerClose :: !(m ())

    -- * Queries

    -- | Read ledger tables from disk.
  , forkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
    -- | Range-read ledger tables from disk.
  , forkerRangeReadTables :: !(RangeQuery l -> m (LedgerTables l ValuesMK))
    -- | Get the full ledger state without tables.
    --
    -- If empty ledger state is all you need, use 'getVolatileTip',
    -- 'getImmutableTip', or 'getPastLedgerState' instead.
  , forkerGetLedgerState  :: !(STM m (l EmptyMK))
    -- | Get statistics about the current state of the handle if possible.
    --
    -- Returns 'Nothing' if the implementation is backed by @lsm-tree@.
  , forkerReadStatistics :: !(m (Maybe Statistics))

    -- * Updates

    -- | Advance the fork handle by pushing a new ledger state to the tip of the
    -- current fork.
  , forkerPush :: !(l DiffMK -> m ())
    -- | Commit the fork, which was constructed using 'forkerPush', as the
    -- current version of the LedgerDB.
  , forkerCommit :: !(STM m ())
  }

-- TODO: document
data RangeQuery l = RangeQuery {
    rqPrev  :: !(Maybe (LedgerTables l KeysMK))
  , rqCount :: !Int
  }

-- TODO: document
newtype Statistics = Statistics {
    ledgerTableSize :: Int
  }

getTipForker :: MonadSTM m => LedgerDB m l blk -> m (Forker m l blk)
getTipForker ldb = fromStaticLeft . snd <$> getForker ldb (StaticLeft ()) (pure ())

withTipForker ::
     (MonadSTM m, MonadThrow m)
  => LedgerDB m l blk -> (Forker m l blk -> m a) -> m a
withTipForker ldb = bracket (getTipForker ldb) forkerClose

-- | Read a table of values at the requested point.
readLedgerTablesAtFor ::
     (Monad m, Applicative (STM m), StandardHash blk)
  => LedgerDB m l blk
  -> Point blk
  -> LedgerTables l KeysMK
  -> m (Either
          (PointNotFound blk)
          (LedgerTables l ValuesMK))
readLedgerTablesAtFor ldb p ks = do
    foEith <- fromStaticRight . snd <$> getForker ldb (StaticRight p) (pure ())
    case foEith of
      Left e -> error $ show e
      Right fo -> do
        r <- fo `forkerReadTables` ks
        forkerClose fo
        pure $ Right r

-- | The requested point is not found on the ledger db
newtype PointNotFound blk = PointNotFound (Point blk) deriving (Eq, Show)

-- | Get statistics from the tip of the LedgerDB.
getTipStatistics ::
     (MonadSTM m, MonadThrow m)
  => LedgerDB m l blk -> m (Maybe Statistics)
getTipStatistics ldb = withTipForker ldb forkerReadStatistics

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | Counters to keep track of when we made the last snapshot.
data SnapCounters = SnapCounters {
    -- | When was the last time we made a snapshot
    prevSnapshotTime      :: !(Maybe Time)
    -- | How many blocks have we processed since the last snapshot
  , ntBlocksSinceLastSnap :: !Word64
  }

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

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | When validating a sequence of blocks, these are the possible outcomes.
--
-- TODO: add forker to ledger error
data ValidateResult m l blk =
    ValidateSuccessful       (Forker m l blk)
  | ValidateLedgerError      (AnnLedgerError l blk)
  | ValidateExceededRollBack ExceededRollback

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

validate ::
     forall m l blk. (
       IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     , l ~ ExtLedgerState blk
     , MonadBase m m
     )
  => LedgerDB m l blk
  -> (TraceValidateEvent blk -> m ())
  -> TopLevelConfig blk
  -> BlockCache blk
  -> Word64          -- ^ How many blocks to roll back
  -> [Header blk]
  -> m (ValidateResult m l blk)
validate ldb trace config blockCache numRollbacks hdrs = do
    resolve <- atomically $ getResolveBlock ldb
    aps <- mkAps <$> atomically (getPrevApplied ldb)
    res <- fmap rewrap $ defaultResolveWithErrors resolve $
             switch
               ldb
               (ExtLedgerCfg config)
               numRollbacks
               (lift . lift . trace)
               aps
    liftBase $ atomically $ addPrevApplied ldb (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError l blk) (Either ExceededRollback (Forker bm l blk))
           -> ValidateResult bm l blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l'. l' ~ ExtLedgerState blk
          => Set (RealPoint blk)
          -> [Ap n l blk ( ResolvesBlocks    n   blk
                         , ThrowsLedgerError n l' blk
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
    validBlockPoints :: forall n. ValidateResult n l blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch :: (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
       => LedgerDB bm l blk
       -> LedgerCfg l
       -> Word64          -- ^ How many blocks to roll back
       -> (TraceValidateEvent blk -> m ())
       -> [Ap m l blk c]  -- ^ New blocks to apply
       -> m (Either ExceededRollback (Forker bm l blk))
switch ldb cfg numRollbacks trace newBlocks = do
  foEith <- liftBase $ getForkerAtFromTip ldb numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Left rbExceeded
    Right fo -> do
      case newBlocks of
        [] -> pure ()
        -- no blocks to apply to ledger state, return the forker
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          void $ applyThenPushMany
                    (trace . StartedPushingBlockToTheLedgerDb start goal)
                    cfg
                    newBlocks
                    fo
      pure $ Right fo

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

newtype ValidLedgerState l = ValidLedgerState { getValidLedgerState :: l }

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--
--     1. Are we passing the block by value or by reference?
--
--     2. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--
--     1. If we are passing a block by reference, we must be able to resolve it.
--
--     2. If we are applying rather than reapplying, we might have ledger errors.
type Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type
data Ap m l blk c where
  ReapplyVal ::           blk -> Ap m l blk ()
  ApplyVal   ::           blk -> Ap m l blk ( ThrowsLedgerError m l blk )
  ReapplyRef :: RealPoint blk -> Ap m l blk ( ResolvesBlocks    m   blk )
  ApplyRef   :: RealPoint blk -> Ap m l blk ( ResolvesBlocks    m   blk
                                            , ThrowsLedgerError m l blk )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l blk c -> Ap m l blk c'

toRealPoint :: HasHeader blk => Ap m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply blocks to the given forker
applyBlock :: forall m bm c l blk. (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
           => LedgerCfg l
           -> Ap m l blk c
           -> Forker bm l blk
           -> m (ValidLedgerState (l DiffMK))
applyBlock cfg ap fo = case ap of
    ReapplyVal b ->
          ValidLedgerState
      <$> withValues b (return . tickThenReapply cfg b)
    ApplyVal b ->
          ValidLedgerState
      <$> withValues b
          ( either (throwLedgerError (Proxy @l) (blockRealPoint b)) return
            . runExcept
            . tickThenApply cfg b
          )
    ReapplyRef r  -> do
      b <- doResolveBlock r
      applyBlock cfg (ReapplyVal b) fo
    ApplyRef r -> do
      b <- doResolveBlock r
      applyBlock cfg (ApplyVal b) fo
    Weaken ap' ->
      applyBlock cfg ap' fo
  where
    withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (l DiffMK)
    withValues blk f = do
        l <- liftBase $ atomically $ forkerGetLedgerState fo
        vs <- withLedgerTables l
              <$> liftBase (forkerReadTables fo (getBlockKeySets blk))
        f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
applyThenPush :: (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
              => LedgerCfg l
              -> Ap m l blk c
              -> Forker bm l blk
              -> m ()
applyThenPush cfg ap fo =
    liftBase . forkerPush fo . getValidLedgerState =<<
      applyBlock cfg ap fo

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany :: (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
                  => (Pushing blk -> m ())
                  -> LedgerCfg l
                  -> [Ap m l blk c]
                  -> Forker bm l blk
                  -> m ()
applyThenPushMany trace cfg aps fo = mapM_ pushAndTrace aps
  where
    pushAndTrace ap = do
      trace $ Pushing . toRealPoint $ ap
      applyThenPush cfg ap fo

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError {
      -- | Reference to the block that had the error
      annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

class Monad m => ThrowsLedgerError m l blk where
  throwLedgerError :: proxy l -> RealPoint blk -> LedgerErr l -> m a

instance Monad m => ThrowsLedgerError (ExceptT (AnnLedgerError l blk) m) l blk where
  throwLedgerError _ l r = throwError $ AnnLedgerError l r

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk) m a
                         -> m (Either (AnnLedgerError l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

{-------------------------------------------------------------------------------
  Finding blocks
-------------------------------------------------------------------------------}

-- | Resolve a block
--
-- Resolving a block reference to the actual block lives in @m@ because
-- it might need to read the block from disk (and can therefore not be
-- done inside an STM transaction).
--
-- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
-- must exist. If the 'ChainDB' is unable to fulfill the request, data
-- corruption must have happened and the 'ChainDB' should trigger
-- validation mode.
type ResolveBlock m blk = RealPoint blk -> m blk

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks m blk | m -> blk where
  doResolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  doResolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks (ExceptT e (ReaderT (ResolveBlock m blk) m)) blk where
  doResolveBlock = lift . doResolveBlock

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

newtype TraceLedgerDBEvent blk =
    LedgerDBSnapshotEvent (TraceSnapshotEvent blk)
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Trace replay events
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
  Tracing snapshot events
-------------------------------------------------------------------------------}

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Trace validation events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data TraceValidateEvent blk =
    -- | Event fired when we are about to push a block to a forker
      StartedPushingBlockToTheLedgerDb
        !(PushStart blk)
        -- ^ Point from which we started pushing new blocks
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
        !(Pushing blk)
        -- ^ Point which block we are about to push
  deriving (Show, Eq, Generic)
