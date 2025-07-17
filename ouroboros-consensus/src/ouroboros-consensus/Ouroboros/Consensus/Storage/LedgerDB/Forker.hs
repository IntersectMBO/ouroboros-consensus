{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( -- * Forker API
    ExceededRollback (..)
  , Forker (..)
  , Forker'
  , ForkerKey (..)
  , GetForkerError (..)
  , RangeQuery (..)
  , RangeQueryPrevious (..)
  , Statistics (..)
  , forkerCurrentPoint
  , castRangeQueryPrevious
  , ledgerStateReadOnlyForker

    -- ** Read only
  , ReadOnlyForker (..)
  , ReadOnlyForker'
  , readOnlyForker

    -- ** Tracing
  , TraceForkerEvent (..)
  , TraceForkerEventWithKey (..)

    -- * Validation
  , AnnLedgerError (..)
  , AnnLedgerError'
  , ResolveBlock
  , ValidateArgs (..)
  , ValidateResult (..)
  , validate

    -- ** Tracing
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
  ) where

import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (..)
  , runExcept
  , runExceptT
  )
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.ResourceRegistry
import Data.Kind
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point in the LedgerDB, which can be advanced to
-- evaluate forks in the chain.
type Forker :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Forker m l blk = Forker
  { forkerClose :: !(m ())
  -- ^ Close the current forker (idempotent).
  --
  -- Other functions on forkers should throw a 'ClosedForkError' once the
  -- forker is closed.
  --
  -- Note: always use this functions before the forker is forgotten!
  -- Otherwise, cleanup of (on-disk) state might not be prompt or guaranteed.
  --
  -- This function should release any resources that are held by the forker,
  -- and not by the LedgerDB.
  , -- Queries

    forkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  -- ^ Read ledger tables from disk.
  , forkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK))
  -- ^ Range-read ledger tables from disk.
  --
  -- This range read will return as many values as the 'QueryBatchSize' that
  -- was passed when opening the LedgerDB.
  , forkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ Get the full ledger state without tables.
  --
  -- If an empty ledger state is all you need, use 'getVolatileTip',
  -- 'getImmutableTip', or 'getPastLedgerState' instead of using a 'Forker'.
  , forkerReadStatistics :: !(m (Maybe Statistics))
  -- ^ Get statistics about the current state of the handle if possible.
  --
  -- Returns 'Nothing' if the implementation is backed by @lsm-tree@.
  , -- Updates

    forkerPush :: !(l DiffMK -> m ())
  -- ^ Advance the fork handle by pushing a new ledger state to the tip of the
  -- current fork.
  , forkerCommit :: !(STM m ())
  -- ^ Commit the fork, which was constructed using 'forkerPush', as the
  -- current version of the LedgerDB.
  }
  deriving Generic
  deriving NoThunks via OnlyCheckWhnf (Forker m l blk)

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks, Num)

type instance HeaderHash (Forker m l blk) = HeaderHash l

type Forker' m blk = Forker m (ExtLedgerState blk) blk

instance
  (GetTip l, HeaderHash l ~ HeaderHash blk, MonadSTM m) =>
  GetTipSTM m (Forker m l blk)
  where
  getTipSTM forker = castPoint . getTip <$> forkerGetLedgerState forker

data RangeQueryPrevious l = NoPreviousQuery | PreviousQueryWasFinal | PreviousQueryWasUpTo (TxIn l)

castRangeQueryPrevious :: TxIn l ~ TxIn l' => RangeQueryPrevious l -> RangeQueryPrevious l'
castRangeQueryPrevious NoPreviousQuery = NoPreviousQuery
castRangeQueryPrevious PreviousQueryWasFinal = PreviousQueryWasFinal
castRangeQueryPrevious (PreviousQueryWasUpTo txin) = PreviousQueryWasUpTo txin

data RangeQuery l = RangeQuery
  { rqPrev :: !(RangeQueryPrevious l)
  , rqCount :: !Int
  }

-- | This type captures the size of the ledger tables at a particular point in
-- the LedgerDB.
--
-- This is for now the only metric that was requested from other components, but
-- this type might be augmented in the future with more statistics.
newtype Statistics = Statistics
  { ledgerTableSize :: Int
  }

-- | Errors that can be thrown while acquiring forkers.
data GetForkerError
  = -- | The requested point was not found in the LedgerDB, but the point is
    -- recent enough that the point is not in the immutable part of the chain,
    -- i.e. it belongs to an unselected fork.
    PointNotOnChain
  | -- | The requested point was not found in the LedgerDB because the point
    -- older than the immutable tip.
    PointTooOld !(Maybe ExceededRollback)
  deriving (Show, Eq)

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback
  { rollbackMaximum :: Word64
  , rollbackRequested :: Word64
  }
  deriving (Show, Eq)

forkerCurrentPoint ::
  (GetTip l, HeaderHash l ~ HeaderHash blk, Functor (STM m)) =>
  Forker m l blk ->
  STM m (Point blk)
forkerCurrentPoint forker =
  castPoint
    . getTip
    <$> forkerGetLedgerState forker

ledgerStateReadOnlyForker ::
  IOLike m => ReadOnlyForker m (ExtLedgerState blk) blk -> ReadOnlyForker m (LedgerState blk) blk
ledgerStateReadOnlyForker frk =
  ReadOnlyForker
    { roforkerClose = roforkerClose
    , roforkerReadTables = fmap castLedgerTables . roforkerReadTables . castLedgerTables
    , roforkerRangeReadTables = fmap castLedgerTables . roforkerRangeReadTables . castRangeQueryPrevious
    , roforkerGetLedgerState = ledgerState <$> roforkerGetLedgerState
    , roforkerReadStatistics = roforkerReadStatistics
    }
 where
  ReadOnlyForker
    { roforkerClose
    , roforkerReadTables
    , roforkerRangeReadTables
    , roforkerGetLedgerState
    , roforkerReadStatistics
    } = frk

{-------------------------------------------------------------------------------
  Read-only forkers
-------------------------------------------------------------------------------}

-- | Read-only 'Forker'.
--
-- These forkers are not allowed to commit. They are used everywhere except in
-- Chain Selection. In particular they are now used in:
--
-- - LocalStateQuery server, via 'getReadOnlyForkerAtPoint'
--
-- - Forging loop.
--
-- - Mempool.
type ReadOnlyForker :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data ReadOnlyForker m l blk = ReadOnlyForker
  { roforkerClose :: !(m ())
  -- ^ See 'forkerClose'
  , roforkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  -- ^ See 'forkerReadTables'
  , roforkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK))
  -- ^ See 'forkerRangeReadTables'.
  , roforkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ See 'forkerGetLedgerState'
  , roforkerReadStatistics :: !(m (Maybe Statistics))
  -- ^ See 'forkerReadStatistics'
  }
  deriving Generic

instance NoThunks (ReadOnlyForker m l blk) where
  wNoThunks _ _ = pure Nothing
  showTypeOf _ = "ReadOnlyForker"

type instance HeaderHash (ReadOnlyForker m l blk) = HeaderHash l

type ReadOnlyForker' m blk = ReadOnlyForker m (ExtLedgerState blk) blk

readOnlyForker :: Forker m l blk -> ReadOnlyForker m l blk
readOnlyForker forker =
  ReadOnlyForker
    { roforkerClose = forkerClose forker
    , roforkerReadTables = forkerReadTables forker
    , roforkerRangeReadTables = forkerRangeReadTables forker
    , roforkerGetLedgerState = forkerGetLedgerState forker
    , roforkerReadStatistics = forkerReadStatistics forker
    }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateArgs m blk = ValidateArgs
  { resolve :: !(ResolveBlock m blk)
  -- ^ How to retrieve blocks from headers
  , validateConfig :: !(TopLevelConfig blk)
  -- ^ The config
  , addPrevApplied :: !([RealPoint blk] -> STM m ())
  -- ^ How to add a previously applied block to the set of known blocks
  , prevApplied :: !(STM m (Set (RealPoint blk)))
  -- ^ Get the current set of previously applied blocks
  , forkerAtFromTip :: !(ResourceRegistry m -> Word64 -> m (Either GetForkerError (Forker' m blk)))
  -- ^ Create a forker from the tip
  , resourceReg :: !(ResourceRegistry m)
  -- ^ The resource registry
  , trace :: !(TraceValidateEvent blk -> m ())
  -- ^ A tracer for validate events
  , blockCache :: BlockCache blk
  -- ^ The block cache
  , numRollbacks :: Word64
  -- ^ How many blocks to roll back before applying the blocks
  , hdrs :: [Header blk]
  -- ^ The headers we want to apply
  }

validate ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  ) =>
  ComputeLedgerEvents ->
  ValidateArgs m blk ->
  m (ValidateResult' m blk)
validate evs args = do
  aps <- mkAps <$> atomically prevApplied
  res <-
    fmap rewrap $
      defaultResolveWithErrors resolve $
        switch
          forkerAtFromTip
          resourceReg
          evs
          (ExtLedgerCfg validateConfig)
          numRollbacks
          (lift . lift . trace)
          aps
  liftBase $ atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint hdrs))
  return res
 where
  ValidateArgs
    { resolve
    , validateConfig
    , addPrevApplied
    , prevApplied
    , forkerAtFromTip
    , resourceReg
    , trace
    , blockCache
    , numRollbacks
    , hdrs
    } = args

  rewrap ::
    Either (AnnLedgerError' n blk) (Either GetForkerError (Forker' n blk)) ->
    ValidateResult' n blk
  rewrap (Left e) = ValidateLedgerError e
  rewrap (Right (Left (PointTooOld (Just e)))) = ValidateExceededRollBack e
  rewrap (Right (Left _)) = error "Unreachable, validating will always rollback from the tip"
  rewrap (Right (Right l)) = ValidateSuccessful l

  mkAps ::
    forall bn n l.
    l ~ ExtLedgerState blk =>
    Set (RealPoint blk) ->
    [ Ap
        bn
        n
        l
        blk
        ( ResolvesBlocks n blk
        , ThrowsLedgerError bn n l blk
        )
    ]
  mkAps prev =
    [ case ( Set.member (headerRealPoint hdr) prev
           , BlockCache.lookup (headerHash hdr) blockCache
           ) of
        (False, Nothing) -> ApplyRef (headerRealPoint hdr)
        (True, Nothing) -> Weaken $ ReapplyRef (headerRealPoint hdr)
        (False, Just blk) -> Weaken $ ApplyVal blk
        (True, Just blk) -> Weaken $ ReapplyVal blk
    | hdr <- hdrs
    ]

  -- \| Based on the 'ValidateResult', return the hashes corresponding to
  -- valid blocks.
  validBlockPoints :: forall n. ValidateResult' n blk -> [RealPoint blk] -> [RealPoint blk]
  validBlockPoints = \case
    ValidateExceededRollBack _ -> const []
    ValidateSuccessful _ -> id
    ValidateLedgerError e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch ::
  (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm) =>
  (ResourceRegistry bm -> Word64 -> bm (Either GetForkerError (Forker bm l blk))) ->
  ResourceRegistry bm ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  -- | How many blocks to roll back
  Word64 ->
  (TraceValidateEvent blk -> m ()) ->
  -- | New blocks to apply
  [Ap bm m l blk c] ->
  m (Either GetForkerError (Forker bm l blk))
switch forkerAtFromTip rr evs cfg numRollbacks trace newBlocks = do
  foEith <- liftBase $ forkerAtFromTip rr numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Left rbExceeded
    Right fo -> do
      case newBlocks of
        [] -> pure ()
        -- no blocks to apply to ledger state, return the forker
        (firstBlock : _) -> do
          let start = PushStart . toRealPoint $ firstBlock
              goal = PushGoal . toRealPoint . last $ newBlocks
          void $
            applyThenPushMany
              (trace . StartedPushingBlockToTheLedgerDb start goal)
              evs
              cfg
              newBlocks
              fo
      pure $ Right fo

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

newtype ValidLedgerState l = ValidLedgerState {getValidLedgerState :: l}

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
type Ap :: (Type -> Type) -> (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type
data Ap bm m l blk c where
  ReapplyVal :: blk -> Ap bm m l blk ()
  ApplyVal :: blk -> Ap bm m l blk (ThrowsLedgerError bm m l blk)
  ReapplyRef :: RealPoint blk -> Ap bm m l blk (ResolvesBlocks m blk)
  ApplyRef ::
    RealPoint blk ->
    Ap
      bm
      m
      l
      blk
      ( ResolvesBlocks m blk
      , ThrowsLedgerError bm m l blk
      )
  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap bm m l blk c -> Ap bm m l blk c'

toRealPoint :: HasHeader blk => Ap bm m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk) = blockRealPoint blk
toRealPoint (ReapplyRef rp) = rp
toRealPoint (ApplyRef rp) = rp
toRealPoint (Weaken ap) = toRealPoint ap

-- | Apply blocks to the given forker
applyBlock ::
  forall m bm c l blk.
  (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm) =>
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap bm m l blk c ->
  Forker bm l blk ->
  m (ValidLedgerState (l DiffMK))
applyBlock evs cfg ap fo = case ap of
  ReapplyVal b ->
    ValidLedgerState
      <$> withValues b (return . tickThenReapply evs cfg b)
  ApplyVal b ->
    ValidLedgerState
      <$> withValues
        b
        ( either (throwLedgerError fo (blockRealPoint b)) return
            . runExcept
            . tickThenApply evs cfg b
        )
  ReapplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ReapplyVal b) fo
  ApplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ApplyVal b) fo
  Weaken ap' ->
    applyBlock evs cfg ap' fo
 where
  withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (l DiffMK)
  withValues blk f = do
    l <- liftBase $ atomically $ forkerGetLedgerState fo
    vs <-
      withLedgerTables l
        <$> liftBase (forkerReadTables fo (getBlockKeySets blk))
    f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
applyThenPush ::
  (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm) =>
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap bm m l blk c ->
  Forker bm l blk ->
  m ()
applyThenPush evs cfg ap fo =
  liftBase . forkerPush fo . getValidLedgerState
    =<< applyBlock evs cfg ap fo

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany ::
  (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm) =>
  (Pushing blk -> m ()) ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  [Ap bm m l blk c] ->
  Forker bm l blk ->
  m ()
applyThenPushMany trace evs cfg aps fo = mapM_ pushAndTrace aps
 where
  pushAndTrace ap = do
    trace $ Pushing . toRealPoint $ ap
    applyThenPush evs cfg ap fo

{-------------------------------------------------------------------------------
  Annotated ledger errors
-------------------------------------------------------------------------------}

class Monad m => ThrowsLedgerError bm m l blk where
  throwLedgerError :: Forker bm l blk -> RealPoint blk -> LedgerErr l -> m a

instance Monad m => ThrowsLedgerError bm (ExceptT (AnnLedgerError bm l blk) m) l blk where
  throwLedgerError f l r = throwError $ AnnLedgerError f l r

defaultThrowLedgerErrors ::
  ExceptT (AnnLedgerError bm l blk) m a ->
  m (Either (AnnLedgerError bm l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors ::
  ResolveBlock m blk ->
  ExceptT
    (AnnLedgerError bm l blk)
    (ReaderT (ResolveBlock m blk) m)
    a ->
  m (Either (AnnLedgerError bm l blk) a)
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

defaultResolveBlocks ::
  ResolveBlock m blk ->
  ReaderT (ResolveBlock m blk) m a ->
  m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance
  Monad m =>
  ResolvesBlocks (ExceptT e (ReaderT (ResolveBlock m blk) m)) blk
  where
  doResolveBlock = lift . doResolveBlock

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | When validating a sequence of blocks, these are the possible outcomes.
data ValidateResult m l blk
  = ValidateSuccessful (Forker m l blk)
  | ValidateLedgerError (AnnLedgerError m l blk)
  | ValidateExceededRollBack ExceededRollback

type ValidateResult' m blk = ValidateResult m (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError m l blk = AnnLedgerError
  { annLedgerState :: Forker m l blk
  -- ^ The ledger DB just /before/ this block was applied
  , annLedgerErrRef :: RealPoint blk
  -- ^ Reference to the block that had the error
  , annLedgerErr :: LedgerErr l
  -- ^ The ledger error itself
  }

type AnnLedgerError' m blk = AnnLedgerError m (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  Trace validation events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart {unPushStart :: RealPoint blk}
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal {unPushGoal :: RealPoint blk}
  deriving (Show, Eq)

newtype Pushing blk = Pushing {unPushing :: RealPoint blk}
  deriving (Show, Eq)

data TraceValidateEvent blk
  = -- | Event fired when we are about to push a block to a forker
    StartedPushingBlockToTheLedgerDb
      -- | Point from which we started pushing new blocks
      !(PushStart blk)
      -- | Point to which we are updating the ledger, the last event
      -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
      -- wrapping over the same RealPoint
      (PushGoal blk)
      -- | Point which block we are about to push
      !(Pushing blk)
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Forker events
-------------------------------------------------------------------------------}

data TraceForkerEventWithKey
  = TraceForkerEventWithKey ForkerKey TraceForkerEvent
  deriving (Show, Eq)

data TraceForkerEvent
  = ForkerOpen
  | ForkerCloseUncommitted
  | ForkerCloseCommitted
  | ForkerReadTablesStart
  | ForkerReadTablesEnd
  | ForkerRangeReadTablesStart
  | ForkerRangeReadTablesEnd
  | ForkerReadStatistics
  | ForkerPushStart
  | ForkerPushEnd
  | DanglingForkerClosed
  deriving (Show, Eq)
