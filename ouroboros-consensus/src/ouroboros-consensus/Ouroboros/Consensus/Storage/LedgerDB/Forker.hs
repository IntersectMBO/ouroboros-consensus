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
  , ForkerWasCommitted (..)

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

import Control.Monad.Except
  ( runExcept
  )
import Data.Bifunctor (first)
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
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.EscapableResources
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point in the LedgerDB, which can be advanced to
-- evaluate forks in the chain.
type Forker :: (Type -> Type) -> LedgerStateKind -> Type
data Forker m l = Forker
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
  , forkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l)))
  -- ^ Range-read ledger tables from disk.
  --
  -- This range read will return as many values as the 'QueryBatchSize' that was
  -- passed when opening the LedgerDB.
  --
  -- The second component of the returned tuple is the maximal key found by the
  -- forker. This is only necessary because some backends have a different
  -- sorting for the keys than the order defined in Haskell.
  --
  -- The last key retrieved is part of the map too. It is intended to be fed
  -- back into the next iteration of the range read. If the function returns
  -- Nothing, it means the read returned no results, or in other words, we
  -- reached the end of the ledger tables.
  , forkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ Get the full ledger state without tables.
  --
  -- If an empty ledger state is all you need, use 'getVolatileTip',
  -- 'getImmutableTip', or 'getPastLedgerState' instead of using a 'Forker'.
  , forkerReadStatistics :: !(m Statistics)
  -- ^ Get statistics about the current state of the handle if possible.
  --
  -- Returns 'Nothing' if the implementation is backed by @lsm-tree@.
  , -- Updates

    forkerPush :: !(forall r. l DiffMK -> ContT r m ())
  -- ^ Advance the fork handle by pushing a new ledger state to the tip of the
  -- current fork.
  , forkerCommit :: !(STM m ())
  -- ^ Commit the fork, which was constructed using 'forkerPush', as the
  -- current version of the LedgerDB.
  , forkerUntrack :: !(STM m ())
  }
  deriving NoThunks via OnlyCheckWhnf (Forker m l)

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks, Num)

type instance HeaderHash (Forker m l) = HeaderHash l

type Forker' m blk = Forker m (ExtLedgerState blk)

instance
  (GetTip l, MonadSTM m) =>
  GetTipSTM m (Forker m l)
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
  Proxy blk ->
  Forker m l ->
  STM m (Point blk)
forkerCurrentPoint _ forker =
  castPoint
    . getTip
    <$> forkerGetLedgerState forker

ledgerStateReadOnlyForker ::
  IOLike m => ReadOnlyForker m (ExtLedgerState blk) -> ReadOnlyForker m (LedgerState blk)
ledgerStateReadOnlyForker frk =
  ReadOnlyForker
    { roforkerClose = roforkerClose
    , roforkerReadTables = fmap castLedgerTables . roforkerReadTables . castLedgerTables
    , roforkerRangeReadTables =
        fmap (first castLedgerTables) . roforkerRangeReadTables . castRangeQueryPrevious
    , roforkerGetLedgerState = ledgerState <$> roforkerGetLedgerState
    , roforkerReadStatistics = roforkerReadStatistics
    , roforkerUntrack = roforkerUntrack
    }
 where
  ReadOnlyForker
    { roforkerClose
    , roforkerReadTables
    , roforkerRangeReadTables
    , roforkerGetLedgerState
    , roforkerReadStatistics
    , roforkerUntrack
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
type ReadOnlyForker :: (Type -> Type) -> LedgerStateKind -> Type
data ReadOnlyForker m l = ReadOnlyForker
  { roforkerClose :: !(m ())
  -- ^ See 'forkerClose'
  , roforkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  -- ^ See 'forkerReadTables'
  , roforkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l)))
  -- ^ See 'forkerRangeReadTables'.
  , roforkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ See 'forkerGetLedgerState'
  , roforkerReadStatistics :: !(m Statistics)
  -- ^ See 'forkerReadStatistics'
  , roforkerUntrack :: !(STM m ())
  }
  deriving Generic

instance NoThunks (ReadOnlyForker m l) where
  wNoThunks _ _ = pure Nothing
  showTypeOf _ = "ReadOnlyForker"

type instance HeaderHash (ReadOnlyForker m l) = HeaderHash l

type ReadOnlyForker' m blk = ReadOnlyForker m (ExtLedgerState blk)

readOnlyForker :: Forker m l -> ReadOnlyForker m l
readOnlyForker forker =
  ReadOnlyForker
    { roforkerClose = forkerClose forker
    , roforkerReadTables = forkerReadTables forker
    , roforkerRangeReadTables = forkerRangeReadTables forker
    , roforkerGetLedgerState = forkerGetLedgerState forker
    , roforkerReadStatistics = forkerReadStatistics forker
    , roforkerUntrack = forkerUntrack forker
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
  , forkerAtFromTip :: !(forall r. Word64 -> ContT r m (Either GetForkerError (Forker' m blk)))
  -- ^ Create a forker from the tip
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
  forall m r blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  ) =>
  ComputeLedgerEvents ->
  ValidateArgs m blk ->
  ContT r m (ValidateResult' m blk)
validate evs args = do
  aps <- lift $ mkAps <$> atomically prevApplied
  res <-
    fmap rewrap $
      switch
        forkerAtFromTip
        evs
        (ExtLedgerCfg validateConfig)
        numRollbacks
        (lift . trace)
        aps
        resolve

  lift $ atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint hdrs))
  return res
 where
  ValidateArgs
    { resolve
    , validateConfig
    , addPrevApplied
    , prevApplied
    , forkerAtFromTip
    , trace
    , blockCache
    , numRollbacks
    , hdrs
    } = args

  rewrap ::
    Either (Either GetForkerError (AnnLedgerError' blk)) (Forker' m blk) ->
    ValidateResult' m blk
  rewrap (Left (Right e)) = ValidateLedgerError e
  rewrap (Left (Left (PointTooOld (Just e)))) = ValidateExceededRollBack e
  rewrap (Left (Left _)) = error "Unreachable, validating will always rollback from the tip"
  rewrap (Right l) = ValidateSuccessful l

  mkAps ::
    Set (RealPoint blk) ->
    [ Ap
        m
        (ExtLedgerState blk)
        blk
    ]
  mkAps prev =
    [ case ( Set.member (headerRealPoint hdr) prev
           , BlockCache.lookup (headerHash hdr) blockCache
           ) of
        (False, Nothing) -> ApplyRef (headerRealPoint hdr)
        (True, Nothing) -> ReapplyRef (headerRealPoint hdr)
        (False, Just blk) -> ApplyVal blk
        (True, Just blk) -> ReapplyVal blk
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
  (ApplyBlock (ExtLedgerState blk) blk, MonadSTM m) =>
  (Word64 -> ContT r m (Either GetForkerError (Forker m (ExtLedgerState blk)))) ->
  ComputeLedgerEvents ->
  LedgerCfg (ExtLedgerState blk) ->
  -- | How many blocks to roll back
  Word64 ->
  (TraceValidateEvent blk -> ContT r m ()) ->
  -- | New blocks to apply
  [Ap m (ExtLedgerState blk) blk] ->
  ResolveBlock m blk ->
  ContT r m (Either (Either GetForkerError (AnnLedgerError' blk)) (Forker m (ExtLedgerState blk)))
switch forkerAtFromTip _ _ numRollbacks _ [] _ = do
  foEith <- forkerAtFromTip numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Left $ Left rbExceeded
    Right frk -> pure $ Right frk
switch forkerAtFromTip evs cfg numRollbacks trace newBlocks@(firstBlock : _) doResolveBlock = do
  foEith <- forkerAtFromTip numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Left $ Left rbExceeded
    Right frk -> do
      let start = PushStart . toRealPoint $ firstBlock
          goal = PushGoal . toRealPoint . last $ newBlocks
      ePush <-
        applyThenPushMany
          (trace . StartedPushingBlockToTheLedgerDb start goal)
          evs
          cfg
          newBlocks
          frk
          doResolveBlock
      case ePush of
        Left err -> pure $ Left $ Right err
        Right () -> pure $ Right frk

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

newtype ValidLedgerState l = ValidLedgerState l

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
type Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Ap m l blk where
  ReapplyVal :: blk -> Ap m l blk
  ApplyVal :: blk -> Ap m l blk
  ReapplyRef :: RealPoint blk -> Ap m l blk
  ApplyRef ::
    RealPoint blk ->
    Ap
      m
      l
      blk

toRealPoint :: HasHeader blk => Ap m l blk -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk) = blockRealPoint blk
toRealPoint (ReapplyRef rp) = rp
toRealPoint (ApplyRef rp) = rp

-- | Apply blocks to the given forker
applyBlock ::
  forall m blk.
  (ApplyBlock (ExtLedgerState blk) blk, MonadSTM m) =>
  ComputeLedgerEvents ->
  LedgerCfg (ExtLedgerState blk) ->
  Ap m (ExtLedgerState blk) blk ->
  Forker m (ExtLedgerState blk) ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError' blk) (ValidLedgerState (ExtLedgerState blk DiffMK)))
applyBlock evs cfg ap fo doResolveBlock = case ap of
  ReapplyVal b ->
    fmap ValidLedgerState <$> withValues b (return . Right . tickThenReapply evs cfg b)
  ApplyVal b ->
    fmap ValidLedgerState
      <$> withValues
        b
        ( \v ->
            case runExcept $ tickThenApply evs cfg b v of
              Left lerr -> pure (Left (AnnLedgerError (blockRealPoint b) lerr))
              Right st -> pure (Right st)
        )
  ReapplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ReapplyVal b) fo doResolveBlock
  ApplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ApplyVal b) fo doResolveBlock
 where
  withValues ::
    blk ->
    (ExtLedgerState blk ValuesMK -> m (Either (AnnLedgerError' blk) (ExtLedgerState blk DiffMK))) ->
    m (Either (AnnLedgerError' blk) (ExtLedgerState blk DiffMK))
  withValues blk f = do
    l <- atomically $ forkerGetLedgerState fo
    vs <-
      withLedgerTables l
        <$> forkerReadTables fo (getBlockKeySets blk)
    f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
applyThenPush ::
  (ApplyBlock (ExtLedgerState blk) blk, MonadSTM m) =>
  ComputeLedgerEvents ->
  LedgerCfg (ExtLedgerState blk) ->
  Ap m (ExtLedgerState blk) blk ->
  Forker m (ExtLedgerState blk) ->
  ResolveBlock m blk ->
  ContT r m (Either (AnnLedgerError' blk) ())
applyThenPush evs cfg ap fo doResolveBlock = do
  eLerr <- lift $ applyBlock evs cfg ap fo doResolveBlock
  case eLerr of
    Left err -> pure (Left err)
    Right (ValidLedgerState st) -> Right <$> forkerPush fo st

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany ::
  (ApplyBlock (ExtLedgerState blk) blk, MonadSTM m) =>
  (Pushing blk -> ContT r m ()) ->
  ComputeLedgerEvents ->
  LedgerCfg (ExtLedgerState blk) ->
  [Ap m (ExtLedgerState blk) blk] ->
  Forker m (ExtLedgerState blk) ->
  ResolveBlock m blk ->
  ContT r m (Either (AnnLedgerError' blk) ())
applyThenPushMany trace evs cfg aps fo doResolveBlock = pushAndTrace aps
 where
  pushAndTrace [] = pure $ Right ()
  pushAndTrace (ap : aps') = do
    trace $ Pushing . toRealPoint $ ap
    res <- applyThenPush evs cfg ap fo doResolveBlock
    case res of
      Left err -> pure (Left err)
      Right () -> pushAndTrace aps'

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

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | When validating a sequence of blocks, these are the possible outcomes.
data ValidateResult m l blk
  = ValidateSuccessful (Forker m l)
  | ValidateLedgerError (AnnLedgerError l blk)
  | ValidateExceededRollBack ExceededRollback

type ValidateResult' m blk = ValidateResult m (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError
  { annLedgerErrRef :: RealPoint blk
  -- ^ Reference to the block that had the error
  , annLedgerErr :: LedgerErr l
  -- ^ The ledger error itself
  }

type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

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
  | ForkerReadTables EnclosingTimed
  | ForkerRangeReadTables EnclosingTimed
  | ForkerReadStatistics
  | ForkerPush EnclosingTimed
  | ForkerClose ForkerWasCommitted
  deriving (Show, Eq)

data ForkerWasCommitted
  = ForkerWasCommitted
  | ForkerWasUncommitted
  deriving (Eq, Show)
