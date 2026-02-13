{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.ResourceRegistry
import Data.Bifunctor (first)
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose
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

    forkerPush :: !(l DiffMK -> m ())
  -- ^ Advance the fork handle by pushing a new ledger state to the tip of the
  -- current fork.
  , forkerCommit :: !(STM m ())
  -- ^ Commit the fork, which was constructed using 'forkerPush', as the
  -- current version of the LedgerDB.
  }
  deriving Generic
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
    }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateArgs m l blk = ValidateArgs
  { resolve :: !(ResolveBlock m blk)
  -- ^ How to retrieve blocks from headers
  , validateConfig :: !(LedgerCfg l)
  -- ^ The config
  , addPrevApplied :: !([RealPoint blk] -> STM m ())
  -- ^ How to add a previously applied block to the set of known blocks
  , prevApplied :: !(STM m (Set (RealPoint blk)))
  -- ^ Get the current set of previously applied blocks
  , forkerAtFromTip :: !(ResourceRegistry m -> Word64 -> m (Either GetForkerError (Forker m l)))
  -- ^ Create a forker from the tip
  , resourceReg :: !(ResourceRegistry m)
  -- ^ The resource registry
  , trace :: !(TraceValidateEvent blk -> m ())
  -- ^ A tracer for validate events
  , blockCache :: BlockCache blk
  -- ^ The block cache
  , numRollbacks :: Word64
  -- ^ How many blocks to roll back before applying the blocks
  , hdrs :: NonEmpty (Header blk)
  -- ^ The headers we want to apply
  }

validate ::
  forall m l blk.
  ( IOLike m
  , HasCallStack
  , ApplyBlock l blk
  ) =>
  ComputeLedgerEvents ->
  ValidateArgs m l blk ->
  m (ValidateResult m l blk)
validate evs args = do
  aps <- mkAps <$> atomically prevApplied
  res <-
    fmap rewrap $
      switch
        forkerAtFromTip
        resourceReg
        evs
        validateConfig
        numRollbacks
        trace
        aps
        resolve
  atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint $ NE.toList hdrs))
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
    Either (AnnLedgerError l blk) (Either GetForkerError (Forker n l)) ->
    ValidateResult n l blk
  rewrap (Left e) = ValidateLedgerError e
  rewrap (Right (Left (PointTooOld (Just e)))) = ValidateExceededRollBack e
  rewrap (Right (Left _)) = error "Unreachable, validating will always rollback from the tip"
  rewrap (Right (Right l)) = ValidateSuccessful l

  mkAps ::
    Set (RealPoint blk) ->
    NonEmpty (Ap m l blk)
  mkAps prev =
    NE.map
      ( \hdr -> case ( Set.member (headerRealPoint hdr) prev
                     , BlockCache.lookup (headerHash hdr) blockCache
                     ) of
          (False, Nothing) -> ApplyRef (headerRealPoint hdr)
          (True, Nothing) -> ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> ApplyVal blk
          (True, Just blk) -> ReapplyVal blk
      )
      hdrs

  -- \| Based on the 'ValidateResult', return the hashes corresponding to
  -- valid blocks.
  validBlockPoints :: ValidateResult m l blk -> [RealPoint blk] -> [RealPoint blk]
  validBlockPoints = \case
    ValidateExceededRollBack _ -> const []
    ValidateSuccessful _ -> id
    ValidateLedgerError e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch ::
  (ApplyBlock l blk, MonadSTM m) =>
  (ResourceRegistry m -> Word64 -> m (Either GetForkerError (Forker m l))) ->
  ResourceRegistry m ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  -- | How many blocks to roll back
  Word64 ->
  (TraceValidateEvent blk -> m ()) ->
  -- | New blocks to apply
  NonEmpty (Ap m l blk) ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) (Either GetForkerError (Forker m l)))
switch forkerAtFromTip rr evs cfg numRollbacks trace newBlocks doResolve = do
  foEith <- forkerAtFromTip rr numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Right $ Left rbExceeded
    Right fo -> do
      let start = PushStart . toRealPoint . NE.head $ newBlocks
          goal = PushGoal . toRealPoint . NE.last $ newBlocks
      ePush <-
        applyThenPushMany
          (trace . StartedPushingBlockToTheLedgerDb start goal)
          evs
          cfg
          (NE.toList newBlocks)
          fo
          doResolve
      case ePush of
        Left err -> forkerClose fo >> pure (Left err)
        Right () -> pure $ Right $ Right fo

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors provide answers to two questions:
--
--     1. Are we passing the block by value or by reference?
--
--     2. Are we applying or reapplying the block?
type Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Ap m l blk where
  ReapplyVal :: blk -> Ap m l blk
  ApplyVal :: blk -> Ap m l blk
  ReapplyRef :: RealPoint blk -> Ap m l blk
  ApplyRef :: RealPoint blk -> Ap m l blk

toRealPoint :: HasHeader blk => Ap m l blk -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk) = blockRealPoint blk
toRealPoint (ReapplyRef rp) = rp
toRealPoint (ApplyRef rp) = rp

-- | Apply blocks to the given forker
applyBlock ::
  forall m l blk.
  (ApplyBlock l blk, MonadSTM m) =>
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap m l blk ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) (l DiffMK))
applyBlock evs cfg ap fo doResolveBlock = case ap of
  ReapplyVal b ->
    withValues b (return . Right . tickThenReapply evs cfg b)
  ApplyVal b ->
    withValues
      b
      ( \v ->
          case runExcept $ tickThenApply evs cfg b v of
            Left lerr -> pure (Left (AnnLedgerError (castPoint $ getTip v) (blockRealPoint b) lerr))
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
    (l ValuesMK -> m (Either (AnnLedgerError l blk) (l DiffMK))) ->
    m (Either (AnnLedgerError l blk) (l DiffMK))
  withValues blk f = do
    l <- atomically $ forkerGetLedgerState fo
    vs <- withLedgerTables l <$> forkerReadTables fo (getBlockKeySets blk)
    f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
applyThenPush ::
  (ApplyBlock l blk, MonadSTM m) =>
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap m l blk ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) ())
applyThenPush evs cfg ap fo doResolve = do
  eLerr <- applyBlock evs cfg ap fo doResolve
  case eLerr of
    Left err -> pure (Left err)
    Right st -> Right <$> forkerPush fo st

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany ::
  (ApplyBlock l blk, MonadSTM m) =>
  (Pushing blk -> m ()) ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  [Ap m l blk] ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) ())
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

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError
  { annLedgerBaseRef :: Point blk
  -- ^ The last block that was valid
  , annLedgerErrRef :: RealPoint blk
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
