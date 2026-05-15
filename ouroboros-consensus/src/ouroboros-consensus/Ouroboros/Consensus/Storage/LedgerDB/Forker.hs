{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , GetForkerError (..)
  , forkerCommit
  , forkerTip
  , forkerPush
  , forkerClose

    -- * Validation
  , AnnLedgerError (..)
  , AnnLedgerError'
  , ResolveBlock
  , SuccessForkerAction (..)
  , ValidateArgs (..)
  , ValidateResult (..)
  , validate

    -- ** Tracing
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
  , ForkerKey (..)
  , TraceForkerEventWithKey (..)
  , TraceForkerEvent (..)
  ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.RAWLock
import Control.Tracer
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

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

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks, Num)

data Forker m l blk = Forker
  { foeLedgerSeq :: !(StrictTVar m (LedgerSeq m l blk))
  , foeSwitchVar :: !(StrictTVar m (LedgerSeq m l blk))
  , foeWasCommitted :: !(StrictTVar m Bool)
  , foeLedgerDbLock :: !(RAWLock m ())
  , foeTracer :: !(Tracer m TraceForkerEvent)
  }

-- TODO @js we were also pruning here, what happened with that?
forkerPush ::
  (GetTip l blk, MonadSTM m, StateRefHasState m l blk) =>
  Forker m l blk -> StateRef m l blk -> STM m ()
forkerPush (Forker frk _ _ _ _) st = modifyTVar frk (extend st)

forkerTip ::
  (GetTip l blk, MonadSTM m, StateRefHasState m l blk) => Forker m l blk -> STM m (StateRef m l blk)
forkerTip (Forker frk _ _ _ _) = currentHandle <$> readTVar frk

-- | Will release all handles in the 'foeLedgerSeq', which will be only the
-- first duplicate if the forker has been committed.
forkerClose ::
  (IOLike m, StateRefHasState m l blk) =>
  Forker m l blk ->
  m ()
forkerClose env = do
  wasCommitted <- readTVarIO (foeWasCommitted env)
  if wasCommitted
    then
      traceWith (foeTracer env) (ForkerClose ForkerWasCommitted)
    else
      traceWith (foeTracer env) (ForkerClose ForkerWasUncommitted)
  closeLedgerSeq =<< readTVarIO (foeLedgerSeq env)

forkerCommit ::
  (IOLike m, GetTip l blk, StandardHash blk, StateRefHasState m l blk) =>
  Forker m l blk ->
  STM m (m ())
forkerCommit env = do
  wasCommitted <- readTVar (foeWasCommitted env)
  when wasCommitted $
    throw $
      CriticalInvariantViolation "Critical invariant violation: forker has been committed twice"
  ls@(LedgerSeq lseq) <- readTVar (foeLedgerSeq env)
  let intersectionSlot = getTipSlot $ anchor ls
  let predicate = (== getTipHash (anchor ls)) . getTipHash . state
  (toCloseForker, toCloseLdb) <-
    stateTVar
      (foeSwitchVar env)
      ( \(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (toKeepBase, toCloseLdb) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) toKeepBase lseq
          -- Do /not/ close the anchor of @toClose@, as that is also the
          -- tip of @olddb'@ which will be used in @newdb@.
          let ldbToClose = case toCloseLdb of
                AS.Empty _ -> Nothing
                _ AS.:< closeOld' -> Just (LedgerSeq closeOld')
          pure ((AS.anchor lseq, ldbToClose), LedgerSeq newdb)
      )
  writeTVar (foeWasCommitted env) True
  -- We put 'toCloseForker' in the LedgerSeq to then close it when closing the
  -- forker.
  writeTVar (foeLedgerSeq env) (LedgerSeq (AS.Empty toCloseForker))
  pure
    ( whenJust toCloseLdb $ \seqToClose ->
        withWriteAccess (foeLedgerDbLock env) $ \() -> do
          closeLedgerSeq seqToClose
          pure ((), ())
    )
 where
  theImpossible =
    throw $
      CriticalInvariantViolation $
        unwords
          [ "Critical invariant violation:"
          , "Forker chain does no longer intersect with selected chain."
          ]

newtype CriticalInvariantViolation = CriticalInvariantViolation {message :: String}
  deriving Show
  deriving anyclass Exception

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateArgs m l blk = ValidateArgs
  { resolve :: !(ResolveBlock m blk)
  -- ^ How to retrieve blocks from headers
  , validateConfig :: !(LedgerCfg l blk)
  -- ^ The config
  , addPrevApplied :: !([RealPoint blk] -> STM m ())
  -- ^ How to add a previously applied block to the set of known blocks
  , prevApplied :: !(STM m (Set (RealPoint blk)))
  -- ^ Get the current set of previously applied blocks
  , withForkerAtFromTip ::
      !(forall r. Word64 -> (Forker m l blk -> m r) -> m (Either GetForkerError r))
  -- ^ Create a forker from the tip
  , onSuccess :: !(SuccessForkerAction m l blk)
  -- ^ Continuation to run when the validation was successful
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
  , StateRefHasState m (Ticked l) blk
  , StateRefHasState m l blk
  ) =>
  ComputeLedgerEvents ->
  ValidateArgs m l blk ->
  m (ValidateResult l blk)
validate evs args = do
  aps <- mkAps <$> atomically prevApplied
  res <-
    rewrap
      <$> switch
        withForkerAtFromTip
        evs
        validateConfig
        numRollbacks
        trace
        aps
        resolve
        onSuccess
  atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint $ NE.toList hdrs))
  pure res
 where
  ValidateArgs
    { resolve
    , validateConfig
    , addPrevApplied
    , prevApplied
    , withForkerAtFromTip
    , trace
    , blockCache
    , numRollbacks
    , hdrs
    , onSuccess
    } = args

  rewrap ::
    Either GetForkerError (Either (AnnLedgerError l blk) ()) ->
    ValidateResult l blk
  rewrap (Right (Left e)) = ValidateLedgerError e
  rewrap (Left (PointTooOld (Just e))) = ValidateExceededRollBack e
  rewrap (Left _) = error "Unreachable, validating will always rollback from the tip"
  rewrap (Right (Right ())) = ValidateSuccessful

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
  validBlockPoints :: ValidateResult l blk -> [RealPoint blk] -> [RealPoint blk]
  validBlockPoints = \case
    ValidateExceededRollBack _ -> const []
    ValidateSuccessful -> id
    ValidateLedgerError e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch ::
  (ApplyBlock l blk, MonadSTM m, StateRefHasState m (Ticked l) blk, StateRefHasState m l blk) =>
  (forall r. Word64 -> (Forker m l blk -> m r) -> m (Either GetForkerError r)) ->
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  -- | How many blocks to roll back
  Word64 ->
  (TraceValidateEvent blk -> m ()) ->
  -- | New blocks to apply
  NonEmpty (Ap m l blk) ->
  ResolveBlock m blk ->
  SuccessForkerAction m l blk ->
  m (Either GetForkerError (Either (AnnLedgerError l blk) ()))
switch withForkerAtFromTip evs cfg numRollbacks trace newBlocks doResolve onSuccess = do
  withForkerAtFromTip numRollbacks $ \fo -> do
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
      Left err -> pure (Left err)
      Right () -> fmap Right $ applySuccessForkerAction onSuccess fo

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
type Ap :: (Type -> Type) -> (Type -> Type) -> Type -> Type
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
  (ApplyBlock l blk, MonadSTM m, StateRefHasState m (Ticked l) blk, StateRefHasState m l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  Ap m l blk ->
  Forker m l blk ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) (StateRef m l blk))
applyBlock evs cfg ap fo doResolveBlock = case ap of
  ReapplyVal b ->
    Right <$> (tickThenReapply evs cfg b =<< atomically (forkerTip fo))
  ApplyVal b -> do
    ftip <- atomically (forkerTip fo)
    (runExceptT $ tickThenApply evs cfg b ftip) >>= \case
      Left lerr -> pure (Left (AnnLedgerError (castPoint $ getTip $ state ftip) (blockRealPoint b) lerr))
      Right st -> pure (Right st)
  ReapplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ReapplyVal b) fo doResolveBlock
  ApplyRef r -> do
    b <- doResolveBlock r
    applyBlock evs cfg (ApplyVal b) fo doResolveBlock
 where

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
applyThenPush ::
  (ApplyBlock l blk, MonadSTM m, StateRefHasState m (Ticked l) blk, StateRefHasState m l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  Ap m l blk ->
  Forker m l blk ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) ())
applyThenPush evs cfg ap fo doResolve = do
  eLerr <- applyBlock evs cfg ap fo doResolve
  case eLerr of
    Left err -> pure (Left err)
    Right st -> Right <$> atomically (forkerPush fo st)

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany ::
  (ApplyBlock l blk, MonadSTM m, StateRefHasState m (Ticked l) blk, StateRefHasState m l blk) =>
  (Pushing blk -> m ()) ->
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  [Ap m l blk] ->
  Forker m l blk ->
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

-- | A helpful type for a callback of the validation logic
--
-- The latest iteration of the maintenance of backend resources held
-- by 'Forker's relies heavily on 'bracket'. For that reason, we end
-- up passing a "success continuation" through several layers of
-- interface, which runs inside of those brackets.
--
-- This type makes that continuation easier to recognize. In
-- particular, any continuation that ends with @res -> m ()@ is
-- commonly used as "how to close a @res@", which is *NOT* the case
-- here. So it's preferable to use this more perspicious type in
-- signatures.
newtype SuccessForkerAction m l blk = MkSuccessForkerAction
  { applySuccessForkerAction :: Forker m l blk -> m ()
  }

-- | When validating a sequence of blocks, these are the possible outcomes.
data ValidateResult l blk
  = ValidateSuccessful
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
  , annLedgerErr :: LedgerErr l blk
  -- ^ The ledger error itself
  }

type AnnLedgerError' blk = AnnLedgerError ExtLedgerState blk

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
