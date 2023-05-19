{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | Accessors for the DbChangelog and management
--
-- This module defines the operations that can be done on a DbChangelog, as well
-- as the procedures to apply a block to a DbChangelog and pushing the resulting
-- LedgerState into the DB.
module Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update (
    -- * Applying blocks
    AnnLedgerError (..)
  , AnnLedgerError'
  , Ap (..)
  , ExceededRollback (..)
  , ThrowsLedgerError (..)
  , defaultThrowLedgerErrors
    -- * Block resolution
  , ResolveBlock
  , ResolvesBlocks (..)
  , defaultResolveBlocks
    -- * Updates
  , DiffsToFlush (..)
  , defaultResolveWithErrors
  , extend
  , prune
  , push
  , pushMany
  , rollbackN
  , splitForFlushing
  , switch
  , volatileStatesBimap
    -- * Pure API
  , push'
  , pushMany'
  , switch'
    -- * Trace
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
  ) where

import           Cardano.Slotting.Slot
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Bifunctor (bimap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Semigroup (All (..), Sum (..))
import           Data.SOP.Functors
import           Data.Word
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query as Query
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Util
import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--   a. Are we passing the block by value or by reference?
--   b. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--   a. If we are passing a block by reference, we must be able to resolve it.
--   b. If we are applying rather than reapplying, we might have ledger errors.
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

{-------------------------------------------------------------------------------
  Internal utilities for 'Ap'
-------------------------------------------------------------------------------}

toRealPoint :: HasHeader blk => Ap m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'DbChangelog' because we record that as part of errors.
applyBlock :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> KeySetsReader m l
           -> DbChangelog l
           -> m (l DiffMK)
applyBlock cfg ap ksReader db = case ap of
    ReapplyVal b ->
      withValues b $ return . tickThenReapply cfg b
    ApplyVal b ->
      withValues b $
          either (throwLedgerError db (blockRealPoint b)) return
        . runExcept
        . tickThenApply cfg b
    ReapplyRef r  -> do
      b <- doResolveBlock r
      applyBlock cfg (ReapplyVal b) ksReader db
    ApplyRef r -> do
      b <- doResolveBlock r
      applyBlock cfg (ApplyVal b) ksReader db
    Weaken ap' ->
      applyBlock cfg ap' ksReader db
  where
    l :: l EmptyMK
    l = Query.current db

    withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (l DiffMK)
    withValues = withKeysReadSets l ksReader db . getBlockKeySets

{-------------------------------------------------------------------------------
  Resolving blocks maybe from disk
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
  A ledger error annotated with the DbChangelog
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: DbChangelog l

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

class Monad m => ThrowsLedgerError m l blk where
  throwLedgerError :: DbChangelog l -> RealPoint blk -> LedgerErr l -> m a

instance Monad m
      => ThrowsLedgerError (ExceptT (AnnLedgerError l blk) m) l blk where
  throwLedgerError l r e = throwError $ AnnLedgerError l r e

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
  DbChangelog management
-------------------------------------------------------------------------------}

-- | Transform the underlying 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> DbChangelog l
  -> AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap f g
    . changelogVolatileStates

-- | Prune ledger states until at we have at most @k@ in the DbChangelog,
-- excluding the one stored at the anchor.
prune :: GetTip l => SecurityParam -> DbChangelog l -> DbChangelog l
prune (SecurityParam k) dblog =
    dblog {
        changelogVolatileStates = vol'
      }
  where
    DbChangelog {
        changelogVolatileStates = vol
      } = dblog

    nvol = AS.length vol

    vol' =
      if toEnum nvol <= k
      then vol
      else snd $ AS.splitAt (nvol - fromEnum k) vol

 -- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
 -- 'DbChangelog' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE prune #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state
pushLedgerState ::
     (IsLedger l, HasLedgerTables l)
  => SecurityParam
  -> l DiffMK -- ^ Updated ledger state
  -> DbChangelog l
  -> DbChangelog l
pushLedgerState secParam =
    prune secParam .: extend

extend :: (GetTip l, HasLedgerTables l)
       => l DiffMK
       -> DbChangelog l
       -> DbChangelog l
extend newState dblog =
  DbChangelog {
      changelogLastFlushedState
    , changelogDiffs          =
        zipLedgerTables ext changelogDiffs tablesDiff
    , changelogVolatileStates =
        changelogVolatileStates AS.:> l'
    }
  where
    slot = case getTipSlot l' of
      Origin -> error "impossible! extendDbChangelog"
      At s   -> s

    ext ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (SeqDiffMK sq) (DiffMK d) =
      SeqDiffMK $ DS.extend sq slot d

    l'         = forgetLedgerTables  newState
    tablesDiff = projectLedgerTables newState

    DbChangelog {
        changelogLastFlushedState
      , changelogDiffs
      , changelogVolatileStates
      } = dblog
{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollbackN ::
     (GetTip l, HasLedgerTables l)
  => Word64
  -> DbChangelog l
  -> Maybe (DbChangelog l)
rollbackN n dblog
    | n <= Query.maxRollback dblog
    = Just $ dblog {
        changelogDiffs          = mapLedgerTables trunc changelogDiffs
      , changelogVolatileStates = AS.dropNewest (fromIntegral n) changelogVolatileStates
      }
    | otherwise
    = Nothing
  where
    trunc ::
      (Ord k, Eq v)
      => SeqDiffMK k v -> SeqDiffMK k v
    trunc (SeqDiffMK sq) =
      SeqDiffMK $ fst $ DS.splitAtFromEnd (fromIntegral n) sq

    DbChangelog {
        changelogDiffs
      , changelogVolatileStates
      } = dblog

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

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

push :: (ApplyBlock l blk, Monad m, c)
     => LedgerDbCfg l
     -> Ap m l blk c -> KeySetsReader m l -> DbChangelog l -> m (DbChangelog l)
push cfg ap ksReader db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap ksReader db

-- | Push a bunch of blocks (oldest first)
pushMany :: (ApplyBlock l blk, Monad m, c)
         => (Pushing blk -> m ())
         -> LedgerDbCfg l
         -> [Ap m l blk c]
         -> KeySetsReader m l
         -> DbChangelog l
         -> m (DbChangelog l)
pushMany trace cfg aps ksReader = repeatedlyM pushAndTrace aps
  where
    pushAndTrace ap db = do
      let pushing = Pushing . toRealPoint $ ap
      trace pushing
      push cfg ap ksReader db

-- | Switch to a fork
switch :: (ApplyBlock l blk, Monad m, c)
       => LedgerDbCfg l
       -> Word64          -- ^ How many blocks to roll back
       -> (UpdateLedgerDbTraceEvent blk -> m ())
       -> [Ap m l blk c]  -- ^ New blocks to apply
       -> KeySetsReader m l
       -> DbChangelog l
       -> m (Either ExceededRollback (DbChangelog l))
switch cfg numRollbacks trace newBlocks ksReader db =
  case rollbackN numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = Query.maxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current DbChangelog
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          Right <$> pushMany
                      (trace . StartedPushingBlockToTheLedgerDb start goal)
                      cfg
                      newBlocks
                      ksReader
                      db'


-- | A simplified 'DbChangelog' that should be used for flushing.
data DiffsToFlush l = DiffsToFlush {
    -- | The set of differences that should be flushed into the 'BackingStore'
    toFlushDiffs :: !(LedgerTables l DiffMK)
    -- | At which slot the diffs were split. This must be the slot of the state
    -- considered as "last flushed" in the kept 'DbChangelog'
  , toFlushSlot  :: !SlotNo
  }

-- | "Flush" the 'DbChangelog' by splitting it into the diffs that should be
-- flushed and the new 'DbChangelog'.
splitForFlushing ::
     forall l.
     (GetTip l, HasLedgerTables l)
  => FlushPolicy
  -> DbChangelog l
  -> (Maybe (DiffsToFlush l), DbChangelog l)
splitForFlushing policy dblog =
    if getTipSlot immTip == Origin || foldLedgerTables (\(SeqDiffMK sq) -> Sum $ DS.length sq) l == 0
    then (Nothing, dblog)
    else (Just ldblog, rdblog)
  where
    DbChangelog {
        changelogLastFlushedState
      , changelogDiffs
      , changelogVolatileStates
      } = dblog

    immTip = AS.anchor changelogVolatileStates
    volTip = either id id $ AS.head changelogVolatileStates

    -- TODO: #4371 by point, not by count, so sequences can be ragged
    splitSeqDiff ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
    splitSeqDiff (SeqDiffMK sq) = case policy of
      FlushAll          -> (SeqDiffMK sq, emptyMK)
      FlushAllImmutable ->
       let numToFlush = DS.length sq - AS.length changelogVolatileStates
       in bimap (maybe emptyMK SeqDiffMK) SeqDiffMK
        $ if numToFlush > 0
          then let (tf, tk) = DS.splitAt numToFlush sq
               in (Just tf, tk)
          else (Nothing, sq)

    lr = mapLedgerTables (uncurry Pair2 . splitSeqDiff) changelogDiffs
    l = mapLedgerTables (\(Pair2 x _) -> x) lr
    r = mapLedgerTables (\(Pair2 _ y) -> y) lr

    (newTip, newStates) =
        if getAll $ foldLedgerTables (\(SeqDiffMK sq) -> All $ 0 == DS.length sq) l
        then (changelogLastFlushedState, changelogVolatileStates)
        else case policy of
          FlushAll          -> (volTip, AS.Empty volTip)
          FlushAllImmutable -> (immTip, changelogVolatileStates)

    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

    ldblog = DiffsToFlush {
        toFlushDiffs = mapLedgerTables prj l
      , toFlushSlot  =
            fromWithOrigin (error "Flushing a DbChangelog at origin should never happen")
          $ getTipSlot
          $ case policy of
              FlushAll          -> volTip
              FlushAllImmutable -> immTip
      }

    rdblog = DbChangelog {
        changelogLastFlushedState = newTip
      , changelogDiffs            = r
      , changelogVolatileStates   = newStates
      }

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the DbChangelog
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

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk ()
pureBlock = ReapplyVal

push' :: ApplyBlock l blk
      => LedgerDbCfg l
      -> blk
      -> KeySetsReader Identity l
      -> DbChangelog l
      -> DbChangelog l
push' cfg b bk = runIdentity . push cfg (pureBlock b) bk

pushMany' :: ApplyBlock l blk
          => LedgerDbCfg l
          -> [blk]
          -> KeySetsReader Identity l
          -> DbChangelog l
          -> DbChangelog l
pushMany' cfg bs bk =
  runIdentity . pushMany (const $ pure ()) cfg (map pureBlock bs) bk

switch' :: ApplyBlock l blk
        => LedgerDbCfg l
        -> Word64
        -> [blk]
        -> KeySetsReader Identity l
        -> DbChangelog l
        -> Maybe (DbChangelog l)
switch' cfg n bs bk db =
  case runIdentity $ switch cfg n (const $ pure ()) (map pureBlock bs) bk db of
    Left  ExceededRollback{} -> Nothing
    Right db'                -> Just db'
