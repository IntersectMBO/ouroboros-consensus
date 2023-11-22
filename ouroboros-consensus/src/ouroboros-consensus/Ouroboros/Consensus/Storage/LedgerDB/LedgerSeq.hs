{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Consensus.Storage.LedgerDB.LedgerSeq (
    -- * Handles
    Handle (..)
  , newHandle
    -- * The ledger seq
  , LedgerSeq (..)
  , closeLedgerSeq
  , empty
    -- * Apply Blocks
  , applyBlock
  , applyThenPush
  , applyThenPushMany
  , extend
  , prune
  , pruneToImmTipOnly
  , switch
    -- * Queries
  , anchor
  , current
  , currentHandle
  , getPastLedgerAt
  , immutableTipSlot
  , isSaturated
  , maxRollback
  , rollback
  , rollbackN
  , rollbackToAnchor
  , rollbackToPoint
  , snapshots
  , tip
  , volatileStatesBimap
    -- * Testing
  , applyThenPush'
  , applyThenPushMany'
  , switch'
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.IORef
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.Common
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Network.AnchoredSeq hiding (anchor, last, map,
                     rollback)
import qualified Ouroboros.Network.AnchoredSeq as AS hiding (map)
import           Prelude hiding (read)

{-------------------------------------------------------------------------------
  Handles
-------------------------------------------------------------------------------}

data Handle m l = MonadIO m => Handle {
    close     :: m ()
  , duplicate :: m (Handle m l)
  , read      :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
  , write     :: LedgerTables l DiffMK -> m ()
  , state     :: l EmptyMK
  }

instance Eq (l EmptyMK) => Eq (Handle m l) where
  (==) = (==) `on` state

instance Show (l EmptyMK) => Show (Handle m l) where
  show = show . state

{-------------------------------------------------------------------------------
  InMemory handles
-------------------------------------------------------------------------------}

data HandleState l = HandleOpen (l ValuesMK) | HandleClosed

guardClosed :: HandleState l -> (l ValuesMK -> a) -> a
guardClosed HandleClosed    _ = error "Closed"
guardClosed (HandleOpen st) f = f st

newHandle :: (MonadIO m, HasLedgerTables l) => l ValuesMK -> m (Handle m l)
newHandle l = do
  ioref <- liftIO $ newIORef (HandleOpen l)
  pure Handle {
      close = liftIO $ atomicModifyIORef' ioref (const (HandleClosed, ()))
    , duplicate = do
        hs <- liftIO $ readIORef ioref
        guardClosed hs newHandle
    , read = \keys -> do
        hs <- liftIO $ readIORef ioref
        guardClosed hs (\st -> pure $ ltliftA2 rawRestrictValues (ltprj st) keys)
    , write = \diffs -> do
        liftIO
        $ atomicModifyIORef' ioref
        (`guardClosed` (\st -> (HandleOpen $ st `withLedgerTables` ltliftA2 rawApplyDiffs (ltprj st) diffs, ())))
    , state = forgetLedgerTables l
    }

{-------------------------------------------------------------------------------
  The LedgerSeq
-------------------------------------------------------------------------------}

newtype LedgerSeq m l = LedgerSeq {
    getLedgerSeq :: AnchoredSeq (WithOrigin SlotNo) (Handle m l) (Handle m l)
  }

deriving instance Eq (l EmptyMK) => Eq (LedgerSeq m l)
deriving instance Show (l EmptyMK) => Show (LedgerSeq m l)

instance GetTip l => Anchorable (WithOrigin SlotNo) (Handle m l) (Handle m l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . state

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @LedgerSeq@.
empty :: (GetTip l, MonadIO m, HasLedgerTables l) => l ValuesMK -> m (LedgerSeq m l)
empty = fmap (LedgerSeq . AS.Empty) . newHandle

closeLedgerSeq :: Monad m => LedgerSeq m l -> m ()
closeLedgerSeq = mapM_ close . toOldestFirst . getLedgerSeq

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | Apply block to the current tip of the ledger db
applyBlock :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
           => LedgerCfg l
           -> Ap (LedgerSeq m) m l blk c
           -> LedgerSeq m l
           -> m (ValidLedgerState (Handle m l))
applyBlock cfg ap ldb = case ap of
    ReapplyVal b ->
          ValidLedgerState
      <$> withValues b (return . tickThenReapply cfg b)
    ApplyVal b ->
          ValidLedgerState
      <$> withValues b
          ( either (throwLedgerError ldb (blockRealPoint b)) return
            . runExcept
            . tickThenApply cfg b
          )
    ReapplyRef r  -> do
      b <- doResolveBlock r
      applyBlock cfg (ReapplyVal b) ldb
    ApplyRef r -> do
      b <- doResolveBlock r
      applyBlock cfg (ApplyVal b) ldb
    Weaken ap' ->
      applyBlock cfg ap' ldb
  where
    withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (Handle m l)
    withValues blk app = do
      let keys = getBlockKeySets blk
          cur = currentHandle ldb
      vals <- read cur keys
      diffs <- fmap projectLedgerTables . app $ state cur `withLedgerTables` vals
      cur' <- duplicate cur
      write cur' diffs
      pure cur'

-- | If applying a block on top of the ledger state at the tip is succesful,
-- extend the LedgerDB with the resulting ledger state.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
applyThenPush :: (ApplyBlock l blk, Monad m, c)
              => LedgerDbCfg l
              -> Ap (LedgerSeq m) m l blk c
              ->    LedgerSeq m l
              -> m (LedgerSeq m l)
applyThenPush cfg ap db =
    (\current' -> prune (ledgerDbCfgSecParam cfg) $ extend current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany :: (ApplyBlock l blk, Monad m, c)
                  => (Pushing blk -> m ())
                  -> LedgerDbCfg l
                  -> [Ap (LedgerSeq m) m l blk c]
                  -> LedgerSeq m l
                  -> m (LedgerSeq m l)
applyThenPushMany trace cfg = repeatedlyM pushAndTrace
  where
    pushAndTrace ap db = do
      trace $ Pushing . toRealPoint $ ap
      applyThenPush cfg ap db

-- | Prune ledger states from the front until at we have at most @k@ in the
-- LedgerDB, excluding the one stored at the anchor.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> ldb' = LedgerSeq $ AS.fromOldestFirst     l1 [l2, l3]
-- >>> prune (SecurityParam 2) ldb == ldb'
-- True
prune :: GetTip l
      => SecurityParam
      -> LedgerSeq m l
      -> LedgerSeq m l
prune (SecurityParam k) (LedgerSeq ldb) =
    LedgerSeq ldb'
  where
    nvol = AS.length ldb

    ldb' =
      if toEnum nvol <= k
      then ldb
      else snd $ AS.splitAt (nvol - fromEnum k) ldb

-- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
-- 'LedgerSeq' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
-- NOTE (@js): this INLINE was inherited from before UTxO-HD, so maybe it is not
-- needed anymore.
{-# INLINE prune #-}

-- | Extending the LedgerDB with a valid ledger state.
--
-- >>> ldb            = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = extend (ValidLedgerState l4) ldb
-- >>> AS.toOldestFirst ldb' == [l1, l2, l3, l4]
-- True
extend :: GetTip l
       => ValidLedgerState (Handle m l)
       -> LedgerSeq m l
       -> LedgerSeq m l
extend (ValidLedgerState newState) =
  LedgerSeq . (:> newState) . getLedgerSeq

{-------------------------------------------------------------------------------
  Switching to a fork
-------------------------------------------------------------------------------}

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch :: (ApplyBlock l blk, Monad m, c)
       => LedgerDbCfg l
       -> Word64          -- ^ How many blocks to roll back
       -> (UpdateLedgerDbTraceEvent blk -> m ())
       -> [Ap (LedgerSeq m) m l blk c]  -- ^ New blocks to apply
       -> LedgerSeq m l
       -> m (Either ExceededRollback (LedgerSeq m l))
switch cfg numRollbacks trace newBlocks db =
  case rollbackN numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = maxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current LedgerDB
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          Right <$> applyThenPushMany
                      (trace . StartedPushingBlockToTheLedgerDb start goal)
                      cfg
                      newBlocks
                      db'

{-------------------------------------------------------------------------------
  Reset
-------------------------------------------------------------------------------}

-- | When creating a new @LedgerDB@, we should load whichever snapshot we find
-- and then replay the chain up to the immutable tip. When we get there, the
-- @LedgerDB@ will have a @k@-long sequence of states, which all come from
-- immutable blocks, so we just prune all of them and only keep the last one as
-- an anchor, as it is the immutable tip. Then we can proceed with opening the
-- VolatileDB.
--
-- If we didn't do this step, the @LedgerDB@ would accept rollbacks into the
-- immutable part of the chain, which must never be possible.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = pruneToImmTipOnly ldb
-- >>> AS.anchor ldb' == l3 && AS.toOldestFirst ldb' == []
-- True
pruneToImmTipOnly :: GetTip l
                  => LedgerSeq m l
                  -> LedgerSeq m l
pruneToImmTipOnly = prune (SecurityParam 0)

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback @n@ ledger states.
--
-- Returns 'Nothing' if maximum rollback (usually @k@, but can be less on
-- startup or under corruption) is exceeded.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> fmap (([l1] ==) . AS.toOldestFirst . getLedgerSeq) (rollbackN 2 ldb)
-- Just True
rollbackN ::
     GetTip l
  => Word64
  -> LedgerSeq m l
  -> Maybe (LedgerSeq m l)
rollbackN n ldb
    | n <= maxRollback ldb
    = Just $ LedgerSeq (AS.dropNewest (fromIntegral n) $ getLedgerSeq ldb)
    | otherwise
    = Nothing

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l3s == current ldb
-- True
current :: GetTip l => LedgerSeq m l -> l EmptyMK
current = state . headAnchor . getLedgerSeq

-- | The handle at the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l3 == currentHandle ldb
-- True
currentHandle :: GetTip l => LedgerSeq m l -> Handle m l
currentHandle = headAnchor . getLedgerSeq

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l0s == anchor ldb
-- True
anchor :: LedgerSeq m l -> l EmptyMK
anchor = state . AS.anchor . getLedgerSeq

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> [(0, l3s), (1, l2s), (2, l1s)] == snapshots ldb
-- True
snapshots :: LedgerSeq m l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . map state
    . AS.toNewestFirst
    . getLedgerSeq

-- | How many blocks can we currently roll back?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> maxRollback ldb
-- 3
maxRollback :: GetTip l => LedgerSeq m l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . getLedgerSeq

-- | Reference to the block at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> tip ldb == getTip l3s
-- True
tip :: GetTip l => LedgerSeq m l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> isSaturated (SecurityParam 3) ldb
-- True
-- >>> isSaturated (SecurityParam 4) ldb
-- False
isSaturated :: GetTip l => SecurityParam -> LedgerSeq m l -> Bool
isSaturated (SecurityParam k) db =
    maxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> getPastLedgerAt (Point (At (Block 4 4)) :: Point B) ldb == Nothing
-- True
-- >>> getPastLedgerAt (Point (At (Block 1 1)) :: Point B) ldb == Just l2s
-- True
getPastLedgerAt ::
     ( HasHeader blk, GetTip l, HeaderHash l ~ HeaderHash blk
     , StandardHash l
     )
  => Point blk
  -> LedgerSeq m l
  -> Maybe (l EmptyMK)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Roll back the volatile states up to the specified point.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> Just (LedgerSeq ldb') = rollbackToPoint (Point Origin) ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == []
-- True
-- >>> rollbackToPoint (Point (At (Block 1 2))) ldb == Nothing
-- True
-- >>> Just (LedgerSeq ldb') = rollbackToPoint (Point (At (Block 1 1))) ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == [l1, l2]
-- True
rollbackToPoint ::
     ( StandardHash l
     , GetTip l
     )
  => Point l -> LedgerSeq m l -> Maybe (LedgerSeq m l)
rollbackToPoint pt (LedgerSeq ldb) = do
    LedgerSeq <$>
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . either state state)
        ldb

-- | Rollback the volatile states up to the volatile anchor.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = rollbackToAnchor ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == []
-- True
rollbackToAnchor ::
     GetTip l
  => LedgerSeq m l -> LedgerSeq m l
rollbackToAnchor (LedgerSeq vol) =
    LedgerSeq (AS.Empty (AS.anchor vol))

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
     ( HasHeader blk, GetTip l, HeaderHash l ~ HeaderHash blk
     , StandardHash l
     )
  => Point blk
  -> LedgerSeq m l
  -> Maybe (LedgerSeq m l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just $ rollbackToAnchor db
    | otherwise
    = rollbackToPoint (castPoint pt) db

immutableTipSlot ::
     GetTip l
  => LedgerSeq m l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . state
    . AS.anchor
    . getLedgerSeq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     AS.Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> LedgerSeq m l
  -> AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap (f . state) (g . state)
    . getLedgerSeq

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap (LedgerSeq m) m l blk ()
pureBlock = ReapplyVal

applyThenPush' :: ApplyBlock l blk
               => LedgerDbCfg l
               -> blk
               -> LedgerSeq Identity l
               -> LedgerSeq Identity l
applyThenPush' cfg b = runIdentity . applyThenPush cfg (pureBlock b)

applyThenPushMany' :: ApplyBlock l blk
                   => LedgerDbCfg l
                   -> [blk]
                   -> LedgerSeq Identity l
                   -> LedgerSeq Identity l
applyThenPushMany' cfg bs =
  runIdentity . applyThenPushMany (const $ pure ()) cfg (map pureBlock bs)

switch' :: ApplyBlock l blk
        => LedgerDbCfg l
        -> Word64
        -> [blk]
        -> LedgerSeq Identity l
        -> Maybe (LedgerSeq Identity l)
switch' cfg n bs db =
  case runIdentity $ switch cfg n (const $ pure ()) (map pureBlock bs) db of
    Left  ExceededRollback{} -> Nothing
    Right db'                -> Just db'

{-------------------------------------------------------------------------------
  docspec setup
-------------------------------------------------------------------------------}

-- $setup
-- >>> :set -XTypeFamilies -XUndecidableInstances
-- >>> import qualified Ouroboros.Network.AnchoredSeq as AS
-- >>> import Ouroboros.Network.Block
-- >>> import Ouroboros.Network.Point
-- >>> import Ouroboros.Consensus.Ledger.Tables
-- >>> import Ouroboros.Consensus.Ledger.Basics
-- >>> import Ouroboros.Consensus.Config
-- >>> import Ouroboros.Consensus.Storage.LedgerDB.Common
-- >>> import Data.Void
-- >>> import Cardano.Slotting.Slot
-- >>> data B
-- >>> data LS (mk :: MapKind) = LS (Point LS)
-- >>> type instance HeaderHash LS = Int
-- >>> type instance HeaderHash B = HeaderHash LS
-- >>> instance StandardHash LS
-- >>> type instance Key LS = Void
-- >>> type instance Value LS = Void
-- >>> instance LedgerTablesAreTrivial LS where convertMapKind (LS p) = LS p
-- >>> instance HasLedgerTables LS
-- >>> s = [LS (Point Origin), LS (Point (At (Block 0 0))), LS (Point (At (Block 1 1))), LS (Point (At (Block 2 2))), LS (Point (At (Block 3 3)))]
-- >>> [l0s, l1s, l2s, l3s, l4s] = s
-- >>> [l0, l1, l2, l3, l4] <- mapM newHandle s
-- >>> instance GetTip LS where getTip (LS p) = p
-- >>> instance Eq (LS EmptyMK) where LS p1 == LS p2 = p1 == p2
-- >>> instance StandardHash B
-- >>> instance HasHeader B where getHeaderFields = undefined
