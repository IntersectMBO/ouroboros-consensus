{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | A 'DbChangelog' is the component of the
-- 'Ouroboros.Consensus.Storage.LedgerDB.LedgerDB' implementation that
-- responsible for:
--
-- - Maintaining the last \(k\) in-memory ledger states without on-disk parks.
--
-- - Holding the in-memory ledger state that a snapshot would write to the disk.
--
-- - Providing sequences of differences from said state to any requested state
--     in the last \(k\) ledger states, which combined with the values in the
--     'BackingStore', can provide 'LedgerTable's at any of those ledger states.
--
-- A 'DbChangelog' is said to be /anchored/ #anchored# at a 'BackingStore' when
-- the slot of the values in the backing store is the predecesor of the slots in
-- the sequence of differences, with the overall sequence of slots being defined
-- by the blocks on the chain.
--
-- This design is based on the technical report "Storing the Cardano ledger
-- state on disk: API design concepts" by Duncan Coutts and Douglas Wilson.
--
-- = Implementation details
--
-- The 'DbChangelog' is in fact a pure data structure, of which the 'LedgerDB'
-- will carry a value in some mutable state, see
-- 'Ouroboros.Consensus.Storage.LedgerDB.LedgerDBState'.
--
-- == Carrying states
--
-- The 'DbChangelog' contains an instantiation of the 'AnchoredSeq' data type to
-- hold the last \(k\) in-memory ledger states. This data type is impemented
-- using the /finger tree/ data structure and has the following time
-- complexities:
--
-- - Appending a new ledger state to the end in constant time.
--
-- - Rolling back to a previous ledger state in logarithmic time.
--
-- - Looking up a past ledger state by its point in logarithmic time.
--
-- One can think of 'AnchoredSeq' as a 'Seq' from "Data.Sequence" with a custom
-- /finger tree measure/ allowing for efficient lookups by point, combined with
-- an /anchor/. When fully /saturated/, the sequence will contain \(k\) ledger
-- states. In case of a complete rollback of all \(k\) blocks and thus ledger
-- states, the sequence will become empty. A ledger state is still needed, i.e.,
-- one corresponding to the most recent immutable block that cannot be rolled
-- back. The ledger state at the anchor plays this role.
--
-- == Appending in-memory states
--
-- When a new ledger state is appended to a fully saturated 'DbChangelog' (i.e.
-- that contains \(k\) states), the ledger state at the anchor is dropped and
-- the oldest element in the sequence becomes the new anchor, as it has become
-- immutable. This maintains the invariant that only the last \(k\) in-memory
-- ledger states are stored, /excluding/ the ledger state at the anchor. This
-- means that in practice, \(k + 1\) ledger states will be kept in memory. When
-- the 'DbChangelog' contains fewer than \(k\) elements, new ones are appended
-- without shifting the anchor until it is saturated.
--
-- == Getting and appending differences
--
-- For the differences, the 'DbChangelog' contains a 'SeqDiffMK' (see
-- "Ouroboros.Consensus.Ledger.Tables.DiffSeq") which in turn is just an
-- instantiation of a /root-measured finger tree/ (see
-- [fingertree-rm](https://github.com/input-output-hk/anti-diffs/tree/main/fingertree-rm))
-- which is a specialization of the finger trees that carries a root-measure
-- which is the monoidal sum of all the measures of all the elements.
--
-- This allows us to very efficiently lookup the combined difference of the
-- whole 'DbChangelog', while still having a good complexity when splitting this
-- tree.
--
-- When a block is to be applied to a ledger state (which must be in the
-- 'DbChangelog' or application would directly fail), applying the root-measure
-- of the sub-sequence of differences from the backing store slot up to the
-- requested slot to the values read from the backing store will provide the
-- 'LedgerTable's needed for applying the block.
--
-- Once a new ledger state is appended to the 'DbChangelog', said ledger state
-- will carry 'DiffMK' tables (obtained by diffing the input and output ledger
-- tables when calling the Ledger rules). Adding those differences to the
-- 'DbChangelog' is just a matter of extending the carried 'SeqDiffMK'.
--
-- Only when flushing, the 'SeqDiffMK' is pruned, by extracting the differences
-- in between the last flushed state and the current immutable tip.
module Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog (
    -- * The DbChangelog
    DbChangelog (..)
  , DbChangelog'
    -- ** Views
  , AnchorlessDbChangelog (..)
  , AnchorlessDbChangelog'
  , StatesSequence
    -- * Construction
  , empty
  , pruneToImmTipOnly
    -- * Mapping changelogs
    --
    -- | These functions are analogous to 'fmap' for modifying the inner
    -- 'AnchorlessDbChangelog'.
  , onChangelog
  , onChangelogM
    -- * Updating a @DbChangelog@
    -- ** Applying blocks #applying#
    --
    -- | Applying blocks to the 'DbChangelog' will extend it if the result is
    -- successful.
    --
    -- In order to do so, we first need to [find the particular
    -- block](#g:findingBlocks), then prepare the ledger tables by [hydrating
    -- the ledger state](#g:hydratingTheLedgerState) and then finally call the
    -- ledger, which might throw errors.
  , reapplyThenPush
    -- *** Hydrating the ledger state #hydratingTheLedgerState#
    --
    -- | When trying to get tables at a specific ledger state, we must follow a
    -- process we call /hydrating the ledger state/. This process consists of 3 steps:
    --
    -- 1. Rewind the requested keys to the beginning of the DbChangelog. For
    -- UTxO entries this just means that we record at which slot the db
    -- changelog was when rewinding.
    --
    -- 2. Query the 'BackingStore' for the actual values for the requested keys.
    --
    -- 3. Forward those values by applying the differences in the 'DbChangelog' up to
    -- the requested point.
  , withKeysReadSets
    -- **** Rewind
  , RewoundTableKeySets (..)
  , rewindTableKeySets
    -- **** Read
  , KeySetsReader
  , UnforwardedReadSets (..)
  , getLedgerTablesFor
  , readKeySets
  , readKeySetsWith
  , trivialKeySetsReader
    -- **** Forward
  , RewindReadFwdError (..)
  , forwardTableKeySets
  , forwardTableKeySets'
    -- ** Flushing
  , DiffsToFlush (..)
  , splitForFlushing
    -- * Queries
  , anchor
  , current
  , flushableLength
  , getPastLedgerAt
  , rollback
  , snapshots
  , tip
  , volatileStatesBimap
    -- * 🧪 Testing
    -- ** Internal
  , extend
  , immutableTipSlot
  , isSaturated
  , maxRollback
  , prune
  , rollbackN
  , rollbackToAnchor
  , rollbackToPoint
    -- * Testing
  , reapplyThenPush'
  , reapplyThenPushMany'
  , switch
  , switch'
  ) where

import           Cardano.Slotting.Slot
import           Control.Exception as Exn
import           Data.Bifunctor (bimap)
import           Data.Functor.Identity
import           Data.Map.Diff.Strict as AntiDiff (applyDiffForKeys)
import           Data.Monoid (Sum (..))
import           Data.SOP (K, unK)
import           Data.SOP.Functors
import           Data.Word
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Diff (fromAntiDiff,
                     toAntiDiff)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS

{-------------------------------------------------------------------------------
  The DbChangelog
-------------------------------------------------------------------------------}

-- | Holds a sequence of split ledger states, where the in-memory part is in a
-- sequence and the on-disk part is represented by a sequence of differences
-- that need a 'BackingStore' as an anchor point.
--
-- We illustrate its contents below, where @k = 3@ (for a state @Li@, the
-- corresponding set of differences is @Di@):
--
-- +----------------+------------------------------------+------------------------------------------+
-- | lastFlushed    | states                             | tableDiffs                               |
-- +================+====================================+==========================================+
-- |      @L0@      | @L0 :> [ ]                       @ | @[ ]                                   @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L0@      | @L0 :> [ L1 ]                    @ | @[ D1 ]                                @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L0@      | @L0 :> [ L1, L2 ]                @ | @[ D1, D2 ]                            @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L0@      | @L0 :> [ L1, L2, L3 ]            @ | @[ D1, D2, D3 ]                        @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L0@      | @L1 :> [     L2, L3, L4 ]        @ | @[ D1, D2, D3, D4 ]                    @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L0@      | @L2 :> [         L3, L4, L5 ]    @ | @[ D1, D2, D3, D4, D5 ] -- (*)         @ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L2@      | @L2 :> [         L3, L4, L5 ]    @ | @[         D3, D4, D5 ]   -- flush (**)@ |
-- +----------------+------------------------------------+------------------------------------------+
-- |      @L2@      | @L3 :> [             L4, L5, L6 ]@ | @[         D3, D4, D5, D6 ]            @ |
-- +----------------+------------------------------------+------------------------------------------+
--
-- Notice that @length states@ is usually @k@ except when rollbacks or data
-- corruption take place and will be less than @k@ when we just loaded a
-- snapshot. We cannot roll back more than @k@ blocks. This means that after a
-- rollback of @k@ blocks at @(*)@, the changelog will look something like this:
--
-- +------+-------------+--------------+
-- | @L0@ | @L2 :> [ ]@ | @[ D1, D2 ]@ |
-- +------+-------------+--------------+
--
-- And a rollback of @k@ blocks at @(**)@ will look something like this:
--
-- +------+-------------+-------+
-- | @L2@ | @L2 :> [ ]@ | @[ ]@ |
-- +------+-------------+-------+
--
-- Notice how the states list always contains the in-memory state of the anchor,
-- but the table differences might not contain the differences for that anchor
-- if they have been flushed to the backend.
--
-- As said above, this @DbChangelog@ has to be coupled with a @BackingStore@
-- which provides the pointers to the on-disk data.
data DbChangelog l = DbChangelog {
    -- | The last flushed ledger state.
    --
    -- We need to keep track of this one as this will be the state written to
    -- disk when we make a snapshot
    changelogLastFlushedState :: !(l EmptyMK)

    -- | The in memory part of the DbChangelog. Most of the operations we do
    -- with the @DbChangelog@ happen with the in-memory data only.
  , anchorlessChangelog       :: !(AnchorlessDbChangelog l)
  }
  deriving (Generic)

deriving instance (Eq       (Key l), Eq       (Value l), Eq       (l EmptyMK))
               =>  Eq       (DbChangelog l)
deriving instance (NoThunks (Key l), NoThunks (Value l), NoThunks (l EmptyMK))
               =>  NoThunks (DbChangelog l)
deriving instance (Show     (Key l), Show     (Value l), Show     (l EmptyMK))
               =>  Show     (DbChangelog l)

-- | A 'DbChangelog' variant that contains only the information in memory. To
-- perform reads of Ledger Tables, this needs to be coupled with a
-- 'BackingStoreValueHandle' as done in
-- 'Ouroboros.Consensus.LedgerDB.API.LedgerDBView'.
data AnchorlessDbChangelog l = AnchorlessDbChangelog {
    -- | Slot of the last flushed changelog state from which this variant
    -- originated. Used just for asserting correctness when forwarding.
    adcLastFlushedSlot :: !(WithOrigin SlotNo)
    -- | The sequence of differences between the last flushed state
    -- ('changelogLastFlushedState') and the tip of the volatile sequence
    -- ('adcStates').
  , adcDiffs           :: !(LedgerTables l SeqDiffMK)
    -- | The volatile sequence of states.
    --
    -- The anchor of this sequence is the immutable tip, so whenever we flush,
    -- we should do so up until that point. The length of this sequence will be
    -- @k@ except in abnormal circumstances like rollbacks or data corruption.
  , adcStates          :: !(StatesSequence l)
  } deriving (Generic)

deriving instance (Eq       (LedgerTables l SeqDiffMK), Eq       (l EmptyMK))
               =>  Eq       (AnchorlessDbChangelog l)
deriving instance (NoThunks (LedgerTables l SeqDiffMK), NoThunks (l EmptyMK))
               =>  NoThunks (AnchorlessDbChangelog l)
deriving instance (Show     (LedgerTables l SeqDiffMK), Show     (l EmptyMK))
               =>  Show     (AnchorlessDbChangelog l)

type StatesSequence l = AnchoredSeq
                        (WithOrigin SlotNo)
                        (l EmptyMK)
                        (l EmptyMK)

type AnchorlessDbChangelog' blk = AnchorlessDbChangelog (ExtLedgerState blk)

instance GetTip l => AS.Anchorable (WithOrigin SlotNo) (l EmptyMK) (l EmptyMK) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot

instance IsLedger l => GetTip (K (DbChangelog l)) where
  getTip = castPoint
         . getTip
         . either id id
         . AS.head
         . adcStates
         . anchorlessChangelog
         . unK

instance IsLedger l => GetTip (K (AnchorlessDbChangelog l)) where
  getTip = castPoint
         . getTip
         . either id id
         . AS.head
         . adcStates
         . unK

type instance HeaderHash (K @MapKind (DbChangelog l)) =
              HeaderHash l

type instance HeaderHash (K @MapKind (AnchorlessDbChangelog l)) =
              HeaderHash l

type DbChangelog' blk = DbChangelog (ExtLedgerState blk)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @DbChangelog@.
empty ::
     (HasLedgerTables l, GetTip l)
  => l EmptyMK -> DbChangelog l
empty theAnchor =
    DbChangelog {
        changelogLastFlushedState = theAnchor
        , anchorlessChangelog     = AnchorlessDbChangelog {
              adcLastFlushedSlot = pointSlot $ getTip theAnchor
            , adcDiffs           = ltpure (SeqDiffMK DS.empty)
            , adcStates          = AS.Empty theAnchor
            }
      }

{-------------------------------------------------------------------------------
  Mapping changelogs
-------------------------------------------------------------------------------}

onChangelog :: (AnchorlessDbChangelog l -> AnchorlessDbChangelog l)
            -> DbChangelog l
            -> DbChangelog l
onChangelog f dbch = runIdentity $ onChangelogM (Identity . f) dbch

onChangelogM :: Monad m
             => (AnchorlessDbChangelog l -> m (AnchorlessDbChangelog l))
             -> DbChangelog l
             -> m (DbChangelog l)
onChangelogM f dbch = do
  anchorlessChangelog' <- f $ anchorlessChangelog dbch
  pure dbch { anchorlessChangelog =  anchorlessChangelog' }

reapplyBlock :: forall m l blk. (ApplyBlock l blk, Monad m)
           => LedgerCfg l
           -> blk
           -> KeySetsReader m l
           -> AnchorlessDbChangelog l
           -> m (l DiffMK)
reapplyBlock cfg b ksReader db =
   withKeysReadSets (current db) ksReader db (getBlockKeySets b) (return . tickThenReapply cfg b)

-- | If applying a block on top of the ledger state at the tip is succesful,
-- extend the DbChangelog with the resulting ledger state.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
reapplyThenPush :: (Monad m, ApplyBlock l blk)
              => LedgerDbCfg l
              -> blk
              -> KeySetsReader m l
              ->    AnchorlessDbChangelog l
              -> m (AnchorlessDbChangelog l)
reapplyThenPush cfg ap ksReader db =
    (\current' -> prune (ledgerDbCfgSecParam cfg) $ extend current' db) <$>
      reapplyBlock (ledgerDbCfg cfg) ap ksReader db

-- | Prune ledger states from the front until at we have at most @k@ in the
-- DbChangelog, excluding the one stored at the anchor.
--
-- +--------------+----------------------------+----------------------+
-- | lastFlushed  | states                     | tableDiffs           |
-- +==============+============================+======================+
-- |     @L0@     | @L0 :> [ L1, L2, L3, L4 ]@ | @[ D1, D2, D3, D4 ]@ |
-- +--------------+----------------------------+----------------------+
-- | @>> prune (SecurityParam 3)@                                     |
-- +--------------+----------------------------+----------------------+
-- |     @L0@     | @L2 :> [         L3, L4 ]@ | @[ D1, D2, D3, D4 ]@ |
-- +--------------+----------------------------+----------------------+
prune :: GetTip l
      => SecurityParam
      -> AnchorlessDbChangelog l
      -> AnchorlessDbChangelog l
prune (SecurityParam k) dblog =
    dblog { adcStates = vol' }
  where
    AnchorlessDbChangelog { adcStates } = dblog

    nvol = AS.length adcStates

    vol' =
      if toEnum nvol <= k
      then adcStates
      else snd $ AS.splitAt (nvol - fromEnum k) adcStates

-- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
-- 'DbChangelog' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
-- NOTE (@js): this INLINE was inherited from before UTxO-HD, so maybe it is not
-- needed anymore.
{-# INLINE prune #-}

-- | Extending the DbChangelog with a valid ledger state.
--
-- +------+----------------------------+----------------------+
-- | @L2@ | @L2 :> [ L3, L4, L5 ]@     | @[ D3, D4, D5 ]@     |
-- +------+----------------------------+----------------------+
-- | @>> extend L6 (D6)@                                      |
-- +------+----------------------------+----------------------+
-- | @L2@ | @L2 :> [ L3, L4, L5, L6 ]@ | @[ D3, D4, D5, D6 ]@ |
-- +------+----------------------------+----------------------+
extend :: (GetTip l, HasLedgerTables l)
       => l DiffMK
       -> AnchorlessDbChangelog l
       -> AnchorlessDbChangelog l
extend newState dblog =
  AnchorlessDbChangelog {
      adcLastFlushedSlot = adcLastFlushedSlot
    , adcDiffs           = ltliftA2 ext adcDiffs tablesDiff
    , adcStates          = adcStates AS.:> l'
    }
  where
    slot = case getTipSlot l' of
      Origin -> error "impossible! extending a DbChangelog with a state at Origin"
      At s   -> s

    ext ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (SeqDiffMK sq) (DiffMK d) =
      SeqDiffMK $ DS.extend sq slot $ toAntiDiff d

    l'         = forgetLedgerTables  newState
    tablesDiff = projectLedgerTables newState

    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs
      , adcStates
      } = dblog

{-------------------------------------------------------------------------------
  Rewind
-------------------------------------------------------------------------------}

data RewoundTableKeySets l =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewound
      !(LedgerTables l KeysMK)

rewindTableKeySets :: AnchorlessDbChangelog l
                   -> LedgerTables l KeysMK
                   -> RewoundTableKeySets l
rewindTableKeySets = RewoundTableKeySets . adcLastFlushedSlot

{-------------------------------------------------------------------------------
  Read
-------------------------------------------------------------------------------}

type KeySetsReader m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

readKeySets ::
     IOLike m
  => LedgerBackingStore m l
  -> KeySetsReader m l
readKeySets backingStore rew = do
    withBsValueHandle backingStore (`readKeySetsWith` rew)

readKeySetsWith ::
     Monad m
  => LedgerBackingStoreValueHandle m l
  -> RewoundTableKeySets l
  -> m (UnforwardedReadSets l)
readKeySetsWith bsvh (RewoundTableKeySets _seqNo rew) = do
    values <- bsvhRead bsvh rew
    pure UnforwardedReadSets {
        ursSeqNo  = bsvhAtSlot bsvh
      , ursValues = values
      , ursKeys   = rew
    }

withKeysReadSets ::
     (HasLedgerTables l, Monad m)
  => l mk1
  -> KeySetsReader m l
  -> AnchorlessDbChangelog l
  -> LedgerTables l KeysMK
  -> (l ValuesMK -> m a)
  -> m a
withKeysReadSets st ksReader dbch ks f = do
      let aks = rewindTableKeySets dbch ks
      urs <- ksReader aks
      case withHydratedLedgerState urs of
        Left err ->
          -- We performed the rewind;read;forward sequence in this function. So
          -- the forward operation should not fail. If this is the case we're in
          -- the presence of a problem that we cannot deal with at this level,
          -- so we throw an error.
          --
          -- When we introduce pipelining, if the forward operation fails it
          -- could be because the DB handle was modified by a DB flush that took
          -- place when __after__ we read the unforwarded keys-set from disk.
          -- However, performing rewind;read;forward with the same __locked__
          -- changelog should always succeed.
          error $ "Changelog rewind;read;forward sequence failed, " <> show err
        Right res -> res

  where
   withHydratedLedgerState urs =
          f
      .   withLedgerTables st
      <$> forwardTableKeySets dbch urs

-- | The requested point is not found on the ledger db
newtype PointNotFound blk = PointNotFound (Point blk) deriving (Eq, Show)

-- | Read and forward the values up to the tip of the given ledger db. Returns
-- Left if the anchor moved. If Left is returned, then the caller was just
-- unlucky and scheduling of events happened to move the backing store. Reading
-- again the LedgerDB and calling this function must eventually succeed.
getLedgerTablesFor ::
     (Monad m, HasLedgerTables l)
  => AnchorlessDbChangelog l
  -> LedgerTables l KeysMK
  -> KeySetsReader m l
  -> m (Either RewindReadFwdError (LedgerTables l ValuesMK))
getLedgerTablesFor db keys ksRead = do
  let aks = rewindTableKeySets db keys
  urs <- ksRead aks
  pure $ forwardTableKeySets db urs

trivialKeySetsReader :: (Monad m, LedgerTablesAreTrivial l) => KeySetsReader m l
trivialKeySetsReader (RewoundTableKeySets s _) =
  pure $ UnforwardedReadSets s trivialLedgerTables trivialLedgerTables

{-------------------------------------------------------------------------------
  Forward
-------------------------------------------------------------------------------}

data UnforwardedReadSets l = UnforwardedReadSets {
    -- | The Slot number of the anchor of the 'DbChangelog' that was used when
    -- rewinding and reading.
    ursSeqNo  :: !(WithOrigin SlotNo)
    -- | The values that were found in the 'BackingStore'.
  , ursValues :: !(LedgerTables l ValuesMK)
    -- | All the requested keys, being or not present in the 'BackingStore'.
  , ursKeys   :: !(LedgerTables l KeysMK)
  }

-- | The DbChangelog and the BackingStore got out of sync. This is a critical
-- error, we cannot recover from this.
data RewindReadFwdError = RewindReadFwdError {
    rrfBackingStoreAt :: !(WithOrigin SlotNo)
  , rrfDbChangelogAt  :: !(WithOrigin SlotNo)
  } deriving Show

forwardTableKeySets' ::
     HasLedgerTables l
  => WithOrigin SlotNo
  -> LedgerTables l SeqDiffMK
  -> UnforwardedReadSets l
  -> Either RewindReadFwdError
            (LedgerTables l ValuesMK)
forwardTableKeySets' seqNo chdiffs = \(UnforwardedReadSets seqNo' values keys) ->
    if seqNo /= seqNo'
    then Left $ RewindReadFwdError seqNo' seqNo
    else Right $ ltliftA3 forward values keys chdiffs
  where
    forward ::
         (Ord k, Eq v)
      => ValuesMK  k v
      -> KeysMK    k v
      -> SeqDiffMK k v
      -> ValuesMK  k v
    forward (ValuesMK values) (KeysMK keys) (SeqDiffMK diffs) =
      ValuesMK $ AntiDiff.applyDiffForKeys values keys (DS.cumulativeDiff diffs)

forwardTableKeySets ::
     HasLedgerTables l
  => AnchorlessDbChangelog l
  -> UnforwardedReadSets l
  -> Either RewindReadFwdError
            (LedgerTables l ValuesMK)
forwardTableKeySets dblog =
  forwardTableKeySets'
    (adcLastFlushedSlot dblog)
    (adcDiffs dblog)

{-------------------------------------------------------------------------------
  Reset
-------------------------------------------------------------------------------}

-- | When creating a new @DbChangelog@, we should load whichever snapshot we
-- find and then replay the chain up to the immutable tip. When we get there,
-- the @DbChangelog@ will have a @k@-long sequence of states, which all come
-- from immutable blocks, so we just prune all of them and only keep the last
-- one as an anchor, as it is the immutable tip. Then we can proceed with
-- opening the VolatileDB.
--
-- If we didn't do this step, the @DbChangelog@ would accept rollbacks into the
-- immutable part of the chain, which must never be possible.
--
-- +--------------+----------------------------+----------------------+
-- |  lastFlushed | states                     | tableDiffs           |
-- +==============+============================+======================+
-- |     @L0@     | @L0 :> [ L1, L2, L3, L4 ]@ | @[ D1, D2, D3, D4 ]@ |
-- +--------------+----------------------------+----------------------+
-- | @>> pruneToImmTipOnly@                                           |
-- +--------------+----------------------------+----------------------+
-- |     @L0@     | @L4 :> [                ]@ | @[ D1, D2, D3, D4 ]@ |
-- +--------------+----------------------------+----------------------+
pruneToImmTipOnly :: GetTip l
                  => AnchorlessDbChangelog l
                  -> AnchorlessDbChangelog l
pruneToImmTipOnly = prune (SecurityParam 0)

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback @n@ ledger states.
--
-- Returns 'Nothing' if maximum rollback (usually @k@, but can be less on
-- startup or under corruption) is exceeded.
--
-- +--------------+------------------------+--------------------------+
-- |  lastFlushed | states                 | tableDiffs               |
-- +==============+========================+==========================+
-- |     @L2@     | @L3 :> [ L4, L5, L6 ]@ | @[ D2, D3, D4, D5, D6 ]@ |
-- +--------------+------------------------+--------------------------+
-- | @>> rollback 3@                                                  |
-- +--------------+------------------------+--------------------------+
-- |     @L2@     | @L3 :> [ ]           @ | @[ D2, D3             ]@ |
-- +--------------+------------------------+--------------------------+
rollbackN ::
     (GetTip l, HasLedgerTables l)
  => Word64
  -> AnchorlessDbChangelog l
  -> Maybe (AnchorlessDbChangelog l)
rollbackN n dblog
    | n <= maxRollback dblog
    = Just $ dblog {
        adcDiffs  = ltmap truncSeqDiff adcDiffs
      , adcStates = AS.dropNewest (fromIntegral n) adcStates
      }
    | otherwise
    = Nothing
  where
    truncSeqDiff :: (Ord k, Eq v) => SeqDiffMK k v -> SeqDiffMK k v
    truncSeqDiff (SeqDiffMK sq) =
      SeqDiffMK $ fst $ DS.splitAtFromEnd (fromIntegral n) sq

    AnchorlessDbChangelog {
        adcDiffs
      , adcStates
      } = dblog

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

-- | " Flush " the 'DbChangelog' by splitting it into the diffs that should be
-- flushed and the new 'DbChangelog'.
--
-- +--------------+------------------------+------------------------------------------+
-- |  lastFlushed | states                 | tableDiffs                               |
-- +==============+========================+==========================================+
-- |     @L2@     | @L3 :> [ L4, L5, L6 ]@ | @[ D2, D3, D4, D5, D6 ]@                 |
-- +--------------+------------------------+------------------------------------------+
-- | @>> splitForFlushing@                                                            |
-- +--------------+------------------------+------------------------------------------+
-- |     @L2@     | --                     | @[ D2, D3 ] -- this is a 'DiffsToFlush'@ |
-- +--------------+------------------------+------------------------------------------+
-- |     @L3@     | @L3 :> [ L4, L5, L6 ]@ | @[         D4, D5, D6 ]@                 |
-- +--------------+------------------------+------------------------------------------+
splitForFlushing ::
     forall l.
     (GetTip l, HasLedgerTables l)
  => DbChangelog l
  -> (Maybe (DiffsToFlush l), DbChangelog l)
splitForFlushing dblog =
    if getTipSlot immTip == Origin || ltcollapse (ltmap (K2 . DS.length . getSeqDiffMK) l) == 0
    then (Nothing, dblog)
    else (Just ldblog, rdblog)
  where
    DbChangelog {
        changelogLastFlushedState
      , anchorlessChangelog = AnchorlessDbChangelog {
            adcDiffs
          , adcStates
          }
      } = dblog

    immTip = AS.anchor adcStates

    splitSeqDiff ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
    splitSeqDiff (SeqDiffMK sq) =
       let numToFlush = DS.length sq - AS.length adcStates
       in bimap (maybe emptyMK SeqDiffMK) SeqDiffMK
        $ if numToFlush > 0
          then let (tf, tk) = DS.splitAt numToFlush sq
               in (Just tf, tk)
          else (Nothing, sq)

    lr = ltmap (uncurry Pair2 . splitSeqDiff) adcDiffs
    l  = ltmap (\(Pair2 x _) -> x) lr
    r  = ltmap (\(Pair2 _ y) -> y) lr

    (newTip, newStates) =
        if ltcollapse $ ltmap (\(SeqDiffMK sq) -> K2 $ 0 == DS.length sq) l
        then (changelogLastFlushedState, adcStates)
        else (immTip, adcStates)

    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (fromAntiDiff $ DS.cumulativeDiff sq)

    ldblog = DiffsToFlush {
        toFlushDiffs = ltmap prj l
      , toFlushSlot  =
            fromWithOrigin (error "Flushing a DbChangelog at origin should never happen")
          $ getTipSlot immTip
      }

    rdblog = DbChangelog {
        changelogLastFlushedState = newTip
      , anchorlessChangelog       = AnchorlessDbChangelog {
            adcLastFlushedSlot = getTipSlot newTip
          , adcDiffs           = r
          , adcStates          = newStates
          }
      }

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
current :: GetTip l => AnchorlessDbChangelog l -> l EmptyMK
current =
    either id id
  . AS.head
  . adcStates

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
anchor :: AnchorlessDbChangelog l -> l EmptyMK
anchor =
    AS.anchor
  . adcStates

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
snapshots :: AnchorlessDbChangelog l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . AS.toNewestFirst
    . adcStates

-- | How many blocks can we currently roll back?
maxRollback :: GetTip l => AnchorlessDbChangelog l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . adcStates

-- | Reference to the block at the tip of the chain
tip :: GetTip l => AnchorlessDbChangelog l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
isSaturated :: GetTip l => SecurityParam -> AnchorlessDbChangelog l -> Bool
isSaturated (SecurityParam k) db =
    maxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
getPastLedgerAt ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasLedgerTables l
     )
  => Point blk
  -> AnchorlessDbChangelog l
  -> Maybe (l EmptyMK)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Roll back the volatile states up to the specified point.
rollbackToPoint ::
     ( StandardHash l
     , GetTip l
     , HasLedgerTables l
     )
  => Point l -> AnchorlessDbChangelog l -> Maybe (AnchorlessDbChangelog l)
rollbackToPoint pt dblog = do
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . either id id)
        adcStates
    let ndropped = AS.length adcStates - AS.length vol'
        diffs'   = ltmap (trunc ndropped) adcDiffs
    Exn.assert (ndropped >= 0) $ pure AnchorlessDbChangelog {
          adcLastFlushedSlot
        , adcDiffs  = diffs'
        , adcStates = vol'
        }
  where
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs
      , adcStates
      } = dblog

-- | Rollback the volatile states up to the volatile anchor.
rollbackToAnchor ::
     (GetTip l, HasLedgerTables l)
  => AnchorlessDbChangelog l -> AnchorlessDbChangelog l
rollbackToAnchor dblog =
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs  = diffs'
      , adcStates = AS.Empty (AS.anchor vol)
      }
  where
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs
      , adcStates = vol
      } = dblog

    ndropped = AS.length vol
    diffs'   =
      ltmap (trunc ndropped) adcDiffs

trunc ::
     (Ord k, Eq v)
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (SeqDiffMK sq) =
  SeqDiffMK $ fst $ DS.splitAtFromEnd n sq

-- | Get a prefix of the DbChangelog that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasLedgerTables l
     )
  => Point blk
  -> AnchorlessDbChangelog l
  -> Maybe (AnchorlessDbChangelog l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just $ rollbackToAnchor db
    | otherwise
    = rollbackToPoint (castPoint pt) db

immutableTipSlot ::
     GetTip l
  => AnchorlessDbChangelog l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . AS.anchor
    . adcStates

-- | How many diffs we can flush to the backing store?
--
-- NOTE: This will be wrong once we have more than one table.
flushableLength :: (HasLedgerTables l, GetTip l)
                => AnchorlessDbChangelog l
                -> Word64
flushableLength chlog =
    (\(Sum x) -> x - fromIntegral (AS.length (adcStates chlog)))
  . ltcollapse
  . ltmap (K2 . f)
  $ adcDiffs chlog
 where
   f :: (Ord k, Eq v)
     => SeqDiffMK k v
     -> Sum Word64
   f (SeqDiffMK sq) = Sum $ fromIntegral $ DS.length sq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     AS.Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> DbChangelog l
  -> AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap f g
    . adcStates
    . anchorlessChangelog

{-------------------------------------------------------------------------------
  Testing
-------------------------------------------------------------------------------}
reapplyThenPush' :: ApplyBlock l blk
               => LedgerDbCfg l
               -> blk
               -> KeySetsReader Identity l
               -> AnchorlessDbChangelog l
               -> AnchorlessDbChangelog l
reapplyThenPush' cfg b bk = runIdentity . reapplyThenPush cfg b bk

reapplyThenPushMany' :: ApplyBlock l blk
                   => LedgerDbCfg l
                   -> [blk]
                   -> KeySetsReader Identity l
                   -> AnchorlessDbChangelog l
                   -> AnchorlessDbChangelog l
reapplyThenPushMany' cfg bs bk =
  runIdentity . reapplyThenPushMany cfg bs bk

reapplyThenPushMany ::
     (ApplyBlock l blk, Monad m)
  => LedgerDbCfg l
  -> [blk]
  -> KeySetsReader m l
  -> AnchorlessDbChangelog l
  -> m (AnchorlessDbChangelog l)
reapplyThenPushMany cfg aps ksReader =
  repeatedlyM (\ap -> reapplyThenPush cfg ap ksReader) aps

switch ::
     (ApplyBlock l blk, Monad m)
  => LedgerDbCfg l
  -> Word64
  -> [blk]
  -> KeySetsReader m l
  -> AnchorlessDbChangelog l
  -> m (Either ExceededRollback (AnchorlessDbChangelog l))
switch cfg numRollbacks newBlocks ksReader db =
  case rollbackN numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = maxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current DbChangelog
        _ -> Right <$> reapplyThenPushMany
                      cfg
                      newBlocks
                      ksReader
                      db'

switch' :: ApplyBlock l blk
        => LedgerDbCfg l
        -> Word64
        -> [blk]
        -> KeySetsReader Identity l
        -> AnchorlessDbChangelog l
        -> Maybe (AnchorlessDbChangelog l)
switch' cfg n bs bk db =
  case runIdentity $ switch cfg n bs bk db of
    Left  ExceededRollback{} -> Nothing
    Right db'                -> Just db'