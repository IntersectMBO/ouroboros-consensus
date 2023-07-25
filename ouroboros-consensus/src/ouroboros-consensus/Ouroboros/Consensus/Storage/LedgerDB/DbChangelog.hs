{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A 'DbChangelog' is a data structure that holds a sequence of "virtual"
-- ledger states by internally maintaining:
--
-- - A sequence of in-memory ledger states for the volatile part of the chain.
--
-- - A ledger state that is the last flushed state. Usually this will coincide
-- - with the immutable tip, but this is not necessarily the case.
--
-- - A sequence of differences that are associated with each ledger state and
--   represent the delta between the associated ledger state and its predecesor.
--   These differences are defined with respect to a 'BackingStore' that
--   provides the set of values at the anchor of the sequence, i.e. at the last
--   flushed state.
--
-- This design is based on the technical report "Storing the Cardano ledger
-- state on disk: analysis and design options" by Duncan Coutts and Douglas
-- Wilson.
--
-- See 'DbChangelog' for more information.
module Ouroboros.Consensus.Storage.LedgerDB.DbChangelog (
    -- * The DbChangelog
    DbChangelog (..)
  , DbChangelog'
  , StatesSequence
    -- * Construction
  , empty
    -- * Views
  , AnchorlessDbChangelog (..)
  , AnchorlessDbChangelog'
  , onChangelog
  , onChangelogM
    -- * Mapping
  , mapAnchorlessDbChangelog
  , mapDbChangelog
  ) where

import           Cardano.Slotting.Slot
import           Data.Functor.Identity
import           Data.SOP (K, unK)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

-- | Holds a sequence of split ledger states, where the in-memory part is in a
-- sequence and the on-disk part is represented by a sequence of differences
-- that need a @BackingStore@ as an anchor point.
--
-- We illustrate its contents below, where @k = 3@ (for a state @Li@, the
-- corresponding set of differences is @Di@):
--
-- >  lastFlushed | states                           | tableDiffs
-- > -------------------------------------------------------------
-- >       0      | L0 :> [ ]                        | [ ]
-- >       0      | L0 :> [ L1 ]                     | [ D1 ]
-- >       0      | L0 :> [ L1, L2 ]                 | [ D1, D2 ]
-- >       0      | L0 :> [ L1, L2, L3 ]             | [ D1, D2, D3 ]
-- >       0      | L1 :> [     L2, L3, L4 ]         | [ D1, D2, D3, D4 ]
-- >       0      | L2 :> [         L3, L4, L5 ]     | [ D1, D2, D3, D4, D5 ]    (*)
-- >       2      | L2 :> [         L3, L4, L5 ]     | [         D3, D4, D5 ]   -- flush (**)
-- >       2      | L3 :> [             L4, L5, L6 ] | [         D3, D4, D5, D6 ]
--
-- The disk anchor moves when we flush data to disk. Notice that @length states@
-- is usually @k@ except when rollbacks or data corruption take place and will
-- be less than @k@ when we just loaded a snapshot. We cannot roll back more
-- than @k@ blocks. This means that after a rollback of @k@ blocks at (*), the
-- changelog will look something like this:
--
-- >       0      | L2 :> [ ]                        | [ D1, D2 ]
--
-- And a rollback of @k@ blocks at (**) will look something like this:
--
-- >       2      | L2 :> [ ]                        | [ ]
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

    -- | The in memory part of the DbChangelog
  , anchorlessChangelog       :: !(AnchorlessDbChangelog l)
  }
  deriving (Generic)

deriving instance (Eq       (Key l), Eq       (Value l), Eq       (l EmptyMK))
               =>  Eq       (DbChangelog l)
deriving instance (NoThunks (Key l), NoThunks (Value l), NoThunks (l EmptyMK))
               =>  NoThunks (DbChangelog l)
deriving instance (Show     (Key l), Show     (Value l), Show     (l EmptyMK))
               =>  Show     (DbChangelog l)

mapDbChangelog :: GetTip l'
               => (l EmptyMK -> l' EmptyMK)
               -> (LedgerTables l SeqDiffMK -> LedgerTables l' SeqDiffMK)
               -> DbChangelog l
               -> DbChangelog l'
mapDbChangelog mapState mapTable dblog =
  DbChangelog {
      changelogLastFlushedState = mapState changelogLastFlushedState
    , anchorlessChangelog       =
        mapAnchorlessDbChangelog mapState mapTable anchorlessChangelog
    }
  where
    DbChangelog { changelogLastFlushedState, anchorlessChangelog } = dblog

-- | A 'DbChangelog' variant that contains only the information in memory. To
-- perform reads of Ledger Tables, this needs to be coupled with a
-- BackingStoreValueHandle as done in 'LedgerDBView'.
data AnchorlessDbChangelog l = AnchorlessDbChangelog {
    -- | Slot of the last flushed changelog state from which this variant
    -- originated. Used just for asserting correctness when forwarding
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

mapAnchorlessDbChangelog :: GetTip l'
                         => (l EmptyMK -> l' EmptyMK)
                         -> (LedgerTables l SeqDiffMK -> LedgerTables l' SeqDiffMK)
                         -> AnchorlessDbChangelog l
                         -> AnchorlessDbChangelog l'
mapAnchorlessDbChangelog mapState mapTable adb =
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs           = mapTable adcDiffs
      , adcStates          = AS.bimap mapState mapState adcStates
      }
  where
    AnchorlessDbChangelog { adcLastFlushedSlot, adcDiffs, adcStates } = adb

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
  Views
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
