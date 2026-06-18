{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | The single module that reads or writes the @UTxO@ field of a Shelley
-- 'SL.NewEpochState' (NES).
--
-- The UTxO lives /outside/ the ledger state, threaded as the
-- @'Values'@\/@'Diff'@ payloads of
-- 'Ouroboros.Consensus.Ledger.Basics.BlockSupportsUTxOHD'. The
-- cardano-ledger NES we wrap physically has a UTxO field (at
-- @nesEsL . esLStateL . lsUTxOStateL . utxoL@) that we neither own nor can
-- remove. If a stored ledger state retained a populated field, the LedgerDB's
-- @LedgerSeq@ would hold @~k@ duplicate copies of the whole UTxO, a
-- catastrophic space leak.
--
-- This module encapsulates that field behind 'NewEpochStateNoUTxOs', whose only
-- constructors set it to 'mempty'. The @ShelleyLedgerState@ stores a
-- 'NewEpochStateNoUTxOs', so \"every stored NES has an empty UTxO field\" is a
-- one-module truth rather than a codebase-wide discipline. The 'stowUTxO' \/
-- 'clearUTxOWithDiff' pair is the only place a UTxO is written into or read back
-- out of the field, and it is a lens /set/ (never a merge), so the values
-- supplied by the backend are always authoritative.
module Ouroboros.Consensus.Shelley.Ledger.LedgerCallShim
  ( -- * The UTxO-free new-epoch state
    NewEpochStateNoUTxOs -- opaque: the data constructor is intentionally hidden
  , mkNewEpochStateNoUTxOs
  , nesView
  , splitUTxO
  , stowUTxO

    -- * Ledger entry points
    -- $door
  , applyTickShim
  , applyBlockShim
  , applyTxShim
  , reapplyTxShim
  ) where

import qualified Cardano.Ledger.BHeaderView as SL (BHeaderView)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import Cardano.Slotting.Slot (SlotNo)
import Control.Exception (assert)
import Control.Monad.Except (Except, liftEither)
import qualified Control.State.Transition.Extended as STS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.Basics (ComputeLedgerEvents (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (WhetherToIntervene)
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Shelley.Eras
  ( ShelleyBasedEra
  , applyShelleyBasedTx
  )

-- | A 'SL.NewEpochState' whose UTxO field is guaranteed empty.
--
-- The data constructor is /not/ exported: the only ways to obtain a value are
-- 'mkNewEpochStateNoUTxOs' and 'clearUTxOWithDiff', both of which set the field
-- to 'mempty'.
newtype NewEpochStateNoUTxOs era = NewEpochStateNoUTxOs (SL.NewEpochState era)
  deriving stock Generic

deriving newtype instance Eq (SL.NewEpochState era) => Eq (NewEpochStateNoUTxOs era)
deriving newtype instance Show (SL.NewEpochState era) => Show (NewEpochStateNoUTxOs era)
deriving newtype instance
  NoThunks (SL.NewEpochState era) => NoThunks (NewEpochStateNoUTxOs era)

-- | The UTxO field lens. This is the /only/ reference to it in consensus.
utxoL ::
  Core.EraTxOut era =>
  Lens' (SL.NewEpochState era) (SL.UTxO era)
utxoL = SL.nesEsL . SL.esLStateL . SL.lsUTxOStateL . SL.utxoL

-- | A van-Laarhoven lens, kept local so the module needs no lens library type.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Establish the empty-UTxO-field invariant by clearing the field, asserting it
-- was already empty as a check on callers that promise a UTxO-free state.
mkNewEpochStateNoUTxOs ::
  Core.EraTxOut era => SL.NewEpochState era -> NewEpochStateNoUTxOs era
mkNewEpochStateNoUTxOs nes =
  assert (Map.null (SL.unUTxO (nes ^. utxoL))) $
    NewEpochStateNoUTxOs (nes & utxoL .~ mempty)

-- | Read-only access to the wrapped state. Safe to expose: the field is empty,
-- so callers cannot learn any UTxO entries from it.
nesView :: NewEpochStateNoUTxOs era -> SL.NewEpochState era
nesView (NewEpochStateNoUTxOs nes) = nes

-- | Split a 'SL.NewEpochState' that /does/ carry a UTxO (genesis, or a snapshot
-- with a populated field) into the UTxO-free state and the extracted entries.
-- Unlike 'mkNewEpochStateNoUTxOs' this does not assert the field is empty; it
-- is precisely the path that pulls a populated UTxO out into the backend.
splitUTxO ::
  Core.EraTxOut era =>
  SL.NewEpochState era ->
  (NewEpochStateNoUTxOs era, Map SL.TxIn (Core.TxOut era))
splitUTxO nes =
  ( NewEpochStateNoUTxOs (nes & utxoL .~ mempty)
  , SL.unUTxO (nes ^. utxoL)
  )

-- | Stow the read values into the (empty) UTxO field, yielding a raw NES ready
-- for a ledger computation. This is a lens /set/, so any stale field is
-- overwritten rather than merged: the supplied values are authoritative.
stowUTxO ::
  Core.EraTxOut era =>
  Map SL.TxIn (Core.TxOut era) ->
  NewEpochStateNoUTxOs era ->
  SL.NewEpochState era
stowUTxO values (NewEpochStateNoUTxOs nes) = nes & utxoL .~ SL.UTxO values

-- | Extract the resulting UTxO from a NES produced by a ledger computation,
-- clear the field (re-establishing the invariant), and compute the diff of the
-- result against the values that were stowed in.
clearUTxOWithDiff ::
  (Core.EraTxOut era, Eq (Core.TxOut era)) =>
  -- | The values that were read in (the pre-image).
  Map SL.TxIn (Core.TxOut era) ->
  SL.NewEpochState era ->
  (NewEpochStateNoUTxOs era, Diff.Diff SL.TxIn (Core.TxOut era))
clearUTxOWithDiff values nes =
  ( NewEpochStateNoUTxOs (nes & utxoL .~ mempty)
  , Diff.diff values (SL.unUTxO (nes ^. utxoL))
  )

-- $door
-- The functions below are the /sole/ callers of the cardano-ledger entry points
-- that read or write the UTxO field: TICK, BBODY, and the mempool tx
-- apply\/reapply. They take the UTxO 'Values' and a UTxO-free
-- 'NewEpochStateNoUTxOs' /separately/: each stows the values into the field
-- internally, runs the ledger rule, and clears the field on the way out (a lens
-- /set/, never a merge). A ledger call with the UTxO baked into the stored state
-- is therefore unexpressible outside this module, and an hlint rule forbids the
-- raw ledger functions ('SL.applyTick', 'SL.applyBlockEither', 'SL.applyTx',
-- 'SL.reapplyTx', …) elsewhere.
--
-- The block\/tick shims return the /raw/ ledger events; the caller maps them to
-- its own event type.

-- | TICK. Within a single era ticking does not touch the UTxO (the field stays
-- empty), so no 'Values' are needed and the result is again UTxO-free. Cross-era
-- UTxO changes (e.g. AVVM removal) happen in the hard-fork era translation, not
-- here.
applyTickShim ::
  ShelleyBasedEra era =>
  ComputeLedgerEvents ->
  SL.Globals ->
  SlotNo ->
  NewEpochStateNoUTxOs era ->
  (NewEpochStateNoUTxOs era, [STS.Event (Core.EraRule "TICK" era)])
applyTickShim evs globals slot nesNoUTxO =
  let nes = nesView nesNoUTxO
      (nes', events) = case evs of
        ComputeLedgerEvents -> SL.applyTick STS.EPReturn globals nes slot
        OmitLedgerEvents -> (SL.applyTickNoEvents globals nes slot, [])
   in (mkNewEpochStateNoUTxOs nes', events)

-- | BBODY (block application). Stow the read 'Values', run the rule, then clear
-- the field and diff the resulting UTxO against the read values.
applyBlockShim ::
  ( ShelleyBasedEra era
  , SL.ApplyBlock h era
  ) =>
  ComputeLedgerEvents ->
  STS.ValidationPolicy ->
  SL.Globals ->
  SL.Block h era ->
  Map SL.TxIn (Core.TxOut era) ->
  NewEpochStateNoUTxOs era ->
  Either
    (SL.BlockTransitionError era)
    ( NewEpochStateNoUTxOs era
    , Diff.Diff SL.TxIn (Core.TxOut era)
    , [STS.Event (Core.EraRule "BBODY" era)]
    )
applyBlockShim evs doValidate globals blk values nesNoUTxO = do
  let nesIn = stowUTxO values nesNoUTxO
  (nesOut, events) <- case evs of
    ComputeLedgerEvents -> SL.applyBlockEither STS.EPReturn doValidate globals nesIn blk
    OmitLedgerEvents -> (,[]) <$> SL.applyBlockEitherNoEvents doValidate globals nesIn blk
  let (nesCleared, diff) = clearUTxOWithDiff values nesOut
  pure (nesCleared, diff, events)

-- | Mempool tx apply. Stow the read 'Values', run the per-era tx rule, set the
-- updated ledger state back into the NES, then clear the field and diff.
applyTxShim ::
  ShelleyBasedEra era =>
  SL.Globals ->
  WhetherToIntervene ->
  SlotNo ->
  Core.Tx Core.TopTx era ->
  Map SL.TxIn (Core.TxOut era) ->
  NewEpochStateNoUTxOs era ->
  Except
    (SL.ApplyTxError era)
    ( NewEpochStateNoUTxOs era
    , Diff.Diff SL.TxIn (Core.TxOut era)
    , SL.Validated (Core.Tx Core.TopTx era)
    )
applyTxShim globals wti slot tx values nesNoUTxO = do
  let nesIn = stowUTxO values nesNoUTxO
  (mempoolState', vtx) <-
    applyShelleyBasedTx
      globals
      (SL.mkMempoolEnv nesIn slot)
      (SL.mkMempoolState nesIn)
      wti
      tx
  let (nesCleared, diff) =
        clearUTxOWithDiff values (nesIn & SL.overNewEpochState .~ mempoolState')
  pure (nesCleared, diff, vtx)

-- | Mempool tx reapply (no revalidation).
reapplyTxShim ::
  ShelleyBasedEra era =>
  SL.Globals ->
  SlotNo ->
  SL.Validated (Core.Tx Core.TopTx era) ->
  Map SL.TxIn (Core.TxOut era) ->
  NewEpochStateNoUTxOs era ->
  Except
    (SL.ApplyTxError era)
    (NewEpochStateNoUTxOs era, Diff.Diff SL.TxIn (Core.TxOut era))
reapplyTxShim globals slot vtx values nesNoUTxO = do
  let nesIn = stowUTxO values nesNoUTxO
  mempoolState' <-
    liftEither $
      SL.reapplyTx globals (SL.mkMempoolEnv nesIn slot) (SL.mkMempoolState nesIn) vtx
  let (nesCleared, diff) =
        clearUTxOWithDiff values (nesIn & SL.overNewEpochState .~ mempoolState')
  pure (nesCleared, diff)
