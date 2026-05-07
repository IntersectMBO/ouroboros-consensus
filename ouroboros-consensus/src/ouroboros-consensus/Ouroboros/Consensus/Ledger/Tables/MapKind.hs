-- | Compatibility shim for the removed @Tables.MapKind@ module.
--
-- The concrete map kinds have been renamed and re-indexed by @blk@ (so they
-- now have kind @Type -> Type@ instead of @Type -> Type -> Type@). Because the
-- kinds changed, deprecated type synonyms cannot be provided; callers must
-- update their type signatures. The renames are:
--
-- * @EmptyMK@ → 'Ouroboros.Consensus.Ledger.Tables.NoTables'
-- * @KeysMK@ → 'Ouroboros.Consensus.Ledger.Tables.Keys'
-- * @ValuesMK@ → 'Ouroboros.Consensus.Ledger.Tables.Values'
-- * @DiffMK@ → 'Ouroboros.Consensus.Ledger.Tables.Diffs'
-- * @ZeroableMK@ → 'Ouroboros.Consensus.Ledger.Tables.EmptyTable'
-- * @CanMapMK@/@CanMapKeysMK@/@bimapLedgerTables@ → folded into the
--   'Ouroboros.Consensus.Ledger.Tables.BimapTables' class.
--
-- The following entities have been removed without replacement:
--
-- * @TrackingMK@ and all its instances. Producers now emit
--   'Ouroboros.Consensus.Ledger.Tables.Diffs' directly.
-- * @CodecMK@. Per-table CBOR is handled via
--   'Ouroboros.Consensus.Ledger.Tables.SerializeTablesWithHint'.
-- * The @ShowMK@/@EqMK@/@NoThunksMK@ convenience superclasses (instances are
--   constrained on @TxIn blk@ / @TxOut blk@ directly now).
module Ouroboros.Consensus.Ledger.Tables.MapKind
  {-# DEPRECATED
    "Use Ouroboros.Consensus.Ledger.Tables. EmptyMK→NoTables, KeysMK→Keys, ValuesMK→Values, DiffMK→Diffs, ZeroableMK→EmptyTable, CanMapMK/CanMapKeysMK/bimapLedgerTables→BimapTables. TrackingMK and CodecMK have been removed."
    #-}
  (
  ) where
