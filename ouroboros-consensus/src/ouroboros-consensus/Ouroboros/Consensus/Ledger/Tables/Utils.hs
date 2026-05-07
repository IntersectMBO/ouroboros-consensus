-- | Compatibility shim for the removed @Tables.Utils@ module.
--
-- The helpers that operated on tables-as-bifunctors are gone alongside the
-- @LedgerTables@ newtype and the @TrackingMK@ map kind. Where a direct
-- replacement exists, the new name is re-exported here.
--
-- Renames (same call shape, only the table-kind type names changed):
--
-- * @applyDiffs@, @prependDiffs@, @valuesAsDiffs@, @calculateDifference@,
--   @forgetLedgerTables@, @unionValues@ are still exported from
--   "Ouroboros.Consensus.Ledger.Tables".
-- * @applyDiffForKeys@/@applyDiffForKeysOnTables@ →
--   'Ouroboros.Consensus.Ledger.Tables.applyDiffRestricted'.
-- * @restrictValuesMK@ → 'Ouroboros.Consensus.Ledger.Tables.restrictValues'.
--
-- Removed without replacement:
--
-- * The @TrackingMK@ helpers: @trackingToDiffs@, @trackingToValues@,
--   @rawTrackingDiffs@, @attachAndApplyDiffs@ (and the @'@ variant),
--   @attachEmptyDiffs@, @prependTrackingDiffs@ (and the @'@ variant),
--   @rawAttachAndApplyDiffs@, @rawPrependTrackingDiffs@,
--   @calculateDifference'@. Producers now emit
--   'Ouroboros.Consensus.Ledger.Tables.Diffs' directly.
-- * The @lt*@/raw helpers @ltprj@, @ltwith@, @emptyLedgerTables@,
--   @noNewTickingDiffs@, @applyDiffs'@, @applyDiffsMK@,
--   @rawApplyDiffForKeys@, @rawCalculateDifference@, @rawPrependDiffs@.
module Ouroboros.Consensus.Ledger.Tables.Utils
  {-# DEPRECATED
    "Use Ouroboros.Consensus.Ledger.Tables. TrackingMK helpers were removed; lt*/raw* helpers and the *MK suffixed wrappers are gone. See the module docs for the rename table."
    #-}
  ( applyDiffRestricted
  , applyDiffs
  , calculateDifference
  , forgetLedgerTables
  , prependDiffs
  , restrictValues
  , unionValues
  , valuesAsDiffs
  ) where

import Ouroboros.Consensus.Ledger.Tables
  ( applyDiffRestricted
  , applyDiffs
  , calculateDifference
  , forgetLedgerTables
  , prependDiffs
  , restrictValues
  , unionValues
  , valuesAsDiffs
  )
