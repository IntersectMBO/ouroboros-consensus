{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBSync (
    AppResult
  , CardanoBlock
  , ExtLedger
  , Ledger
  , LedgerB
  , reapplyBlock
  ) where

import           Cardano.Ledger.Crypto
import           Data.Map.Diff.Strict (applyDiff)
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           Legacy.LegacyBlock ()
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils

type ExtLedger = ExtLedgerState CardanoBlock EmptyMK
type AppResult = LedgerResult (LedgerState CardanoBlock) Ledger

type LedgerB blk = LedgerState blk EmptyMK
type Ledger = LedgerState CardanoBlock EmptyMK

type CardanoBlock = LegacyCardanoBlock StandardCrypto

reapplyBlock ::
  LegacyCardanoHardForkConstraints StandardCrypto =>
  LedgerCfg (LedgerState CardanoBlock) ->
  CardanoBlock ->
  Ledger ->
  AppResult
reapplyBlock cfg block lsb =
    fmap (stowLedgerTables . applyDiffToTables) res
  where
    unstowedLedger = unstowLedgerTables lsb
    tables = projectLedgerTables unstowedLedger
    res = tickThenReapplyLedgerResult cfg block unstowedLedger
    applyDiffToTables st = zipOverLedgerTables f st tables

    f :: Ord k => DiffMK k v -> ValuesMK k v -> ValuesMK k v
    f (DiffMK d) (ValuesMK v) = ValuesMK (applyDiff v d)
