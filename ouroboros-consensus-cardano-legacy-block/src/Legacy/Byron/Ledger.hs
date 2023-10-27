{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Byron.Ledger () where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Config (castTopLevelConfig)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Legacy.Util

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

instance IsLedger (LedgerState ByronBlock)
      => IsLedger (LedgerState (LegacyBlock ByronBlock)) where

  type LedgerErr (LedgerState (LegacyBlock ByronBlock)) =
          LedgerErr (LedgerState ByronBlock)

  type AuxLedgerEvent (LedgerState (LegacyBlock ByronBlock)) =
          AuxLedgerEvent (LedgerState ByronBlock)

  applyChainTickLedgerResult cfg slot st0 =
      fmap castTickedLedgerState . castLedgerResult $ inner
    where
      st :: LedgerState ByronBlock EmptyMK
      st = getLegacyLedgerState st0

      inner :: LedgerResult
                 (LedgerState ByronBlock)
                 (Ticked1 (LedgerState ByronBlock) DiffMK)
      inner = applyChainTickLedgerResult cfg slot st

      castTickedLedgerState ::
           Ticked1 (LedgerState ByronBlock) DiffMK
        -> Ticked1 (LedgerState (LegacyBlock ByronBlock)) DiffMK
      castTickedLedgerState = TickedLegacyLedgerState
                            . flip withLedgerTables emptyLedgerTables

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ByronBlock) ByronBlock
      => ApplyBlock (LedgerState (LegacyBlock ByronBlock)) (LegacyBlock ByronBlock) where
  applyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg (LedgerState (LegacyBlock ByronBlock))
    -> LegacyBlock ByronBlock
    -> Ticked1 (LedgerState (LegacyBlock ByronBlock)) ValuesMK
    -> Except
         (LedgerErr (LedgerState (LegacyBlock ByronBlock)))
         (LedgerResult
           (LedgerState (LegacyBlock ByronBlock))
           (LedgerState (LegacyBlock ByronBlock) DiffMK)
         )
  applyBlockLedgerResult cfg blk0 tst0 =
      fmap castLedgerState . castLedgerResult <$> inner
    where
      blk :: ByronBlock
      blk = getLegacyBlock blk0

      tst :: Ticked1 (LedgerState ByronBlock) ValuesMK
      tst = flip withLedgerTables emptyLedgerTables
          $ getTickedLegacyLedgerState tst0

      inner :: Except
                 (LedgerErr (LedgerState ByronBlock))
                 (LedgerResult
                   (LedgerState ByronBlock)
                   (LedgerState ByronBlock DiffMK)
                 )
      inner = applyBlockLedgerResult cfg blk tst

      castLedgerState ::
           LedgerState ByronBlock DiffMK
        -> LedgerState (LegacyBlock ByronBlock) DiffMK
      castLedgerState = LegacyLedgerState
                      . flip withLedgerTables emptyLedgerTables

  reapplyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg (LedgerState (LegacyBlock ByronBlock))
    -> LegacyBlock ByronBlock
    -> Ticked1 (LedgerState (LegacyBlock ByronBlock)) ValuesMK
    -> LedgerResult
         (LedgerState (LegacyBlock ByronBlock))
         (LedgerState (LegacyBlock ByronBlock) DiffMK)
  reapplyBlockLedgerResult cfg blk0 tst0 =
      fmap castLedgerState . castLedgerResult $ inner
    where
      blk :: ByronBlock
      blk = getLegacyBlock blk0

      tst :: Ticked1 (LedgerState ByronBlock) ValuesMK
      tst = flip withLedgerTables emptyLedgerTables
          $ getTickedLegacyLedgerState tst0

      inner :: LedgerResult
                 (LedgerState ByronBlock)
                 (LedgerState ByronBlock DiffMK)
      inner = reapplyBlockLedgerResult cfg blk tst

      castLedgerState ::
           LedgerState ByronBlock DiffMK
        -> LedgerState (LegacyBlock ByronBlock) DiffMK
      castLedgerState = LegacyLedgerState
                      . flip withLedgerTables emptyLedgerTables

  getBlockKeySets ::
       LegacyBlock ByronBlock
    -> LedgerTables (LedgerState (LegacyBlock ByronBlock)) KeysMK
  getBlockKeySets = const trivialLedgerTables

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

instance BlockSupportsLedgerQuery (LegacyBlock ByronBlock) where
  answerPureBlockQuery cfg (LegacyBlockQuery GetUpdateInterfaceState) ext =
    answerPureBlockQuery (castExtLedgerCfg castTopLevelConfig cfg) GetUpdateInterfaceState $
      castExtLedgerState coerce ext
  answerBlockQueryLookup _cfg q _dlv = case q of {}
  answerBlockQueryTraverse _cfg q _dlv = case q of {}
