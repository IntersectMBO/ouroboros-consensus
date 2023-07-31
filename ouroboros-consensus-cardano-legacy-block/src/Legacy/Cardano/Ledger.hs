{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.Ledger () where

import           Control.Monad.Except
import           Data.SOP.Strict hiding (shape, tl)
import           Data.Void (Void)
import           GHC.Stack (HasCallStack)
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork (LegacyCardanoHardForkConstraints)
import           Legacy.LegacyBlock
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => IsLedger (LedgerState (LegacyCardanoBlock c)) where
  type LedgerErr (LedgerState (LegacyCardanoBlock c)) =
         LedgerErr (LedgerState (HardForkBlock (LegacyCardanoEras c)))

  type AuxLedgerEvent (LedgerState (LegacyCardanoBlock c)) =
         AuxLedgerEvent (LedgerState (HardForkBlock (LegacyCardanoEras c)))

  applyChainTickLedgerResult ::
       LedgerCfg (LedgerState (LegacyCardanoBlock c))
    -> SlotNo
    -> LedgerState (LegacyCardanoBlock c) EmptyMK
    -> LedgerResult
         (LedgerState (LegacyCardanoBlock c))
         (Ticked1 (LedgerState (LegacyCardanoBlock c)) DiffMK)
  applyChainTickLedgerResult cfg slot st0 =
        fmap castTickedLedgerState . castLedgerResult $ inner
    where
      st :: LedgerState (HardForkBlock (LegacyCardanoEras c)) EmptyMK
      st = getLegacyLedgerState st0

      inner :: LedgerResult
                 (LedgerState (HardForkBlock (LegacyCardanoEras c)))
                 (Ticked1 (LedgerState (HardForkBlock (LegacyCardanoEras c))) DiffMK)
      inner = applyChainTickLedgerResult cfg slot st

      castTickedLedgerState ::
           Ticked1 (LedgerState (HardForkBlock (LegacyCardanoEras c))) DiffMK
        -> Ticked1 (LedgerState (LegacyCardanoBlock c)) DiffMK
      castTickedLedgerState = TickedLegacyLedgerState
                            . flip withLedgerTables emptyLedgerTables

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => ApplyBlock (LedgerState (LegacyCardanoBlock c)) (LegacyCardanoBlock c) where
  applyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg (LedgerState (LegacyCardanoBlock c))
    -> LegacyCardanoBlock c
    -> Ticked1 (LedgerState (LegacyCardanoBlock c)) ValuesMK
    -> Except
         (LedgerErr (LedgerState (LegacyCardanoBlock c)))
         (LedgerResult
           (LedgerState (LegacyCardanoBlock c))
           (LedgerState (LegacyCardanoBlock c) DiffMK)
         )
  applyBlockLedgerResult cfg blk0 tst0 =
      fmap castLedgerState . castLedgerResult <$> inner
    where
      blk :: HardForkBlock (LegacyCardanoEras c)
      blk = getLegacyBlock blk0

      tst :: Ticked1 (LedgerState (HardForkBlock (LegacyCardanoEras c))) ValuesMK
      tst = flip withLedgerTables emptyLedgerTables
          $ getTickedLegacyLedgerState tst0

      inner :: Except
                 (LedgerErr (LedgerState (HardForkBlock (LegacyCardanoEras c))))
                 (LedgerResult
                   (LedgerState (HardForkBlock (LegacyCardanoEras c)))
                   (LedgerState (HardForkBlock (LegacyCardanoEras c)) DiffMK)
                 )
      inner = applyBlockLedgerResult cfg blk tst

      castLedgerState ::
           LedgerState (HardForkBlock (LegacyCardanoEras c)) DiffMK
        -> LedgerState (LegacyCardanoBlock c) DiffMK
      castLedgerState = LegacyLedgerState
                      . flip withLedgerTables emptyLedgerTables

  reapplyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg (LedgerState (LegacyCardanoBlock c))
    -> LegacyCardanoBlock c
    -> Ticked1 (LedgerState (LegacyCardanoBlock c)) ValuesMK
    -> LedgerResult
         (LedgerState (LegacyCardanoBlock c))
         (LedgerState (LegacyCardanoBlock c) DiffMK)
  reapplyBlockLedgerResult cfg blk0 tst0 =
      fmap castLedgerState . castLedgerResult $ inner
    where
      blk :: HardForkBlock (LegacyCardanoEras c)
      blk = getLegacyBlock blk0

      tst :: Ticked1 (LedgerState (HardForkBlock (LegacyCardanoEras c))) ValuesMK
      tst = flip withLedgerTables emptyLedgerTables
          $ getTickedLegacyLedgerState tst0

      inner :: LedgerResult
                 (LedgerState (HardForkBlock (LegacyCardanoEras c)))
                 (LedgerState (HardForkBlock (LegacyCardanoEras c)) DiffMK)
      inner = reapplyBlockLedgerResult cfg blk tst

      castLedgerState ::
           LedgerState (HardForkBlock (LegacyCardanoEras c)) DiffMK
        -> LedgerState (LegacyCardanoBlock c) DiffMK
      castLedgerState = LegacyLedgerState
                      . flip withLedgerTables emptyLedgerTables

  getBlockKeySets ::
        LegacyCardanoBlock c
    -> LedgerTables (LedgerState (LegacyCardanoBlock c)) KeysMK
  getBlockKeySets = const trivialLedgerTables

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

type instance Key   (LedgerState (HardForkBlock (LegacyCardanoEras c))) = Void
type instance Value (LedgerState (HardForkBlock (LegacyCardanoEras c))) = Void
instance CanSerializeLedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c)))

{-------------------------------------------------------------------------------
  LedgerTablesCanHardFork
-------------------------------------------------------------------------------}

instance LedgerTablesCanHardFork (LegacyCardanoEras c) where
  hardForkInjectLedgerTables =
         injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* injLegacyLedgerTables
      :* Nil
    where
      injLegacyLedgerTables :: InjectLedgerTables (LegacyCardanoEras c) (LegacyBlock blk)
      injLegacyLedgerTables = InjectLedgerTables {
          applyInjectLedgerTables  = const emptyLedgerTables
        , applyDistribLedgerTables = const trivialLedgerTables
        }
