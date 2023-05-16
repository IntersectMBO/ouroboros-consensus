{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.Block (
    LegacyCardanoBlock
  , LegacyCardanoEras
  , LegacyCardanoShelleyEras
  ) where

import           Control.Monad.Except
import           Data.Kind
import           Data.Proxy
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Strict hiding (shape, tl)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Legacy.LegacyBlock
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Ledger

type LegacyCardanoEras :: Type -> [Type]
type LegacyCardanoEras c =  LegacyBlock ByronBlock
                         ': LegacyCardanoShelleyEras c

type LegacyCardanoShelleyEras :: Type -> [Type]
type LegacyCardanoShelleyEras c =
  '[ LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (BabbageEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (ConwayEra c))
   ]

type LegacyCardanoBlock c = LegacyBlock (HardForkBlock (LegacyCardanoEras c))

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

instance CanHardFork (LegacyCardanoEras c)
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
                            . flip withLedgerTablesTicked emptyLedgerTables

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance CanHardFork (LegacyCardanoEras c)
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
      tst = flip withLedgerTablesTicked emptyLedgerTables
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
      tst = flip withLedgerTablesTicked emptyLedgerTables
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
  getBlockKeySets = const NoLegacyLedgerTables

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c))) where
  data instance LedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c))) mk =
      NoLegacyCardanoLedgerTables
    deriving (Show, Eq, Generic, NoThunks)

instance LedgerTablesAreTrivial (LedgerState (HardForkBlock (LegacyCardanoEras c))) where
  convertMapKind (HardForkLedgerState x) = HardForkLedgerState $
      hcmap
        (Proxy @(Compose LedgerTablesAreTrivial LedgerState))
        (Flip . convertMapKind . unFlip) x

  trivialLedgerTables = NoLegacyCardanoLedgerTables

instance CanSerializeLedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c)))

instance CanStowLedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c)))

instance HasTickedLedgerTables (LedgerState (HardForkBlock (LegacyCardanoEras c))) where
  withLedgerTablesTicked (TickedHardForkLedgerState x st) NoLegacyCardanoLedgerTables =
      TickedHardForkLedgerState x $
        hcmap
          (Proxy @(And
                    (Compose HasTickedLedgerTables LedgerState)
                    (Compose LedgerTablesAreTrivial LedgerState)
          ))
          ( FlipTickedLedgerState
          . flip withLedgerTablesTicked trivialLedgerTables
          . getFlipTickedLedgerState
          )
          st

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
        , applyDistribLedgerTables = const NoLegacyLedgerTables
        }
