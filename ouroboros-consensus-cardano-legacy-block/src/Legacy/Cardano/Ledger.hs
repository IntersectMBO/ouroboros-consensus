{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.Ledger () where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Control.Monad.Except
import           Data.SOP.Index
import           Data.Void (Void, absurd)
import           GHC.Stack (HasCallStack)
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Legacy.Block

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
  Queries
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => BlockSupportsLedgerQuery (LegacyCardanoBlock c) where
  answerPureBlockQuery _ = undefined -- TODO
  answerBlockQueryLookup _cfg q _dlv = case q of {}
  answerBlockQueryTraverse _cfg q _dlv = case q of {}

{-------------------------------------------------------------------------------
  Tables
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => HasCanonicalTxIn (LegacyCardanoEras c) where
  newtype instance CanonicalTxIn (LegacyCardanoEras c) = LegacyCardanoTxIn {
      getLegacyCardanoTxIn :: Void
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NoThunks, FromCBOR, ToCBOR)

  injectCanonicalTxIn IZ  byronTxIn   = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
      IZ                               -> absurd shelleyTxIn
      IS IZ                            -> absurd shelleyTxIn
      IS (IS IZ)                       -> absurd shelleyTxIn
      IS (IS (IS IZ))                  -> absurd shelleyTxIn
      IS (IS (IS (IS IZ)))             -> absurd shelleyTxIn
      IS (IS (IS (IS (IS IZ))))        -> absurd shelleyTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  distribCanonicalTxIn _ key = absurd $ getLegacyCardanoTxIn key

  encodeCanonicalTxIn = toCBOR

  decodeCanonicalTxIn = fromCBOR

instance LegacyCardanoHardForkConstraints c => HasHardForkTxOut (LegacyCardanoEras c) where
  type instance HardForkTxOut (LegacyCardanoEras c) = Void
  injectHardForkTxOut idx txOut = case idx of
      IZ                                    -> absurd txOut
      IS IZ                                 -> absurd txOut
      IS (IS IZ)                            -> absurd txOut
      IS (IS (IS IZ))                       -> absurd txOut
      IS (IS (IS (IS IZ)))                  -> absurd txOut
      IS (IS (IS (IS (IS IZ))))             -> absurd txOut
      IS (IS (IS (IS (IS (IS IZ)))))        -> absurd txOut
      IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}
  distribHardForkTxOut idx txOut =  case idx of
      IZ                                    -> absurd txOut
      IS IZ                                 -> absurd txOut
      IS (IS IZ)                            -> absurd txOut
      IS (IS (IS IZ))                       -> absurd txOut
      IS (IS (IS (IS IZ)))                  -> absurd txOut
      IS (IS (IS (IS (IS IZ))))             -> absurd txOut
      IS (IS (IS (IS (IS (IS IZ)))))        -> absurd txOut
      IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}

instance LegacyCardanoHardForkConstraints c => SerializeHardForkTxOut (LegacyCardanoEras c) where
  encodeHardForkTxOut _ = toCBOR
  decodeHardForkTxOut _ = fromCBOR
