{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module DBSync (
    initLedger
  , reapplyBlock
    -- * Conversion
  , Convert (..)
  , Convert'
  , convertExtLedgerState
  , convertExtLedgerState'
  , convertLedgerState
  , convertLedgerState'
  ) where

import           Cardano.Ledger.Crypto
import           Data.Map.Diff.Strict (applyDiff)
import           Data.SOP.Functors
import           Data.SOP.Strict
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           Legacy.LegacyBlock
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.ProtocolInfo

initLedger ::
     ProtocolInfo IO (LegacyCardanoBlock StandardCrypto)
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
initLedger = stowLedgerTables . pInfoInitLedger

reapplyBlock ::
     LegacyCardanoHardForkConstraints StandardCrypto
  => ExtLedgerCfg (LegacyCardanoBlock StandardCrypto)
  -> LegacyCardanoBlock StandardCrypto
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> LedgerResult
       (ExtLedgerState (LegacyCardanoBlock StandardCrypto))
       (ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK)
reapplyBlock cfg block lsb =
    fmap (stowLedgerTables . applyDiffToTables) res
  where
    unstowedLedger = unstowLedgerTables lsb
    tables = projectLedgerTables unstowedLedger
    res = tickThenReapplyLedgerResult cfg block unstowedLedger
    applyDiffToTables st = zipOverLedgerTables f st tables

    f :: Ord k => DiffMK k v -> ValuesMK k v -> ValuesMK k v
    f (DiffMK d) (ValuesMK v) = ValuesMK (applyDiff v d)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

convertLedgerState ::
     LedgerState (CardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
convertLedgerState =
      LegacyLedgerState
    . HardForkLedgerState
    . htrans (Proxy @Convert) transOne
    . hardForkLedgerStatePerEra
  where
    transOne ::
             (Flip LedgerState EmptyMK) x
          -> (Flip LedgerState EmptyMK) (LegacyBlock x)
    transOne = Flip . mkLegacyLedgerState . unFlip

convertLedgerState' ::
     LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (CardanoBlock StandardCrypto) EmptyMK
convertLedgerState' =
      HardForkLedgerState
    . htrans (Proxy @Convert') transOne
    . hardForkLedgerStatePerEra
    . getLegacyLedgerState
  where
    transOne ::
             (Flip LedgerState EmptyMK) (LegacyBlock x)
          -> (Flip LedgerState EmptyMK) x
    transOne = Flip . unmkLegacyLedgerState . unFlip

convertExtLedgerState ::
     ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
convertExtLedgerState est = ExtLedgerState {
      ledgerState = convertLedgerState ledgerState
    , headerState = undefined headerState -- TODO: castHeaderState headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

convertExtLedgerState' ::
     ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK
convertExtLedgerState' est = ExtLedgerState {
      ledgerState = convertLedgerState' ledgerState
    , headerState = undefined headerState -- TODO: castHeaderState headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

class y ~ LegacyBlock x => Convert x y where
  mkLegacyBlock :: x -> y
  mkLegacyLedgerState :: LedgerState x EmptyMK -> LedgerState y EmptyMK
  unmkLegacyBlock :: y -> x
  unmkLegacyLedgerState :: LedgerState y EmptyMK -> LedgerState x EmptyMK

class Convert x y => Convert' y x

instance Convert x y => Convert' y x

instance Convert x (LegacyBlock x) where
  mkLegacyBlock = LegacyBlock
  mkLegacyLedgerState = LegacyLedgerState
  unmkLegacyBlock = getLegacyBlock
  unmkLegacyLedgerState = getLegacyLedgerState
