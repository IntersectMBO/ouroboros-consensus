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
import           Data.Coerce
import           Data.SOP.Functors
import           Data.SOP.Strict
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           Legacy.LegacyBlock
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

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
    fmap (stowLedgerTables . applyDiffs tables) res
  where
    unstowedLedger = unstowLedgerTables lsb
    tables = projectLedgerTables unstowedLedger
    res = tickThenReapplyLedgerResult cfg block unstowedLedger

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
    , headerState = convertHeaderState headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

convertHeaderState ::
     HeaderState (CardanoBlock StandardCrypto)
  -> HeaderState (LegacyCardanoBlock StandardCrypto)
convertHeaderState hstate = HeaderState {
      headerStateTip = convertAnnTip <$> headerStateTip
    , headerStateChainDep = convertChainDepState headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hstate

convertAnnTip ::
     AnnTip (CardanoBlock StandardCrypto)
  -> AnnTip (LegacyCardanoBlock StandardCrypto)
convertAnnTip anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = convertTipInfo annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

convertTipInfo ::
     TipInfo (CardanoBlock StandardCrypto)
  -> TipInfo (LegacyCardanoBlock StandardCrypto)
convertTipInfo =
      OneEraTipInfo
    . htrans (Proxy @Convert) transOne
    . getOneEraTipInfo
  where
    transOne ::
         WrapTipInfo blk
      -> WrapTipInfo (LegacyBlock blk)
    transOne = WrapTipInfo . coerce . unwrapTipInfo

convertChainDepState ::
     ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
convertChainDepState  =
    htrans (Proxy @Convert) transOne
  where
    transOne ::
         WrapChainDepState blk
      -> WrapChainDepState (LegacyBlock blk)
    transOne = WrapChainDepState . coerce . unwrapChainDepState

convertExtLedgerState' ::
     ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK
convertExtLedgerState' est = ExtLedgerState {
      ledgerState = convertLedgerState' ledgerState
    , headerState = convertHeaderState' headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

convertHeaderState' ::
     HeaderState (LegacyCardanoBlock StandardCrypto)
  -> HeaderState (CardanoBlock StandardCrypto)
convertHeaderState' hstate = HeaderState {
      headerStateTip = convertAnnTip' <$> headerStateTip
    , headerStateChainDep = convertChainDepState' headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hstate

convertAnnTip' ::
     AnnTip (LegacyCardanoBlock StandardCrypto)
  -> AnnTip (CardanoBlock StandardCrypto)
convertAnnTip' anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = convertTipInfo' annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

convertTipInfo' ::
     TipInfo (LegacyCardanoBlock StandardCrypto)
  -> TipInfo (CardanoBlock StandardCrypto)
convertTipInfo' =
      OneEraTipInfo
    . htrans (Proxy @Convert') transOne
    . getOneEraTipInfo
  where
    transOne ::
         WrapTipInfo (LegacyBlock blk)
      -> WrapTipInfo blk
    transOne = WrapTipInfo . coerce . unwrapTipInfo

convertChainDepState' ::
     ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
convertChainDepState'  =
    htrans (Proxy @Convert') transOne
  where
    transOne ::
         WrapChainDepState (LegacyBlock blk)
      -> WrapChainDepState blk
    transOne = WrapChainDepState . coerce . unwrapChainDepState

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
