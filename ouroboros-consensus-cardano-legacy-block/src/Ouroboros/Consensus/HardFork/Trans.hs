{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Ouroboros.Consensus.HardFork.Trans (
    -- * Header
    hcoerce_Header
    -- * NestedCtxt_
  , hcoerce_NestedCtxt_
  , htrans_NestedCtxt_
    -- * ProtocolClientInfo
  , hcoerce_ProtocolClientInfo
  , htrans_ProtocolClientInfo
    -- * TopLevelConfig
  , hcoerce_TopLevelConfig
  , htrans_TopLevelConfig
    -- * ConsensusConfig
  , hcoerce_ConsensusConfig
  , htrans_ConsensusConfig
    -- * LedgerConfig
  , hcoerce_LedgerConfig
  , htrans_LedgerConfig
    -- * BlockConfig
  , hcoerce_BlockConfig
  , htrans_BlockConfig
    -- * CodecConfig
  , hcoerce_CodecConfig
  , htrans_CodecConfig
    -- * StorageConfig
  , hcoerce_StorageConfig
  , htrans_StorageConfig
    -- * Shape
  , hcoerce_Shape
  , htrans_Shape
    -- * CanBeLeader
  , hcoerce_CanBeLeader
  , htrans_CanBeLeader
    -- * LedgerState
  , hcoerce_LedgerState
  , htrans_LedgerState
    -- * Ticked LedgerState
  , hcoerce_TickedLedgerState
  , htrans_TickedLedgerState
    -- * ExtLedgerState
  , hcoerce_ExtLedgerState
  , htrans_ExtLedgerState
    -- * HeaderState
  , hcoerce_HeaderState
  , htrans_HeaderState
    -- * AnnTip
  , hcoerce_AnnTip
  , htrans_AnnTip
    -- * TipInfo
  , hcoerce_TipInfo
  , htrans_TipInfo
    -- * ChainDepState
  , hcoerce_ChainDepState
  , htrans_ChainDepState
    -- * Ticked ChainDepState
  , hcoerce_TickedChainDepState
    -- * ForgeStateUpdateInfo
  , hcoerce_ForgeStateUpdateInfo
    -- * ForgeStateInfo
  , hcoerce_ForgeStateInfo
    -- * EraIndex
  , hcoerce_EraIndex
    -- * CannotForge
  , hcoerce_CannotForge
    -- * IsLeader
  , hcoerce_IsLeader
    -- * HardForkBlock
  , hcoerce_HardForkBlock
    -- * Validated GenTx
  , hcoerce_ValidatedGenTx
    -- * ProtocolInfo
  , hcoerce_ProtocolInfo
  , htrans_ProtocolInfo
    -- * ApplyTxErr
  , hcoerce_ApplyTxErr
    -- * GenTxId
  , hcoerce_GenTxId
    -- * GenTx
  , hcoerce_GenTx
  ) where

import           Data.Coerce
import           Data.SOP hiding (Shape (..))
import           Data.SOP.Counting
import           Data.SOP.Functors
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

hcoerce_Header ::
     AllZip (LiftedCoercible Header Header) xs ys
  => Header (HardForkBlock xs)
  -> Header (HardForkBlock ys)
hcoerce_Header =
      HardForkHeader
    . OneEraHeader
    . hcoerce
    . getOneEraHeader
    . getHardForkHeader

{-------------------------------------------------------------------------------
  NestedCtxt_
-------------------------------------------------------------------------------}

class Coercible (NestedCtxt_ x f a) (NestedCtxt_ y f a)
   => CoercibleNestedCtxt_ f a x y
instance Coercible (NestedCtxt_ x f a) (NestedCtxt_ y f a)
      => CoercibleNestedCtxt_ f a x y

htrans_NestedCtxt_ ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => NestedCtxt_ x f a -> NestedCtxt_ y f a)
  -> NestedCtxt_ (HardForkBlock xs) f a
  -> NestedCtxt_ (HardForkBlock ys) f a
htrans_NestedCtxt_ p t = \case
    NCZ x  -> NCZ $ t x
    NCS xs -> NCS $ htrans_NestedCtxt_ p t xs

hcoerce_NestedCtxt_ ::
     AllZip (CoercibleNestedCtxt_ f a) xs ys
  => NestedCtxt_ (HardForkBlock xs) f a
  -> NestedCtxt_ (HardForkBlock ys) f a
hcoerce_NestedCtxt_ = \case
    NCZ x  -> NCZ $ coerce x
    NCS xs -> NCS $ hcoerce_NestedCtxt_ xs

{-------------------------------------------------------------------------------
  ProtocolClientInfo
-------------------------------------------------------------------------------}

htrans_ProtocolClientInfo ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => CodecConfig x -> CodecConfig y)
  -> ProtocolClientInfo (HardForkBlock xs)
  -> ProtocolClientInfo (HardForkBlock ys)
htrans_ProtocolClientInfo p f pcinfo = ProtocolClientInfo {
      pClientInfoCodecConfig = htrans_CodecConfig p f pClientInfoCodecConfig
    }
  where
    ProtocolClientInfo {
        pClientInfoCodecConfig
      } = pcinfo

hcoerce_ProtocolClientInfo ::
     AllZip (LiftedCoercible CodecConfig CodecConfig) xs ys
  => ProtocolClientInfo (HardForkBlock xs)
  -> ProtocolClientInfo (HardForkBlock ys)
hcoerce_ProtocolClientInfo pcinfo = ProtocolClientInfo {
      pClientInfoCodecConfig = hcoerce_CodecConfig pClientInfoCodecConfig
    }
  where
    ProtocolClientInfo {
        pClientInfoCodecConfig
      } = pcinfo

{-------------------------------------------------------------------------------
  TopLevelConfig
-------------------------------------------------------------------------------}

htrans_TopLevelConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => K EraParams x -> K EraParams y)
  -> (forall x y. c x y => WrapPartialConsensusConfig x -> WrapPartialConsensusConfig y)
  -> (forall x y. c x y => WrapPartialLedgerConfig x -> WrapPartialLedgerConfig y)
  -> (forall x y. c x y => BlockConfig x -> BlockConfig y)
  -> (forall x y. c x y => CodecConfig x -> CodecConfig y)
  -> (forall x y. c x y => StorageConfig x -> StorageConfig y)
  -> TopLevelConfig (HardForkBlock xs)
  -> TopLevelConfig (HardForkBlock ys)
htrans_TopLevelConfig p f1 f2 f3 f4 f5 f6 tlcfg = TopLevelConfig {
      topLevelConfigProtocol = htrans_ConsensusConfig p f1 f2 topLevelConfigProtocol
    , topLevelConfigLedger = htrans_LedgerConfig p f1 f3 topLevelConfigLedger
    , topLevelConfigBlock = htrans_BlockConfig p f4 topLevelConfigBlock
    , topLevelConfigCodec = htrans_CodecConfig p f5 topLevelConfigCodec
    , topLevelConfigStorage = htrans_StorageConfig p f6 topLevelConfigStorage
    }
  where
    TopLevelConfig {
        topLevelConfigProtocol
      , topLevelConfigLedger
      , topLevelConfigBlock
      , topLevelConfigCodec
      , topLevelConfigStorage
      } = tlcfg

hcoerce_TopLevelConfig ::
     ( AllZip (LiftedCoercible (K EraParams) (K EraParams)) xs ys
     , AllZip (LiftedCoercible WrapPartialConsensusConfig WrapPartialConsensusConfig) xs ys
     , AllZip (LiftedCoercible WrapPartialLedgerConfig WrapPartialLedgerConfig) xs ys
     , AllZip (LiftedCoercible BlockConfig BlockConfig) xs ys
     , AllZip (LiftedCoercible CodecConfig CodecConfig) xs ys
     , AllZip (LiftedCoercible StorageConfig StorageConfig) xs ys
     )
  => TopLevelConfig (HardForkBlock xs)
  -> TopLevelConfig (HardForkBlock ys)
hcoerce_TopLevelConfig tlcfg = TopLevelConfig {
      topLevelConfigProtocol = hcoerce_ConsensusConfig topLevelConfigProtocol
    , topLevelConfigLedger = hcoerce_LedgerConfig topLevelConfigLedger
    , topLevelConfigBlock = hcoerce_BlockConfig topLevelConfigBlock
    , topLevelConfigCodec = hcoerce_CodecConfig topLevelConfigCodec
    , topLevelConfigStorage = hcoerce_StorageConfig topLevelConfigStorage
    }
  where
    TopLevelConfig {
        topLevelConfigProtocol
      , topLevelConfigLedger
      , topLevelConfigBlock
      , topLevelConfigCodec
      , topLevelConfigStorage
      } = tlcfg

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

htrans_ConsensusConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => K EraParams x -> K EraParams y)
  -> (forall x y. c x y => WrapPartialConsensusConfig x -> WrapPartialConsensusConfig y)
  -> ConsensusConfig (BlockProtocol (HardForkBlock xs))
  -> ConsensusConfig (BlockProtocol (HardForkBlock ys))
htrans_ConsensusConfig p f f' ccfg = HardForkConsensusConfig {
      hardForkConsensusConfigK
    , hardForkConsensusConfigShape = htrans_Shape p f hardForkConsensusConfigShape
    , hardForkConsensusConfigPerEra =
          PerEraConsensusConfig
        . htrans p f'
        . getPerEraConsensusConfig
        $ hardForkConsensusConfigPerEra
    }
  where
    HardForkConsensusConfig {
        hardForkConsensusConfigK
      , hardForkConsensusConfigShape
      , hardForkConsensusConfigPerEra
      } = ccfg

hcoerce_ConsensusConfig ::
     ( AllZip (LiftedCoercible (K EraParams) (K EraParams)) xs ys
     , AllZip (LiftedCoercible WrapPartialConsensusConfig WrapPartialConsensusConfig) xs ys
     )
  => ConsensusConfig (BlockProtocol (HardForkBlock xs))
  -> ConsensusConfig (BlockProtocol (HardForkBlock ys))
hcoerce_ConsensusConfig ccfg = HardForkConsensusConfig {
      hardForkConsensusConfigK
    , hardForkConsensusConfigShape = hcoerce_Shape hardForkConsensusConfigShape
    , hardForkConsensusConfigPerEra =
          PerEraConsensusConfig
        . hcoerce
        . getPerEraConsensusConfig
        $ hardForkConsensusConfigPerEra
    }
  where
    HardForkConsensusConfig {
        hardForkConsensusConfigK
      , hardForkConsensusConfigShape
      , hardForkConsensusConfigPerEra
      } = ccfg

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

htrans_LedgerConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => K EraParams x -> K EraParams y)
  -> (forall x y. c x y => WrapPartialLedgerConfig x -> WrapPartialLedgerConfig y)
  -> LedgerConfig (HardForkBlock xs)
  -> LedgerConfig (HardForkBlock ys)
htrans_LedgerConfig p f g hfLedgerCfg = HardForkLedgerConfig {
      hardForkLedgerConfigShape = htrans_Shape p f hardForkLedgerConfigShape
    , hardForkLedgerConfigPerEra =
        PerEraLedgerConfig
      . htrans p g
      . getPerEraLedgerConfig
      $ hardForkLedgerConfigPerEra
    }
  where
    HardForkLedgerConfig {
        hardForkLedgerConfigShape
      , hardForkLedgerConfigPerEra
      } = hfLedgerCfg

hcoerce_LedgerConfig ::
     ( AllZip (LiftedCoercible (K EraParams) (K EraParams)) xs ys
     , AllZip (LiftedCoercible WrapPartialLedgerConfig WrapPartialLedgerConfig) xs ys
     )
  => LedgerConfig (HardForkBlock xs)
  -> LedgerConfig (HardForkBlock ys)
hcoerce_LedgerConfig hfLedgerCfg = HardForkLedgerConfig {
      hardForkLedgerConfigShape = hcoerce_Shape hardForkLedgerConfigShape
    , hardForkLedgerConfigPerEra =
        PerEraLedgerConfig
      . hcoerce
      . getPerEraLedgerConfig
      $ hardForkLedgerConfigPerEra
    }
  where
    HardForkLedgerConfig {
        hardForkLedgerConfigShape
      , hardForkLedgerConfigPerEra
      } = hfLedgerCfg

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

htrans_BlockConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => BlockConfig x -> BlockConfig y)
  -> BlockConfig (HardForkBlock xs)
  -> BlockConfig (HardForkBlock ys)
htrans_BlockConfig p f =
      HardForkBlockConfig
    . PerEraBlockConfig
    . htrans p f
    . getPerEraBlockConfig
    . hardForkBlockConfigPerEra

hcoerce_BlockConfig ::
     AllZip (LiftedCoercible BlockConfig BlockConfig) xs ys
  => BlockConfig (HardForkBlock xs)
  -> BlockConfig (HardForkBlock ys)
hcoerce_BlockConfig =
      HardForkBlockConfig
    . PerEraBlockConfig
    . hcoerce
    . getPerEraBlockConfig
    . hardForkBlockConfigPerEra

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

htrans_CodecConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => CodecConfig x -> CodecConfig y)
  -> CodecConfig (HardForkBlock xs)
  -> CodecConfig (HardForkBlock ys)
htrans_CodecConfig p f =
      HardForkCodecConfig
    . PerEraCodecConfig
    . htrans p f
    . getPerEraCodecConfig
    . hardForkCodecConfigPerEra

hcoerce_CodecConfig ::
     AllZip (LiftedCoercible CodecConfig CodecConfig) xs ys
  => CodecConfig (HardForkBlock xs)
  -> CodecConfig (HardForkBlock ys)
hcoerce_CodecConfig =
      HardForkCodecConfig
    . PerEraCodecConfig
    . hcoerce
    . getPerEraCodecConfig
    . hardForkCodecConfigPerEra

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

htrans_StorageConfig ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => StorageConfig x -> StorageConfig y)
  -> StorageConfig (HardForkBlock xs)
  -> StorageConfig (HardForkBlock ys)
htrans_StorageConfig p f =
      HardForkStorageConfig
    . PerEraStorageConfig
    . htrans p f
    . getPerEraStorageConfig
    . hardForkStorageConfigPerEra

hcoerce_StorageConfig ::
     AllZip (LiftedCoercible StorageConfig StorageConfig) xs ys
  => StorageConfig (HardForkBlock xs)
  -> StorageConfig (HardForkBlock ys)
hcoerce_StorageConfig=
      HardForkStorageConfig
    . PerEraStorageConfig
    . hcoerce
    . getPerEraStorageConfig
    . hardForkStorageConfigPerEra

{-------------------------------------------------------------------------------
  Shape
-------------------------------------------------------------------------------}

htrans_Shape ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => K EraParams x -> K EraParams y)
  -> Shape xs
  -> Shape ys
htrans_Shape p f =
      Shape
    . Exactly
    . htrans p f
    . getExactly
    . getShape

hcoerce_Shape ::
     AllZip (LiftedCoercible (K EraParams) (K EraParams)) xs ys
  => Shape xs
  -> Shape ys
hcoerce_Shape =
      Shape
    . Exactly
    . hcoerce
    . getExactly
    . getShape

{-------------------------------------------------------------------------------
  CanBeLeader
-------------------------------------------------------------------------------}

htrans_CanBeLeader ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => WrapCanBeLeader x -> WrapCanBeLeader y)
  -> CanBeLeader (BlockProtocol (HardForkBlock xs))
  -> CanBeLeader (BlockProtocol (HardForkBlock ys))
htrans_CanBeLeader p f =
      SomeErasCanBeLeader
    . htrans p f
    . getSomeErasCanBeLeader

hcoerce_CanBeLeader ::
     AllZip (LiftedCoercible WrapCanBeLeader WrapCanBeLeader) xs ys
  => CanBeLeader (BlockProtocol (HardForkBlock xs))
  -> CanBeLeader (BlockProtocol (HardForkBlock ys))
hcoerce_CanBeLeader =
      SomeErasCanBeLeader
    . hcoerce
    . getSomeErasCanBeLeader


{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

htrans_LedgerState ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => Flip LedgerState mk x -> Flip LedgerState mk y)
  -> LedgerState (HardForkBlock xs) mk
  -> LedgerState (HardForkBlock ys) mk
htrans_LedgerState p f =
      HardForkLedgerState
    . htrans p f
    . hardForkLedgerStatePerEra

hcoerce_LedgerState ::
     AllZip (LiftedCoercible (Flip LedgerState mk) (Flip LedgerState mk)) xs ys
  => LedgerState (HardForkBlock xs) mk
  -> LedgerState (HardForkBlock ys) mk
hcoerce_LedgerState =
      HardForkLedgerState
    . hcoerce
    . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticked LedgerState
-------------------------------------------------------------------------------}

htrans_TickedLedgerState ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => FlipTickedLedgerState mk x -> FlipTickedLedgerState mk y)
  -> Ticked1 (LedgerState (HardForkBlock xs)) mk
  -> Ticked1 (LedgerState (HardForkBlock ys)) mk
htrans_TickedLedgerState p f tlst = TickedHardForkLedgerState {
      tickedHardForkLedgerStateTransition
    , tickedHardForkLedgerStatePerEra = htrans p f tickedHardForkLedgerStatePerEra
    }
  where
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition
      , tickedHardForkLedgerStatePerEra
      } = tlst

hcoerce_TickedLedgerState ::
     AllZip (LiftedCoercible (FlipTickedLedgerState mk) (FlipTickedLedgerState mk)) xs ys
  => Ticked1 (LedgerState (HardForkBlock xs)) mk
  -> Ticked1 (LedgerState (HardForkBlock ys)) mk
hcoerce_TickedLedgerState tlst = TickedHardForkLedgerState {
      tickedHardForkLedgerStateTransition
    , tickedHardForkLedgerStatePerEra = hcoerce tickedHardForkLedgerStatePerEra
    }
  where
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition
      , tickedHardForkLedgerStatePerEra
      } = tlst

{-------------------------------------------------------------------------------
  ExtLedgerState
-------------------------------------------------------------------------------}

htrans_ExtLedgerState ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => Flip LedgerState mk x -> Flip LedgerState mk y)
  -> (forall x y. c x y => WrapTipInfo x -> WrapTipInfo y)
  -> (forall x y. c x y => WrapChainDepState x -> WrapChainDepState y)
  -> ExtLedgerState (HardForkBlock xs) mk
  -> ExtLedgerState (HardForkBlock ys) mk
htrans_ExtLedgerState p f f' f'' est = ExtLedgerState {
      ledgerState = htrans_LedgerState p f ledgerState
    , headerState = htrans_HeaderState p f' f'' headerState
    }
  where
    ExtLedgerState {
        ledgerState
      , headerState
      } = est

hcoerce_ExtLedgerState ::
     ( AllZip (LiftedCoercible (Flip LedgerState mk) (Flip LedgerState mk)) xs ys
     , AllZip (LiftedCoercible WrapTipInfo WrapTipInfo) xs ys
     , AllZip (LiftedCoercible WrapChainDepState WrapChainDepState) xs ys
     )
  => ExtLedgerState (HardForkBlock xs) mk
  -> ExtLedgerState (HardForkBlock ys) mk
hcoerce_ExtLedgerState est = ExtLedgerState {
      ledgerState = hcoerce_LedgerState ledgerState
    , headerState = hcoerce_HeaderState headerState
    }
  where
    ExtLedgerState {
        ledgerState
      , headerState
      } = est

{-------------------------------------------------------------------------------
  HeaderState
-------------------------------------------------------------------------------}

htrans_HeaderState ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => WrapTipInfo x -> WrapTipInfo y)
  -> (forall x y. c x y => WrapChainDepState x -> WrapChainDepState y)
  -> HeaderState (HardForkBlock xs)
  -> HeaderState (HardForkBlock ys)
htrans_HeaderState p f f' hst = HeaderState {
      headerStateTip = htrans_AnnTip p f <$> headerStateTip
    , headerStateChainDep = htrans_ChainDepState p f' headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hst

hcoerce_HeaderState ::
     ( AllZip (LiftedCoercible WrapTipInfo WrapTipInfo) xs ys
     , AllZip (LiftedCoercible WrapChainDepState WrapChainDepState) xs ys
     )
  => HeaderState (HardForkBlock xs)
  -> HeaderState (HardForkBlock ys)
hcoerce_HeaderState hst = HeaderState {
      headerStateTip = hcoerce_AnnTip <$> headerStateTip
    , headerStateChainDep = hcoerce_ChainDepState headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hst

{-------------------------------------------------------------------------------
  AnnTip
-------------------------------------------------------------------------------}

htrans_AnnTip ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => WrapTipInfo x -> WrapTipInfo y)
  -> AnnTip (HardForkBlock xs)
  -> AnnTip (HardForkBlock ys)
htrans_AnnTip p f anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = htrans_TipInfo p f annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

hcoerce_AnnTip ::
     AllZip (LiftedCoercible WrapTipInfo WrapTipInfo) xs ys
  => AnnTip (HardForkBlock xs)
  -> AnnTip (HardForkBlock ys)
hcoerce_AnnTip anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = hcoerce_TipInfo annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

htrans_TipInfo ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => WrapTipInfo x -> WrapTipInfo y)
  -> TipInfo (HardForkBlock xs)
  -> TipInfo (HardForkBlock ys)
htrans_TipInfo p f = OneEraTipInfo . htrans p f . getOneEraTipInfo

hcoerce_TipInfo ::
     AllZip (LiftedCoercible WrapTipInfo WrapTipInfo) xs ys
  => TipInfo (HardForkBlock xs)
  -> TipInfo (HardForkBlock ys)
hcoerce_TipInfo = OneEraTipInfo . hcoerce . getOneEraTipInfo

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

htrans_ChainDepState ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => WrapChainDepState x -> WrapChainDepState y)
  -> ChainDepState (BlockProtocol (HardForkBlock xs))
  -> ChainDepState (BlockProtocol (HardForkBlock ys))
htrans_ChainDepState = htrans

hcoerce_ChainDepState ::
     AllZip (LiftedCoercible WrapChainDepState WrapChainDepState) xs ys
  => ChainDepState (BlockProtocol (HardForkBlock xs))
  -> ChainDepState (BlockProtocol (HardForkBlock ys))
hcoerce_ChainDepState = hcoerce

{-------------------------------------------------------------------------------
  Ticked ChainDepState
-------------------------------------------------------------------------------}

hcoerce_TickedChainDepState ::
     AllZip
       (LiftedCoercible (Ticked :.: WrapChainDepState) (Ticked :.: WrapChainDepState))
       xs
       ys
  => Ticked (ChainDepState (BlockProtocol (HardForkBlock xs)))
  -> Ticked (ChainDepState (BlockProtocol (HardForkBlock ys)))
hcoerce_TickedChainDepState tcdst = TickedHardForkChainDepState {
      tickedHardForkChainDepStatePerEra = hcoerce tickedHardForkChainDepStatePerEra
    , tickedHardForkChainDepStateEpochInfo
    }
  where
    TickedHardForkChainDepState {
        tickedHardForkChainDepStatePerEra
      , tickedHardForkChainDepStateEpochInfo
      } = tcdst

{-------------------------------------------------------------------------------
  ForgeStateUpdateInfo
-------------------------------------------------------------------------------}

hcoerce_ForgeStateUpdateInfo ::
     ( AllZip (LiftedCoercible WrapForgeStateInfo WrapForgeStateInfo) xs ys
     , AllZip (LiftedCoercible WrapForgeStateUpdateError WrapForgeStateUpdateError) xs ys
     , AllZip (LiftedCoercible (K ()) (K ())) xs ys
     )
  => ForgeStateUpdateInfo (HardForkBlock xs)
  -> ForgeStateUpdateInfo (HardForkBlock ys)
hcoerce_ForgeStateUpdateInfo = \case
    ForgeStateUpdated fsi -> ForgeStateUpdated $ hcoerce_ForgeStateInfo fsi
    ForgeStateUpdateFailed fsue -> ForgeStateUpdateFailed $
      hcoerce_ForgeStateUpdateError fsue
    ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

{-------------------------------------------------------------------------------
  ForgeStateInfo
-------------------------------------------------------------------------------}

hcoerce_ForgeStateInfo ::
     ( AllZip (LiftedCoercible WrapForgeStateInfo WrapForgeStateInfo) xs ys
     , AllZip (LiftedCoercible (K ()) (K ())) xs ys
     )
  => ForgeStateInfo (HardForkBlock xs)
  -> ForgeStateInfo (HardForkBlock ys)
hcoerce_ForgeStateInfo = \case
    CurrentEraLacksBlockForging ix -> CurrentEraLacksBlockForging $
      hcoerce_EraIndex ix
    CurrentEraForgeStateUpdated fsi ->
        CurrentEraForgeStateUpdated
      . OneEraForgeStateInfo
      . hcoerce
      . getOneEraForgeStateInfo
      $ fsi

{-------------------------------------------------------------------------------
  ForgeStateUpdateError
-------------------------------------------------------------------------------}

hcoerce_ForgeStateUpdateError ::
     AllZip (LiftedCoercible WrapForgeStateUpdateError WrapForgeStateUpdateError) xs ys
  => ForgeStateUpdateError (HardForkBlock xs)
  -> ForgeStateUpdateError (HardForkBlock ys)
hcoerce_ForgeStateUpdateError =
      OneEraForgeStateUpdateError
    . hcoerce
    . getOneEraForgeStateUpdateError

{-------------------------------------------------------------------------------
  EraIndex
-------------------------------------------------------------------------------}

hcoerce_EraIndex ::
     AllZip (LiftedCoercible (K ()) (K ())) xs ys
  => EraIndex xs
  -> EraIndex ys
hcoerce_EraIndex =
      EraIndex
    . hcoerce
    . getEraIndex

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

hcoerce_CannotForge ::
     AllZip (LiftedCoercible WrapCannotForge WrapCannotForge) xs ys
  => CannotForge (HardForkBlock xs)
  -> CannotForge (HardForkBlock ys)
hcoerce_CannotForge =
      OneEraCannotForge
    . hcoerce
    . getOneEraCannotForge

{-------------------------------------------------------------------------------
  IsLeader
-------------------------------------------------------------------------------}

hcoerce_IsLeader ::
     AllZip (LiftedCoercible WrapIsLeader WrapIsLeader) xs ys
  => IsLeader (BlockProtocol (HardForkBlock xs))
  -> IsLeader (BlockProtocol (HardForkBlock ys))
hcoerce_IsLeader =
      OneEraIsLeader
    . hcoerce
    . getOneEraIsLeader

{-------------------------------------------------------------------------------
  HardForkBlock
-------------------------------------------------------------------------------}

hcoerce_HardForkBlock ::
     AllZip (LiftedCoercible I I) xs ys
  => HardForkBlock xs
  -> HardForkBlock ys
hcoerce_HardForkBlock =
    HardForkBlock
  . OneEraBlock
  . hcoerce
  . getOneEraBlock
  . getHardForkBlock

{-------------------------------------------------------------------------------
  Validated GenTx
-------------------------------------------------------------------------------}

hcoerce_ValidatedGenTx ::
     AllZip (LiftedCoercible WrapValidatedGenTx WrapValidatedGenTx) xs ys
  => Validated (GenTx (HardForkBlock xs))
  -> Validated (GenTx (HardForkBlock ys))
hcoerce_ValidatedGenTx =
    HardForkValidatedGenTx
  . OneEraValidatedGenTx
  . hcoerce
  . getOneEraValidatedGenTx
  . getHardForkValidatedGenTx

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

hcoerce_ProtocolInfo ::
     ( AllZip (LiftedCoercible (K EraParams) (K EraParams)) xs ys
     , AllZip (LiftedCoercible WrapPartialConsensusConfig WrapPartialConsensusConfig) xs ys
     , AllZip (LiftedCoercible WrapPartialLedgerConfig WrapPartialLedgerConfig) xs ys
     , AllZip (LiftedCoercible BlockConfig BlockConfig) xs ys
     , AllZip (LiftedCoercible CodecConfig CodecConfig) xs ys
     , AllZip (LiftedCoercible StorageConfig StorageConfig) xs ys
     , AllZip (LiftedCoercible (Flip LedgerState ValuesMK) (Flip LedgerState ValuesMK)) xs ys
     , AllZip (LiftedCoercible WrapTipInfo WrapTipInfo) xs ys
     , AllZip (LiftedCoercible WrapChainDepState WrapChainDepState) xs ys
     )
  => ProtocolInfo (HardForkBlock xs)
  -> ProtocolInfo (HardForkBlock ys)
hcoerce_ProtocolInfo pinfo = ProtocolInfo {
      pInfoConfig     = hcoerce_TopLevelConfig pInfoConfig
    , pInfoInitLedger = hcoerce_ExtLedgerState pInfoInitLedger
    }
  where
    ProtocolInfo {
        pInfoConfig
      , pInfoInitLedger
      } = pinfo

htrans_ProtocolInfo ::
     AllZip c xs ys
  => proxy c
  -> (forall x y. c x y => K EraParams x -> K EraParams y)
  -> (forall x y. c x y => WrapPartialConsensusConfig x -> WrapPartialConsensusConfig y)
  -> (forall x y. c x y => WrapPartialLedgerConfig x -> WrapPartialLedgerConfig y)
  -> (forall x y. c x y => BlockConfig x -> BlockConfig y)
  -> (forall x y. c x y => CodecConfig x -> CodecConfig y)
  -> (forall x y. c x y => StorageConfig x -> StorageConfig y)
  -> (forall x y. c x y => Flip LedgerState ValuesMK x -> Flip LedgerState ValuesMK y)
  -> (forall x y. c x y => WrapTipInfo x -> WrapTipInfo y)
  -> (forall x y. c x y => WrapChainDepState x -> WrapChainDepState y)
  -> ProtocolInfo (HardForkBlock xs)
  -> ProtocolInfo (HardForkBlock ys)
htrans_ProtocolInfo p f1 f2 f3 f4 f5 f6 f7 f8 f9 pinfo = ProtocolInfo {
      pInfoConfig     = htrans_TopLevelConfig p f1 f2 f3 f4 f5 f6 pInfoConfig
    , pInfoInitLedger = htrans_ExtLedgerState p f7 f8 f9 pInfoInitLedger
    }
  where
    ProtocolInfo {
        pInfoConfig
      , pInfoInitLedger
      } = pinfo

{-------------------------------------------------------------------------------
  ApplyTxErr
-------------------------------------------------------------------------------}

hcoerce_ApplyTxErr ::
     ( AllZip (LiftedCoercible WrapApplyTxErr WrapApplyTxErr) xs ys
     , AllZip (LiftedCoercible LedgerEraInfo LedgerEraInfo) xs ys
     )
  => ApplyTxErr (HardForkBlock xs)
  -> ApplyTxErr (HardForkBlock ys)
hcoerce_ApplyTxErr = \case
    HardForkApplyTxErrFromEra err -> HardForkApplyTxErrFromEra
                                   . OneEraApplyTxErr
                                   . hcoerce
                                   . getOneEraApplyTxErr
                                   $ err
    HardForkApplyTxErrWrongEra err -> HardForkApplyTxErrWrongEra
                                    . MismatchEraInfo
                                    . hcoerce
                                    . getMismatchEraInfo
                                    $ err

{-------------------------------------------------------------------------------
  GenTxId
-------------------------------------------------------------------------------}

hcoerce_GenTxId ::
     AllZip (LiftedCoercible WrapGenTxId WrapGenTxId) xs ys
  => GenTxId (HardForkBlock xs)
  -> GenTxId (HardForkBlock ys)
hcoerce_GenTxId =
      HardForkGenTxId
    . OneEraGenTxId
    . hcoerce
    . getOneEraGenTxId
    . getHardForkGenTxId

{-------------------------------------------------------------------------------
  GenTx
-------------------------------------------------------------------------------}

hcoerce_GenTx ::
     AllZip (LiftedCoercible GenTx GenTx) xs ys
  => GenTx (HardForkBlock xs)
  -> GenTx (HardForkBlock ys)
hcoerce_GenTx =
      HardForkGenTx
    . OneEraGenTx
    . hcoerce
    . getOneEraGenTx
    . getHardForkGenTx
