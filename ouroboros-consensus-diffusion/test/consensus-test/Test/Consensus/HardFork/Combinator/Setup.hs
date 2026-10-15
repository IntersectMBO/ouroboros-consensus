{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator.Setup (TestBlock) where

import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.SOP.BasicFunctors
import Data.SOP.InPairs (RequiringBoth (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index (Index (..), hcimap)
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import Data.Void (Void, absurd)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Condense ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  , TrivialTxMeasurePhase2 (..)
  , TxEbMeasure
  , TxLimits (..)
  , TxMeasure (..)
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import Test.Consensus.HardFork.Combinator.A
import Test.Consensus.HardFork.Combinator.B

{-------------------------------------------------------------------------------
  Canonical TxIn
-------------------------------------------------------------------------------}

instance HasCanonicalTxIn '[BlockA, BlockB] where
  newtype CanonicalTxIn '[BlockA, BlockB] = BlockABTxIn
    { getBlockABTxIn :: Void
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NoThunks, MemPack)

  injectCanonicalTxIn IZ key = absurd key
  injectCanonicalTxIn (IS IZ) key = absurd key
  injectCanonicalTxIn (IS (IS idx')) _ = case idx' of {}

  ejectCanonicalTxIn _ key = absurd $ getBlockABTxIn key

instance HasHardForkTxOut '[BlockA, BlockB] where
  type HardForkTxOut '[BlockA, BlockB] = DefaultHardForkTxOut '[BlockA, BlockB]
  injectHardForkTxOut = injectHardForkTxOutDefault
  ejectHardForkTxOut = ejectHardForkTxOutDefault

{-------------------------------------------------------------------------------
  Hard fork
-------------------------------------------------------------------------------}

type TestBlock = HardForkBlock '[BlockA, BlockB]

instance CanHardFork '[BlockA, BlockB] where
  type HardForkTxMeasurePhase1 '[BlockA, BlockB] = IgnoringOverflow ByteSize32
  type HardForkTxMeasurePhase2 '[BlockA, BlockB] = TrivialTxMeasurePhase2
  type HardForkTxEbMeasure '[BlockA, BlockB] = TxEbMeasure BlockB

  hardForkEraTranslation =
    EraTranslation
      { translateLedgerState = PCons ledgerState_AtoB PNil
      , translateLedgerTables = PCons ledgerTables_AtoB PNil
      , translateChainDepState = PCons chainDepState_AtoB PNil
      , crossEraForecast = PCons forecast_AtoB PNil
      }
  hardForkChainSel = Tails.mk2 NoTiebreakerAcrossEras

  hardForkInjTxMeasurePhase1 = \case
    (Z (WrapTxMeasurePhase1 x)) -> x
    S (Z (WrapTxMeasurePhase1 x)) -> x

  hardForkInjTxMeasurePhase2 = \case
    (Z (WrapTxMeasurePhase2 x)) -> x
    S (Z (WrapTxMeasurePhase2 x)) -> x

  hardForkInjTxEbMeasure = \case
    (Z (WrapTxEbMeasure (TxMeasure p1 p2))) ->
      txEbMeasure (Proxy @BlockB) (TxMeasure p1 p2)
    S (Z (WrapTxEbMeasure x)) -> x

  hardForkTxEbMeasure _ p1 p2 = txEbMeasure (Proxy @BlockB) (TxMeasure p1 p2)

  hardForkMempoolEbReservation _ eb =
    let TxMeasure p1 p2 = mempoolEbReservation (Proxy @BlockB) eb in (p1, p2)

  hardForkEqGenTxId = (==) `on` rawHashNS
  hardForkCompareGenTxId = compare `on` rawHashNS

versionN2N :: BlockNodeToNodeVersion TestBlock
versionN2N =
  HardForkNodeToNodeEnabled
    maxBound
    ( WrapNodeToNodeVersion ()
        :* WrapNodeToNodeVersion ()
        :* Nil
    )

versionN2C :: BlockNodeToClientVersion TestBlock
versionN2C =
  HardForkNodeToClientEnabled
    maxBound
    ( EraNodeToClientEnabled ()
        :* EraNodeToClientEnabled ()
        :* Nil
    )

instance SupportedNetworkProtocolVersion TestBlock where
  supportedNodeToNodeVersions _ = Map.singleton maxBound versionN2N
  supportedNodeToClientVersions _ = Map.singleton maxBound versionN2C

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

instance SerialiseHFC '[BlockA, BlockB]

-- Use defaults

instance SerializeTablesWithHint LedgerState (HardForkBlock '[BlockA, BlockB]) where
  encodeTablesWithHint = defaultEncodeTablesWithHint
  decodeTablesWithHint = defaultDecodeTablesWithHint

instance
  IndexedMemPack
    LedgerState
    (HardForkBlock '[BlockA, BlockB])
    (DefaultHardForkTxOut '[BlockA, BlockB])
  where
  indexedTypeName _ _ = typeName @(DefaultHardForkTxOut '[BlockA, BlockB])
  indexedPackedByteCount _ txout =
    hcollapse $
      hcmap
        (Proxy @MemPackTxOut)
        (K . packedByteCount . unwrapTxOut)
        txout
  indexedPackM _ =
    hcollapse
      . hcimap
        (Proxy @MemPackTxOut)
        ( \_ (WrapTxOut txout) -> K $ do
            packM txout
        )
  indexedUnpackM (HardForkLedgerState (HardForkState idx)) = do
    hsequence'
      $ hcmap
        (Proxy @MemPackTxOut)
        (const $ Comp $ WrapTxOut <$> unpackM)
      $ Telescope.tip idx

instance
  IndexedMemPack
    (Ticked LedgerState)
    (HardForkBlock '[BlockA, BlockB])
    (DefaultHardForkTxOut '[BlockA, BlockB])
  where
  indexedTypeName _ _ = typeName @(DefaultHardForkTxOut '[BlockA, BlockB])
  indexedPackedByteCount _ txout =
    hcollapse $
      hcmap
        (Proxy @MemPackTxOut)
        (K . packedByteCount . unwrapTxOut)
        txout
  indexedPackM _ =
    hcollapse
      . hcimap
        (Proxy @MemPackTxOut)
        ( \_ (WrapTxOut txout) -> K $ do
            packM txout
        )
  indexedUnpackM (TickedHardForkLedgerState _ (HardForkState idx)) = do
    hsequence'
      $ hcmap
        (Proxy @MemPackTxOut)
        (const $ Comp $ WrapTxOut <$> unpackM)
      $ Telescope.tip idx

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

ledgerState_AtoB ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    BlockA
    BlockB
ledgerState_AtoB =
  InPairs.ignoringBoth $
    TranslateLedgerState
      { translateLedgerStateWith = \_ LgrA{..} ->
          LgrB
            { lgrB_tip = castPoint lgrA_tip
            }
      }

ledgerTables_AtoB :: TranslateLedgerTables BlockA BlockB
ledgerTables_AtoB =
  TranslateLedgerTables
    { translateTxInWith = id
    , translateTxOutWith = id
    }

chainDepState_AtoB ::
  RequiringBoth
    WrapConsensusConfig
    (Translate WrapChainDepState)
    BlockA
    BlockB
chainDepState_AtoB = InPairs.ignoringBoth $ Translate $ \_ _ ->
  WrapChainDepState ()

forecast_AtoB ::
  RequiringBoth
    WrapLedgerConfig
    (CrossEraForecaster LedgerState WrapLedgerView)
    BlockA
    BlockB
forecast_AtoB = InPairs.ignoringBoth $ CrossEraForecaster $ \_ _ _ ->
  return $
    WrapLedgerView ()

{-------------------------------------------------------------------------------
  Query HF
-------------------------------------------------------------------------------}

instance BlockSupportsHFLedgerQuery '[BlockA, BlockB] where
  answerBlockQueryHFLookup IZ _ q = case q of {}
  answerBlockQueryHFLookup (IS IZ) _cfg q = case q of {}
  answerBlockQueryHFLookup (IS (IS idx)) _cfg _q = case idx of {}

  answerBlockQueryHFTraverse IZ _cfg q = case q of {}
  answerBlockQueryHFTraverse (IS IZ) _cfg q = case q of {}
  answerBlockQueryHFTraverse (IS (IS idx)) _cfg _q = case idx of {}

  queryLedgerGetTraversingFilter IZ q = case q of {}
  queryLedgerGetTraversingFilter (IS IZ) q = case q of {}
  queryLedgerGetTraversingFilter (IS (IS idx)) _q = case idx of {}
