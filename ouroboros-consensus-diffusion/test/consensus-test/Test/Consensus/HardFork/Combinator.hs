{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator (tests) where

import Cardano.Ledger.BaseTypes (nonZero, unNonZero)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.SOP.BasicFunctors
import Data.SOP.Counting
import Data.SOP.Functors (Flip (..))
import Data.SOP.InPairs (RequiringBoth (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index (Index (..), hcimap)
import Data.SOP.OptNP (OptNP (..))
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import Data.Void (Void, absurd)
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Condense ()
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.HardFork.History (EraParams (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.LeaderSchedule
  ( LeaderSchedule (..)
  , leaderScheduleFor
  )
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.Mock.Chain as Mock
import Quiet (Quiet (..))
import Test.Consensus.HardFork.Combinator.A
import Test.Consensus.HardFork.Combinator.B
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.ThreadNet.General
import Test.ThreadNet.Network
import Test.ThreadNet.TxGen
import Test.ThreadNet.Util
import Test.ThreadNet.Util.NodeJoinPlan
import Test.ThreadNet.Util.NodeRestarts
import Test.ThreadNet.Util.NodeToNodeVersion
import Test.ThreadNet.Util.NodeTopology
import Test.ThreadNet.Util.Seed
import Test.Util.HardFork.Future
import Test.Util.SanityCheck (prop_sanityChecks)
import Test.Util.Slots (NumSlots (..))
import Test.Util.Time (dawnOfTime)

tests :: TestTree
tests =
  testGroup
    "Consensus"
    [ testProperty "simple convergence" $
        prop_simple_hfc_convergence
    ]

data AB a = AB {getA, getB :: a}
  deriving (Foldable, Functor, Generic, Traversable)
  deriving Show via (Quiet (AB a))

instance Applicative AB where
  pure x = AB x x
  AB af bf <*> AB a b = AB (af a) (bf b)

data TestSetup = TestSetup
  { testSetupEpochSize :: AB EpochSize
  -- ^ INVARIANT: @> 0@
  , testSetupK :: SecurityParam
  , testSetupSeed :: Seed
  , testSetupSlotLength :: AB SlotLength
  , testSetupTxSlot :: SlotNo
  }
  deriving Show

instance Arbitrary TestSetup where
  arbitrary = do
    testSetupEpochSize <- abM $ EpochSize <$> choose (1, 10)
    testSetupK <- SecurityParam <$> choose (2, 10) `suchThatMap` nonZero
    -- TODO why does k=1 cause the nodes to only forge in the first epoch?
    testSetupTxSlot <- SlotNo <$> choose (0, 9)

    testSetupSeed <- arbitrary
    testSetupSlotLength <- abM arbitrary
    return TestSetup{..}
   where
    abM :: Monad m => m a -> m (AB a)
    abM = sequence . pure

-- | The number of epochs in the A era
testSetupEraSizeA :: TestSetup -> EraSize
testSetupEraSizeA TestSetup{..} =
  -- This function, as a specification, intentionally independently
  -- reimplements the interpretation of the 'InitiateAtoB' transaction by the
  -- A ledger.
  EraSize $ succ lastEpochA
 where
  lastEpochA = lastSlotA `div` unEpochSize (getA testSetupEpochSize)
  lastSlotA =
    unSlotNo testSetupTxSlot
      + stabilityWindowA testSetupK
      + safeFromTipA testSetupK

-- | Minimum number of slots needed to include exactly one epoch of the B era
testSetupNumSlots :: TestSetup -> NumSlots
testSetupNumSlots testSetup@TestSetup{..} =
  -- this test doesn't need more than one B epoch
  NumSlots $ eraSizeA * epoSizeA + epoSizeB
 where
  EraSize eraSizeA = testSetupEraSizeA testSetup
  AB epoSizeA epoSizeB = unEpochSize <$> testSetupEpochSize

prop_simple_hfc_convergence :: TestSetup -> Property
prop_simple_hfc_convergence testSetup@TestSetup{..} =
  counterexample (show testConfig) $
    counterexample ("eraSizeA: " <> show eraSizeA) $
      tabulate "epochs in era A" [labelEraSizeA] $
        prop_general args testOutput
          .&&. prop_sanityChecks (topLevelConfig (CoreNodeId 0))
          .&&. prop_allExpectedBlocks
 where
  k :: SecurityParam
  k = testSetupK

  eraParamsA, eraParamsB :: EraParams
  AB eraParamsA eraParamsB =
    EraParams
      <$> testSetupEpochSize
      <*> testSetupSlotLength
      <*> AB
        (History.StandardSafeZone (safeFromTipA k))
        (safeZoneB k)
      <*> pure (GenesisWindow ((unNonZero $ maxRollbacks k) * 2))

  shape :: History.Shape '[BlockA, BlockB]
  shape = History.Shape $ exactlyTwo eraParamsA eraParamsB

  leaderSchedule :: LeaderSchedule
  leaderSchedule = roundRobinLeaderSchedule numCoreNodes numSlots
   where
    TestConfig{..} = testConfig

  args :: PropGeneralArgs TestBlock
  args =
    PropGeneralArgs
      { pgaBlockProperty = const $ property True
      , pgaCountTxs = fromIntegral . length . extractTxs
      , pgaExpectedCannotForge = noExpectedCannotForges
      , pgaFirstBlockNo = BlockNo 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule = Just leaderSchedule
      , pgaSecurityParam = k
      , pgaTestConfig = testConfig
      , pgaTestConfigB = testConfigB
      }

  testConfig :: TestConfig
  testConfig =
    TestConfig
      { numCoreNodes = ncn
      , numSlots = testSetupNumSlots testSetup
      , nodeTopology = meshNodeTopology ncn
      , initSeed = testSetupSeed
      }
   where
    ncn :: NumCoreNodes
    ncn = NumCoreNodes 2

  eraSizeA :: EraSize
  eraSizeA = testSetupEraSizeA testSetup

  testConfigB :: TestConfigB TestBlock
  testConfigB =
    TestConfigB
      { forgeEbbEnv = Nothing
      , future =
          EraCons
            (eraSlotLength eraParamsA)
            (eraEpochSize eraParamsA)
            eraSizeA
            $ EraFinal
              (eraSlotLength eraParamsB)
              (eraEpochSize eraParamsB)
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra = ()
      , version = newestVersion (Proxy @TestBlock)
      }
   where
    TestConfig{..} = testConfig

  testConfigMB :: Monad m => TestConfigMB m TestBlock
  testConfigMB =
    TestConfigMB
      { nodeInfo = \a ->
          plainTestNodeInitialization
            (protocolInfo a)
            (return blockForging)
      , mkRekeyM = Nothing
      }

  labelEraSizeA :: String
  labelEraSizeA =
    if sz >= 10 then ">=10" else show sz
   where
    EraSize sz = eraSizeA

  protocolInfo :: CoreNodeId -> ProtocolInfo TestBlock
  protocolInfo nid =
    ProtocolInfo
      { pInfoConfig =
          topLevelConfig nid
      , pInfoInitLedger =
          ExtLedgerState
            { ledgerState =
                HardForkLedgerState $
                  initHardForkState
                    (Flip initLedgerState)
            , headerState =
                genesisHeaderState $
                  initHardForkState
                    (WrapChainDepState initChainDepState)
            }
      }

  blockForging :: Monad m => [BlockForging m TestBlock]
  blockForging =
    [ hardForkBlockForging "Test" $
        OptCons blockForgingA $
          OptCons blockForgingB $
            OptNil
    ]

  initLedgerState :: LedgerState BlockA ValuesMK
  initLedgerState =
    LgrA
      { lgrA_tip = GenesisPoint
      , lgrA_transition = Nothing
      }

  initChainDepState :: ChainDepState ProtocolA
  initChainDepState = ()

  topLevelConfig :: CoreNodeId -> TopLevelConfig TestBlock
  topLevelConfig nid =
    TopLevelConfig
      { topLevelConfigProtocol =
          HardForkConsensusConfig
            { hardForkConsensusConfigK = k
            , hardForkConsensusConfigShape = shape
            , hardForkConsensusConfigPerEra =
                PerEraConsensusConfig $
                  (WrapPartialConsensusConfig $ consensusConfigA nid)
                    :* (WrapPartialConsensusConfig $ consensusConfigB nid)
                    :* Nil
            }
      , topLevelConfigLedger =
          HardForkLedgerConfig
            { hardForkLedgerConfigShape = shape
            , hardForkLedgerConfigPerEra =
                PerEraLedgerConfig $
                  (WrapPartialLedgerConfig $ ledgerConfigA nid)
                    :* (WrapPartialLedgerConfig $ ledgerConfigB nid)
                    :* Nil
            }
      , topLevelConfigBlock =
          HardForkBlockConfig
            { hardForkBlockConfigPerEra =
                PerEraBlockConfig $
                  blockConfigA nid
                    :* blockConfigB nid
                    :* Nil
            }
      , topLevelConfigCodec =
          HardForkCodecConfig
            { hardForkCodecConfigPerEra =
                PerEraCodecConfig $
                  CCfgA
                    :* CCfgB
                    :* Nil
            }
      , topLevelConfigStorage =
          HardForkStorageConfig
            { hardForkStorageConfigPerEra =
                PerEraStorageConfig $
                  SCfgA
                    :* SCfgB
                    :* Nil
            }
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }

  consensusConfigA :: CoreNodeId -> ConsensusConfig ProtocolA
  consensusConfigA nid =
    CfgA
      { cfgA_k = k
      , cfgA_leadInSlots = leaderScheduleFor nid leaderSchedule
      }

  consensusConfigB :: CoreNodeId -> ConsensusConfig ProtocolB
  consensusConfigB nid =
    CfgB
      { cfgB_k = k
      , cfgB_leadInSlots = leaderScheduleFor nid leaderSchedule
      }

  ledgerConfigA :: CoreNodeId -> PartialLedgerConfig BlockA
  ledgerConfigA _nid =
    LCfgA
      { lcfgA_k = k
      , lcfgA_systemStart = SystemStart dawnOfTime -- required for RunNode
      , lcfgA_forgeTxs =
          Map.fromList
            [ (testSetupTxSlot, [TxA (TxIdA 0) InitiateAtoB])
            ]
      }

  ledgerConfigB :: CoreNodeId -> LedgerConfig BlockB
  ledgerConfigB _nid = ()

  blockConfigA :: CoreNodeId -> BlockConfig BlockA
  blockConfigA _ = BCfgA

  blockConfigB :: CoreNodeId -> BlockConfig BlockB
  blockConfigB _ = BCfgB

  testOutput :: TestOutput TestBlock
  testOutput = runTestNetwork testConfig testConfigB testConfigMB

  prop_allExpectedBlocks :: Property
  prop_allExpectedBlocks =
    counterexample
      ( "some final chain does not have "
          <> show a
          <> " blocks from A and "
          <> show b
          <> " blocks from B"
      )
      $ counterexample (show $ Map.toList counts)
      $ property
      $ all (== (a, b)) counts
   where
    TestConfig{..} = testConfig
    NumSlots t = numSlots

    -- we expect one epoch from B and the rest from A
    b = unEpochSize (getB testSetupEpochSize)
    a = t - b

  -- counts of A blocks and of B blocks for each final chain
  counts :: Map.Map NodeId (Word64, Word64)
  counts =
    (\c -> (chainLen isA c, chainLen isB c)) <$> testOutputNodes
   where
    TestOutput{..} = testOutput

    isA, isB :: TestBlock -> Bool
    isA (HardForkBlock (OneEraBlock blk)) = index_NS blk == 0
    isB (HardForkBlock (OneEraBlock blk)) = index_NS blk == 1

    chainLen :: (a -> Bool) -> NodeOutput a -> Word64
    chainLen p NodeOutput{..} =
      fromIntegral
        . length
        . filter p
        $ Mock.chainToList nodeOutputFinalChain

-- We ignore the mempool for these tests
instance TxGen TestBlock where
  testGenTxs _ _ _ _ _ _ = return []

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
  type HardForkTxMeasure '[BlockA, BlockB] = IgnoringOverflow ByteSize32

  hardForkEraTranslation =
    EraTranslation
      { translateLedgerState = PCons ledgerState_AtoB PNil
      , translateLedgerTables = PCons ledgerTables_AtoB PNil
      , translateChainDepState = PCons chainDepState_AtoB PNil
      , crossEraForecast = PCons forecast_AtoB PNil
      }
  hardForkChainSel = Tails.mk2 NoTiebreakerAcrossEras
  hardForkInjectTxs = InPairs.mk2 injectTx_AtoB

  hardForkInjTxMeasure = \case
    (Z (WrapTxMeasure x)) -> x
    S (Z (WrapTxMeasure x)) -> x

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

instance SerializeTablesWithHint (LedgerState (HardForkBlock '[BlockA, BlockB])) where
  encodeTablesWithHint = defaultEncodeTablesWithHint
  decodeTablesWithHint = defaultDecodeTablesWithHint

instance
  IndexedMemPack
    (LedgerState (HardForkBlock '[BlockA, BlockB]) EmptyMK)
    (DefaultHardForkTxOut '[BlockA, BlockB])
  where
  indexedTypeName _ = typeName @(DefaultHardForkTxOut '[BlockA, BlockB])
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

injectTx_AtoB ::
  RequiringBoth
    WrapLedgerConfig
    (Product2 InjectTx InjectValidatedTx)
    BlockA
    BlockB
injectTx_AtoB =
  InPairs.ignoringBoth $ Pair2 cannotInjectTx cannotInjectValidatedTx

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
