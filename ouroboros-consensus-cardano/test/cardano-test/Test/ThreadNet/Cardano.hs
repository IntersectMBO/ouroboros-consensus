{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.ThreadNet.Cardano (tests) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (unEpochSlots)
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as SL
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import           Control.Exception (assert)
import           Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Lens.Micro
import           Ouroboros.Consensus.Block.Forging (BlockForging)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Byron.Ledger (LedgerState (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                     (isHardForkNodeToNodeEnabled)
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Test.Consensus.Cardano.ProtocolInfo
                     (hardForkOnDefaultProtocolVersions, mkTestProtocolInfo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Infra.TwoEras
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import           Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.TestEnv

-- | Use 'MockCryptoCompatByron' so that bootstrap addresses and
-- bootstrap witnesses are supported.
type Crypto = StandardCrypto

-- | The varying data of this test
--
-- Note: The Byron nodes in this test all join, propose an update, vote for it,
-- and endorse it literally as soon as possible. Therefore, if the test reaches
-- the end of the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupD                 :: Shelley.DecentralizationParam
  , setupHardFork          :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce      :: SL.Nonce
    -- ^ the initial Shelley 'SL.ticknStateEpochNonce'
    --
    -- We vary it to ensure we explore different leader schedules.
  , setupK                 :: SecurityParam
  , setupPartition         :: Partition
  , setupSlotLengthByron   :: SlotLength
  , setupSlotLengthShelley :: SlotLength
  , setupTestConfig        :: TestConfig
  , setupVersion           :: (NodeToNodeVersion, BlockNodeToNodeVersion (CardanoBlock Crypto))
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    setupD <- arbitrary
                -- The decentralization parameter cannot be 0 in the first
                -- Shelley epoch, since stake pools can only be created and
                -- delegated to via Shelley transactions.
                `suchThat` ((/= 0) . Shelley.decentralizationParamToRational)
    setupK <- SecurityParam <$> choose (8, 10)
                -- If k < 8, common prefix violations become too likely in
                -- Praos mode for thin overlay schedules (ie low d), even for
                -- f=0.2.

    setupInitialNonce <- genNonce

    setupSlotLengthByron   <- arbitrary
    setupSlotLengthShelley <- arbitrary

    setupTestConfig <- genTestConfig
                         setupK
                         ( EpochSize $ byronEpochSize setupK
                         , EpochSize $ shelleyEpochSize setupK
                         )

    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupHardFork        <- frequency [(49, pure True), (1, pure False)]
    setupPartition       <- genPartition numCoreNodes numSlots setupK

    setupVersion         <- genVersionFiltered
                              isHardForkNodeToNodeEnabled
                              (Proxy @(CardanoBlock Crypto))

    pure TestSetup
      { setupD
      , setupHardFork
      , setupInitialNonce
      , setupK
      , setupPartition
      , setupSlotLengthByron
      , setupSlotLengthShelley
      , setupTestConfig
      , setupVersion
      }

  -- TODO shrink

tests :: TestTree
tests = testGroup "Cardano ThreadNet" [
          let name = "simple convergence" in
          askTestEnv $ adjustTestMode $
            testProperty name prop_simple_cardano_convergence
    ]
    where
      adjustTestMode :: TestTree -> TestEnv ->  TestTree
      adjustTestMode tree = \case
        Nightly -> tree
        _       -> adjustQuickCheckTests (\n -> (2 * n) `div` 5) tree

prop_simple_cardano_convergence :: TestSetup -> Property
prop_simple_cardano_convergence TestSetup
  { setupD
  , setupHardFork
  , setupInitialNonce
  , setupK
  , setupPartition
  , setupSlotLengthByron
  , setupSlotLengthShelley
  , setupTestConfig
  , setupVersion
  } =
    prop_general_semisync pga testOutput .&&.
    prop_inSync testOutput .&&.
    prop_ReachesEra2 reachesEra2 .&&.
    prop_noCPViolation .&&.
    ( tabulate "ReachesEra2 label" [label_ReachesEra2 reachesEra2] $
      tabulate "Observed forge during a non-overlay Shelley slot"
        [label_hadActiveNonOverlaySlots testOutput overlaySlots] $
      tabulatePartitionDuration setupK setupPartition $
      tabulateFinalIntersectionDepth
        setupK
        (NumBlocks finalIntersectionDepth)
        finalBlockEra $
      tabulatePartitionPosition
        (NumSlots numByronSlots)
        setupPartition
        (ledgerReachesEra2 reachesEra2) $
      property True
    )
  where
    TestConfig
      { initSeed
      , numCoreNodes
      , numSlots
      } = setupTestConfig

    pga = PropGeneralArgs
        { pgaBlockProperty       = const $ property True
        , pgaCountTxs            = fromIntegral . length . extractTxs
        , pgaExpectedCannotForge = noExpectedCannotForges
        , pgaFirstBlockNo        = 1
        , pgaFixedMaxForkLength  = Just maxForkLength
        , pgaFixedSchedule       =
            -- the leader schedule isn't fixed because the Shelley leader
            -- schedule is (at least ideally) unpredictable
            Nothing
        , pgaSecurityParam       = setupK
        , pgaTestConfig          = setupTestConfig
        , pgaTestConfigB         = testConfigB
        }

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          -- In this case the PVU will trigger the transition to Shelley.
          --
          -- By FACT (B), the PVU is always successful if we reach the second
          -- era.
          EraCons  setupSlotLengthByron   epochSizeByron   eraSizeByron $
          EraFinal setupSlotLengthShelley epochSizeShelley
          else
          EraFinal setupSlotLengthByron   epochSizeByron
      , messageDelay = mkMessageDelay setupPartition
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = CardanoTxGenExtra
        { ctgeByronGenesisKeys = generatedSecrets
        , ctgeNetworkMagic     =
            CC.Common.makeNetworkMagic $
            CC.Genesis.configProtocolMagic genesisByron
        , ctgeShelleyCoreNodes = coreNodes
        }
      , version      = setupVersion
      }

    testOutput :: TestOutput (CardanoBlock Crypto)
    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \coreNodeId@(CoreNodeId nid) ->
                mkProtocolCardanoAndHardForkTxs
                  pbftParams
                  coreNodeId
                  genesisByron
                  generatedSecrets
                  propPV
                  genesisShelley
                  setupInitialNonce
                  (coreNodes !! fromIntegral nid)
            , mkRekeyM = Nothing
            }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $
        if rsEra2Blocks reachesEra2
        then
          -- Shelley inherently creates small forks, but we haven't yet seen a
          -- Common Prefix violation in this test even though @k@ is small
          --
          -- TODO I'd value having a repro that demonstrates a violation of
          -- this typical limit, so I'm leaving it in for now. If it never
          -- fails, we should figure out why not. Even with @k=2 ncn=5 d=0.1@
          -- fixed the deepest fork I'm seeing is ~2.5% @k-1@
          -- 'finalIntersectionDepth'.
          maxRollbacks setupK
        else
          -- Recall that all nodes join ASAP, so the partition is the only
          -- potential cause for a fork during Byron. See the reasoning in
          -- 'genPartition' for the motivation of this limit.
          div partitionDuration 2 + mod partitionDuration 2

    partitionDuration :: Word64
    partitionDuration = dur
      where
        Partition _ (NumSlots dur) = setupPartition

    -- Byron

    pbftParams :: PBftParams
    pbftParams = Byron.byronPBftParams setupK numCoreNodes

    -- the Byron ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeByron :: EpochSize
    epochSizeByron =
        fromByronEpochSlots $ CC.Genesis.configEpochSlots genesisByron

    eraSizeByron :: EraSize
    eraSizeByron = EraSize numFirstEraEpochs

    genesisByron     :: CC.Genesis.Config
    generatedSecrets :: CC.Genesis.GeneratedSecrets
    (genesisByron, generatedSecrets) =
        Byron.generateGenesisConfig setupSlotLengthByron pbftParams

    -- Shelley

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [Shelley.CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          Shelley.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    -- Same value as for mainnet. Must be larger than the amount of Lovelace in
    -- circulation in the Byron ledger. Since this is the maximum value of
    -- lovelace, this is guaranteed to be the case.
    maxLovelaceSupply :: Word64
    maxLovelaceSupply = 45000000000000000

    genesisShelley :: ShelleyGenesis Crypto
    genesisShelley =
        Shelley.mkGenesisConfig
          (SL.ProtVer shelleyMajorVersion 0)
          setupK
          activeSlotCoeff
          setupD
          maxLovelaceSupply
          setupSlotLengthShelley
          (Shelley.mkKesConfig (Proxy @Crypto) numSlots)
          coreNodes

    -- the Shelley ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeShelley :: EpochSize
    epochSizeShelley = sgEpochLength genesisShelley

    -- the protocol version of the Byron era proposal
    --
    -- FACT (B) This proposal is always adopted at the first epoch boundary.
    --
    -- o 'genTestConfig' ensures the test reaches the epoch boundary unless
    --   there's a fatal error during execution. Specifically, 'rsEra1Slots'
    --   will always be 'Enabled'.
    --
    -- o 'genPartition' limits the partition duration to at most @2k-2@ slots.
    --   This leaves at least @10k - (2k-2) = 8k+2@ slots in the epoch
    --   unaffected by the partition. Moreover, the blocks forged during the
    --   partition do make some progress, even though it's not full progress.
    --   So @8k+2@ is conservative.
    --
    -- o As " crucial transactions ", the proposal and vote are added to the
    --   chain eventually and ASAP, even if they are initially placed on the
    --   losing partition class's chain.
    --
    -- o Thus, within the first two of the @8k+2@ unaffected slots, the
    --   proposal has been confirmed. Similar reasoning ensures that it is then
    --   stably confirmed, endorsed, and stably endorsed, before the epoch
    --   boundary and @SafeZone@. IE @2 + 2k + q + 2k + 2k < 8k+2@, since the
    --   quorum @q@ is ~60% of 'numCoreNodes' and so @q < 2k@, since
    --   'numCoreNodes' is at most 5 and @k@ is at least @2@. (Also recall that
    --   @8k+2@ is conservative.)
    propPV :: CC.Update.ProtocolVersion
    propPV =
      if setupHardFork
      then
        -- this new version must induce the hard fork if accepted
        CC.Update.ProtocolVersion (SL.getVersion shelleyMajorVersion) 0 0
      else
        -- this new version must not induce the hard fork if accepted
        CC.Update.ProtocolVersion
          byronMajorVersion (byronInitialMinorVersion + 1) 0

    -- Classifying test cases

    reachesEra2 :: ReachesEra2
    reachesEra2 = ReachesEra2
      { rsEra1Slots    =
          BoolProps.enabledIf $ t > numByronSlots
      , rsPV            = BoolProps.enabledIf setupHardFork
      , rsEra2Blocks =
          or $
          [ not $ isFirstEraBlock blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputForges} = no
          , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
                -- the last block the node forged
          ]
      , rsEra2Slots  =
          assert (w >= k) $
          BoolProps.requiredIf $
          -- The active slots in the first two Shelley epochs are all overlay
          -- slots, so the first Shelley block will arise from one of those.
          not $ Set.null $ overlaySlots
      }
      where
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

        k :: Word64
        k = maxRollbacks setupK

        coeff :: SL.ActiveSlotCoeff
        coeff = SL.sgActiveSlotCoeff genesisShelley

        w :: Word64
        w = SL.computeStabilityWindow k coeff

    overlaySlots :: Set SlotNo
    overlaySlots =
        secondEraOverlaySlots
          numSlots
          (NumSlots numByronSlots)
          (sgProtocolParams genesisShelley ^. SL.ppDL)
          epochSizeShelley

    numByronSlots :: Word64
    numByronSlots = numFirstEraEpochs * unEpochSize epochSizeByron

    finalBlockEra :: String
    finalBlockEra =
        if rsEra2Blocks reachesEra2 then "Shelley" else "Byron"

    finalIntersectionDepth :: Word64
    finalIntersectionDepth = depth
      where
        NumBlocks depth = calcFinalIntersectionDepth pga testOutput

    prop_noCPViolation :: Property
    prop_noCPViolation =
        counterexample
          ( "finalChains: " <>
            show (nodeOutputFinalChain <$> testOutputNodes testOutput)
          ) $
        counterexample "CP violation in final chains!" $
        property $ maxRollbacks setupK >= finalIntersectionDepth

mkProtocolCardanoAndHardForkTxs ::
     forall c m. (IOLike m, c ~ StandardCrypto)
     -- Byron
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
  -> CC.Update.ProtocolVersion
     -- Shelley
  -> ShelleyGenesis c
  -> SL.Nonce
  -> Shelley.CoreNode c
  -> TestNodeInitialization m (CardanoBlock c)
mkProtocolCardanoAndHardForkTxs
    pbftParams coreNodeId genesisByron generatedSecretsByron propPV
    genesisShelley initialNonce coreNodeShelley
  =
    TestNodeInitialization
      { tniCrucialTxs   = crucialTxs
      , tniProtocolInfo = protocolInfo
      , tniBlockForging = blockForging
      }
  where
    crucialTxs :: [GenTx (CardanoBlock c)]
    crucialTxs =
        GenTxByron <$> tniCrucialTxs tniByron
      where
        -- reuse the Byron logic for generating the crucial txs, ie the
        -- proposal and votes
        tniByron :: TestNodeInitialization m ByronBlock
        tniByron =
            Byron.mkProtocolByronAndHardForkTxs
              pbftParams
              coreNodeId
              genesisByron
              generatedSecretsByron
              propPV

    protocolInfo :: ProtocolInfo (CardanoBlock c)
    blockForging :: m [BlockForging m (CardanoBlock c)]
    (setByronProtVer -> protocolInfo, blockForging) =
      mkTestProtocolInfo
        (coreNodeId, coreNodeShelley)
        genesisShelley
        propPV
        initialNonce
        genesisByron
        generatedSecretsByron
        (Just $ PBftSignatureThreshold 1) -- Trivialize the PBFT signature
                                          -- window so that the forks induced by
                                          -- the network partition are as deep
                                          -- as possible.
        -- This test only enters the Shelley era.
        (SL.ProtVer shelleyMajorVersion 0)
        hardForkOnDefaultProtocolVersions

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of the Byron era in this test.
byronMajorVersion :: Integral a => a
byronMajorVersion = SL.getVersion shelleyMajorVersion - 1

byronInitialMinorVersion :: Num a => a
byronInitialMinorVersion = 0

-- | The (first) major protocol version of the Shelley era, as used by
-- 'hardForkOnDefaultProtocolVersions'.
shelleyMajorVersion :: SL.Version
shelleyMajorVersion = L.eraProtVerLow @(ShelleyEra StandardCrypto)

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

byronEpochSize :: SecurityParam -> Word64
byronEpochSize (SecurityParam k) =
    unEpochSlots $ kEpochSlots $ CC.Common.BlockCount k

-- | By default, the initial major Byron protocol version is @0@, but we want to
-- set it to 'byronMajorVersion'.
setByronProtVer :: ProtocolInfo (CardanoBlock c) -> ProtocolInfo (CardanoBlock c)
setByronProtVer =
    modifyInitLedger $ modifyExtLedger $ modifyHFLedgerState $ \st ->
      let cvs = byronLedgerState st
          us  = (CC.cvsUpdateState cvs) {
              CC.adoptedProtocolVersion =
                CC.Update.ProtocolVersion byronMajorVersion byronInitialMinorVersion 0
            }
       in st { byronLedgerState = cvs { CC.cvsUpdateState = us } }
  where
    modifyInitLedger f pinfo = pinfo { pInfoInitLedger = f (pInfoInitLedger pinfo) }
    modifyExtLedger f elgr = elgr { ledgerState = f (ledgerState elgr ) }

    modifyHFLedgerState ::
         (LedgerState x -> LedgerState x)
      -> LedgerState (HardForkBlock (x : xs))
      -> LedgerState (HardForkBlock (x : xs))
    modifyHFLedgerState f (HardForkLedgerState (HardForkState (TZ st))) =
        HardForkLedgerState (HardForkState (TZ st {currentState = f (currentState st)}))
    modifyHFLedgerState _ st = st
