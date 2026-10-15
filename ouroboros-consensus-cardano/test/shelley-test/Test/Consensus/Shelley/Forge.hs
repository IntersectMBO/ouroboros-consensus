{-# LANGUAGE NamedFieldPuns #-}

-- | A forged Dijkstra block announces the endorser block that 'forgeBlock'
-- returns.
module Test.Consensus.Shelley.Forge (tests) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as SL (Globals (..), ProtVer (..))
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import Cardano.Ledger.Core (ppProtocolVersionL)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (ppMaxEndorserBlockReferencesSizeL)
import Cardano.Ledger.MemoBytes (getMemoRawType)
import qualified Cardano.Ledger.Shelley.API as SL
  ( NewEpochState
  , mkMempoolEnv
  , mkMempoolState
  , unsafeMakeValidatedTx
  )
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.TPraos.OCert as SL (KESPeriod (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Control.Monad.Except (runExceptT)
import Control.Tracer (nullTracer)
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config
  ( SecurityParam (..)
  , TopLevelConfig (..)
  , emptyCheckpointsMap
  )
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Leios.Types
  ( ForgedLeiosEb (..)
  , encodeLeiosEbSize
  , hashLeiosEb
  )
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , instantiatePraosCredentials
  )
import Ouroboros.Consensus.Protocol.Praos2 (ConsensusConfig (..), Praos2)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger
  ( GenTx (..)
  , LedgerState (..)
  , ShelleyLedgerConfig (..)
  , Ticked (..)
  , Validated (..)
  , mkShelleyBlockConfig
  , shelleyHeaderRaw
  )
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( CodecConfig (..)
  , StorageConfig (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node.Common (ShelleyLeaderCredentials (..))
import Ouroboros.Consensus.Shelley.Node.Praos (praos2SharedBlockForging)
import Test.Consensus.Shelley.Examples (examplesDijkstra, testShelleyGenesis)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.ThreadNet.Infra.Shelley (CoreNode, genCoreNode, mkLeaderCredentials)
import Test.Util.Serialisation.Examples (Examples, Labelled)
import qualified Test.Util.Serialisation.Examples as Examples

tests :: TestTree
tests =
  testGroup
    "Forge"
    [ testGroup
        "Dijkstra"
        [ testProperty
            "Forges and announces an endorser block from the EB transactions"
            prop_dijkstraForgesAndAnnouncesEb
        , testProperty
            "Forges and announces no endorser block without EB transactions"
            prop_dijkstraForgesAndAnnouncesNoEb
        ]
    ]

prop_dijkstraForgesAndAnnouncesEb :: Property
prop_dijkstraForgesAndAnnouncesEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode ->
    forAll genNumEbTxs $ \numEbTxs -> ioProperty $
      forgeAndCheck coreNode (replicate numEbTxs (validateEbTx exampleTx)) $ \(_, forgedEb) ->
        counterexample "no endorser block" (forgedEb =/= Nothing)

prop_dijkstraForgesAndAnnouncesNoEb :: Property
prop_dijkstraForgesAndAnnouncesNoEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode -> ioProperty $
    forgeAndCheck coreNode [] $ \(_, forgedEb) ->
      counterexample "got an endorser block" (forgedEb === Nothing)

forgeAndCheck ::
  CoreNode StandardCrypto ->
  [Validated (GenTx StandardDijkstraBlock)] ->
  ((StandardDijkstraBlock, Maybe ForgedLeiosEb) -> Property) ->
  IO Property
forgeAndCheck coreNode ebTxs k = do
  blockForging <- mkDijkstraBlockForging coreNode
  result <-
    runExceptT $
      propForgedMatchesHeader
        dijkstraEbMatchesHeader
        blockForging
        (mkDijkstraForgeArgs coreNode ebTxs)
  pure $ case result of
    Left (msg, (blk, mayEb)) ->
      counterexample msg $
        counterexample ("  header announcement: " <> show (announcement (getHeader blk))) $
          counterexample ("  forged eb: " <> show (fmap forgedLeiosEbBody mayEb)) False
    Right res -> k res

-- | A number of endorser-block transactions. 'encodeLeiosEbSize' writes a
-- longer map header from 24 and from 256 references.
genNumEbTxs :: Gen Int
genNumEbTxs =
  frequency
    [ (1, elements [1, 23, 24, 255, 256])
    , (3, chooseInt (1, 300))
    ]

{-------------------------------------------------------------------------------
  Dijkstra block forging setup
-------------------------------------------------------------------------------}

-- | Allocate the 'BlockForging' handle for a Dijkstra test node.
mkDijkstraBlockForging ::
  CoreNode StandardCrypto ->
  IO (BlockForging IO StandardDijkstraBlock)
mkDijkstraBlockForging coreNode = do
  hotKey <-
    instantiatePraosCredentials
      (SL.maxKESEvo testGlobals)
      nullTracer
      (praosCanBeLeaderCredentialsSource canBeLeader)
  pure $ praos2SharedBlockForging hotKey slotToPeriod credentials
 where
  credentials = mkLeaderCredentials coreNode
  canBeLeader = shelleyLeaderCredentialsCanBeLeader credentials
  slotToPeriod (SlotNo slotNo) =
    SL.KESPeriod $ fromIntegral $ slotNo `div` SL.slotsPerKESPeriod testGlobals

-- | Build 'ForgeBlockArgs' for a Dijkstra test. The only variable part across
-- test cases is 'fbEbTxs'; all other fields are fixed by the example ledger state.
mkDijkstraForgeArgs ::
  CoreNode StandardCrypto ->
  [Validated (GenTx StandardDijkstraBlock)] ->
  ForgeBlockArgs StandardDijkstraBlock
mkDijkstraForgeArgs coreNode ebTxs =
  ForgeBlockArgs
    { fbConfig = topLevelConfig
    , fbCurrentBlockNo = BlockNo 4
    , fbCurrentSlotNo = testSlot
    , fbPerasCert = Nothing
    , fbCurrentTickedLedgerState = tickedLedgerState
    , fbTxs = []
    , fbEbTxs = ebTxs
    , fbIsLeader = PraosIsLeader $ Leios.hbrVrfRes (rawHeaderBody (getHeader exampleDijkstraBlock))
    }
 where
  credentials = mkLeaderCredentials coreNode
  canBeLeader = shelleyLeaderCredentialsCanBeLeader credentials
  topLevelConfig =
    TopLevelConfig
      { topLevelConfigProtocol = testProtocolConfig
      , topLevelConfigLedger = testLedgerConfig
      , topLevelConfigBlock =
          mkShelleyBlockConfig
            testProtVer
            testShelleyGenesis
            [praosCanBeLeaderColdVerKey canBeLeader]
      , topLevelConfigCodec = ShelleyCodecConfig
      , topLevelConfigStorage = testStorageConfig
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }
  tickedLedgerState =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = shelleyLedgerTip testLedgerState
      , tickedShelleyLedgerTransition = shelleyLedgerTransition testLedgerState
      , tickedShelleyLedgerState = testEpochState
      , tickedShelleyLedgerTables = shelleyLedgerTables testLedgerState
      }

-- | Validate a 'GenTx' for use as an endorser-block transaction.
validateEbTx ::
  GenTx StandardDijkstraBlock ->
  Validated (GenTx StandardDijkstraBlock)
validateEbTx (ShelleyTx txid tx) =
  ShelleyValidatedTx txid $
    SL.unsafeMakeValidatedTx
      testGlobals
      (SL.mkMempoolEnv testEpochState testSlot)
      (SL.mkMempoolState testEpochState)
      tx

testLedgerConfig :: ShelleyLedgerConfig DijkstraEra
testLedgerConfig = example Examples.exampleLedgerConfig

testGlobals :: SL.Globals
testGlobals = shelleyLedgerGlobals testLedgerConfig

testLedgerState :: LedgerState StandardDijkstraBlock EmptyMK
testLedgerState = example Examples.exampleLedgerState

-- | The example epoch state with 'ppMaxEndorserBlockReferencesSizeL' raised to
-- 512 KiB. The example parameters set it too small to fit the example
-- transaction's endorser block.
testEpochState :: SL.NewEpochState DijkstraEra
testEpochState =
  shelleyLedgerState testLedgerState
    & nesEsL
      . curPParamsEpochStateL
      . ppMaxEndorserBlockReferencesSizeL
      .~ 512
        * 1024

testSlot :: SlotNo
testSlot = SlotNo 10

testProtVer :: SL.ProtVer
testProtVer = testEpochState ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL

testProtocolConfig :: ConsensusConfig (Praos2 StandardCrypto)
testProtocolConfig =
  LeiosConfig $
    PraosConfig
      { praosParams =
          PraosParams
            { praosSlotsPerKESPeriod = SL.slotsPerKESPeriod testGlobals
            , praosLeaderF = SL.activeSlotCoeff testGlobals
            , praosSecurityParam = SecurityParam $ SL.securityParameter testGlobals
            , praosMaxKESEvo = SL.maxKESEvo testGlobals
            , praosMaxMajorPV = MaxMajorProtVer $ SL.pvMajor testProtVer
            , praosRandomnessStabilisationWindow = SL.randomnessStabilisationWindow testGlobals
            }
      , praosEpochInfo = fixedEpochInfo (EpochSize 4) (slotLengthFromSec 7)
      }

testStorageConfig :: StorageConfig StandardDijkstraBlock
testStorageConfig =
  ShelleyStorageConfig
    (SL.slotsPerKESPeriod testGlobals)
    (SecurityParam $ SL.securityParameter testGlobals)

{-------------------------------------------------------------------------------
  Dijkstra-specific EB/header matching
-------------------------------------------------------------------------------}

-- | Check that a Dijkstra block header's EB announcement is consistent with
-- the optional endorser block: present and matching when given 'Just', absent
-- when given 'Nothing'.
dijkstraEbMatchesHeader :: Header StandardDijkstraBlock -> Maybe ForgedLeiosEb -> Bool
dijkstraEbMatchesHeader hdr Nothing = announcement hdr == SNothing
dijkstraEbMatchesHeader hdr (Just ForgedLeiosEb{forgedLeiosEbBody}) =
  announcement hdr
    == SJust
      EbReferencesAnnouncement
        { ebReferencesAnnouncementHash = hashLeiosEb forgedLeiosEbBody
        , ebReferencesAnnouncementSize = encodeLeiosEbSize forgedLeiosEbBody
        }

announcement :: Header StandardDijkstraBlock -> StrictMaybe EbReferencesAnnouncement
announcement = Leios.hbrEbReferencesAnnouncement . rawHeaderBody

rawHeaderBody :: Header StandardDijkstraBlock -> Leios.HeaderBodyRaw StandardCrypto
rawHeaderBody = getMemoRawType . Leios.headerBody . shelleyHeaderRaw

{-------------------------------------------------------------------------------
  Test data
-------------------------------------------------------------------------------}

exampleTx :: GenTx StandardDijkstraBlock
exampleTx = example Examples.exampleGenTx

exampleDijkstraBlock :: StandardDijkstraBlock
exampleDijkstraBlock = example Examples.exampleBlock

example :: (Examples StandardDijkstraBlock -> Labelled b) -> b
example field = case field examplesDijkstra of
  (_, x) : _ -> x
  [] -> error "no example"
