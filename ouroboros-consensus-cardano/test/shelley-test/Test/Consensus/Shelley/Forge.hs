{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | A forged Dijkstra block announces the endorser block that 'forgeBlock'
-- returns.
module Test.Consensus.Shelley.Forge (tests) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as SL (Globals (..), ProtVer (..))
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import Cardano.Ledger.Core (ppProtocolVersionL)
import Cardano.Ledger.Dijkstra.PParams (ppMaxEndorserBlockReferencesSizeL)
import Cardano.Ledger.MemoBytes (getMemoRawType)
import qualified Cardano.Ledger.Shelley.API as SL
  ( mkMempoolEnv
  , mkMempoolState
  , unsafeMakeValidatedTx
  )
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.TPraos.OCert as SL (KESPeriod (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Control.Tracer (nullTracer)
import Data.Maybe (isJust)
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config
  ( SecurityParam (..)
  , TopLevelConfig (..)
  , emptyCheckpointsMap
  )
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
import Ouroboros.Consensus.Protocol.Praos2 (ConsensusConfig (..))
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
    [ testProperty "a block announces the endorser block it comes with" prop_announcesEb
    , testProperty "a block without endorser-block transactions announces nothing" prop_noEb
    ]

prop_announcesEb :: Property
prop_announcesEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode ->
    forAll genNumEbTxs $ \numEbTxs -> ioProperty $ do
      (blk, forgedEb) <- forgeWith coreNode (replicate numEbTxs exampleTx)
      pure $ case forgedEb of
        Nothing -> counterexample "no endorser block" False
        Just ForgedLeiosEb{forgedLeiosEbBody = eb} ->
          conjoin
            [ announcement blk
                === SJust
                  EbReferencesAnnouncement
                    { ebReferencesAnnouncementHash = hashLeiosEb eb
                    , ebReferencesAnnouncementSize = encodeLeiosEbSize eb
                    }
            , counterexample "the block does not match its header" $
                blockMatchesHeader (getHeader blk) blk
            ]

prop_noEb :: Property
prop_noEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode -> ioProperty $ do
    (blk, forgedEb) <- forgeWith coreNode []
    pure $ (announcement blk, isJust forgedEb) === (SNothing, False)

-- | A number of endorser-block transactions. 'encodeLeiosEbSize' writes a
-- longer map header from 24 and from 256 references.
genNumEbTxs :: Gen Int
genNumEbTxs =
  frequency
    [ (1, elements [1, 23, 24, 255, 256])
    , (3, chooseInt (1, 300))
    ]

-- | Forge the block after the example ledger state, with the given endorser
-- block transactions and no ranking block transactions.
forgeWith ::
  CoreNode StandardCrypto ->
  [GenTx StandardDijkstraBlock] ->
  IO (StandardDijkstraBlock, Maybe ForgedLeiosEb)
forgeWith coreNode ebTxs = do
  hotKey <-
    instantiatePraosCredentials
      (SL.maxKESEvo globals)
      nullTracer
      (praosCanBeLeaderCredentialsSource canBeLeader)
  forgeBlock
    (praos2SharedBlockForging hotKey slotToPeriod credentials)
    ForgeBlockArgs
      { fbConfig = topLevelConfig
      , fbCurrentBlockNo = BlockNo 4
      , fbCurrentSlotNo = slot
      , fbPerasCert = Nothing
      , fbCurrentTickedLedgerState = tickedLedgerState
      , fbTxs = []
      , fbEbTxs =
          [ ShelleyValidatedTx txid $
              SL.unsafeMakeValidatedTx
                globals
                (SL.mkMempoolEnv newEpochState slot)
                (SL.mkMempoolState newEpochState)
                tx
          | ShelleyTx txid tx <- ebTxs
          ]
      , fbIsLeader = PraosIsLeader $ Leios.hbrVrfRes (rawHeaderBody (getHeader exampleDijkstraBlock))
      }
 where
  slot = SlotNo 10
  credentials = mkLeaderCredentials coreNode
  canBeLeader = shelleyLeaderCredentialsCanBeLeader credentials

  slotToPeriod (SlotNo slotNo) =
    SL.KESPeriod $ fromIntegral $ slotNo `div` SL.slotsPerKESPeriod globals

  topLevelConfig =
    TopLevelConfig
      { topLevelConfigProtocol =
          LeiosConfig $
            PraosConfig
              { praosParams =
                  PraosParams
                    { praosSlotsPerKESPeriod = SL.slotsPerKESPeriod globals
                    , praosLeaderF = SL.activeSlotCoeff globals
                    , praosSecurityParam = SecurityParam $ SL.securityParameter globals
                    , praosMaxKESEvo = SL.maxKESEvo globals
                    , praosMaxMajorPV = MaxMajorProtVer $ SL.pvMajor protVer
                    , praosRandomnessStabilisationWindow = SL.randomnessStabilisationWindow globals
                    }
              , praosEpochInfo = fixedEpochInfo (EpochSize 4) (slotLengthFromSec 7)
              }
      , topLevelConfigLedger = ledgerConfig
      , topLevelConfigBlock =
          mkShelleyBlockConfig
            protVer
            testShelleyGenesis
            [praosCanBeLeaderColdVerKey canBeLeader]
      , topLevelConfigCodec = ShelleyCodecConfig
      , topLevelConfigStorage =
          ShelleyStorageConfig
            (SL.slotsPerKESPeriod globals)
            (SecurityParam $ SL.securityParameter globals)
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }

  protVer = newEpochState ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  ledgerConfig = example Examples.exampleLedgerConfig
  globals = shelleyLedgerGlobals ledgerConfig

  tickedLedgerState =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = shelleyLedgerTip ledgerState
      , tickedShelleyLedgerTransition = shelleyLedgerTransition ledgerState
      , tickedShelleyLedgerState = newEpochState
      , tickedShelleyLedgerTables = shelleyLedgerTables ledgerState
      }

  -- The 'maxEndorserBlockReferencesSize' of the example parameters is smaller
  -- than the endorser block of the example transaction.
  newEpochState =
    shelleyLedgerState ledgerState
      & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockReferencesSizeL .~ 512 * 1024

  ledgerState = example Examples.exampleLedgerState

announcement :: StandardDijkstraBlock -> StrictMaybe EbReferencesAnnouncement
announcement = Leios.hbrEbReferencesAnnouncement . rawHeaderBody . getHeader

rawHeaderBody :: Header StandardDijkstraBlock -> Leios.HeaderBodyRaw StandardCrypto
rawHeaderBody = getMemoRawType . Leios.headerBody . shelleyHeaderRaw

exampleTx :: GenTx StandardDijkstraBlock
exampleTx = example Examples.exampleGenTx

exampleDijkstraBlock :: StandardDijkstraBlock
exampleDijkstraBlock = example Examples.exampleBlock

example :: (Examples StandardDijkstraBlock -> Labelled b) -> b
example field = case field examplesDijkstra of
  (_, x) : _ -> x
  [] -> error "no example"
