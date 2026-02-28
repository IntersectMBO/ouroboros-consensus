{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Leios (tests) where

import qualified Cardano.Chain.Update as Byron
import Cardano.Ledger.Api
  ( Addr (..)
  , ConwayEra
  , PParams
  , Tx
  , TxOut
  , addrTxOutL
  , bodyTxL
  , eraProtVerLow
  , feeTxBodyL
  , getMinFeeTx
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , mkBasicTxOut
  , outputsTxBodyL
  , valueTxOutL
  )
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.Api.Tx.In (TxIn)
import Cardano.Ledger.BaseTypes (ProtVer (..), knownNonZeroBounded, unNonZero)
import Cardano.Ledger.Val (coin, inject, (<->))
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import Control.Monad (replicateM)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import LeiosDemoTypes (TraceLeiosKernel (..))
import Lens.Micro ((%~), (.~), (^.))
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Cardano
  ( CardanoBlock
  , Nonce (NeutralNonce)
  , ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  , ShelleyGenesis (..)
  )
import Ouroboros.Consensus.Cardano.Node (CardanoProtocolParams (..), protocolInfoCardano)
import Ouroboros.Consensus.Config (SecurityParam (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Cardano.Ledger.Alonzo.Examples.Consensus (exampleAlonzoGenesis)
import Test.Cardano.Ledger.Conway.Examples.Consensus (exampleConwayGenesis)
import Test.Consensus.Cardano.ProtocolInfo (Era (Conway), hardForkInto)
import Test.QuickCheck (Gen, Property, conjoin, counterexample, withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.ThreadNet.General
  ( TestConfig (..)
  , TestConfigB (..)
  , TestConfigMB (..)
  , TestOutput (..)
  , noCalcMessageDelay
  , runTestNetwork
  )
import Test.ThreadNet.Infra.Byron (theProposedSoftwareVersion)
import qualified Test.ThreadNet.Infra.Byron as Byron
import Test.ThreadNet.Infra.Shelley
  ( CoreNode (..)
  , DecentralizationParam (..)
  , genCoreNode
  , mkCredential
  , mkGenesisConfig
  , mkKesConfig
  , mkLeaderCredentials
  , signTx
  )
import Test.ThreadNet.Network
  ( NodeOutput (..)
  , TestNodeInitialization (..)
  )
import Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..))
import Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import Test.ThreadNet.Util.NodeRestarts (noRestarts)
import Test.ThreadNet.Util.NodeToNodeVersion (newestVersion)
import Test.ThreadNet.Util.NodeTopology (meshNodeTopology)
import Test.ThreadNet.Util.Seed (Seed (..), runGen)
import Test.Util.HardFork.Future (Future (EraFinal))
import Test.Util.Slots (NumSlots (..))

tests :: TestTree
tests =
  testGroup
    "Leios ThreadNet"
    [ testProperty "EB production" $
        withMaxSuccess 1 prop_leios_blocksProduced
    ]

prop_leios_blocksProduced :: Seed -> Property
prop_leios_blocksProduced seed = do
  conjoin
    [ not (null forgedBlocks)
        & counterexample "no praos blocks were forged"
    , any (not . null) includedTxs
        & counterexample ("txs per active slot: " <> show (length <$> includedTxs))
        & counterexample "all forged blocks were empty (no transactions)"
    , not (null leiosForgedEBs)
        & counterexample ("leios kernel traces: " <> show leiosTraces)
        & counterexample "no endorser blocks were forged"
    ]
 where
  forgedBlocks = foldMap nodeOutputForges testOutput.testOutputNodes

  includedTxs = extractTxs <$> forgedBlocks

  leiosForgedEBs = flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockForged{eb} -> Just eb
    _ -> Nothing

  leiosTraces = traceOfType testOutput @TraceLeiosKernel

  testOutput = runThreadNet seed numSlots

  -- NOTE: There must be k Praos blocks after this time.
  numSlots = NumSlots $ ceiling (3 * k / activeSlotCoeff :: Rational)

runThreadNet :: Seed -> NumSlots -> TestOutput (CardanoBlock StandardCrypto)
runThreadNet initSeed numSlots =
  runTestNetwork
    testConfig
    testConfigB
    TestConfigMB
      { nodeInfo = \(CoreNodeId nid) ->
          let (protocolInfo, blockForging) =
                protocolInfoCardano
                  CardanoProtocolParams
                    { byronProtocolParams =
                        ProtocolParamsByron
                          { byronGenesis
                          , byronPbftSignatureThreshold = Nothing
                          , byronProtocolVersion = Byron.ProtocolVersion 0 0 0
                          , byronSoftwareVersion = theProposedSoftwareVersion
                          , byronLeaderCredentials = Nothing
                          }
                    , shelleyBasedProtocolParams =
                        ProtocolParamsShelleyBased
                          { shelleyBasedInitialNonce = NeutralNonce
                          , shelleyBasedLeaderCredentials =
                              -- NOTE: Needed to hard-fork into shelley. After
                              -- that, with d=0, it's stake based leaders.
                              pure . mkLeaderCredentials $ coreNodes !! fromIntegral nid
                          }
                    , cardanoHardForkTriggers = hardForkInto Conway
                    , cardanoLedgerTransitionConfig =
                        -- TODO: provide better alonzo/conway genesis
                        mkLatestTransitionConfig shelleyGenesis exampleAlonzoGenesis exampleConwayGenesis
                    , cardanoCheckpoints = mempty
                    , cardanoProtocolVersion = conwayProtVer
                    }
           in TestNodeInitialization
                { tniProtocolInfo = protocolInfo
                , tniCrucialTxs = []
                , tniBlockForging = blockForging
                }
      , mkRekeyM = Nothing
      }
 where
  conwayProtVer = ProtVer (eraProtVerLow @ConwayEra) 0

  n = 3

  numCoreNodes = NumCoreNodes n

  coreNodes =
    runGen initSeed $
      replicateM (fromIntegral n) $
        genCoreNode (KESPeriod 0)

  (byronGenesis, _generatedSecrets) =
    Byron.generateGenesisConfig slotLength $
      Byron.byronPBftParams securityParam numCoreNodes

  shelleyGenesis =
    mkGenesisConfig
      conwayProtVer
      securityParam
      activeSlotCoeff
      (DecentralizationParam 0)
      maxLovelaceSupply
      slotLength
      (mkKesConfig (Proxy @StandardCrypto) numSlots)
      coreNodes

  testConfig =
    TestConfig
      { numSlots
      , numCoreNodes
      , nodeTopology = meshNodeTopology numCoreNodes
      , initSeed
      }

  testConfigB =
    TestConfigB
      { forgeEbbEnv = Nothing
      , future = EraFinal slotLength shelleyGenesis.sgEpochLength
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra =
          CardanoTxGenExtra
            { ctgeByronGenesisKeys = error "unused"
            , ctgeNetworkMagic = error "unused"
            , ctgeShelleyCoreNodes = coreNodes
            , -- TODO: configurable demand, currently n txs per slot
              ctgeExtraTxGen = respendTxGen
            }
      , version = newestVersion (Proxy @(CardanoBlock StandardCrypto))
      }

-- * Fixtures

k :: Num a => a
k = fromIntegral . unNonZero $ maxRollbacks securityParam

securityParam :: SecurityParam
securityParam = SecurityParam $ knownNonZeroBounded @10

activeSlotCoeff :: Rational
activeSlotCoeff = 1 / 20

slotLength :: SlotLength
slotLength = slotLengthFromSec 1

maxLovelaceSupply :: Num a => a
maxLovelaceSupply = 100_000_000_000_000

-- * Transaction generation

respendTxGen ::
  CoreNode StandardCrypto ->
  SlotNo ->
  PParams ConwayEra ->
  Map.Map TxIn (TxOut ConwayEra) ->
  Gen [Tx ConwayEra]
respendTxGen coreNode _slotNo pparams utxos =
  pure $ case Map.toList myUtxos of
    [] -> []
    (txIn, txOut) : _ -> [mkRespendTx txIn txOut]
 where
  myUtxos = Map.filter (ownedBy paymentSK) utxos

  CoreNode{cnDelegateKey = paymentSK} = coreNode

  mkRespendTx txIn txOut = buildTx $ \tx ->
    let inCoin = coin (txOut ^. valueTxOutL)
        feeCoin = getMinFeeTx pparams tx 0
        outCoin = inCoin <-> feeCoin
     in tx
          & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
          & bodyTxL . outputsTxBodyL
            .~ StrictSeq.singleton (mkBasicTxOut (txOut ^. addrTxOutL) (inject outCoin))
          & bodyTxL . feeTxBodyL .~ feeCoin
          & signTx paymentSK

  ownedBy sk txOut = case txOut ^. addrTxOutL of
    Addr _ cred _ -> cred == mkCredential sk
    _ -> False

buildTx :: (Tx ConwayEra -> Tx ConwayEra) -> Tx ConwayEra
buildTx f = until (\x -> f x == x) f (mkBasicTx mkBasicTxBody)
