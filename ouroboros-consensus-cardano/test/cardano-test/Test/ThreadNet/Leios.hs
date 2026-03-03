{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Leios (tests) where

import qualified Cardano.Chain.Update as Byron
import Cardano.Ledger.Api
  ( Addr (..)
  , ConwayEra
  , EraTx
  , PParams
  , Tx
  , TxOut
  , addrTxOutL
  , bodyTxL
  , eraProtVerLow
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , mkBasicTxOut
  , outputsTxBodyL
  , txIdTx
  , valueTxOutL
  )
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.Api.Tx.In (TxIn (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), TxIx (..), knownNonZeroBounded, unNonZero, unSlotNo)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import Data.Word (Word64)
import LeiosDemoTypes (TraceLeiosKernel (..))
import Lens.Micro (each, (%~), (^.), (^..))
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
import Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Cardano.Ledger.Alonzo.Examples.Consensus (exampleAlonzoGenesis)
import Test.Cardano.Ledger.Conway.Examples.Consensus (exampleConwayGenesis)
import Test.Consensus.Cardano.ProtocolInfo (Era (Conway), hardForkInto)
import Test.QuickCheck (Property, Testable, conjoin, counterexample, tabulate, withMaxSuccess)
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
  , _FromLeios
  , _FromMempool
  , _nodeEvent
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
prop_leios_blocksProduced seed =
  do
    conjoin
      [ not (null forgedBlocks)
          & counterexample "no praos blocks were forged"
      , any (> 0) includedTxCounts
          & counterexample "all forged blocks were empty (no transactions)"
          & prettyCounterexampleMap "txs per active slot" 120 includedTxCounts
      , not (null forgedEBs)
          & counterexample "no endorser blocks were forged"
          & prettyCounterexampleMap "forged leios EBs" 120 forgedEBs
          & prettyCounterexampleList "leios kernel traces" 120 leiosTraces
      ]
    & counterexample ("mempool total added: " <> show (length mempoolAddedTxs))
    & counterexample ("mempool total rejected: " <> show (length mempoolRejectedTxs))
    & tabulate "Praos blocks forged" [show $ length forgedBlocks]
    & tabulate "Leios blocks forged" [show $ length forgedEBs]
    & tabulate "Effective throughput" [show throughput]
 where
  traces = testOutput.allTraces

  forgedBlocks = foldMap nodeOutputForges testOutput.testOutputNodes

  includedTxCounts = length . extractTxs <$> forgedBlocks

  leiosTraces = traces ^.. each . _nodeEvent . _FromLeios

  forgedEBs = Map.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockForged{slot, eb} -> Just (slot, eb)
    _ -> Nothing

  mempoolTraces = traces ^.. each . _nodeEvent . _FromMempool

  mempoolAddedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolAddedTx tx _ _ -> Just tx
    _ -> Nothing

  mempoolRejectedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolRejectedTx tx _ _ -> Just tx
    _ -> Nothing

  throughput = fromIntegral (sum includedTxCounts) / fromRational numSlots :: Double

  testOutput = runThreadNet seed (NumSlots $ ceiling numSlots)

  -- NOTE: There must be k Praos blocks after this time.
  numSlots = 3 * k / activeSlotCoeff :: Rational

-- * Running the thread net

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
            , ctgeExtraTxGen = \slot cn pparams utxo ->
                pure $ constantLoadTxs numCoreNodes (TPS 100) slot cn pparams utxo
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

newtype TxPerSecond = TPS Word64

-- | Generate a constant load of transactions per second over all nodes.
constantLoadTxs ::
  NumCoreNodes ->
  TxPerSecond ->
  SlotNo ->
  CoreNode StandardCrypto ->
  PParams ConwayEra ->
  Map TxIn (TxOut ConwayEra) ->
  [Tx ConwayEra]
constantLoadTxs (NumCoreNodes n) (TPS txPerSecond) slot cn pparams utxo
  -- FIXME: The node generator is called on every slot, but the ledger state /
  -- utxo is only updated when a block was forged and adopted. This leads to the
  -- same txs being generated, but the mempool rejecting them.
  --
  -- XXX: As a workaround, we only submit every 1/f slots, that is, on the
  -- stochastic expected time between blocks.
  | shouldSubmit =
      take (fromIntegral $ txPerSecondPerNode * expectedBlockTime) $
        infiniteRespendTxs cn pparams utxo
  | otherwise = []
 where
  shouldSubmit = unSlotNo slot `mod` expectedBlockTime == 0

  expectedBlockTime = truncate $ 1 / activeSlotCoeff

  txPerSecondPerNode = txPerSecond `div` n

-- | Generates an infinite list of transactions that respend the first output
-- owned by given 'CoreNode' (delegate key interpreted as payment key).
infiniteRespendTxs ::
  CoreNode StandardCrypto ->
  PParams ConwayEra ->
  Map TxIn (TxOut ConwayEra) ->
  [Tx ConwayEra]
infiniteRespendTxs coreNode pparams utxo =
  case Map.toList myUtxo of
    [] -> []
    (txIn, txOut) : _ ->
      let tx = respendTx txIn txOut
          utxo' = Map.delete txIn utxo <> utxoOfTx tx
       in tx : infiniteRespendTxs coreNode pparams utxo'
 where
  myUtxo = Map.filter (ownedBy paymentSK) utxo

  CoreNode{cnDelegateKey = paymentSK} = coreNode

  respendTx txIn txOut = do
    mkBasicTx mkBasicTxBody
      & bodyTxL . inputsTxBodyL %~ Set.insert txIn
      & bodyTxL . outputsTxBodyL %~ (|> mkBasicTxOut (txOut ^. addrTxOutL) (txOut ^. valueTxOutL))
      -- NOTE: Fees are zero in thread net
      -- & bodyTxL . feeTxBodyL .~ feeCoin
      & signTx paymentSK

  ownedBy sk txOut = case txOut ^. addrTxOutL of
    Addr _ cred _ -> cred == mkCredential sk
    _ -> False

-- | Get the UTxO produced by a given Tx.
utxoOfTx :: EraTx era => Tx era -> Map TxIn (TxOut era)
utxoOfTx tx =
  Map.fromList $ zip (map mkTxIn [0 ..]) outs
 where
  mkTxIn ix = TxIn txId $ TxIx ix
  txId = txIdTx tx
  outs = toList $ tx ^. bodyTxL . outputsTxBodyL

-- * Property utilities

-- | Pretty print a map of counterexamples, one on each row and eliding long
-- entries to given maxLength. If maxLength is 0 or negative, no elision is
-- performed.
prettyCounterexampleMap ::
  (Testable prop, Show a2, Show p) =>
  String -> Int -> Map a2 p -> prop -> Property
prettyCounterexampleMap title maxLength m prop =
  prop
    & counterexample (title <> ":\n" <> prettyMap)
 where
  prettyMap =
    Map.toList m
      & map (\(a, b) -> indented 2 . elided maxLength $ show a <> " -> " <> show b)
      & unlines

-- | Pretty print a list of counterexamples, one on each row and eliding long
-- entries to given maxLength. If maxLength is 0 or negative, no elision is
-- performed.
prettyCounterexampleList ::
  (Testable prop, Show a) =>
  String -> Int -> [a] -> prop -> Property
prettyCounterexampleList title maxLength xs prop =
  prop
    & counterexample (title <> ":\n" <> prettyList)
 where
  prettyList =
    map (indented 2 . elided maxLength . show) xs
      & unlines

-- | Indent each line in a string by a given number of spaces.
indented :: Int -> String -> String
indented n =
  unlines' . map (indent <>) . lines
 where
  indent = replicate n ' '

  unlines' [] = []
  unlines' [x] = x
  unlines' (x : xs) = x <> "\n" <> unlines' xs

-- | Elide a string to a target length by keeping the prefix and suffix and
-- replacing the middle with an ellipsis. If target length is 0 or negative, no
-- elision is performed.
elided :: Int -> String -> String
elided targetLength s
  | targetLength <= 0 = s
  | l < targetLength = s
  | otherwise = prefix <> elipsis <> suffix
 where
  l = length s

  halfLength = targetLength `div` 2

  prefix = take halfLength s

  suffix = drop (l - halfLength - length elipsis) s

  elipsis = "..."
