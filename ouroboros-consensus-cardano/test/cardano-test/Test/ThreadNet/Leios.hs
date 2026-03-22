{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Cardano.Ledger.BaseTypes (ProtVer (..), TxIx (..), knownNonZeroBounded)
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Core (fromTxSeq, sizeTxF)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as StrictTVar
import Control.Monad (foldM, replicateM)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import Data.Word (Word64)
import LeiosDemoDb (LeiosDbConnection, newLeiosDBInMemoryWith, withLeiosDb)
import LeiosDemoTypes (LeiosPoint (..), TraceLeiosKernel (..), hashLeiosEb, minCertificationGap)
import Lens.Micro (each, (%~), (^.), (^..))
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot)
import Ouroboros.Consensus.Cardano
  ( CardanoBlock
  , Nonce (NeutralNonce)
  , ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  , ShelleyGenesis (..)
  )
import Ouroboros.Consensus.Cardano.Block (pattern BlockConway, pattern LedgerStateConway)
import Ouroboros.Consensus.Cardano.Node (CardanoProtocolParams (..), protocolInfoCardano)
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig)
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , LedgerCfg
  , tickThenReapply
  )
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerState, ValuesMK)
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ledgerState
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs, forgetLedgerTables)
import Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..), ProtocolInfo (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (shelleyBlockRaw)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( shelleyCumulativeTxBytes
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Test.Cardano.Ledger.Alonzo.Examples.Consensus (exampleAlonzoGenesis)
import Test.Cardano.Ledger.Conway.Examples.Consensus (exampleConwayGenesis)
import Test.Consensus.Cardano.ProtocolInfo (Era (Conway), hardForkInto)
import Test.QuickCheck
  ( Property
  , Testable
  , conjoin
  , counterexample
  , tabulate
  , (.&&.)
  , (.||.)
  , (===)
  )
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
  ( LeiosState (..)
  , NodeOutput (..)
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
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "Leios ThreadNet"
    [ adjustQuickCheckTests (`div` 10) $
        testProperty "basic functionality" prop_leios
    ]

-- | Verify a suite of basic Leios ThreadNet invariants in a single run:
--
-- * EB production, transaction inclusion, and EB diffusion.
-- * Replaying the chain with EB resolution produces the same ledger state
--   as the one computed by the ChainDB during the simulation. This proves
--   that EB transactions from certified EBs are actually applied to the
--   ledger.
-- * 'shelleyCumulativeTxBytes' in the final ledger state matches an
--   independently computed sum of individual transaction sizes over the
--   chain (resolving certifying blocks via the LeiosDB and summing
--   'sizeTxF' per transaction — the same data the accumulator sees, but
--   computed outside of block application).
prop_leios :: Seed -> Property
prop_leios seed =
  conjoin
    [ blocksProduced
    , ebCertificateInclusion
    , cumulativeTxBytes
    , propConsistentChains
    , certificationGapIsCorrect
        .||. length certifyingBlocks <= 1
    , propVoting
    ]
 where
  numNodes = 3 :: Integer
  numSlots = 200 :: Word64

  (testOutput, ProtocolInfo{pInfoConfig, pInfoInitLedger}) =
    runThreadNet seed (NumSlots numSlots) (NumCoreNodes $ fromIntegral numNodes)

  traces = testOutput.allTraces

  forgedBlocks = foldMap nodeOutputForges testOutput.testOutputNodes

  includedTxCounts = length . extractTxs <$> forgedBlocks

  leiosTraces = traces ^.. each . _nodeEvent . _FromLeios

  forgedPoints = Map.keysSet forgedEBs

  forgedEBs = Map.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockForged{slot, eb} -> Just (MkLeiosPoint slot (hashLeiosEb eb), eb)
    _ -> Nothing

  acquiredPoints = Set.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockTxsAcquired point -> Just point
    _ -> Nothing

  propVoting =
    length votedPoints > 0
      & counterexample "never voted"

  votedPoints = Set.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosVoted{point} -> Just point
    _ -> Nothing

  mempoolTraces = traces ^.. each . _nodeEvent . _FromMempool

  mempoolAddedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolAddedTx tx _ _ -> Just tx
    _ -> Nothing

  mempoolRejectedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolRejectedTx tx _ _ -> Just tx
    _ -> Nothing

  nodeChains = Chain.toOldestFirst . nodeOutputFinalChain <$> testOutput.testOutputNodes

  certifyingBlocks =
    -- NOTE: Assumes all nodeChains are consistent
    toList . Set.fromList $
      [ blockSlot blk
      | blk@(BlockConway conwayBlk) <- concat nodeChains
      , case shelleyBlockRaw conwayBlk of
          SL.Block _ (SL.BodyCertificate _ _) -> True
          _ -> False
      ]

  throughput = fromIntegral (sum includedTxCounts) / fromIntegral numSlots :: Double

  -- Pick any node — all nodes should converge to the same chain.
  someNode = snd . Map.findMin $ testOutput.testOutputNodes

  blocksProduced =
    conjoin
      [ isNothing testOutput.exceptionThrown
          & counterexample "test threw an exception"
          & prettyCounterexampleList "all traces" 120 (show <$> traces)
      , not (null forgedBlocks)
          & counterexample "no praos blocks were forged"
      , any (> 0) includedTxCounts
          & counterexample "all forged blocks were empty (no transactions)"
          & prettyCounterexampleMap "txs per active slot" 120 includedTxCounts
      , not (null forgedEBs)
          & counterexample "no endorser blocks were forged"
          & prettyCounterexampleMap "forged leios EBs" 120 forgedEBs
          & prettyCounterexampleList "leios kernel traces" 120 leiosTraces
      , length forgedPoints === length acquiredPoints
          & counterexample "endorser blocks not fully diffused"
          & prettyCounterexampleList "acquired leios EBs" 120 acquiredPoints
          & prettyCounterexampleList "forged leios EBs" 120 forgedPoints
      ]
      & counterexample ("mempool total added: " <> show (length mempoolAddedTxs))
      & counterexample ("mempool total rejected: " <> show (length mempoolRejectedTxs))
      & tabulate "Praos blocks forged" [show $ length forgedBlocks]
      & tabulate "Leios blocks forged" [show $ length forgedEBs]
      & tabulate "Certifying blocks" [show $ length certifyingBlocks]
      & tabulate "Effective throughput" [show throughput]

  ebCertificateInclusion =
    let expectedLedger = nodeOutputFinalLedger someNode
        foldedLedger = replayNodeChain pInfoConfig pInfoInitLedger someNode
     in ( not (null certifyingBlocks)
            & counterexample "no certifying blocks — test is vacuous"
        )
          .&&. (foldedLedger === expectedLedger)

  cumulativeTxBytes =
    let actual = case nodeOutputFinalLedger someNode of
          LedgerStateConway st -> shelleyCumulativeTxBytes st
          _ -> error "expected Conway ledger state"
        expected = sumChainTxBytes pInfoConfig pInfoInitLedger someNode
     in ( actual > 0
            & counterexample "cumulative tx bytes is 0 — no transactions were applied"
        )
          .&&. ( actual === expected
                   & counterexample ("ledger state: " <> show actual)
                   & counterexample ("independent sum: " <> show expected)
               )

  propConsistentChains =
    all (== head (Map.elems nodeChains)) nodeChains
      & counterexample "nodes have different chains"

  -- NOTE: Leios demands minCertificationGap between announcement and
  -- certification. Thus, the slots between certifying blocks must be at least
  -- that far apart.
  certificationGapIsCorrect =
    conjoin $
      [ (unSlotNo s2 - unSlotNo s1 > minCertificationGap)
          & counterexample
            ( "Certification blocks too close: slots "
                <> show (unSlotNo s1)
                <> " and "
                <> show (unSlotNo s2)
                <> " (gap = "
                <> show (unSlotNo s2 - unSlotNo s1)
                <> ", expected > "
                <> show minCertificationGap
                <> ")"
            )
          & prettyCounterexampleList "certifying block slots" 120 certifyingBlocks
      | (s1, s2) <- zip certifyingBlocks (drop 1 certifyingBlocks)
      ]

-- | Independently compute cumulative tx bytes by resolving each block in the
-- chain (filling in EB closures from the LeiosDB) and summing individual
-- 'sizeTxF' values per transaction.
--
-- TODO: 'sumChainTxBytes' and 'replayNodeChain' share the same
-- resolve-and-fold structure. Factor out a generic chain fold that accepts a
-- per-block accumulator.
sumChainTxBytes ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  ExtLedgerState (CardanoBlock StandardCrypto) ValuesMK ->
  NodeOutput (CardanoBlock StandardCrypto) ->
  Word64
sumChainTxBytes topConfig initLedger node = runSimOrThrow $ do
  let db = runIdentity . lsLeiosDb . nodeLeiosState $ node
  stateVar <- StrictTVar.newTVarIO db
  leiosDb <- newLeiosDBInMemoryWith stateVar
  withLeiosDb leiosDb $ \leiosConn -> do
    let chain = Chain.toOldestFirst . nodeOutputFinalChain $ node
        cfg = ExtLedgerCfg topConfig
    fst <$> foldM (step leiosConn cfg) (0, initLedger) chain
 where
  step leiosDb cfg (total, st) blk = do
    blk' <- resolveLeiosBlock leiosDb (headerState st) blk
    let txBytes = blockTxSizeSum blk'
        st' = applyDiffs st $ tickThenReapply OmitLedgerEvents cfg blk' st
    pure (total + txBytes, st')

  -- FIXME(bladyjoker): Why not use blockTxBytes?
  blockTxSizeSum (BlockConway shelleyBlk) =
    case SL.blockBody (shelleyBlockRaw shelleyBlk) of
      SL.BodyInline txSeq -> sumTxSizes txSeq
      SL.BodyCertificate _ (Just txSeq) -> sumTxSizes txSeq
      SL.BodyCertificate _ Nothing -> 0
  -- Byron blocks don't go through Shelley block application, so they
  -- contribute 0 to the cumulative tx bytes.
  blockTxSizeSum _ = 0

  sumTxSizes txSeq =
    fromIntegral $ sum $ map (^. sizeTxF) $ toList $ fromTxSeq txSeq

-- | Replay a node's chain with Leios block resolution and return the
-- resulting ledger state.
replayNodeChain ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  ExtLedgerState (CardanoBlock StandardCrypto) ValuesMK ->
  NodeOutput (CardanoBlock StandardCrypto) ->
  LedgerState (CardanoBlock StandardCrypto) EmptyMK
replayNodeChain topConfig initLedger node = runSimOrThrow $ do
  let db = runIdentity . lsLeiosDb . nodeLeiosState $ node
  stateVar <- StrictTVar.newTVarIO db
  leiosDb <- newLeiosDBInMemoryWith stateVar
  withLeiosDb leiosDb $ \leiosConn -> do
    let chain = Chain.toOldestFirst . nodeOutputFinalChain $ node
        cfg = ExtLedgerCfg topConfig
    foldedState <- foldWithResolution leiosConn cfg chain initLedger
    pure $ forgetLedgerTables . ledgerState $ foldedState

-- | Fold a chain of blocks over an initial ledger state, resolving Leios
-- blocks (filling in EB transaction closures for certifying blocks) before
-- each application.
--
-- We use 'tickThenReapply' because the blocks have already been validated
-- by the ChainDB. We use 'applyDiffs' instead of 'applyDiffForKeys'
-- because we need to accumulate the full UTxO — 'applyDiffForKeys' only
-- retains entries referenced by the current block, which is designed for
-- the LedgerDB's backing store architecture.
foldWithResolution ::
  Monad m =>
  LeiosDbConnection m ->
  LedgerCfg (ExtLedgerState (CardanoBlock StandardCrypto)) ->
  [CardanoBlock StandardCrypto] ->
  ExtLedgerState (CardanoBlock StandardCrypto) ValuesMK ->
  m (ExtLedgerState (CardanoBlock StandardCrypto) ValuesMK)
foldWithResolution leiosDb cfg blks initState =
  foldM step initState blks
 where
  step state blk = do
    blk' <- resolveLeiosBlock leiosDb (headerState state) blk
    pure $ applyDiffs state $ tickThenReapply OmitLedgerEvents cfg blk' state

-- * Running the thread net

runThreadNet ::
  Seed ->
  NumSlots ->
  NumCoreNodes ->
  (TestOutput (CardanoBlock StandardCrypto), ProtocolInfo (CardanoBlock StandardCrypto))
runThreadNet initSeed numSlots numCoreNodes =
  ( runTestNetwork
      testConfig
      testConfigB
      TestConfigMB
        { nodeInfo = \(CoreNodeId nid) ->
            let (protocolInfo, blockForging) = protocolInfoCardano (cardanoProtocolParams nid)
             in TestNodeInitialization
                  { tniProtocolInfo = protocolInfo
                  , tniCrucialTxs = []
                  , tniBlockForging = blockForging
                  }
        , mkRekeyM = Nothing
        }
  , protocolInfo0
  )
 where
  protocolInfo0 = fst $ protocolInfoCardano @StandardCrypto @IO (cardanoProtocolParams (0 :: Word64))

  cardanoProtocolParams nid =
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

  conwayProtVer = ProtVer (eraProtVerLow @ConwayEra) 0

  NumCoreNodes n = numCoreNodes

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
                -- NOTE: Stop generating txs 20 slots before end of test run.
                if unSlotNo slot > unNumSlots numSlots - 20
                  then pure []
                  else pure $ constantLoadTxs numCoreNodes (TPS 100) slot cn pparams utxo
            }
      , version = newestVersion (Proxy @(CardanoBlock StandardCrypto))
      }

-- * Fixtures

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
  (Testable prop, Show a, Foldable f) =>
  String -> Int -> f a -> prop -> Property
prettyCounterexampleList title maxLength xs prop =
  prop
    & counterexample (title <> ":\n" <> prettyList)
 where
  prettyList =
    map (indented 2 . elided maxLength . show) (toList xs)
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
