{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | ThreadNet tests of Leios, the high throughput consensus overlay of Praos2.
module Test.ThreadNet.Leios (tests) where

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import Cardano.Ledger.Api
  ( Addr (..)
  , DijkstraEra
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
import Cardano.Ledger.Api.Tx.In (TxIn (..))
import Cardano.Ledger.BaseTypes
  ( Nonce (NeutralNonce)
  , ProtVer (..)
  , TxIx (..)
  , isSJust
  , knownNonZeroBounded
  )
import Cardano.Ledger.Block (blockBodyL, ebReferencesAnnouncementBlockHeaderL)
import Cardano.Ledger.Core (TopTx)
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import Control.DeepSeq (force)
import Control.Monad (replicateM)
import qualified Control.Tracer as Tracer
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro ((%~), (^.))
import Ouroboros.Consensus.Block (GetHeader (..), blockPoint)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, pattern BlockDijkstra)
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Config (SecurityParam (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Protocol.PBFT (PBftSignatureThreshold (..))
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as MockFS
import qualified System.FS.Sim.STM as Sim
import Test.Consensus.Cardano.ProtocolInfo
  ( Era (Dijkstra)
  , hardForkInto
  , mkTestProtocolInfo
  )
import Test.QuickCheck
  ( Property
  , conjoin
  , counterexample
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
import qualified Test.ThreadNet.Infra.Byron as Byron
import Test.ThreadNet.Infra.Shelley
  ( CoreNode (..)
  , DecentralizationParam (..)
  , genCoreNode
  , mkCredential
  , mkGenesisConfig
  , mkKesConfig
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
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup "ThreadNet Leios" $
    [ adjustQuickCheckTests (`div` 10) $
        testProperty "a single node adopts its own endorser blocks (self-certified)" propLeiosSelfCertify
    ]

-- | A single node in Dijkstra, loaded beyond what fits in its ranking blocks,
-- announces endorser blocks and certifies them in later blocks of its own.
--
-- With one node there is no endorser-block diffusion and no committee, so the
-- node certifies alone. What the property checks is that the node adopts
-- every block it forges, and that each of those blocks is one a peer would
-- accept for its header.
propLeiosSelfCertify :: Seed -> Property
propLeiosSelfCertify seed =
  conjoin
    [ Map.null nodeOutputInvalids
        & counterexample ("invalid blocks: " <> show (Map.keys nodeOutputInvalids))
    , all (`Chain.pointOnChain` nodeOutputFinalChain) (blockPoint <$> forged)
        & counterexample "a forged block was not adopted"
    , all (\blk -> blockMatchesHeader (getHeader blk) blk) chain
        & counterexample "a block does not match its header"
    , any announces chain
        & counterexample "no block announces an endorser block"
    , any certifies chain
        & counterexample "no block certifies an endorser block"
    ]
    & counterexample ("transactions per block: " <> show (length . extractTxs <$> chain))
 where
  NodeOutput{nodeOutputFinalChain, nodeOutputForges, nodeOutputInvalids} =
    case Map.elems (testOutputNodes testOutput) of
      [nodeOutput] -> nodeOutput
      outputs -> error $ "expected one node, got " <> show (length outputs)

  forged = Map.elems nodeOutputForges

  chain = Chain.toOldestFirst nodeOutputFinalChain

  testOutput = runThreadNet seed (NumSlots 200)

-- | Whether a Dijkstra block's header announces an endorser block.
announces :: CardanoBlock StandardCrypto -> Bool
announces = \case
  BlockDijkstra ShelleyBlock{shelleyBlockRaw} ->
    isSJust $ shelleyBlockRaw ^. ebReferencesAnnouncementBlockHeaderL
  _ -> False

-- | Whether a Dijkstra block's body carries a Leios certificate.
certifies :: CardanoBlock StandardCrypto -> Bool
certifies = \case
  BlockDijkstra ShelleyBlock{shelleyBlockRaw} ->
    isSJust $ shelleyBlockRaw ^. blockBodyL . leiosCertBlockBodyL
  _ -> False

-- * Running the network

-- | Run a single Dijkstra node from genesis.
runThreadNet :: Seed -> NumSlots -> TestOutput (CardanoBlock StandardCrypto)
runThreadNet initSeed numSlots =
  runTestNetwork
    TestConfig
      { numSlots
      , numCoreNodes
      , nodeTopology = meshNodeTopology numCoreNodes
      , initSeed
      , txLogicVersion = minBound
      }
    TestConfigB
      { forgeEbbEnv = Nothing
      , future = EraFinal slotLength (sgEpochLength shelleyGenesis)
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra =
          CardanoTxGenExtra
            { ctgeByronGenesisKeys = generatedSecretsByron
            , ctgeNetworkMagic =
                CC.Common.makeNetworkMagic $
                  CC.Genesis.configProtocolMagic genesisByron
            , ctgeShelleyCoreNodes = coreNodes
            , ctgeExtraTxGen = \slot coreNode pparams utxo ->
                -- A transaction the mempool takes in just before the end of
                -- the run would never be forged.
                if not (isDijkstraOrLater pparams) || unSlotNo slot > unNumSlots numSlots - 20
                  then pure []
                  else pure $ constantLoadTxs slot coreNode utxo
            }
      , version = newestVersion (Proxy @(CardanoBlock StandardCrypto))
      }
    TestConfigMB
      { nodeInfo = \coreNodeId@(CoreNodeId nid) -> do
          fs <- SomeHasFS <$> Sim.simHasFS' MockFS.empty
          (protocolInfo, blockForging) <-
            mkTestProtocolInfo
              fs
              (coreNodeId, coreNodes !! fromIntegral nid)
              shelleyGenesis
              (CC.Update.ProtocolVersion 0 0 0)
              NeutralNonce
              genesisByron
              generatedSecretsByron
              (Just $ PBftSignatureThreshold 1)
              dijkstraProtVer
              (hardForkInto Dijkstra)
          pure
            TestNodeInitialization
              { tniProtocolInfo = protocolInfo
              , tniCrucialTxs = []
              , tniBlockForging = blockForging Tracer.nullTracer
              }
      , mkRekeyM = Nothing
      }
 where
  numCoreNodes = NumCoreNodes 1

  coreNodes =
    runGen initSeed $
      replicateM (fromIntegral n) $
        genCoreNode (KESPeriod 0)
   where
    NumCoreNodes n = numCoreNodes

  (genesisByron, generatedSecretsByron) =
    Byron.generateGenesisConfig slotLength $
      Byron.byronPBftParams securityParam numCoreNodes

  shelleyGenesis =
    mkGenesisConfig
      dijkstraProtVer
      securityParam
      activeSlotCoeff
      (DecentralizationParam 0)
      maxLovelaceSupply
      slotLength
      (mkKesConfig (Proxy @StandardCrypto) numSlots)
      coreNodes

-- * Fixtures

dijkstraProtVer :: ProtVer
dijkstraProtVer = ProtVer (eraProtVerLow @DijkstraEra) 0

securityParam :: SecurityParam
securityParam = SecurityParam $ knownNonZeroBounded @10

activeSlotCoeff :: Rational
activeSlotCoeff = 1 / 20

slotLength :: SlotLength
slotLength = slotLengthFromSec 1

maxLovelaceSupply :: Word64
maxLovelaceSupply = 100_000_000_000_000

-- * Transaction generation

-- | Whether these are the protocol parameters of Dijkstra or a later era.
isDijkstraOrLater :: forall era. Ledger.Era era => PParams era -> Bool
isDijkstraOrLater _ = eraProtVerLow @era >= eraProtVerLow @DijkstraEra

-- | Every expected block interval, submit more transactions than a ranking
-- block holds, so that the rest is left for an endorser block.
--
-- The generator runs every slot, but the UTxO it sees only changes when a
-- block is adopted, so submitting every slot would only repeat transactions.
constantLoadTxs ::
  EraTx era =>
  SlotNo ->
  CoreNode StandardCrypto ->
  Map TxIn (TxOut era) ->
  [Tx TopTx era]
constantLoadTxs slot coreNode utxo
  | unSlotNo slot `mod` expectedBlockInterval == 0 =
      take (fromIntegral $ txsPerSlot * expectedBlockInterval) $
        infiniteRespendTxs coreNode utxo
  | otherwise = []
 where
  expectedBlockInterval = truncate $ 1 / activeSlotCoeff

  txsPerSlot = 100 :: Word64

-- | An infinite chain of transactions, each spending the output of the one
-- before, starting from the first output the core node owns. The delegate key
-- doubles as the payment key.
infiniteRespendTxs ::
  EraTx era =>
  CoreNode StandardCrypto ->
  Map TxIn (TxOut era) ->
  [Tx TopTx era]
infiniteRespendTxs coreNode utxo =
  case Map.toList (Map.filter ownedBy utxo) of
    [] -> []
    (txIn, txOut) : _ ->
      let tx = respendTx txIn txOut
       in tx : infiniteRespendTxs coreNode (Map.delete txIn utxo <> utxoOfTx tx)
 where
  CoreNode{cnDelegateKey = paymentSK} = coreNode

  respendTx txIn txOut =
    -- Lens updates leave thunks that the mempool's NoThunks invariant rejects.
    force $
      mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL %~ Set.insert txIn
        & bodyTxL . outputsTxBodyL
          %~ (|> mkBasicTxOut (txOut ^. addrTxOutL) (txOut ^. valueTxOutL))
        & signTx paymentSK

  ownedBy txOut = case txOut ^. addrTxOutL of
    Addr _ cred _ -> cred == mkCredential paymentSK
    _ -> False

-- | The outputs a transaction creates.
utxoOfTx :: EraTx era => Tx TopTx era -> Map TxIn (TxOut era)
utxoOfTx tx =
  Map.fromList $ zip (TxIn (txIdTx tx) . TxIx <$> [0 ..]) outs
 where
  outs = toList $ tx ^. bodyTxL . outputsTxBodyL
