{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.Api.Tx.In (TxIn (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..), TxIx (..), knownNonZeroBounded)
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Core (TopTx, sizeTxF, txSeqBlockBodyL)
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import qualified Cardano.Ledger.Shelley.LedgerState as SL
  ( esLState
  , lsCertState
  , lsUTxOState
  , nesEs
  , utxosInstantStake
  )
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as StrictTVar
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, replicateM)
import Control.Monad.IOSim (Time, runSimOrThrow)
import qualified Control.Tracer as Tracer
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.List (isInfixOf, sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import Data.Word (Word64)
import LeiosDemoDb
  ( LeiosDbConnection
  , newLeiosDBInMemoryWith
  , withLeiosDb
  )
import LeiosDemoTypes
  ( LeiosNotVotedReason (..)
  , LeiosPoint (..)
  , LeiosVote (..)
  , RbHash (..)
  , TraceLeiosKernel (..)
  , hashLeiosEb
  , minCertificationGap
  , prettyEbHash
  , prettyLeiosPoint
  )
import Lens.Micro ((%~), (.~), (^.))
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot, getHeader)
import Ouroboros.Consensus.Block.Forging
  ( BlockForging (..)
  , ForgeBlockArgs (..)
  , MkBlockForging (..)
  )
import Ouroboros.Consensus.Cardano
  ( CardanoBlock
  , Nonce (NeutralNonce)
  , ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  , ShelleyGenesis (..)
  )
import Ouroboros.Consensus.Cardano.Block
  ( pattern BlockDijkstra
  , pattern GenTxDijkstra
  , pattern LedgerStateDijkstra
  )
import Ouroboros.Consensus.Cardano.Node (CardanoProtocolParams (..), protocolInfoCardano)
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig, configLedger)
import Ouroboros.Consensus.HeaderValidation (headerStateChainDep)
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , LedgerCfg
  , tickThenReapply
  )
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ledgerState
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, extractTxs)
import Ouroboros.Consensus.Ledger.Tables.MapKind (EmptyMK, ValuesMK)
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs, forgetLedgerTables)
import Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..), ProtocolInfo (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (shelleyBlockRaw)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( shelleyCumulativeTxBytes
  , shelleyLedgerState
  , shelleyLedgerTip
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB (ResolveLeiosBlock (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as MockFS
import qualified System.FS.Sim.STM as Sim
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Cardano.Ledger.Alonzo.Examples as Alonzo
import qualified Test.Cardano.Ledger.Conway.Examples as Conway
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra
import qualified Test.Cardano.Ledger.Shelley.Examples as Shelley (lePParams, leTranslationContext)
import Test.Consensus.Cardano.ProtocolInfo (Era (Dijkstra), hardForkInto)
import Test.QuickCheck
  ( Property
  , Testable
  , choose
  , conjoin
  , counterexample
  , discard
  , forAll
  , ioProperty
  , property
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
  , TraceThreadNet (FromNode)
  , TraceThreadNetNode (FromLeios, FromLeiosPeer, FromMempool)
  )
import Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..))
import Test.ThreadNet.Util.NodeJoinPlan (NodeJoinPlan (..), trivialNodeJoinPlan)
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
    , adjustQuickCheckTests (`div` 10) $
        testProperty "late join" prop_leios_late_join
    , adjustQuickCheckTests (`div` 10) $
        testProperty "invalid endorsed tx is not certified" prop_leios_invalid_eb
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
        & counterexample "[failed] blocksProduced"
    , ebCertificateInclusion
        & counterexample "[failed] ebCertificateInclusion"
    , cumulativeTxBytes
        & counterexample "[failed] cumulativeTxBytes"
    , propConsistentChains
        & counterexample "[failed] propConsistentChains"
    , ( certificationGapIsCorrect
          .||. length certificateBlocks
            <= 1
      )
        & counterexample "[failed] certificationGap"
    , propVoting
        & counterexample "[failed] propVoting"
    , propCertifying
        & counterexample "[failed] propCertifying"
    , propCertifyAndAnnounce
        & counterexample "[failed] propCertifyAndAnnounce"
    , propClosuresValidate
        & counterexample "[failed] propClosuresValidate"
    ]
 where
  numNodes = 3 :: Int

  numSlots = 200 :: Word64

  (testOutput, ProtocolInfo{pInfoConfig, pInfoInitLedger}) =
    runThreadNet seed (NumSlots numSlots) numCoreNodes (trivialNodeJoinPlan numCoreNodes)

  numCoreNodes = NumCoreNodes $ fromIntegral numNodes

  traces = testOutput.allTraces

  forgedBlocks = foldMap nodeOutputForges testOutput.testOutputNodes

  includedTxCounts = length . extractTxs <$> forgedBlocks

  leiosTraces = [ev | FromNode _ (FromLeios ev) <- traces]

  forgedPoints = Map.keysSet forgedEBs

  forgedEBs = Map.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockForged{slot, eb} -> Just (MkLeiosPoint slot (hashLeiosEb eb), eb)
    _ -> Nothing

  acquiredPoints = Set.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockTxsAcquired point _age -> Just point
    _ -> Nothing

  -- An EB forged at slot @s@ is required to diffuse iff it has at least
  -- 'minCertificationGap' slots to propagate before the sim ends, i.e.
  -- @s + minCertificationGap <= numSlots@. 'minCertificationGap' will
  -- eventually be a protocol parameter capturing the worst case diffusion
  -- time; the property therefore assumes it is sufficient by definition.
  diffusionRequired point =
    unSlotNo point.pointSlotNo + minCertGap <= numSlots

  forgedPointsToDiffuse = Set.filter diffusionRequired forgedPoints
  acquiredPointsToDiffuse = Set.filter diffusionRequired acquiredPoints

  propVoting =
    conjoin
      [ length castVotes > 0
          & counterexample "never voted"
      , -- NOTE: We used to require @acquiredRbHashes ⊆ votedAnnouncingRbHashes@,
        -- i.e. "every acquired EB gets a vote from someone". That held when
        -- 'runLeiosVoting' scanned the whole selected chain fragment for any
        -- announcer of the acquired EB. The current protocol-correct policy
        -- only votes when the acquired EB is announced by the *tip* of the
        -- currently selected chain, gated by a '3 * L_hdr' equivocation wait
        -- and an 'L_vote' deadline. An acquired EB whose announcer never sits
        -- on the tip during that window (chain not yet caught up, chain has
        -- moved past, deadline exceeded) is legitimately not voted on. The
        -- weaker "did we vote at all" check above plus 'propCertifying'
        -- suffice.
        --
        -- Every cast vote should reach every node. Compare the number of
        -- distinct cast votes against the number of distinct (node, vote)
        -- acquisition pairs: each cast vote contributes 'numNodes' such
        -- pairs (the casting node's own 'TraceLeiosVoteAcquired' plus one
        -- per receiving node). Counting unique pairs rather than raw event
        -- counts shrugs off the spurious 'TraceLeiosVoteAcquired's emitted
        -- by 'NodeToNode' for relay-redelivered votes that 'addVote'
        -- already classified 'AlreadyKnown'.
        Set.size votePairsToDiffuse === numNodes * Set.size votesToDiffuse
          & counterexample "created votes not diffused"
          & counterexample ("cast votes: " <> show (Set.size castVotes))
          & counterexample ("votes required to diffuse: " <> show (Set.size votesToDiffuse))
          & counterexample
            ( "acquired (node, vote) pairs: "
                <> show (Set.size acquiredVotePairs)
            )
          & counterexample
            ( "peer traces: "
                <> unlines [show (nid, ev) | FromNode nid (FromLeiosPeer ev) <- traces]
            )
          & counterexample
            ( "kernel traces: "
                <> unlines [show (nid, ev) | FromNode nid (FromLeios ev) <- traces]
            )
      ]

  -- Set of votes that were cast (one 'TraceLeiosVoted' per cast).
  castVotes :: Set.Set LeiosVote
  castVotes = Set.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosVoted{vote} -> Just vote
    _ -> Nothing

  -- Which EB each announcing RB hash announced, so that a vote can be dated:
  -- 'LeiosVote' carries only the hash it signed.
  announcedPoints :: Map RbHash LeiosPoint
  announcedPoints = Map.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosBlockAnnounced{announcingRbHashBytes, announcedEbPoint} ->
      Just (MkRbHash announcingRbHashBytes, announcedEbPoint)
    _ -> Nothing

  -- Same requirement the EB diffusion properties make, one step further along:
  -- a vote is required to have diffused iff its EB had 'minCertificationGap'
  -- slots left to propagate. A vote cast near the end of the sim can still be
  -- in flight when it stops, which says nothing about diffusion.
  votesToDiffuse =
    flip Set.filter castVotes $ \vote ->
      maybe False diffusionRequired (Map.lookup vote.announcingRbHash announcedPoints)

  votePairsToDiffuse = Set.filter ((`Set.member` votesToDiffuse) . snd) acquiredVotePairs

  -- Distinct (node, vote) pairs from 'TraceLeiosVoteAcquired'. A vote
  -- received multiple times by the same node (e.g. relayed back via a
  -- mesh peer) only contributes one pair.
  acquiredVotePairs :: Set.Set (CoreNodeId, LeiosVote)
  acquiredVotePairs = Set.fromList $ flip mapMaybe traces $ \case
    FromNode nid (FromLeios TraceLeiosVoteAcquired{vote}) -> Just (nid, vote)
    _ -> Nothing

  -- Certifying an EB and announcing a new one are no longer mutually exclusive, so
  -- an RB can finalise the previous EB (via a cert) while announcing the
  -- next one. With continuous tx flow, a certifying block rebases the
  -- mempool onto the post-certified-EB ledger state and announces a fresh EB
  -- from the survivors. Unless the run produced no certifying blocks at all,
  -- at least one should exercise the combined path.
  propCertifyAndAnnounce =
    (not (null announcedAndCertifiedSlots))
      & counterexample "no block both certified and announced"
      & counterexample ("certifying block slots: " <> show certificateBlocks)
      & counterexample ("announced-and-certified slots: " <> show announcedAndCertifiedSlots)

  -- In an honest net every acquired closure must apply against the announcing
  -- RB's ledger state, because 'partitionMempool' cuts both the RB's and the
  -- EB's transactions out of a single mempool snapshot: the snapshot is
  -- internally consistent, so its transactions apply in sequence against the
  -- state it was taken for, and the announcing RB /is/ that state plus its own
  -- share of the split. The EB's share therefore applies against the RB's
  -- post-state by construction. So a rejection here cannot mean a transaction
  -- was bad; it means the voting thread validated against the wrong state or
  -- the wrong slot.
  propClosuresValidate =
    null closureRejections
      & counterexample "a voter rejected an honestly-produced EB's transactions"
      & prettyCounterexampleList "rejections" 240 closureRejections
      -- The reapply-vs-apply split: how much full validation the LeiosTxCache
      -- spared us. Tabulated only for visibility -- nothing here requires any
      -- particular ratio.
      & tabulate
        "EB closure validation (reapplied/total)"
        [show reapplied <> "/" <> show (reapplied + applied) | (reapplied, applied) <- validatedSplits]

  closureRejections =
    [ (nid, ebPoint, err)
    | FromNode nid (FromLeios TraceLeiosNotVoted{ebPoint, reason = EbTxsInvalid err}) <- traces
    ]

  validatedSplits =
    flip mapMaybe leiosTraces $ \case
      TraceLeiosEbValidated{reapplied, applied} -> Just (reapplied, applied)
      _ -> Nothing

  propCertifying =
    conjoin
      [ length reachedQuorumPoints > 0
          & counterexample "never reached quorum"
          & counterexample
            ( "not-voted events: "
                <> unlines
                  [ show (nid, ev)
                  | FromNode nid (FromLeios ev@TraceLeiosNotVoted{}) <- traces
                  ]
            )
      ]

  reachedQuorumPoints = Set.fromList . flip mapMaybe leiosTraces $ \case
    TraceLeiosCertified{rbHash} -> Just rbHash
    _ -> Nothing

  mempoolTraces = [ev | FromNode _ (FromMempool ev) <- traces]

  mempoolAddedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolAddedTx tx _ _ -> Just tx
    _ -> Nothing

  mempoolRejectedTxs = flip mapMaybe mempoolTraces $ \case
    TraceMempoolRejectedTx tx _ _ _ -> Just tx
    _ -> Nothing

  nodeChains = Chain.toOldestFirst . nodeOutputFinalChain <$> testOutput.testOutputNodes

  certificateBlocks =
    -- NOTE: Assumes all nodeChains are consistent
    toList . Set.fromList $
      [ blockSlot blk
      | blk@(BlockDijkstra dijkstraBlk) <- concat nodeChains
      , let SL.Block _ body = shelleyBlockRaw dijkstraBlk
      , SJust _ <- [body ^. leiosCertBlockBodyL]
      ]

  -- Slots at which a single forging opportunity both certified the
  -- previously-announced EB and announced a fresh one.
  announcedAndCertifiedSlots :: [SlotNo]
  announcedAndCertifiedSlots =
    toList . Set.fromList $ flip mapMaybe leiosTraces $ \case
      TraceLeiosCertifiedAndAnnounced{atSlot} -> Just atSlot
      _ -> Nothing

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
      , -- Only require diffusion for EBs forged early enough that they have
        -- 'minCertificationGap' slots to propagate before the sim ends. In
        -- Leios this gap will become a protocol parameter capturing the worst
        -- case diffusion time; using it here keeps the property aligned with
        -- the protocol's own diffusion assumption.
        length forgedPointsToDiffuse === length acquiredPointsToDiffuse
          & counterexample "endorser blocks not fully diffused"
          & counterexample
            (missingEbTimelines testOutput.allTracesWithTime forgedPointsToDiffuse acquiredPointsToDiffuse)
          & prettyCounterexampleList "acquired leios EBs (diffusion required)" 120 acquiredPointsToDiffuse
          & prettyCounterexampleList "forged leios EBs (diffusion required)" 120 forgedPointsToDiffuse
          & prettyCounterexampleList "acquired leios EBs (all)" 120 acquiredPoints
          & prettyCounterexampleList "forged leios EBs (all)" 120 forgedPoints
      ]
      & counterexample ("mempool total added: " <> show (length mempoolAddedTxs))
      & counterexample ("mempool total rejected: " <> show (length mempoolRejectedTxs))
      & tabulate "Praos blocks forged" [show $ length forgedBlocks]
      & tabulate "Leios blocks forged" [show $ length forgedEBs]
      & tabulate "Certifying blocks" [show $ length certificateBlocks]
      & tabulate "Certify-and-announce blocks" [show $ length announcedAndCertifiedSlots]
      & tabulate "Effective throughput" [show throughput]

  -- FIXME: This only exercises the in-memory replay via
  -- 'foldWithResolution' (essentially the volatile-range
  -- 'Forker.applyBlock' code path). It does NOT cover the
  -- immutable-DB replay path used at node startup
  -- ('replayStartingWith' → V1/V2 'reapplyBlock'). Bug
  -- discovered on the staging-branch testnet: that replay path
  -- was silently bypassing 'resolveLeiosBlock', causing CertRB
  -- bodies to be re-applied as empty (no EB-txs spliced in),
  -- which left the post-restart ledger state missing every
  -- EB-tx output and triggered 'BadInputsUTxO' on the first
  -- volatile block that spent one of those outputs. A
  -- ThreadNet variant that snapshots one node mid-run, kills
  -- it, and restarts from disk would catch this — see the
  -- proto-devnet kill-and-restart drill in
  -- ouroboros-leios/demo/proto-devnet for the manual analogue.
  ebCertificateInclusion
    -- If the run produced no CertRBs (e.g. an unlucky low-leadership
    -- seed where only a handful of blocks land at all), there is no
    -- EB-closure replay to check — pass vacuously, matching the
    -- 'certificationGap' carve-out above.
    | null certificateBlocks = property True
    | otherwise =
        let expectedLedger = nodeOutputFinalLedger someNode
            foldedLedger = replayNodeChain pInfoConfig pInfoInitLedger someNode
            dijkstraOf st = case st of
              LedgerStateDijkstra d -> d
              _ -> error "ebCertificateInclusion: expected Dijkstra ledger state"
            lhs = dijkstraOf foldedLedger
            rhs = dijkstraOf expectedLedger
            -- The full @ShelleyLedgerState DijkstraEra@ is huge, so we
            -- compare salient projections individually. The first failing
            -- assertion narrows the divergence to a specific field.
            nesLhs = shelleyLedgerState lhs
            nesRhs = shelleyLedgerState rhs
            lsLhs = SL.esLState (SL.nesEs nesLhs)
            lsRhs = SL.esLState (SL.nesEs nesRhs)
            chain = Chain.toOldestFirst (nodeOutputFinalChain someNode)
            chainCertRBs =
              [ blockSlot blk
              | blk@(BlockDijkstra dBlk) <- chain
              , let SL.Block _ body = shelleyBlockRaw dBlk
              , SJust _ <- [body ^. leiosCertBlockBodyL]
              ]
            chainSummary =
              "chain length = "
                <> show (length chain)
                <> ", CertRBs in chain = "
                <> show (length chainCertRBs)
                <> " at slots "
                <> show chainCertRBs
                <> "\nblock slots = "
                <> show (map blockSlot chain)
                <> "\nfoldedLedger tip = "
                <> show (shelleyLedgerTip lhs)
                <> ", instantStake = "
                <> show (SL.utxosInstantStake (SL.lsUTxOState lsLhs))
                <> "\nexpectedLedger tip = "
                <> show (shelleyLedgerTip rhs)
                <> ", instantStake = "
                <> show (SL.utxosInstantStake (SL.lsUTxOState lsRhs))
         in conjoin
              [ shelleyLedgerTip lhs === shelleyLedgerTip rhs
                  & counterexample "[ebCertificateInclusion] shelleyLedgerTip"
              , shelleyCumulativeTxBytes lhs === shelleyCumulativeTxBytes rhs
                  & counterexample "[ebCertificateInclusion] shelleyCumulativeTxBytes"
              , SL.lsUTxOState lsLhs === SL.lsUTxOState lsRhs
                  & counterexample "[ebCertificateInclusion] lsUTxOState"
              , SL.lsCertState lsLhs === SL.lsCertState lsRhs
                  & counterexample "[ebCertificateInclusion] lsCertState"
              ]
              & counterexample chainSummary

  cumulativeTxBytes =
    let actual = case nodeOutputFinalLedger someNode of
          LedgerStateDijkstra st -> shelleyCumulativeTxBytes st
          _ -> error "expected Dijkstra ledger state"
        expected = sumChainTxBytes pInfoConfig pInfoInitLedger someNode
     in ( actual > 0
            & counterexample "cumulative tx bytes is 0 — no transactions were applied"
        )
          .&&. ( actual === expected
                   & counterexample ("ledger state: " <> show actual)
                   & counterexample ("independent sum: " <> show expected)
               )

  propConsistentChains =
    ( case Map.elems nodeChains of
        [] -> True
        c : cs -> all (== c) cs
    )
      & counterexample "nodes have different chains"

  -- NOTE: Leios demands minCertificationGap between announcement and
  -- certification. Thus, the slots between certifying blocks must be at least
  -- that far apart.
  certificationGapIsCorrect =
    conjoin $
      [ (unSlotNo s2 - unSlotNo s1 > minCertGap)
          & counterexample
            ( "Certification blocks too close: slots "
                <> show (unSlotNo s1)
                <> " and "
                <> show (unSlotNo s2)
                <> " (gap = "
                <> show (unSlotNo s2 - unSlotNo s1)
                <> ", expected > "
                <> show minCertGap
                <> ")"
            )
          & prettyCounterexampleList "certifying block slots" 120 certificateBlocks
      | (s1, s2) <- zip certificateBlocks (drop 1 certificateBlocks)
      ]

-- | The certification gap the simulated nodes run with.
--
-- 'minCertificationGap' derives it from the protocol parameters and the slot
-- length, and
-- 'runThreadNet'' hard-forks into Dijkstra with the ledger's example parameters,
-- so reading it from the same examples value keeps the two in step.
minCertGap :: Word64
minCertGap =
  unSlotNo $
    minCertificationGap slotLength (Shelley.lePParams Dijkstra.ledgerExamples)

-- | A late-joining node must not crash on a CertRB whose certified EB
-- closure it never observed live.
--
-- 4 nodes, 200 slots. Nodes 0–2 join at slot 0; node 3 joins at a random
-- slot in @[1, numSlots - 1]@, after at least some CertRBs may already
-- have been produced.
prop_leios_late_join :: Seed -> Property
prop_leios_late_join seed =
  forAll (choose (1, fromIntegral numSlots - 1)) $ \lateJoinSlot ->
    let
      joinPlan =
        NodeJoinPlan $
          Map.fromList
            [ (CoreNodeId 0, SlotNo 0)
            , (CoreNodeId 1, SlotNo 0)
            , (CoreNodeId 2, SlotNo 0)
            , (CoreNodeId 3, SlotNo $ fromIntegral (lateJoinSlot :: Int))
            ]

      numCoreNodes = NumCoreNodes 4

      (testOutput, _) =
        runThreadNet seed (NumSlots numSlots) numCoreNodes joinPlan
     in
      -- 'runThreadNet' rethrows simulation exceptions when its result is
      -- forced. Catch them here so the property reports a failure with the
      -- 'lateJoinSlot' counterexample rather than letting the exception
      -- propagate.
      ioProperty $ do
        r <- try @SomeException $ evaluate testOutput
        pure $ case r of
          Left e ->
            -- DEBUG: try to grab traces too. If forcing the traces
            -- also throws (because they share the failing chunk of
            -- IOSim output), wrap in another 'try' and degrade
            -- gracefully.
            counterexample ("late join slot: " <> show lateJoinSlot) $
              counterexample ("threw: " <> show e) False
          Right _ -> property True
 where
  numSlots = 200 :: Word64

-- | An EB whose closure cannot apply must never be certified: the committee is
-- supposed to validate the endorsed transactions before signing a vote.
--
-- 4 nodes, 200 slots, node 1 adversarial. Only the EBs it announces carry the
-- bogus tx — its RBs stay honest — so the chain keeps growing and the honest
-- nodes' own EBs still certify, which is what makes 'poisonedNeverCertified'
-- more than a statement about a stalled network.
prop_leios_invalid_eb :: Seed -> Property
prop_leios_invalid_eb seed
  -- An unlucky seed where the adversary never led early enough to announce an
  -- EB with time to diffuse. There is nothing to conclude either way, so don't
  -- count the run rather than passing it vacuously.
  | Set.null poisonedPointsToDiffuse = discard
  -- Whether any honest EB reaches quorum is seed-dependent: votes scatter
  -- across forks, so a 4-node run can finish with nothing certified at all.
  -- Such a run cannot tell "the poisoned EB was rejected" apart from "nothing
  -- was certified", so it is no evidence either -- discard rather than fail.
  | Set.null honestCertifiedPoints = discard
  | otherwise =
      conjoin
        [ adversaryWasEffective
            & counterexample "[failed] adversaryWasEffective"
        , poisonedNeverCertified
            & counterexample "[failed] poisonedNeverCertified"
        , poisonedNeverValidated
            & counterexample "[failed] poisonedNeverValidated"
        , validationDidRun
            & counterexample "[failed] validationDidRun"
        , isNothing testOutput.exceptionThrown
            & counterexample "[failed] test threw an exception"
            & prettyCounterexampleList "all traces" 120 (show <$> traces)
        ]
        & tabulate "poisoned EB turned down on" poisonedNotVotedReasons
 where
  adversary = CoreNodeId 1

  numCoreNodes = NumCoreNodes 4

  numSlots = 200 :: Word64

  (testOutput, _) =
    runThreadNet'
      seed
      (NumSlots numSlots)
      numCoreNodes
      (trivialNodeJoinPlan numCoreNodes)
      (endorseInvalidTx adversary)

  traces = testOutput.allTraces

  -- 'TraceLeiosBlockForged' is emitted by the forger only, so the emitting
  -- node id attributes each EB to whoever made it.
  forgedBy nid = Set.fromList $ flip mapMaybe traces $ \case
    FromNode nid' (FromLeios TraceLeiosBlockForged{slot, eb})
      | nid' == nid ->
          Just (MkLeiosPoint slot (hashLeiosEb eb))
    _ -> Nothing

  poisonedPoints = forgedBy adversary

  -- Only EBs forged early enough to have 'minCertificationGap' slots left can
  -- be expected to reach an honest node at all — the same bound 'prop_leios'
  -- uses for diffusion.
  poisonedPointsToDiffuse =
    Set.filter
      (\p -> unSlotNo p.pointSlotNo + minCertGap <= numSlots)
      poisonedPoints

  acquiredByHonest = Set.fromList $ flip mapMaybe traces $ \case
    FromNode nid (FromLeios (TraceLeiosBlockTxsAcquired point _age))
      | nid /= adversary -> Just point
    _ -> Nothing

  -- Ground truth from the adopted chains rather than from vote traces: a
  -- CertRB certifies the EB that its parent's header announced.
  certifiedPoints = foldMap certifiedEbPoints nodeChains

  nodeChains = Chain.toOldestFirst . nodeOutputFinalChain <$> testOutput.testOutputNodes

  -- Guard against a vacuous pass: a poisoned EB must actually have reached an
  -- honest node's closure, or nobody was ever in a position to vote on one.
  adversaryWasEffective =
    not (Set.null (Set.intersection poisonedPointsToDiffuse acquiredByHonest))
      & counterexample "no honest node acquired a poisoned EB's closure"
      & prettyCounterexampleList "poisoned EBs (diffusion required)" 120 poisonedPointsToDiffuse
      & prettyCounterexampleList "acquired by honest nodes" 120 acquiredByHonest

  poisonedNeverCertified =
    Set.intersection poisonedPoints certifiedPoints === Set.empty
      & counterexample "a poisoned EB was certified"
      & prettyCounterexampleList "poisoned EBs" 120 poisonedPoints
      & prettyCounterexampleList "certified EBs" 120 certifiedPoints

  -- The EBs certified in this run that were not the adversary's. Empty means
  -- the run proves nothing, which is a discard above rather than a failure.
  honestCertifiedPoints = Set.difference certifiedPoints poisonedPoints

  -- A poisoned closure cannot apply, so no node may ever report it validated.
  -- Unlike 'poisonedNeverCertified' this holds even when the vote never got
  -- close — it is the invariant, not the outcome.
  poisonedNeverValidated =
    Set.null (Set.intersection poisonedPoints validatedPoints)
      & counterexample "an EB carrying the bogus tx was reported as validated"
      & prettyCounterexampleList "validated EBs" 120 validatedPoints

  -- Non-vacuity for the validation path itself: some honest EB must have gone
  -- through it. Without this the property would also pass on a build where
  -- voting never validates anything.
  validationDidRun =
    not (Set.null (Set.difference validatedPoints poisonedPoints))
      & counterexample "no EB's transactions were ever validated"
      & prettyCounterexampleList "validated EBs" 120 validatedPoints

  validatedPoints = Set.fromList $ flip mapMaybe leiosTraces $ \case
    TraceLeiosEbValidated{ebPoint} -> Just ebPoint
    _ -> Nothing

  leiosTraces = [ev | FromNode _ (FromLeios ev) <- traces]

  -- How the poisoned EBs were actually turned down: on their transactions, or
  -- earlier on one of the cheap gates. Only the former exercises validation,
  -- and which one happens depends on chain-tip timing, so report it rather
  -- than requiring it.
  poisonedNotVotedReasons =
    [ reasonLabel reason
    | FromNode _ (FromLeios TraceLeiosNotVoted{ebPoint, reason}) <- traces
    , Set.member ebPoint poisonedPoints
    ]

  reasonLabel = \case
    EbTxsInvalid{} -> "EbTxsInvalid"
    ChainTipDoesNotAnnounce -> "ChainTipDoesNotAnnounce"
    TooLate -> "TooLate"
    NotOnCommittee -> "NotOnCommittee"
    VoteRejected{} -> "VoteRejected"

-- | The EB points certified by a chain's CertRBs. A CertRB carries no
-- announcement of its own for the EB it certifies; the announcement lives on
-- its parent's header, so walk the chain carrying the previous announcement —
-- the same traversal 'sumChainTxBytes' uses to resolve closures.
certifiedEbPoints :: [CardanoBlock StandardCrypto] -> Set.Set LeiosPoint
certifiedEbPoints = go Nothing
 where
  go _ [] = Set.empty
  go prevAnn (blk : rest) =
    here <> go (fst <$> headerLeiosAnnouncement (getHeader blk)) rest
   where
    here = case (blockLeiosCert blk, prevAnn) of
      (Just _, Just point) -> Set.singleton point
      _ -> Set.empty

-- * Misbehaving nodes

-- | Make one node endorse a transaction that can never apply, by appending
-- 'invalidEbTx' to the txs it draws for the EB it is about to announce.
-- 'fbRbTxs' is untouched, so the announcing RB itself stays valid.
endorseInvalidTx ::
  Functor m =>
  -- | Which node misbehaves; every other node is left alone.
  CoreNodeId ->
  CoreNodeId ->
  TestNodeInitialization m (CardanoBlock StandardCrypto) ->
  TestNodeInitialization m (CardanoBlock StandardCrypto)
endorseInvalidTx adversary nid tni
  | nid /= adversary = tni
  | otherwise =
      tni{tniBlockForging = map (mapMkBlockForging poison) <$> tniBlockForging tni}
 where
  poison bf =
    bf
      { forgeBlock = \args ->
          forgeBlock bf args{fbEbTxs = fbEbTxs args <> [lie invalidEbTx]}
      }
  -- The lie is told here and only here: 'assumeValidatedClosureTx' stamps the tx
  -- as mempool-validated so the forge will endorse it, which is exactly the
  -- claim a voter is now supposed to disbelieve. Applying it at the use site
  -- keeps 'invalidEbTx' an ordinary, plainly invalid transaction.
  lie = assumeValidatedClosureTx

-- | A Dijkstra transaction that spends an output which does not exist: its
-- input names the id of a transaction that is never applied anywhere, so the
-- UTxO rule must reject it with 'BadInputsUTxO'.
--
-- Deliberately a /state-dependent/ failure rather than a missing-witness one:
-- a missing witness is a static check, which is exactly what the reapply path
-- is allowed to skip, so a witness-only failure would not pin down the
-- behaviour we care about. This tx is unsigned for the same reason — the
-- signature is irrelevant to the check that has to catch it.
invalidEbTx :: GenTx (CardanoBlock StandardCrypto)
invalidEbTx =
  -- 'force'd because 'ForgedLeiosEb' and the forge path run 'NoThunks'
  -- invariants over what they are handed, as the mempool does in 'respendTx'.
  GenTxDijkstra (mkShelleyTx (force tx))
 where
  tx :: Tx TopTx DijkstraEra
  tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ Set.singleton phantomInput)

  phantomInput = TxIn (txIdTx phantomTx) (TxIx 0)

  -- Never submitted, never forged: its id therefore never appears in any UTxO.
  phantomTx :: Tx TopTx DijkstraEra
  phantomTx = mkBasicTx mkBasicTxBody

-- | Rewrap the 'BlockForging' a 'MkBlockForging' allocates.
mapMkBlockForging ::
  Functor m =>
  (BlockForging m blk -> BlockForging m blk) ->
  MkBlockForging m blk ->
  MkBlockForging m blk
mapMkBlockForging f (MkBlockForging alloc) = MkBlockForging (f <$> alloc)

-- | Independently compute cumulative tx bytes by resolving each block in the
-- chain (filling in EB closures from the LeiosDB via 'inlineLeiosClosure')
-- and summing individual 'sizeTxF' values per transaction.
--
-- Uses the block-serving code path ('resolveLeiosClosure' +
-- 'inlineLeiosClosure') rather than the apply-time bookkeeping in
-- 'applyLeiosClosure', so the two paths cross-check each other.
sumChainTxBytes ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  ExtLedgerState (CardanoBlock StandardCrypto) ValuesMK ->
  NodeOutput (CardanoBlock StandardCrypto) ->
  Word64
sumChainTxBytes _topConfig _initLedger node = runSimOrThrow $ do
  let db = runIdentity . lsLeiosDb . nodeLeiosState $ node
  stateVar <- StrictTVar.newTVarIO db
  leiosDb <- newLeiosDBInMemoryWith stateVar
  withLeiosDb leiosDb $ \leiosConn ->
    foldChain leiosConn Nothing 0 (Chain.toOldestFirst $ nodeOutputFinalChain node)
 where
  -- Fold the chain, inlining each CertRB's EB closure into its
  -- (empty-on-wire) body using the announcement carried by the previous
  -- header — mirroring what the ChainSync server does when serving blocks.
  foldChain _ _ !total [] = pure total
  foldChain leiosDb prevAnn !total (blk : rest) = do
    blk' <- case (blockLeiosCert blk, prevAnn) of
      (Just _, Just point) ->
        inlineLeiosClosure blk . map snd
          <$> resolveLeiosClosure leiosDb (pointEbHash point)
      _ -> pure blk
    let nextAnn = fst <$> headerLeiosAnnouncement (getHeader blk)
    foldChain leiosDb nextAnn (total + blockTxSizeSum blk') rest

  blockTxSizeSum (BlockDijkstra shelleyBlk) =
    let SL.Block _ body = shelleyBlockRaw shelleyBlk
     in sumTxSizes (body ^. txSeqBlockBodyL)
  -- Byron blocks don't go through Shelley block application, so they
  -- contribute 0 to the cumulative tx bytes.
  blockTxSizeSum _ = 0

  sumTxSizes txSeq =
    fromIntegral $ sum $ map (^. sizeTxF) $ toList txSeq

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

-- | Fold a chain of blocks over an initial ledger state, mirroring the
-- LedgerDB's apply path so the replayed final ledger matches the one the
-- chain converged to during the simulation.
--
-- For a CertRB, this mirrors 'Forker.applyBlock' 'ApplyVal': the EB
-- closure's txs are folded onto the parent ledger via 'applyLeiosClosure'
-- (ledger-level 'applyTxValidation ValidateNone') and then the
-- (empty-body) CertRB is applied with 'tickThenReapply' on top — so the
-- LEDGERS rule sees an empty body and 'shelleyCumulativeTxBytes' is not
-- bumped for closure txs.
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
    -- Mirror the production apply path (Forker.applyBlock 'ApplyVal' arm):
    -- for a CertRB, fold the EB closure's txs onto the parent ledger via
    -- 'applyLeiosClosure' (no validation), then 'tickThenReapply' the
    -- (empty-body) CertRB on top. For non-CertRB blocks, this collapses
    -- to plain 'tickThenReapply'.
    let cds = headerStateChainDep (headerState state)
    stateAfterClosure <- case blockLeiosCert blk of
      Nothing -> pure state
      Just _cert -> case protocolStateLeiosAnnouncement @(CardanoBlock StandardCrypto) cds of
        Nothing ->
          error "foldWithResolution: CertRB but no announcement on parent chain-dep state"
        Just (point, _) -> do
          closureTxs <- map snd <$> resolveLeiosClosure leiosDb (pointEbHash point)
          let ls = ledgerState state
              lcfg = configLedger (getExtLedgerCfg cfg)
          case applyLeiosClosure lcfg closureTxs ls of
            Left err -> error $ "foldWithResolution: applyLeiosClosure failed: " <> show err
            Right ls' -> pure state{ledgerState = ls'}
    pure $
      applyDiffs stateAfterClosure $
        tickThenReapply OmitLedgerEvents cfg blk stateAfterClosure

-- * Running the thread net

runThreadNet ::
  Seed ->
  NumSlots ->
  NumCoreNodes ->
  NodeJoinPlan ->
  (TestOutput (CardanoBlock StandardCrypto), ProtocolInfo (CardanoBlock StandardCrypto))
runThreadNet initSeed numSlots numCoreNodes joinPlan =
  runThreadNet' initSeed numSlots numCoreNodes joinPlan (\_nid -> id)

-- | 'runThreadNet' with a per-node tweak of the node's initialization, for
-- tests that need one node to misbehave. Tweaking the whole
-- 'TestNodeInitialization' rather than just its 'BlockForging' means a test can
-- reach the protocol info and the crucial txs too; see 'endorseInvalidTx'.
runThreadNet' ::
  Seed ->
  NumSlots ->
  NumCoreNodes ->
  NodeJoinPlan ->
  ( forall m.
    Functor m =>
    CoreNodeId ->
    TestNodeInitialization m (CardanoBlock StandardCrypto) ->
    TestNodeInitialization m (CardanoBlock StandardCrypto)
  ) ->
  (TestOutput (CardanoBlock StandardCrypto), ProtocolInfo (CardanoBlock StandardCrypto))
runThreadNet' initSeed numSlots numCoreNodes joinPlan tweakNodeInit =
  ( runTestNetwork
      testConfig
      testConfigB
      TestConfigMB
        { nodeInfo = \coreNodeId@(CoreNodeId nid) -> do
            fs <- SomeHasFS <$> Sim.simHasFS' MockFS.empty
            (protocolInfo, blockForging) <- protocolInfoCardano fs (cardanoProtocolParams nid)
            pure $
              tweakNodeInit coreNodeId $
                TestNodeInitialization
                  { tniProtocolInfo = protocolInfo
                  , tniCrucialTxs = []
                  , tniBlockForging = blockForging Tracer.nullTracer
                  }
        , mkRekeyM = Nothing
        }
  , protocolInfo0
  )
 where
  protocolInfo0 = unsafePerformIO $ do
    fs <- SomeHasFS <$> Sim.simHasFS' MockFS.empty
    fst <$> protocolInfoCardano @StandardCrypto @IO fs (cardanoProtocolParams (0 :: Word64))
  {-# NOINLINE protocolInfo0 #-}

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
      , cardanoHardForkTriggers = hardForkInto Dijkstra
      , cardanoLedgerTransitionConfig =
          mkLatestTransitionConfig
            shelleyGenesis
            (Shelley.leTranslationContext Alonzo.ledgerExamples)
            (Shelley.leTranslationContext Conway.ledgerExamples)
            (Shelley.leTranslationContext Dijkstra.ledgerExamples)
      , cardanoCheckpoints = mempty
      , cardanoProtocolVersion = dijkstraProtVer
      }

  dijkstraProtVer = ProtVer (eraProtVerLow @DijkstraEra) 0

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
      dijkstraProtVer
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
      , txLogicVersion = minBound
      }

  testConfigB =
    TestConfigB
      { forgeEbbEnv = Nothing
      , future = EraFinal slotLength shelleyGenesis.sgEpochLength
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan = joinPlan
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
  EraTx era =>
  NumCoreNodes ->
  TxPerSecond ->
  SlotNo ->
  CoreNode StandardCrypto ->
  PParams era ->
  Map TxIn (TxOut era) ->
  [Tx TopTx era]
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
  EraTx era =>
  CoreNode StandardCrypto ->
  PParams era ->
  Map TxIn (TxOut era) ->
  [Tx TopTx era]
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

  respendTx txIn txOut =
    -- The mempool / tx-submission server runs NoThunks invariants over the
    -- buffered tx state. Lens-based updates leave thunks in the constructed
    -- tx, so we 'force' the result before handing it off to the mempool.
    force $
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
utxoOfTx :: EraTx era => Tx TopTx era -> Map TxIn (TxOut era)
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
      & map (\(a, b) -> indented 2 $ elided kvLength (show a) <> arrowStr <> elided kvLength (show b))
      & unlines

  arrowStr = " -> "

  kvLength = (maxLength - length arrowStr) `div` 2

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

-- | For every EB that was required to diffuse but wasn't acquired, dump a
-- time-ordered timeline of every trace event that mentions its hash, tagged by
-- the emitting node. Intended purely as a diagnostic aid on failure — makes it
-- possible to eyeball the diffusion pipeline (forge → announce → offer →
-- fetch → txs-acquired) and see where the latency lives.
missingEbTimelines ::
  [(Time, TraceThreadNet (CardanoBlock StandardCrypto))] ->
  Set.Set LeiosPoint ->
  Set.Set LeiosPoint ->
  String
missingEbTimelines timedTraces required acquired
  | Set.null missing = ""
  | otherwise =
      "missing EB diffusion timelines:\n" <> concatMap oneEb (Set.toAscList missing)
 where
  missing = required `Set.difference` acquired

  oneEb point =
    let hashHex = prettyEbHash point.pointEbHash
        events = filter (\(_, ev) -> hashHex `isInfixOf` show ev) timedTraces
     in "  "
          <> prettyLeiosPoint point
          <> "\n"
          <> unlines
            [ indented 4 (show t <> " " <> show ev)
            | (t, ev) <- sortOn fst events
            ]

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
