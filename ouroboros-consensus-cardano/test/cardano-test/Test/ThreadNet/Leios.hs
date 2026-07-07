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
import Control.Monad.IOSim (runSimOrThrow)
import qualified Control.Tracer as Tracer
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
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
  ( LeiosPoint (..)
  , LeiosVote (..)
  , TraceLeiosKernel (..)
  , hashLeiosEb
  , minCertificationGap
  )
import Lens.Micro ((%~), (^.))
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot, getHeader)
import Ouroboros.Consensus.Cardano
  ( CardanoBlock
  , Nonce (NeutralNonce)
  , ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  , ShelleyGenesis (..)
  )
import Ouroboros.Consensus.Cardano.Block (pattern BlockDijkstra, pattern LedgerStateDijkstra)
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
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
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
import qualified Test.Cardano.Ledger.Shelley.Examples as Shelley (leTranslationContext)
import Test.Consensus.Cardano.ProtocolInfo (Era (Dijkstra), hardForkInto)
import Test.QuickCheck
  ( Property
  , Testable
  , choose
  , conjoin
  , counterexample
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
    TraceLeiosBlockTxsAcquired point -> Just point
    _ -> Nothing

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
        Set.size acquiredVotePairs === numNodes * Set.size castVotes
          & counterexample "created votes not diffused"
          & counterexample ("cast votes: " <> show (Set.size castVotes))
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
      & counterexample ("certify-and-announce block slots: " <> show certifyAndAnnounceBlocks)

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

  -- Slots of blocks that /both/ certify a previous EB (cert in the body)
  -- and announce a fresh one (announcement in the header);
  -- this set being non-empty is the direct evidence that we can certify an EB
  -- and announce a new one in the same RB.
  certifyAndAnnounceBlocks =
    toList . Set.fromList $
      [ blockSlot blk
      | blk@(BlockDijkstra dijkstraBlk) <- concat nodeChains
      , let SL.Block _ body = shelleyBlockRaw dijkstraBlk
      , SJust _ <- [body ^. leiosCertBlockBodyL]
      , isJust (headerLeiosAnnouncement (getHeader blk))
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
      & tabulate "Certifying blocks" [show $ length certificateBlocks]
      & tabulate "Certify-and-announce blocks" [show $ length certifyAndAnnounceBlocks]
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
          & prettyCounterexampleList "certifying block slots" 120 certificateBlocks
      | (s1, s2) <- zip certificateBlocks (drop 1 certificateBlocks)
      ]

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
        inlineLeiosClosure blk
          <$> resolveLeiosClosure leiosDb point
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
          closureTxs <- resolveLeiosClosure leiosDb point
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
  ( runTestNetwork
      testConfig
      testConfigB
      TestConfigMB
        { nodeInfo = \(CoreNodeId nid) -> do
            fs <- SomeHasFS <$> Sim.simHasFS' MockFS.empty
            (protocolInfo, blockForging) <- protocolInfoCardano fs (cardanoProtocolParams nid)
            pure
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
