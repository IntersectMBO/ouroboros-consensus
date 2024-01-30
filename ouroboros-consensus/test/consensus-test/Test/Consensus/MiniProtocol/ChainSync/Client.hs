{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Tests for the chain sync client.
--
-- The chain sync client is a stateful component that tracks the chain of an
-- upstream peer. It validates the headers that it receives from the peer;
-- validated headers are then reported to the block fetch client which will
-- download them and offer them to the chain DB, which makes the final choice
-- whether or not to adopt those blocks.
--
-- The tests mock a series of state changes of the up-stream node as well as the
-- node's own state (the node's own state is relevant because if the node and the
-- up-stream peer diverge too much we are not interested in their chain anymore,
-- and we might not be able to validate their headers). We then check that the
-- chain sync client is reporting the right exceptions if and only if we expect
-- them to be thrown based on the mock state changes (exceptions such as
-- "fork is deep", "up-stream node asked for an invalid rollback", etc.).
--
-- The client's (simulated) wall-clock matters in this test because the
-- ChainSync client has special handling for headers that arrive before the
-- wall-clock reaches the onset of the header's claimed slot, which is
-- inevitable even with only honest peers due to non-global clocks
-- drifting/etc. This test advances time in a way that is unrealistic but does
-- allow for some headers to arrive early (but not so early that the client
-- disconnects from the server).
--
-- The approach to the clocks is as follows. A logical clock drives the whole
-- test; it ticks along the naturals. Each tick causes the local and upstream
-- chains to update and that's the primary content of the whole test. However,
-- the /first/ thing that happens at the start of each logical tick is the
-- client's simulated wall-clock advances (via a single 'threadDelay' call) to
-- the onset of the greatest slot involved in any of that logical tick's
-- server-side chain updates /less/ the randomly-chosen local clock skew. Thus,
-- if the greatest header involved in some logical tick is part of an upstream
-- chain update, then it will arrive as a future header (but only near-future,
-- never far-future). (Client-side updates are also handled, but slightly
-- differently; see the code comments.) Finally, recall that the @io-sim@ layer
-- means those delays happen nearly instantaneously with respect to the real
-- world wall-clock.
module Test.Consensus.MiniProtocol.ChainSync.Client (tests) where

import           Cardano.Crypto.DSIGN.Mock
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad (forM_, unless, void, when)
import           Control.Monad.Class.MonadThrow (Handler (..), catches)
import           Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (contramap, contramapM, nullTracer)
import           Data.DerivingVia (InstantiatedAt (InstantiatedAt))
import           Data.List (intercalate)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Semigroup (Max (Max), getMax)
import qualified Data.Set as Set
import           Data.Time (diffUTCTime)
import           Data.Typeable
import           GHC.Generics (Generic)
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (ClockSkew,
                     clockSkewInSeconds, unClockSkew)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..))
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended hiding (ledgerState)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainDbView (..), ChainSyncClientException,
                     ChainSyncClientResult (..), ConfigEnv (..), Consensus,
                     DynamicEnv (..), Our (..), Their (..),
                     TraceChainSyncClientEvent (..), bracketChainSyncClient,
                     chainSyncClient)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (NodeToNodeVersion)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Storage.ChainDB.API
                     (InvalidBlockReason (ValidationError))
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Consensus.Util.Time (multipleNominalDelay,
                     nominalDelay)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (getTipPoint)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Mock.Chain (Chain (Genesis))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ProducerState (chainState,
                     initChainProducerState)
import qualified Ouroboros.Network.Mock.ProducerState as CPS
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Quiet (Quiet (..))
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChainUpdates (ChainUpdate (..), UpdateBehavior (..),
                     genChainUpdates, toChainUpdates)
import           Test.Util.LogicalClock (Tick (..))
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Schedule (Schedule (..), genSchedule, joinSchedule,
                     lastTick, shrinkSchedule)
import qualified Test.Util.TestBlock as TestBlock
import           Test.Util.TestBlock
import           Test.Util.Tracer (recordingTracerTVar)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "ChainSyncClient"
    [ testProperty "chainSync" prop_chainSync
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

prop_chainSync :: ChainSyncClientSetup -> Property
prop_chainSync testSetup@ChainSyncClientSetup {
    securityParam
  , clientUpdates
  , serverUpdates
  , startTick
  , invalidBlocks
  , clientSlowBy
  } =
    tabulate "TickArrivalTimeStats" [show (tickArrivalTimeStats traceEvents)] $
    counterexample (prettyChainSyncClientSetup testSetup) $
    counterexample
    ("Client chain: "     <> ppChain finalClientChain  <> "\n" <>
     "Server chain: "     <> ppChain finalServerChain  <> "\n" <>
     "Synced fragment: "  <> ppFragment syncedFragment <> "\n" <>
     "Trace:\n"           <> unlines (map ppTraceEvent traceEvents)) $
    -- If an exception has been thrown, we check that it was right to throw
    -- it, but not the other way around: we don't check whether a situation
    -- has occured where an exception should have been thrown, but wasn't.
    case mbResult of
      Just (ClientFinished (ForkTooDeep intersection _ _))     ->
        label "ForkTooDeep" $
        counterexample ("ForkTooDeep intersection: " <> ppPoint intersection) $
        not (withinFragmentBounds intersection clientFragment)
      Just (ClientFinished (NoMoreIntersection (Our ourTip) (Their theirTip))) ->
        label "NoMoreIntersection" $
        counterexample ("NoMoreIntersection ourHead: " <> ppPoint (getTipPoint ourTip) <>
                        ", theirHead: " <> ppPoint (getTipPoint theirTip)) $
        not (clientFragment `forksWithinK` syncedFragment)
      Just (ClientFinished (RolledBackPastIntersection intersection _ _)) ->
        label "RolledBackPastIntersection" $
        counterexample ("RolledBackPastIntersection intersection: " <> ppPoint intersection) $
        not (withinFragmentBounds intersection syncedFragment)
      Just (ClientFinished result) ->
        counterexample ("Terminated with result: " ++ show result) False
      Just (ClientThrew ex) ->
        counterexample ("Exception: " ++ displayException ex) False
      Just (ClientSelectedFutureTip ft) ->
        counterexample ("Client selected future tip: " ++ show ft) False
      Nothing ->
        counterexample "Synced fragment not a suffix of the server chain"
        (syncedFragment `isSuffixOf` finalServerChain) .&&.
        counterexample "Synced fragment doesn't intersect with the client chain"
        (clientFragment `forksWithinK` syncedFragment) .&&.
        counterexample "Synced fragment doesn't have the same anchor as the client fragment"
        (AF.anchorPoint clientFragment === AF.anchorPoint syncedFragment)
  where
    k = maxRollbacks securityParam

    ChainSyncOutcome {
        finalClientChain
      , finalServerChain
      , mbResult
      , syncedFragment
      , traceEvents
      } = runSimOrThrow $
        runChainSync
          (slotLengthTenthsToClockSkew clientSlowBy)
          securityParam
          clientUpdates
          serverUpdates
          invalidBlocks
          startTick

    clientFragment = AF.anchorNewest k $ Chain.toAnchoredFragment finalClientChain

    forksWithinK
      :: AnchoredFragment TestBlock  -- ^ Our chain
      -> AnchoredFragment TestBlock  -- ^ Their chain
      -> Bool
    forksWithinK ourChain theirChain = case AF.intersect ourChain theirChain of
      Nothing -> False
      Just (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) ->
        fromIntegral (AF.length ourSuffix) <= k

-- | Generalization of 'AF.withinFragmentBounds' that returns false if the
-- types don't line up
--
-- This kind of "dynamic type checking" is a bit ugly but is only necessary
-- for the tests.
withinFragmentBounds :: forall blk blk'. (HasHeader blk, Typeable blk')
                     => Point blk -> AnchoredFragment blk' -> Bool
withinFragmentBounds p af =
    case eqT @blk @blk' of
      Just Refl -> AF.withinFragmentBounds p af
      Nothing   -> False

-- | Check whether the anchored fragment is a suffix of the chain.
isSuffixOf :: AnchoredFragment TestBlock -> Chain TestBlock -> Property
isSuffixOf fragment chain =
    fragmentAnchor === chainAnchor .&&.  fragmentBlocks === chainBlocks
  where
    nbBlocks       = AF.length fragment
    fragmentBlocks = AF.toOldestFirst fragment
    fragmentAnchor = AF.anchorPoint fragment
    chainBlocks    = reverse $ take nbBlocks $ Chain.toNewestFirst chain
    chainAnchor    = Chain.headPoint $ Chain.drop nbBlocks chain

{-------------------------------------------------------------------------------
  Infastructure to run a Chain Sync test
-------------------------------------------------------------------------------}

-- | Chain Sync Server
serverId :: CoreNodeId
serverId = CoreNodeId 1

-- | The schedule that determines the evolution of the local chain.
--
-- Note that the 'TestBlock' used in this test is constructed in such a way
-- that the block's slot number equals its block number.
newtype ClientUpdates =
  ClientUpdates { getClientUpdates :: Schedule ChainUpdate }
  deriving (Show)

newtype ServerUpdates =
  ServerUpdates { getServerUpdates :: Schedule ChainUpdate }
  deriving (Show)

-- | A 'Schedule' of events when we learn that a specific block is invalid. Note
-- that it is possible that learning that a block is invalid can precede us
-- receiving it from the ChainSync server (which models the possibility that
-- other peers already sent us that block earlier).
newtype InvalidBlocks =
  InvalidBlocks { getInvalidBlocks :: Schedule TestHash }
  deriving (Show)

type TraceEvent = (Tick, RelativeTime, Either
  (TraceChainSyncClientEvent TestBlock)
  (TraceSendRecv (ChainSync (Header TestBlock) (Point TestBlock) (Tip TestBlock))))

data ChainSyncOutcome = ChainSyncOutcome {
      finalClientChain :: Chain TestBlock
    , finalServerChain :: Chain TestBlock
    , syncedFragment   :: AnchoredFragment TestBlock
    , mbResult         :: Maybe ChainSyncClientTestResult
    , traceEvents      :: [TraceEvent]
    }

-- | We have a client and a server chain that both start at genesis. At
-- certain times, we apply updates to both of these chains to simulate changes
-- to the chains.
--
-- At a certain time, we start the chain sync protocol with a \"real\" chain
-- sync client and the example chain sync server. The chain sync client will
-- start to maintain a candidate fragment that is following the server chain.
-- Note that if client and/or server updates are scheduled at the same time as
-- the start of the syncing, then those updates are applied before syncing
-- starts.
--
-- Both the client and server chain will keep on receiving updates. The chain
-- sync client will keep the candidate fragment in sync with the updating
-- server chain.
--
-- At the end, we return the final chains, the synced candidate fragment, and
-- any exception thrown by the chain sync client. The candidate fragment can
-- then be compared to the actual server chain. If an exception was thrown, no
-- more chain updates are applied so the state at the time of the exception is
-- returned.
--
-- Note that updates that are scheduled before the time at which we start
-- syncing help generate different chains to start syncing from.
runChainSync
    :: forall m. (IOLike m, MonadTime m)
    => ClockSkew
    -> SecurityParam
    -> ClientUpdates
    -> ServerUpdates
    -> InvalidBlocks
    -> Tick  -- ^ Start chain syncing at this time
    -> m ChainSyncOutcome
runChainSync skew securityParam (ClientUpdates clientUpdates)
    (ServerUpdates serverUpdates) (InvalidBlocks invalidBlocks)
    startSyncingAt = withRegistry $ \registry -> do

    clientSystemTime <- do
        initialIoSimClockValue <- getCurrentTime
        pure SystemTime {
            systemTimeWait    = pure ()
          , systemTimeCurrent = do
                now <- getCurrentTime
                -- Subtracting the initial @io-sim@ wall clock to create this
                -- 'RelativeTime' causes the test to behave as if the local
                -- node and the peer were invoked when the "true" wall clock
                -- (which the server's clock happens to equal) is at exactly
                -- the onset of Slot 0.
                pure $ RelativeTime $
                    (now `diffUTCTime` initialIoSimClockValue)
                  -
                    unClockSkew skew
          }
    let _ = clientSystemTime :: SystemTime m

    varCurrentLogicalTick <- uncheckedNewTVarM (Tick 0)
    let clockUpdates :: Schedule NewMaxSlot
        clockUpdates =
            mkClockUpdates
              (ClientUpdates clientUpdates)
              (ServerUpdates serverUpdates)

    -- Set up the client
    varCandidates   <- uncheckedNewTVarM Map.empty
    varClientState  <- uncheckedNewTVarM Genesis
    varClientResult <- uncheckedNewTVarM Nothing
    varKnownInvalid <- uncheckedNewTVarM mempty
    -- Candidates are removed from the candidates map when disconnecting, so
    -- we lose access to them. Therefore, store the candidate 'TVar's in a
    -- separate map too, one that isn't emptied. We can use this map to look
    -- at the final state of each candidate.
    varFinalCandidates <- uncheckedNewTVarM Map.empty
    varHandles     <- uncheckedNewTVarM Map.empty

    (tracer, getTrace) <- do
          (tracer', getTrace) <- recordingTracerTVar
          let pairWithNow ev = do
                logicalNow <- readTVarIO varCurrentLogicalTick
                now        <- systemTimeCurrent clientSystemTime
                pure (logicalNow, now, ev)
          pure (contramapM pairWithNow tracer', getTrace)
    let chainSyncTracer = contramap Left  tracer
        protocolTracer  = contramap Right tracer

    let chainDbView :: ChainDbView m TestBlock
        chainDbView = ChainDbView
          { getCurrentChain =
              AF.mapAnchoredFragment TestHeader . AF.anchorNewest k .
                Chain.toAnchoredFragment <$>
                readTVar varClientState
          , getHeaderStateHistory =
              computeHeaderStateHistory nodeCfg <$>
                readTVar varClientState
          , getPastLedger     = \pt ->
              computePastLedger nodeCfg pt <$>
                readTVar varClientState
          , getIsInvalidBlock = do
              knownInvalid <- readTVar varKnownInvalid
              let isInvalidBlock hash =
                    if hash `Set.member` knownInvalid
                    then Just
                       . ValidationError
                       . ExtValidationErrorLedger
                       $ TestBlock.InvalidBlock
                    else Nothing
                  -- The set of known-invalid blocks grows monotonically (as a
                  -- function in the tick number), so its size can serve as a
                  -- fingerprint.
                  fp = Fingerprint $ fromIntegral $ Set.size knownInvalid
              pure $ WithFingerprint isInvalidBlock fp
          }

        headerInFutureCheck :: InFutureCheck.SomeHeaderInFutureCheck m TestBlock
        headerInFutureCheck =
            InFutureCheck.realHeaderInFutureCheck skew clientSystemTime
            -- Note that this tests passes in the exact difference between the
            -- client's and server's clock as the tolerable clock skew.

        client :: StrictTVar m (AnchoredFragment (Header TestBlock))
               -> (Their (Tip TestBlock) -> STM m ())
               -> Consensus ChainSyncClientPipelined
                    TestBlock
                    m
        client varCandidate setTheirTip =
            chainSyncClient
              ConfigEnv {
                  chainDbView
                , cfg                     = nodeCfg
                , tracer                  = chainSyncTracer
                , someHeaderInFutureCheck = headerInFutureCheck
                , mkPipelineDecision0     =
                    pipelineDecisionLowHighMark 10 20
                }
              DynamicEnv {
                  version             = maxBound :: NodeToNodeVersion
                , controlMessageSTM   = return Continue
                , headerMetricsTracer = nullTracer
                , varCandidate
                , setTheirTip
                }

    -- Set up the server
    varChainProducerState <- uncheckedNewTVarM $ initChainProducerState Genesis
    let server :: ChainSyncServer (Header TestBlock) (Point TestBlock)
                                  (Tip TestBlock) m ()
        server = chainSyncServerExample () (unsafeToUncheckedStrictTVar varChainProducerState) getHeader

    let advanceWallClockForTick :: Tick -> m ()
        advanceWallClockForTick tick = do
            doTick clockUpdates tick $ \case
              [newMaxSlot] -> do
                let target = case newMaxSlot of
                      NewMaxClientSlot slot -> toOnset slot
                      NewMaxServerSlot slot -> toSkewedOnset slot

                      NewMaxClientAndServerSlot cslot sslot ->
                        toOnset cslot `max` toSkewedOnset sslot
                now <- systemTimeCurrent clientSystemTime
                threadDelay $ nominalDelay $ target `diffRelTime` now

              _ -> error "impossible! bad mkClockUpdates"

    -- Do scheduled updates of the client and server chains
    let updateChainsDuringTick :: Tick -> m ()
        updateChainsDuringTick tick = do
            -- Stop updating the client and server chains when the chain sync client
            -- has thrown an exception or has gracefully terminated, so that at the
            -- end, we can read the chains in the states they were in when the
            -- exception was thrown.
            stop <- fmap isJust $ atomically $ readTVar varClientResult
            unless stop $ do
              -- Newly discovered invalid blocks
              whenJust (Map.lookup tick (getSchedule invalidBlocks)) $
                atomically . modifyTVar varKnownInvalid . Set.union . Set.fromList

              -- TODO interleave the client and server chain update
              -- applications in a more interesting way?

              -- Client
              doTick clientUpdates tick $ \chainUpdates ->
                atomically $ modifyTVar varClientState $ updateClientState chainUpdates

              -- Server
              doTick serverUpdates tick $ \chainUpdates ->
                atomically $ do
                  chainProducerState <- readTVar varChainProducerState
                  case CPS.applyChainUpdates
                         (toChainUpdates chainUpdates)
                         chainProducerState of
                    Just chainProducerState' ->
                      writeTVar varChainProducerState chainProducerState'
                    Nothing                  ->
                      error $ "Invalid chainUpdates: " <> show chainUpdates <>
                              " for " <> show (chainState chainProducerState)

    -- Connect client to server and run the chain sync protocol
    --
    -- Happens /immediately after/ the chain and clock effects schedule for
    -- 'startSyncingAt'.
    let initiateChainSync = do
            (clientChannel, serverChannel) <- createConnectedChannels
            -- Don't link the thread (which will cause the exception to be
            -- rethrown in the main thread), just catch the exception and store
            -- it, because we want a "regular ending".
            void $ forkThread registry "ChainSyncClient" $
              bracketChainSyncClient
                 chainSyncTracer
                 chainDbView
                 varCandidates
                 varHandles
                 serverId
                 maxBound $ \varCandidate setTheirTip -> do
                   atomically $ modifyTVar varFinalCandidates $
                     Map.insert serverId varCandidate
                   result <-
                     runPipelinedPeer protocolTracer codecChainSyncId clientChannel $
                       chainSyncClientPeerPipelined $ client varCandidate setTheirTip
                   atomically $ writeTVar varClientResult (Just (ClientFinished result))
                   return ()
              `catchAlsoLinked` \ex -> do
                atomically $ writeTVar varClientResult (Just (ClientThrew ex))
                -- Rethrow, but it will be ignored anyway.
                throwIO ex
            void $ forkLinkedThread registry "ChainSyncServer" $
              runPeer nullTracer codecChainSyncId serverChannel
                      (chainSyncServerPeer server)

    -- If the candidate's tip's slot's onset is ahead of the local wall-clock
    -- (which is skewed by 'clientSlowBy'), then the ChainSync client
    -- mishandled a block from the future.
    let checkTipTime :: m ()
        checkTipTime = do
            now        <- systemTimeCurrent clientSystemTime
            candidates <- atomically $
              readTVar varCandidates >>= traverse readTVar
            forM_ candidates $ \candidate -> do
              let p = castPoint $ AF.headPoint candidate :: Point TestBlock
              case pointSlot p of
                Origin  -> pure ()
                At slot -> when (now < toOnset slot) $ do
                  atomically $ writeTVar varClientResult $ Just
                    $ ClientSelectedFutureTip $ FutureTip {
                          ftNow   = now
                        , ftPoint = (toOnset slot, p)
                        }

    do
      let loop tick = do
              -- first update the clocks
              advanceWallClockForTick tick
              atomically $ writeTVar varCurrentLogicalTick tick

              -- then do the messages
              updateChainsDuringTick tick
              when (tick == startSyncingAt) $ initiateChainSync

              -- check the invariants before advancing the clock again
              --
              -- This is not a perfect check, since the server's chain may have
              -- violated the invariant ephemerally (ie due to a subsequent
              -- rollback during the same logical tick). However, other
              -- QuickCheck seeds/counterexamples should trigger such a bug in
              -- a non-ephemeral way.
              checkTipTime

              when (tick < finalTick) $ loop (tick + 1)
      loop (Tick 1)

      -- This delay seems enough to let all threads finish their final work.
      --
      -- TODO what is the necessary threshold?
      threadDelay 86400

    traceEvents <- getTrace
    -- Collect the return values
    atomically $ do
      finalClientChain  <- readTVar varClientState
      finalServerChain  <- chainState <$> readTVar varChainProducerState
      candidateFragment <- readTVar varFinalCandidates >>= readTVar . (Map.! serverId)
      mbResult          <- readTVar varClientResult
      return ChainSyncOutcome {
          finalClientChain
        , finalServerChain
        , mbResult
        , syncedFragment   = AF.mapAnchoredFragment testHeader candidateFragment
        , traceEvents
        }
  where
    k = maxRollbacks securityParam

    toSkewedOnset :: SlotNo -> RelativeTime
    toSkewedOnset slot =
      let RelativeTime onset = toOnset slot
      in
      RelativeTime $ onset - unClockSkew skew

    doTick :: Schedule a -> Tick -> ([a] -> m ()) -> m ()
    doTick sched tick kont = whenJust (Map.lookup tick (getSchedule sched)) kont

    nodeCfg :: TopLevelConfig TestBlock
    nodeCfg = TopLevelConfig {
        topLevelConfigProtocol = BftConfig {
            bftParams  = BftParams {
                             bftSecurityParam = securityParam
                           , bftNumNodes      = numCoreNodes
                           }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.fromList [
                             (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
                           , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
                           ]
          }
      , topLevelConfigLedger  = testBlockLedgerConfigFrom eraParams
      , topLevelConfigBlock   = TestBlockConfig numCoreNodes
      , topLevelConfigCodec   = TestBlockCodecConfig
      , topLevelConfigStorage = TestBlockStorageConfig
      }

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams securityParam slotLength

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 2

    finalTick :: Tick
    finalTick = maximum
      [ lastTick clientUpdates
      , lastTick serverUpdates
      , startSyncingAt
      ]

    catchAlsoLinked :: Exception e => m a -> (e -> m a) -> m a
    catchAlsoLinked ma handler = ma `catches`
      [ Handler handler
      , Handler $ \(ExceptionInLinkedThread _ ex) -> throwIO ex `catch` handler
      ]

-- | See 'ClientSelectedFutureTip'
data FutureTip = FutureTip {
      ftNow   :: RelativeTime
      -- ^ when the header was selected prematurely
    , ftPoint :: (RelativeTime, Point TestBlock)
      -- ^ point of the header that was selected prematurely, and the
      -- 'RelativeTime' of its slot's onset
    }
  deriving (Show)

data ChainSyncClientTestResult =
    ClientFinished          !ChainSyncClientResult
    -- ^ This is only a property failure if the result was unjustified.
  | ClientSelectedFutureTip !FutureTip
    -- ^ This is always a property failure.
  | ClientThrew             !ChainSyncClientException
    -- ^ This is only a property failure if the exception was unjustified.

updateClientState :: [ChainUpdate] -> Chain TestBlock -> Chain TestBlock
updateClientState chainUpdates chain =
    case Chain.applyChainUpdates (toChainUpdates chainUpdates) chain of
      Just chain' -> chain'
      Nothing     -> error "Client chain update failed"

-- | Simulates 'ChainDB.getPastLedger'.
computePastLedger ::
     TopLevelConfig TestBlock
  -> Point TestBlock
  -> Chain TestBlock
  -> Maybe (ExtLedgerState TestBlock)
computePastLedger cfg pt chain
    | pt `elem` validPoints
    = Just $ go testInitExtLedger (Chain.toOldestFirst chain)
    | otherwise
    = Nothing
  where
    SecurityParam k = configSecurityParam cfg

    curFrag :: AnchoredFragment TestBlock
    curFrag =
          AF.anchorNewest k
        . Chain.toAnchoredFragment
        $ chain

    validPoints :: [Point TestBlock]
    validPoints =
        AF.anchorPoint curFrag : map blockPoint (AF.toOldestFirst curFrag)

    -- | Apply blocks to the ledger state until we have applied the block
    -- matching @pt@, after which we return the resulting ledger.
    --
    -- PRECONDITION: @pt@ is in the list of blocks or genesis.
    go :: ExtLedgerState TestBlock -> [TestBlock] -> ExtLedgerState TestBlock
    go !st blks
        | castPoint (getTip st) == pt
        = st
        | blk:blks' <- blks
        = go (tickThenReapply (ExtLedgerCfg cfg) blk st) blks'
        | otherwise
        = error "point not in the list of blocks"

-- | Simulates 'ChainDB.getHeaderStateHistory'.
computeHeaderStateHistory ::
     TopLevelConfig TestBlock
  -> Chain TestBlock
  -> HeaderStateHistory TestBlock
computeHeaderStateHistory cfg =
      HeaderStateHistory.trim (fromIntegral k)
    . HeaderStateHistory.fromChain cfg testInitExtLedger
  where
    SecurityParam k = configSecurityParam cfg

{-------------------------------------------------------------------------------
  ChainSyncClientSetup
-------------------------------------------------------------------------------}

slotLength :: SlotLength
slotLength = slotLengthFromSec $ toEnum slotLengthInSeconds

slotLengthInSeconds :: Int
slotLengthInSeconds = 10

-- | The onset of the slot
toOnset :: SlotNo -> RelativeTime
toOnset slot = RelativeTime $
    multipleNominalDelay
      (getSlotLength slotLength)
      (unSlotNo slot)

-- | Tenths of a slot length
--
-- This adds some fractionality to the test without over-complicating it.
newtype SlotLengthTenths = SlotLengthTenths Int
  deriving (Show)

slotLengthTenthsToClockSkew :: SlotLengthTenths -> ClockSkew
slotLengthTenthsToClockSkew (SlotLengthTenths tenths) =
    clockSkewInSeconds $ (toEnum slotLengthInSeconds * toEnum tenths) / 10

-- | Bundle dependent arguments for test generation
data ChainSyncClientSetup = ChainSyncClientSetup
  { securityParam :: SecurityParam
  , clientUpdates :: ClientUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , serverUpdates :: ServerUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , startTick     :: Tick
    -- ^ Depends on 'clientUpdates' and 'serverUpdates'
  , invalidBlocks :: InvalidBlocks
    -- ^ Blocks that are discovered to be invalid.
  , clientSlowBy  :: SlotLengthTenths
    -- ^ The server's clock minus the client's clock.
    --
    -- This is also passed to the code-under-test as the tolerable clock skew.
  }
  deriving (Show)

instance Arbitrary ChainSyncClientSetup where
  arbitrary = do
    securityParam  <- SecurityParam <$> choose (2, 5)
    clientUpdates0 <- ClientUpdates <$>
      genUpdateSchedule SelectedChainBehavior securityParam
    serverUpdates  <- ServerUpdates <$>
      genUpdateSchedule TentativeChainBehavior securityParam
    let clientUpdates = removeLateClientUpdates serverUpdates clientUpdates0
        maxStartTick  = maximum
          [ Tick 1
          , lastTick (getClientUpdates clientUpdates) - 1
          , lastTick (getServerUpdates serverUpdates) - 1
          ]
    startTick <- choose (1, maxStartTick)
    let trapBlocks =
          [ blockHash b
          | AddBlock b <- joinSchedule (getServerUpdates serverUpdates)
          , tbValid b == Invalid
          ]
    invalidBlocks <- InvalidBlocks <$> (genSchedule =<< shuffle trapBlocks)

    clientSlowBy <- SlotLengthTenths <$> choose (0, 50)

    return ChainSyncClientSetup {
        securityParam
      , clientUpdates
      , serverUpdates
      , startTick
      , invalidBlocks
      , clientSlowBy
      }
  shrink cscs@ChainSyncClientSetup {
      clientUpdates
    , serverUpdates
    , startTick
    , clientSlowBy
    } =
    -- We don't shrink 'securityParam' because the updates depend on it

    -- We also don't shrink 'invalidBlocks' right now (as it does not impact
    -- correctness), but it might be confusing to see blocks in it that are not
    -- part of the update schedules.
    [ cscs
      { serverUpdates = ServerUpdates serverUpdates'
      , clientUpdates = removeLateClientUpdates
                          (ServerUpdates serverUpdates')
                          clientUpdates
      , startTick     = startTick'
      }
    | serverUpdates' <- shrinkSchedule (getServerUpdates serverUpdates)
    , let maxStartTick = maximum
            [ 1
            , lastTick (getClientUpdates clientUpdates) - 1
            , lastTick serverUpdates' - 1
            ]
    , startTick' <- [1..min startTick maxStartTick]
    ] <>
    [ cscs
      { clientUpdates = clientUpdates'
      , startTick     = startTick'
      }
    | clientUpdates' <-
        removeLateClientUpdates serverUpdates . ClientUpdates <$>
        shrinkSchedule (getClientUpdates clientUpdates)
    , let maxStartTick = maximum
            [ 1
            , lastTick (getClientUpdates clientUpdates') - 1
            , lastTick (getServerUpdates serverUpdates)  - 1
            ]
    , startTick' <- [1..min startTick maxStartTick]
    ] <>
    [ cscs { clientSlowBy = SlotLengthTenths y }
    | let SlotLengthTenths x = clientSlowBy
    , y <- shrink x
    ]

prettyChainSyncClientSetup :: ChainSyncClientSetup -> String
prettyChainSyncClientSetup testSetup =
    unlines
      [ "ChainSyncClientSetup:"
      , "securityParam: " <> show (maxRollbacks securityParam)
      , "clientSlowBy: " <> show (unClockSkew skew)
      , "--"
      , "clockUpdates:"
      , condense (mkClockUpdates clientUpdates serverUpdates) <> "--"
      , "clientUpdates:"
      , condense (getClientUpdates clientUpdates) <> "--"
      , "serverUpdates:"
      , condense (getServerUpdates serverUpdates) <> "--"
      , "startTick: " <> show startTick
      , "invalidBlocks: "
      , condense (getInvalidBlocks invalidBlocks)
      ]
  where
    -- if you add a field to this pattern to avoid warnings, add it below too
    ChainSyncClientSetup _ _ _ _ _ _dummy = testSetup
    ChainSyncClientSetup {
        securityParam
      , clientSlowBy
      , clientUpdates
      , serverUpdates
      , startTick
      , invalidBlocks
      } = testSetup

    skew = slotLengthTenthsToClockSkew clientSlowBy

-- | Remove client updates that happen at a tick after the tick in which the
-- last server updates happened.
--
-- If we don't do this, the client's chain might no longer intersect with the
-- synced candidate. This is because the ChainSync protocol won't have had a
-- chance to update the candidate fragment, as the code to handle this case
-- (our chain has changed such that it no longer intersects with the synced
-- candidate -> initiate the \"find a new intersection\" part of the protocol)
-- is run when we receive new messages (roll forward/backward) from the
-- server.
removeLateClientUpdates :: ServerUpdates -> ClientUpdates -> ClientUpdates
removeLateClientUpdates (ServerUpdates (Schedule sus))
    | Just ((lastServerUpdateTickNo, _), _) <- Map.maxViewWithKey sus
    = \(ClientUpdates (Schedule cus)) ->
       let (cus', _) = Map.split (succ lastServerUpdateTickNo) cus
           -- @cus'@ contains the entries with a key < @succ
           -- lastServerUpdateTickNo@
       in ClientUpdates (Schedule cus')
    | otherwise
    = id

{-------------------------------------------------------------------------------
  Generating a schedule of updates
-------------------------------------------------------------------------------}

genUpdateSchedule
  :: UpdateBehavior
  -> SecurityParam
  -> Gen (Schedule ChainUpdate)
genUpdateSchedule updateBehavior securityParam =
    genChainUpdates updateBehavior securityParam 10 >>= genSchedule

data NewMaxSlot =
    NewMaxClientSlot          SlotNo
    -- ^ the client's chain reaches a new greatest slot
  | NewMaxServerSlot                 SlotNo
    -- ^ the server's chain reaches a new greatest slot
  | NewMaxClientAndServerSlot SlotNo SlotNo
    -- ^ both the client and the server's chain reach a new greatest slot,
    -- respectively
  deriving (Show)

instance Condense NewMaxSlot where
  condense = \case
    NewMaxClientSlot slot -> "c" <> condense slot <> "|s_"
    NewMaxServerSlot slot -> "c_|s" <> condense slot

    NewMaxClientAndServerSlot cslot sslot ->
      "c" <> condense cslot <> "|s" <> condense sslot

-- | The schedule of when the the client and server chains reach a new greatest
-- slot, respectively.
--
-- The resulting schedule has exactly one entry per tick in the map (ie no
-- simultaneity). Moreover, it's monotonic within the client and within the
-- server, but not necessarily in their union.
--
-- We need to track them separately because the client selecting a block in a
-- slot implies the local clock has reached surpassed that onset, whereas the
-- server doing so does not.
mkClockUpdates :: ClientUpdates -> ServerUpdates -> Schedule NewMaxSlot
mkClockUpdates = \(ClientUpdates cupds) (ServerUpdates supds) ->
      Schedule
    $ Map.map ((:[]))
    $ Map.merge
        (Map.mapMissing $ \_ -> NewMaxClientSlot)
        (Map.mapMissing $ \_ -> NewMaxServerSlot)
        (Map.zipWithMatched $ \_ -> NewMaxClientAndServerSlot)
        (newMaxes cupds)
        (newMaxes supds)
  where
    newMaxes :: Schedule ChainUpdate -> Map.Map Tick SlotNo
    newMaxes =
        makeMonotonic
      . Map.mapMaybe (fmap getMax . foldMap maxSlot)
      . getSchedule

    maxSlot :: ChainUpdate -> Maybe (Max SlotNo)
    maxSlot = foldMap (Just . Max . blockSlot) . \case
      AddBlock b      -> [b]
      SwitchFork _ bs -> bs

    makeMonotonic :: (Eq k, Ord v) => Map.Map k v -> Map.Map k v
    makeMonotonic mp = Map.fromAscList $ case Map.toAscList mp of
        []           -> []
        (k0, x) : xs -> (k0, x) : go x xs
    go acc = \case
        []          -> []
        (k, x) : xs -> if x > acc then (k, x) : go x xs else go acc xs

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

ppBlock :: TestBlock -> String
ppBlock = condense

ppPoint :: StandardHash blk => Point blk -> String
ppPoint GenesisPoint              = "Origin"
ppPoint (BlockPoint (SlotNo s) h) = "(S:" <> show s <> "; H:" <> show h <> ")"

ppChain :: Chain TestBlock -> String
ppChain = ppBlocks GenesisPoint . Chain.toOldestFirst

ppFragment :: AnchoredFragment TestBlock -> String
ppFragment f = ppBlocks (AF.anchorPoint f) (AF.toOldestFirst f)

ppBlocks :: Point TestBlock -> [TestBlock] -> String
ppBlocks a bs = ppPoint a <> " ] " <> intercalate " :> " (map ppBlock bs)

ppTraceEvent :: TraceEvent -> String
ppTraceEvent (Tick n, RelativeTime t, ev) = show (n, t) <> " | " <> case ev of
    Left  cl -> "Client: "   <> show cl
    Right pt -> "Protocol: " <> show pt

{-------------------------------------------------------------------------------
  Classifying examples
-------------------------------------------------------------------------------}

data TickArrivalTimeStats a = OnlyNotEarly_SomeEarly {
    onlyNotEarlyTATS :: !a
    -- ^ Logical ticks in which some headers are arriving but none are from the
    -- future
  , someEarlyTATS    :: !a
    -- ^ Logical ticks in which some headers are arriving from the future
  }
  deriving (Functor, Generic)
  deriving (Show) via (Quiet (TickArrivalTimeStats a))
  deriving (Monoid, Semigroup) via
    (InstantiatedAt Generic (TickArrivalTimeStats a))

data ZOM = Zero | One | Many
  deriving (Show)

sizeZOM :: Set.Set a -> ZOM
sizeZOM x = case Set.size x of
    0 -> Zero
    1 -> One
    _ -> Many   -- NB negatives are impossible

tickArrivalTimeStats :: [TraceEvent] -> TickArrivalTimeStats ZOM
tickArrivalTimeStats events =
    fmap sizeZOM $
    OnlyNotEarly_SomeEarly {
        onlyNotEarlyTATS = onlyNotEarly `Set.difference` someEarly
      , someEarlyTATS    = someEarly
      }
  where
    -- if you add a field to this pattern to avoid warnings, add it below too
    OnlyNotEarly_SomeEarly _ _dummy = tickArrivalTimes events
    OnlyNotEarly_SomeEarly {
        onlyNotEarlyTATS = onlyNotEarly
      , someEarlyTATS    = someEarly
      } = tickArrivalTimes events

-- | WARNING 'onlyNotEarlyTATS' is instead merely @someNotEarlyTATS@ in this
-- codomain: it might overlap with the 'someEarlyTATs' field
tickArrivalTimes :: [TraceEvent] -> TickArrivalTimeStats (Set.Set Tick)
tickArrivalTimes = foldMap $ \case
    (n, now, Left (TraceDownloadedHeader hdr)) ->
      let onset    = toOnset (blockSlot hdr)
          thisTick = Set.singleton n
      in
      if now < onset
      then OnlyNotEarly_SomeEarly {
          onlyNotEarlyTATS = Set.empty
        , someEarlyTATS    = thisTick
        }
      else OnlyNotEarly_SomeEarly {
          onlyNotEarlyTATS = thisTick
        , someEarlyTATS    = Set.empty
        }
    _ -> mempty
