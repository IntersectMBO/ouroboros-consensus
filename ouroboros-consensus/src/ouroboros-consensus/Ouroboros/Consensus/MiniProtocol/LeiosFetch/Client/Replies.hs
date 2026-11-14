{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the node's LeiosFetch clients do: send the requests the decision
-- logic assigned to their peer, and take in the replies.
--
-- Taking in a reply is where the node acquires an endorser block or its
-- transactions, so it is also where it emits each acquisition, once the write
-- is durable: 'AcquiredEb' when the body is, and 'AcquiredEbTxs' when no job of
-- its closure remains. An endorser block the node forged is acquired here too,
-- as one that arrived with no peer ('acquireForgedEb').
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( LeiosFetchClientContext (..)
  , ReplyOwnership (..)
  , requestReplyOwnership
  , emitContextObservations
  , ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , awaitTerminate
  , nextLeiosFetchJob
  , processLeiosBlock
  , processLeiosBlockTxs
  , acquireCachedBody
  , queueCachedBody
  , withBodyWorkers
  , acquireForgedEb
  , submitForgedEb
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Control.Monad (foldM, forM_, unless, void, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (fold, toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..))
import Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCache (..)
  , PackedEndorserBlock (..)
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , ForgedEndorserBlock (..)
  , LeiosEbByteSizes (..)
  , SizeRole (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , Serialised (..)
  , TxHash
  , TxLocation
  , TxOffset
  , admitsSizes
  , deserialiseEndorserBlock
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , leiosEbByteSizes
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchJobActions (..)
  , SomeLeiosFetchJob (..)
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Coverage as Coverage
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( AlreadyEmitted (..)
  , EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , abandonClosure
  , acquireEbBody
  , ebStateOnset
  , isFocusedPoint
  , newLeiosJobPool
  , settleReceivingFromForge
  , settleTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse
  ( LocalReuse
  , PreparedTxs (..)
  , prepareBodyTxs
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork as Owned
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , FetchPhase (..)
  , FetchRoutine (..)
  , LeiosBlockTxSources (..)
  , TraceLeiosFetchClientEvent (..)
  , TraceLeiosFetchDetailEvent (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace.Policy as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..), offsetsToBitmap)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( BodyWriteReceipt (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( Exception
  , IOLike
  , STM
  , StrictMVar
  , Time
  , atomically
  , labelThisThread
  , catch
  , check
  , finally
  , getMonotonicTime
  , labelThisThread
  , mask_
  , modifyMVar
  , myThreadId
  , readMVar
  , readTVar
  , retry
  , throwIO
  , throwTo
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)

-- | Attempt-scoped callbacks containing no payload. TX submissions transfer
-- to receipt tasks; complete bodies atomically transfer to the bounded node
-- pool before preparation. Original request identity remains available for
-- audit after its peer handler and physical credit have finished.
data ReplyOwnership m = ReplyOwnership
  { claimUsefulReply :: STM m Bool
  -- ^ Exclusively own this TX job's submission, unless another delivery owns it.
  , claimBodyReply :: STM m (Maybe Natural)
  -- ^ Convert this exact network-body claim to a local token. Compose with
  -- pool admission so neither ownership nor a slot can escape alone.
  , acceptedReply :: Time -> STM m (m ())
  -- ^ Observe actual writer acceptance and idempotently release any remaining
  -- physical credit. Emit the detached observation outside locks.
  , bodyWon :: BytesSize -> m ()
  -- ^ The owned body worker reports its useful write under the original
  -- request identity. Its byte count is unrelated to ordinary TX acceptance.
  , settledReceipt :: Time -> STM m (m ())
  -- ^ Settle this network receipt's claim and return its audit event. Bodies
  -- separately release their exact local token when their worker finishes.
  , abandonUnsubmitted :: STM m ()
  -- ^ Clear only this attempt's unaccepted network claims. Transferred local
  -- body claims remain the node worker's responsibility, even before its write.
  }

requestReplyOwnership ::
  (IOLike m, Ord peer) =>
  LeiosFetchClientContext m peer -> L.AttemptKey peer -> LeiosFetchRequest -> ReplyOwnership m
requestReplyOwnership ctx key req =
  ReplyOwnership
    { claimUsefulReply = R.claimUseful rt key
    , claimBodyReply = do
        useful <- R.claimUseful rt key
        if useful then R.transferBodyClaim rt key else pure Nothing
    , acceptedReply = \at -> do
        before <- readTVar (R.runtimeFacts rt)
        let released = Map.member key (L.leases (R.ledger before))
        R.acceptReply rt key (R.workOf req)
        accepted <- A.transition rt key req FetchWriterAccepted at
        refunded <- A.transition rt key req FetchResourceSettled at
        let !events = A.forceObservations ([accepted] ++ [refunded | released])
        pure (emitContextObservations ctx events)
    , bodyWon = \actual ->
        let !events = A.forceObservations [A.Routine key (FetchBodyWon (A.requestSummaryOf key req) actual)]
         in emitContextObservations ctx events
    , settledReceipt = \at -> do
        R.settleReceipt rt key (R.workOf req)
        event <- A.transition rt key req FetchReceiptSettled at
        pure (emitContextObservations ctx [event])
    , abandonUnsubmitted = R.clearUnsubmitted rt key (R.workOf req)
    }
 where
  rt = lfcRuntime ctx

-- | STM returns already-detached observations; emission follows the owning lock.
emitContextObservations :: Monad m => LeiosFetchClientContext m peer -> [A.Observation peer] -> m ()
emitContextObservations ctx = A.emitObservations (lfcTracer ctx) (lfcDetailTracer ctx)

finishReplyReceipt :: IOLike m => ReplyOwnership m -> m ()
finishReplyReceipt ownership = do
  at <- getMonotonicTime
  emit <- atomically (settledReceipt ownership at)
  emit

-- | What every LeiosFetch client of the node shares.
data LeiosFetchClientContext m peer = MkLeiosFetchClientContext
  { lfcStaticEnv :: !LeiosFetchStaticEnv
  , lfcOutstanding :: !(StrictMVar m (LeiosOutstanding peer))
  , lfcReady :: !(W.Readiness m)
  -- ^ Wake the planner when a state change might unblock a fetch decision.
  , lfcRuntime :: !(R.Runtime m peer)
  -- ^ Physical request, FIFO service and pending-write ownership facts.
  , lfcTxCache :: !(LeiosTxCache m () ProtVerParity PackedEndorserBlock)
  , lfcReceiptTasks :: !(Receipts.ReceiptTasks m)
  -- ^ Node-owned submissions and accepted callbacks, independent of peer lifetime.
  , lfcLocalReuse :: !(LocalReuse m)
  , lfcBodyWork :: !(Owned.OwnedWork m LeiosPoint (m ()))
  -- ^ Two shared entries, including queued, running and receipt-waiting bodies.
  , lfcBodyRetired :: !(LeiosPoint -> STM m Bool)
  -- ^ Retirement frontier used to wake capacity waits without holding the
  -- outstanding-state MVar. Logical relevance is rechecked before admission.
  -- ^ One node-wide bounded mempool pass after DB body fills.
  , lfcWriter :: !(LeiosDbWriter m)
  , lfcNotify :: !(LeiosAcquisition -> STM m ())
  -- ^ Emit an acquisition to the rest of the node.
  , lfcSystemTime :: !(SystemTime m)
  -- ^ For reporting how long after its announcement an endorser block arrived.
  , lfcDetailTracer :: !(Tracer m (TraceLeiosFetchDetailEvent peer))
  -- ^ Independently suppressible request lifecycle observations.
  , lfcTracer :: !(Tracer m (TraceLeiosFetchClientEvent peer))
  }

-- | A reply an honest peer would not send; the ensuing thread death
-- disconnects it.
data ExnLeiosFetchInvalidReply
  = -- | The endorser block does not have the requested hash.
    ExnLeiosBlockWrongHash
      !LeiosPoint
      -- | the hash of what arrived
      !EndorserBlockHash
  | -- | The endorser block has the requested hash, but not the size the peer
    -- offered it at.
    ExnLeiosBlockWrongSize
      !LeiosPoint
      -- | the offered size
      !BytesSize
      -- | its actual size
      !BytesSize
  | -- | The endorser block is larger than the ledger allows.
    ExnLeiosBlockTooBig
      !LeiosPoint
      -- | its size
      !BytesSize
      -- | the most the ledger allows
      !BytesSize
  | -- | The endorser block references more transaction bytes than the ledger
    -- allows.
    ExnLeiosClosureTooBig
      !LeiosPoint
      -- | the bytes it references
      !Word64
      -- | the most the ledger allows
      !Word64
  | -- | Not as many transactions arrived as were requested.
    ExnLeiosBlockTxsWrongCount
      !LeiosPoint
      -- | how many were requested
      !Int
      -- | how many arrived
      !Int
  | -- | The transactions that arrived for a job are not the ones its
    -- endorser block lists.
    ExnLeiosBlockTxsJobMismatch !LeiosPoint !Jobs.LeiosJobId !JobMismatch
  | -- | Transactions arrived whose sizes are not the ones their endorser block
    -- declares, at these offsets, so the LeiosDB took none of them. An honest
    -- peer offers only a closure it holds, and its LeiosDB would not have taken
    -- these either.
    ExnLeiosBlockTxsWrongSize !LeiosPoint ![TxOffset]
  | ExnLeiosBlockUndecodable !LeiosPoint !DecoderError
  deriving Show

instance Exception ExnLeiosFetchInvalidReply

-- | How a job's transactions differ from what it covers.
data JobMismatch = JobBytesSizeMismatch | JobCommitmentMismatch
  deriving Show

-----

-- | Blocks until this peer's client should stop.
awaitTerminate :: IOLike m => ControlMessageSTM m -> STM m ()
awaitTerminate control =
  control >>= \case
    Terminate -> pure ()
    Quiesce -> retry
    Continue -> retry

-- | Dequeue the next assigned request. The final dispatch check still runs
-- after any pipeline-slot wait; dequeuing does not commit the request to wire.
nextLeiosFetchJob ::
  (IOLike m, Ord peer) =>
  LeiosFetchClientContext m peer ->
  R.Client m peer ->
  STM m (SomeLeiosFetchJob LeiosPoint (Serialised EndorserBlock) LeiosTx m)
nextLeiosFetchJob ctx client =
  R.takeRequest client >>= maybe retry (pure . uncurry toJob)
 where
  rt = R.clientRuntime client
  peer = R.clientPeer client
  actions key req =
    LeiosFetchJobActions
      { beforeDispatch = mask_ $ do
          (dispatch, events) <- modifyMVar (lfcOutstanding ctx) $ \out -> do
            now <- getMonotonicTime
            let wanted = isFocusedPoint (point req) out && needed req out
            dispatch <- if wanted then atomically (R.commitRequest client key) else pure False
            if dispatch
              then do
                event <- atomically (A.transition rt key req FetchDispatchCommitted now)
                pure (out, (True, [event]))
              else do
                event <- atomically $ R.discardAttempt rt key >> A.transition rt key req FetchObsoleteSkipped now
                pure (unpick req out, (False, [event]))
          emitContextObservations ctx events
          pure dispatch
      , beforeReceive = atomically (R.installReceiver client key)
      , withReplyHandler = \action ->
          action `finally` do
            now <- getMonotonicTime
            events <- atomically $ do
              before <- readTVar (R.runtimeFacts rt)
              let released = Map.member key (L.leases (R.ledger before))
                  oldHead = A.headSummary peer before
              R.finishHandler client key (R.workOf req) now
              following <- readTVar (R.runtimeFacts rt)
              finished <- A.transition rt key req FetchHandlerFinished now
              refunded <- A.transition rt key req FetchResourceSettled now
              pure $!
                A.forceObservations
                  ([refunded | released] ++ [finished] ++ A.headChanges oldHead (A.headSummary peer following))
            emitContextObservations ctx events
      }
  toJob key req = case req of
    LeiosBlockRequest body ->
      MkSomeLeiosFetchJob
        (actions key req)
        (MsgLeiosBlockRequest (lbrPoint body))
        (\(MsgLeiosBlock eb) -> processLeiosBlock (requestReplyOwnership ctx key req) ctx peer body eb)
    LeiosBlockTxsRequest txReq@(MkLeiosBlockTxsRequest p jobs) ->
      MkSomeLeiosFetchJob
        (actions key req)
        (MsgLeiosBlockTxsRequest p (offsetsToBitmap (foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs)))
        (\(MsgLeiosBlockTxs txs) -> processLeiosBlockTxs (requestReplyOwnership ctx key req) ctx peer txReq txs)
  point = L.workPoint . R.workOf
  needed req out = case Map.lookup (point req) (ebState out) of
    Just (MkEbState _ state) -> case (req, state) of
      (LeiosBlockRequest{}, NoBody) -> True
      (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs), BodyAcquired pool) -> not (IntMap.null (Jobs.restrictToPending (NEIntMap.toMap jobs) pool))
      _ -> False
    Nothing -> False
  unpick (LeiosBlockTxsRequest req) = settleTxRequest peer req IntSet.empty
  unpick _ = id

-- | Validate a complete body, then hand useful work to the bounded node pool.
-- A peer's false declared size is enforced only after preserving its valid body.
processLeiosBlock ::
  forall peer m.
  IOLike m =>
  ReplyOwnership m ->
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockRequest ->
  Serialised EndorserBlock ->
  m ()
processLeiosBlock ownership ctx peer req bytes =
  case deserialiseEndorserBlock (Proxy @DijkstraEra) bytes of
    Left failure -> throwIO $ ExnLeiosBlockUndecodable (lbrPoint req) failure
    Right eb ->
      mask_ $
        Receipts.withReceiptSubmission
          (lfcReceiptTasks ctx)
          ( do
              when (endorserBlockHash eb /= pointEbHash point) $ do
                traceWith (lfcTracer ctx) $
                  TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid actual)
                throwIO $ ExnLeiosBlockWrongHash point (endorserBlockHash eb)
              now <- systemTimeCurrent (lfcSystemTime ctx)
              let enqueue = do
                    delivery <- modifyMVar (lfcOutstanding ctx) $ \out -> do
                      result <- case Map.lookup point (ebState out) of
                        _ | Just exn <- tooBig -> pure (BodyTooBig exn)
                        Nothing -> pure (BodyDiscarded (fetchArrivalEvicted actual))
                        Just (MkEbState _ NoBody) -> atomically $ do
                          stopped <- R.runtimeClosing (lfcRuntime ctx)
                          retired <- lfcBodyRetired ctx point
                          facts <- readTVar (R.runtimeFacts (lfcRuntime ctx))
                          if stopped || retired
                            then pure (BodyDiscarded (fetchArrivalEvicted actual))
                            else
                              if Map.member (L.Body point) (R.pending facts) || Set.member point (R.localForges facts)
                                then pure (BodyDiscarded (fetchArrivalExtra actual))
                                else do
                                  available <- Owned.canSubmitWork (lfcBodyWork ctx) point
                                  if not available
                                    then pure BodyWaiting
                                    else do
                                      token <- claimBodyReply ownership
                                      case token of
                                        Nothing -> pure (BodyDiscarded (fetchArrivalExtra actual))
                                        Just local -> do
                                          _ <-
                                            Owned.submitWork
                                              (lfcBodyWork ctx)
                                              point
                                              ( runUsefulBody ctx (DeliveredBy peer) point eb now local (Just ownership)
                                                  `finally` atomically (R.settleLocalBody (lfcRuntime ctx) point local)
                                              )
                                          pure BodyQueued
                        _ -> pure (BodyDiscarded (fetchArrivalExtra actual))
                      pure ((case result of BodyTooBig _ -> acquireEbBody point Jobs.emptyJobPool out; _ -> out), result)
                    case delivery of
                      BodyWaiting -> waitForBodyCapacity ctx point >> enqueue
                      BodyQueued -> atomically $ W.notifyReadiness (lfcReady ctx)
                      BodyTooBig exn -> do
                        traceWith (lfcTracer ctx) $
                          TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid actual)
                        throwIO exn
                      BodyDiscarded arrival -> traceWith (lfcTracer ctx) $ TraceLeiosFetchClientBlockArrived peer point arrival
              enqueue
              when (actual /= lbrOfferedSize req) $
                throwIO $
                  ExnLeiosBlockWrongSize point (lbrOfferedSize req) actual
          )
          `finally` atomically (abandonUnsubmitted ownership)
     where
      point = lbrPoint req
      sizes = leiosEbByteSizes eb
      actual = lebBodySize sizes
      MkLeiosEbByteSizes{lebBodySize = bodyLimit, lebTxsSize = closureLimit} = lbrEbBounds req
      tooBig
        | actual > bodyLimit = Just (ExnLeiosBlockTooBig point actual bodyLimit)
        | lebTxsSize sizes > closureLimit =
            Just (ExnLeiosClosureTooBig point (lebTxsSize sizes) closureLimit)
        | otherwise = Nothing

-- | No capacity wait occurs under the outstanding-state MVar. A useful owner,
-- forge, retirement or closing can end the wait even while unrelated entries
-- remain full. Admission always rechecks logical state under the MVar.
waitForBodyCapacity :: IOLike m => LeiosFetchClientContext m peer -> LeiosPoint -> m ()
waitForBodyCapacity ctx point = atomically $ do
  stopped <- R.runtimeClosing (lfcRuntime ctx)
  retired <- lfcBodyRetired ctx point
  facts <- readTVar (R.runtimeFacts (lfcRuntime ctx))
  available <- Owned.canSubmitWork (lfcBodyWork ctx) point
  check
    ( stopped
        || retired
        || available
        || Map.member (L.Body point) (R.pending facts)
        || Set.member point (R.localForges facts)
    )

data BodyDelivery
  = BodyTooBig !ExnLeiosFetchInvalidReply
  | BodyDiscarded !FetchArrivalBytes
  | BodyWaiting
  | BodyQueued

-- | All admitted body work shares the two-entry bound through durable receipt
-- reconciliation. The action stored in an entry owns its immutable input and
-- exact local token; there is no second body receipt callback retaining it.
withBodyWorkers :: IOLike m => LeiosFetchClientContext m peer -> m a -> m a
withBodyWorkers ctx = Owned.withOwnedWorkN 2 (lfcBodyWork ctx) (\_ operation -> operation >> pure (pure False))

runUsefulBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  BodySource peer ->
  LeiosPoint ->
  EndorserBlock ->
  RelativeTime ->
  Natural ->
  Maybe (ReplyOwnership m) ->
  m ()
runUsefulBody ctx source point eb now token ownership = do
  needed <- bodyStillNeeded ctx point
  if not needed
    then traceUnusedBody ctx source point eb
    else do
      PreparedTxs{preparedBytes, preparedValidated} <-
        prepareBodyTxs
          (lfcLocalReuse ctx)
          (R.runtimeClosing (lfcRuntime ctx))
          (zip [0 ..] (endorserBlockReferences eb))
      -- A point may retire while waiting for the source. Never enter the writer
      -- for an operation already known to have lost its logical relevance.
      current <- bodyStillNeeded ctx point
      if not current
        then traceUnusedBody ctx source point eb
        else do
          let supplied = IntSet.fromList (map fst preparedBytes)
              collect acc off h size loc
                | IntSet.member off supplied = acc
                | otherwise = snocLocated acc off h size loc
          -- Retain this body-bearing point after preparation and the relevance
          -- recheck. Its cache entry is not proof that the write below landed.
          -- See @docs/fetch-policy/leiosfetch-improved.md#cache-retention@.
          (summary, fills) <-
            insertBody (lfcTxCache ctx) point (packEndorserBlock eb) [] collect
          withLockedInsertAppliedTx (lfcTxCache ctx) $ \initial step ->
            foldM (\acc (h, parity) -> step acc h parity) initial preparedValidated
          written <- writeEbBody (lfcWriter ctx) point eb (Map.fromList fills) (Map.fromList preparedBytes)
          forM_ ownership $ \owner -> do
            at <- getMonotonicTime
            emit <- atomically (acceptedReply owner at)
            bodyWon owner (lebBodySize (leiosEbByteSizes eb))
            emit
          atomically $ R.acceptLocalBody (lfcRuntime ctx) point token
          traceWith (lfcTracer ctx) $
            TraceLeiosFetchClientBlockTxSources
              point
              ( MkLeiosBlockTxSources
                  (length fills)
                  (length preparedBytes)
                  (length (endorserBlockReferences eb) - length fills - length preparedBytes)
              )
              summary
          settleBody ctx source point eb now written
          forM_ ownership finishReplyReceipt

bodyStillNeeded :: IOLike m => LeiosFetchClientContext m peer -> LeiosPoint -> m Bool
bodyStillNeeded ctx point = do
  out <- readMVar (lfcOutstanding ctx)
  pure $ case Map.lookup point (ebState out) of
    Just (MkEbState _ NoBody) -> True
    _ -> False

-- A queued valid body can lose relevance before preparation starts. Its arrival
-- still needs a disposition even though no storage receipt was created.
traceUnusedBody ::
  IOLike m => LeiosFetchClientContext m peer -> BodySource peer -> LeiosPoint -> EndorserBlock -> m ()
traceUnusedBody ctx source point eb = case source of
  CachedBody -> pure ()
  DeliveredBy peer -> do
    out <- readMVar (lfcOutstanding ctx)
    let bucket = case Map.lookup point (ebState out) of
          Just (MkEbState _ BodyAcquired{}) -> fetchArrivalExtra
          Just (MkEbState _ ReceivingFromForge{}) -> fetchArrivalExtra
          _ -> fetchArrivalEvicted
    traceWith (lfcTracer ctx) $
      TraceLeiosFetchClientBlockArrived peer point (bucket (lebBodySize (leiosEbByteSizes eb)))

-- | Where a body that 'settleBody' settles came from.
data BodySource peer
  = -- | This peer delivered it.
    DeliveredBy !peer
  | -- | The LeiosTxCache held it, taken in for another point ('acquireCachedBody').
    CachedBody

-- | Record the body as acquired at its point, now that its write is durable,
-- with jobs for the txs the point does not hold yet, and emit what is acquired.
-- Nothing if the point was pruned meanwhile, or another delivery of it settled
-- first. Either way, trace a delivered body's arrival: its bytes were needed
-- only if it settled first. A body from the LeiosTxCache is traced as taken from
-- there only if it settled first.
--
-- The LeiosDB's GC can overtake a callback after its write committed. A
-- receipt confirms presence at commit, not a pin through notification
-- consumption. Fetch settlement ignores retired points, and each subscriber
-- independently filters retired acquisitions:
--
-- * The ChainDB (@acquiredEbsRunner@) ignores 'AcquiredEb', and records
--   'AcquiredEbTxs' only for a point no older than the slot it last pruned its
--   acquired set to. It prunes that set to the promoted slot right after the
--   promotion, so ChainSel sees an acquisition of a dropped point at most until
--   then. Nor could it act on one: a block certifying that point would fork
--   off below the immutable tip.
--
-- * The LeiosNotify log offers a point only while it holds a live announcement
--   at that point, and by configuration announcements expire before reaching
--   the immutable tip (@maxAnnouncementAgeSend@).
settleBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  BodySource peer ->
  LeiosPoint ->
  EndorserBlock ->
  -- | when the body arrived
  RelativeTime ->
  Promise m BodyWriteReceipt ->
  m ()
settleBody ctx source point eb now bodyWritten = do
  labelThisThread "LeiosFetch.settleBody"
  receipt <- await bodyWritten
  case receipt of
    BodyUnavailable -> pure ()
    BodyPresent confirmed -> do
      let held = toIntSet confirmed
      let heldOffs = IntSet.toAscList held
      traces <- modifyMVar lfcOutstanding $ \outstanding ->
        case Map.lookup point (ebState outstanding) of
          Nothing -> pure (outstanding, notFirst fetchArrivalEvicted)
          Just (MkEbState _onset BodyAcquired{}) -> pure (outstanding, notFirst fetchArrivalExtra)
          -- Only the forge's writes settle it.
          Just (MkEbState _onset ReceivingFromForge{}) -> pure (outstanding, notFirst fetchArrivalExtra)
          Just (MkEbState onset NoBody) -> do
            let
              jobPool =
                newLeiosJobPool lfcStaticEnv (refs `IntMap.withoutKeys` IntSet.fromList heldOffs)
            atomically $ do
              lfcNotify $ AcquiredEb point (encodeEndorserBlockSize eb)
              when (Jobs.nullJobPool jobPool) $
                lfcNotify $
                  AcquiredEbTxs point
            let initialized =
                  withInitialCoverage
                    point
                    (lebTxsSize (leiosEbByteSizes eb))
                    (sum [fromIntegral (snd (refs IntMap.! offset)) | offset <- heldOffs])
                    (acquireEbBody point jobPool outstanding)
                (updated, summary) =
                  if Jobs.nullJobPool jobPool
                    then finishPointCoverage point False initialized
                    else (initialized, Nothing)
            pure
              ( updated
              , first
                  : TraceLeiosFetchClientBlockAcquired point (ageAt now onset)
                  : [ TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)
                    | Jobs.nullJobPool jobPool
                    ]
                  ++ [TraceLeiosFetchClientClosureSummary point value | Just value <- [summary]]
              )
      -- These bytes are durable now: advertise them as fill sources for later
      -- endorser blocks referencing the same txs.
      setTxLocations lfcTxCache point [(off, fst (refs IntMap.! off)) | off <- heldOffs]
      atomically $ W.notifyReadiness lfcReady
      mapM_ (traceWith lfcTracer) traces
 where
  MkLeiosFetchClientContext
    { lfcStaticEnv
    , lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcNotify
    , lfcTracer
    } = ctx
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  arrived from bucket =
    TraceLeiosFetchClientBlockArrived from point (bucket (lebBodySize (leiosEbByteSizes eb)))
  first = case source of
    DeliveredBy from -> arrived from fetchArrivalGood
    CachedBody -> TraceLeiosFetchClientBlockFromCache point
  notFirst bucket = case source of
    DeliveredBy from -> [arrived from bucket]
    CachedBody -> []

-- | Acquire the endorser block at this point from the body the LeiosTxCache
-- holds for it, having taken it in for another point, instead of fetching it
-- again: decode the bytes it was received in, write the body, filling its txs from where they are
-- durably held, and settle them as an arrival would.
-- Nothing if the cache holds no body for it, as after a restart, or if these
-- bounds do not admit it; the point is then fetched like any other.
-- | Optional cache discovery uses the same two-entry pool. A queued lookup is
-- not a useful-body claim; only its validated result can install a local owner.
acquireCachedBody ::
  IOLike m => LeiosFetchClientContext m peer -> LeiosPoint -> (LeiosEbByteSizes SizeLimit) -> m ()
acquireCachedBody ctx point bounds = atomically (queueCachedBody ctx point bounds)

queueCachedBody ::
  IOLike m => LeiosFetchClientContext m peer -> LeiosPoint -> (LeiosEbByteSizes SizeLimit) -> STM m ()
queueCachedBody ctx point bounds = Owned.trySubmitWork (lfcBodyWork ctx) point $ do
  found <- lookupBody (lfcTxCache ctx) (pointEbHash point)
  forM_ found $ \packed -> case deserialiseEndorserBlock (Proxy @DijkstraEra) (Serialised (BSL.fromStrict (SBS.fromShort (pebBytes packed)))) of
    Right eb
      | endorserBlockHash eb == pointEbHash point
      , admitsSizes bounds (leiosEbByteSizes eb) -> do
          claim <- modifyMVar (lfcOutstanding ctx) $ \out -> do
            token <- case Map.lookup point (ebState out) of
              Just (MkEbState _ NoBody) -> atomically (R.claimLocalBody (lfcRuntime ctx) point)
              _ -> pure Nothing
            pure (out, token)
          forM_ claim $ \token ->
            ( do
                now <- systemTimeCurrent (lfcSystemTime ctx)
                runUsefulBody ctx CachedBody point eb now token Nothing
            )
              `finally` atomically (R.settleLocalBody (lfcRuntime ctx) point token)
    _ -> pure ()

-- | Acquire, at this point, an endorser block the node's own forge produced, as
-- one that arrived with no peer: take its body into the LeiosTxCache, write it
-- with all its txs, take those into the LeiosTxCache as validated, and
-- settle the point once the combined write is durable ('settleForgedEb').
--
-- Its bytes are not checked: the forge built the body from these very txs. No
-- tx is filled from another point, since all of them are written.
--
-- So 'AcquiredEb', and with it the node's offer of the body, waits for the txs
-- write in the same transaction as the body.
acquireForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  ForgedEndorserBlock ->
  m ()
acquireForgedEb ctx point forged = void $ submitForgedEb ctx point forged

-- | Submit under the receipt group and return a joinable positive-completion
-- observation. Writer admission returns to the forge caller before durability;
-- the owned operation remains coalescible until its receipt has settled.
-- Nothing means closing rejected the submission, not a promise to wait for.
submitForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  ForgedEndorserBlock ->
  m (Maybe (STM m Bool))
submitForgedEb ctx point forged = Receipts.withReceiptSubmissionResult (lfcReceiptTasks ctx) $ do
  completed <- S.newEmptyTMVarIO
  now <- systemTimeCurrent lfcSystemTime
  _ <-
    insertBody lfcTxCache point (packEndorserBlock eb) () (\w _off _txh _sz _loc -> w)
  written <- writeEbBody lfcWriter point eb Map.empty (Map.fromList (zip [0 ..] (toList (forgedEbTxs forged))))
  Receipts.forkReceipt
    (lfcReceiptTasks ctx)
    ( do
        confirmed <- settleForgedEb ctx point eb now written
        atomically $ S.putTMVar completed confirmed
    )
  withLockedInsertAppliedTx lfcTxCache $ \w0 step ->
    foldM (\w (txHash, _size) -> step w txHash (forgedEbTxsParity forged)) w0 refs
  pure (S.readTMVar completed)
 where
  MkLeiosFetchClientContext{lfcTxCache, lfcWriter, lfcSystemTime} = ctx
  eb = forgedEbBody forged
  refs = endorserBlockReferences eb

-- | Record the forge's endorser block as acquired at this point, with no job
-- left, now that its write is durable, and emit what of its acquisition the
-- node had not already emitted there ('settleReceivingFromForge').
settleForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  EndorserBlock ->
  -- | when the forge handed it over
  RelativeTime ->
  -- | the offsets of the txs the point holds once the write is durable
  Promise m BodyWriteReceipt ->
  m Bool
settleForgedEb ctx point eb now written = do
  labelThisThread "LeiosFetch.settleForgedEb"
  receipt <- await written
  case receipt of
    BodyUnavailable -> pure False
    BodyPresent confirmed -> do
      let heldOffs = Set.toAscList confirmed
      let complete = IntMap.keysSet refs `IntSet.isSubsetOf` IntSet.fromList heldOffs
      (current, acquired) <- modifyMVar lfcOutstanding $ \outstanding -> do
        -- Submission alone does not prove complete target possession. The forge
        -- supplies every exact TX in one transaction; an incomplete confirmation
        -- must not establish acquisition. Retirement still fences a receipt that
        -- is reconciled after its point was pruned.
        let (outstanding', toEmit) =
              if complete then settleReceivingFromForge point outstanding else (outstanding, Nothing)
            age = ageAt now $ maybe SNothing ebStateOnset $ Map.lookup point (ebState outstanding)
            (notifications, traces) = case toEmit of
              Just EmittedNothing ->
                ( [AcquiredEb point (encodeEndorserBlockSize eb), AcquiredEbTxs point]
                , [TraceLeiosFetchClientBlockAcquired point age, TraceLeiosFetchClientBlockTxsAcquired point age]
                )
              Just EmittedBody -> ([AcquiredEbTxs point], [TraceLeiosFetchClientBlockTxsAcquired point age])
              Just EmittedBodyAndClosure -> ([], [])
              Nothing -> ([], [])
        let mayFinish = not (null notifications)
            initialized =
              if not mayFinish || Map.member point (closureCoverage outstanding')
                then outstanding'
                else
                  outstanding'
                    { closureCoverage =
                        Map.insert
                          point
                          (Coverage.beginForgedCoverage (lebTxsSize (leiosEbByteSizes eb)))
                          (closureCoverage outstanding')
                    }
            (updated, summary) = if mayFinish then finishPointCoverage point True initialized else (outstanding', Nothing)
        atomically $ mapM_ lfcNotify notifications
        pure
          ( updated
          ,
            ( Map.member point (ebState updated)
            , traces ++ [TraceLeiosFetchClientClosureSummary point value | Just value <- [summary]]
            )
          )
      -- These bytes are durable now: advertise them as fill sources for later
      -- endorser blocks referencing the same txs.
      setTxLocations lfcTxCache point [(off, fst (refs IntMap.! off)) | off <- heldOffs]
      atomically $ W.notifyReadiness lfcReady
      mapM_ (traceWith lfcTracer) acquired
      pure (complete && current)
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcNotify, lfcTracer} = ctx
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))

-- | Prepend a tx's location, if it has one, as a fill of its offset.
snocLocated :: [(TxOffset, TxLocation)] -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> [(TxOffset, TxLocation)]
snocLocated acc off _txh _sz = \case
  Just loc -> (off, loc) : acc
  Nothing -> acc

-- | Take in transactions that arrived in reply to this request.
processLeiosBlockTxs ::
  forall peer m.
  (Ord peer, IOLike m) =>
  ReplyOwnership m ->
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockTxsRequest ->
  V.Vector LeiosTx ->
  m ()
processLeiosBlockTxs ownership ctx peer req@(MkLeiosBlockTxsRequest point jobs) txs =
  mask_ $
    ( Receipts.withReceiptSubmission (lfcReceiptTasks ctx) $ do
        now <- systemTimeCurrent lfcSystemTime
        -- Cheap checks, without hashing, of the whole reply and of every covered
        -- job, so that an over-send is caught and punished even for a job we have
        -- since completed.
        when (V.length txs /= IntSet.size offsetsSet) $
          invalidReply $
            ExnLeiosBlockTxsWrongCount point (IntSet.size offsetsSet) (V.length txs)
        either invalidReply pure $
          NEIntMap.foldrWithKey
            (\i job acc -> checkJobSize point aligned (Jobs.MkLeiosJobId i) job >> acc)
            (Right ())
            jobs
        -- Only jobs still pending are hashed and ingested; a redundant delivery of a
        -- completed job, or a reply for an endorser block pruned meanwhile, is
        -- discarded. The read-then-complete race is benign: completion is monotonic,
        -- and writing a transaction again is harmless.
        outstanding0 <- readMVar lfcOutstanding
        let (pendingJobs, discardedAs) = case Map.lookup point (ebState outstanding0) of
              Just (MkEbState _onset (BodyAcquired jobPool)) ->
                (Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool, fetchArrivalExtra)
              -- Pruned meanwhile, and perhaps referenced again since: txs are
              -- requested only once the body is held.
              Just (MkEbState _onset NoBody) -> (IntMap.empty, fetchArrivalEvicted)
              -- Our own forge is writing them.
              Just (MkEbState _onset ReceivingFromForge{}) -> (IntMap.empty, fetchArrivalExtra)
              Nothing -> (IntMap.empty, fetchArrivalEvicted)
            discarded =
              discardedAs $
                IntMap.foldr
                  (\(Jobs.MkLeiosJob _ bytes _) acc -> bytes + acc)
                  0
                  (IntMap.difference (NEIntMap.toMap jobs) pendingJobs)
        validated <-
          either invalidReply (pure . fold) $
            IntMap.traverseWithKey
              (\i job -> checkJobCommitment point aligned (Jobs.MkLeiosJobId i) job)
              pendingJobs
        claimed <- if null validated then pure False else atomically (claimUsefulReply ownership)
        let toIngest = if claimed then validated else []
            pendingExtra =
              if claimed
                then mempty
                else fetchArrivalExtra (sum [fromIntegral (BS.length bytes) | (_, _, bytes) <- validated])
        let !metadata =
              IntMap.fromList
                [(off, VerifiedTx txHash (fromIntegral (BS.length bytes))) | (off, txHash, bytes) <- toIngest]
        owner <- myThreadId
        txsWritten <- writeTxs lfcWriter point (Map.fromList [(off, bytes) | (off, _txHash, bytes) <- toIngest])
        acceptedAt <- getMonotonicTime
        emitAccepted <- atomically (acceptedReply ownership acceptedAt)
        -- The writer accepted this immutable job. Its node-owned callback survives
        -- subsequent cache/tracer waits and peer death. Protocol faults retain the
        -- original connection owner; storage failures go to node supervision.
        Receipts.forkReceipt (lfcReceiptTasks ctx) $
          ( settleTxs ctx peer req now metadata txsWritten
              `catch` \(fault :: ExnLeiosFetchInvalidReply) -> throwTo owner fault
          )
            `finally` finishReplyReceipt ownership
        emitAccepted
        arrival <-
          withLockedInsertUnappliedTx lfcTxCache $ \w0 step ->
            foldM (\w (VerifiedTx txHash bytes) -> step w txHash bytes ()) w0 (IntMap.elems metadata)
        traceWith lfcTracer $
          TraceLeiosFetchClientTxsArrived peer point (arrival <> discarded <> pendingExtra)
        atomically $ W.notifyReadiness lfcReady
    )
      `finally` atomically (abandonUnsubmitted ownership)
 where
  -- Logical jobs retire only in the accepted receipt callback above, after
  -- positive confirmation. Physical legacy byte refund remains on arrival.

  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcSystemTime
    , lfcTracer
    } = ctx

  txBytess = V.map leiosTxBytes txs
  batchBytes = V.sum (V.map BS.length txBytess)
  -- The union of the covered jobs' offsets, ascending: the order the peer
  -- decoded our bitmap into, so it aligns position-wise with the arriving txs.
  offsetsSet = foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs
  aligned = IntMap.fromList $ zip (IntSet.toAscList offsetsSet) (V.toList txs)

  invalidReply :: ExnLeiosFetchInvalidReply -> m a
  invalidReply exn = do
    traceWith lfcTracer $
      TraceLeiosFetchClientTxsArrived peer point (fetchArrivalInvalid (fromIntegral batchBytes))
    throwIO exn

-- | Retire the jobs of the request, now that the write of their txs is durable,
-- record in the LeiosTxCache where those txs are durably held, and emit
-- 'AcquiredEbTxs' if no job of the point remains.
--
-- Only fully confirmed jobs complete. Unconfirmed relevant work stays fetchable.
-- Per-transaction sizes are validated by 'checkJobCommitment' before writing.
-- The receipt's wrong-size diagnostic is retained defensively; it must never
-- turn rejected storage into a successful acquisition.
--
-- See 'settleBody' on the LeiosDB's GC overtaking the fetch state's pruning.
settleTxs ::
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockTxsRequest ->
  -- | when the txs arrived
  RelativeTime ->
  -- | the offsets and hashes of the txs written
  IntMap.IntMap VerifiedTx ->
  -- | explicit confirmation after the transaction write commits
  Promise m TxWriteReceipt ->
  m ()
settleTxs ctx peer req@(MkLeiosBlockTxsRequest point _jobs) now written txsWritten = do
  labelThisThread "LeiosFetch.settleTxs"
  receipt <- await txsWritten
  let wrongSize = toIntSet (txWriteWrongSize receipt)
  completed <- modifyMVar lfcOutstanding $ \outstanding -> do
    let confirmedSizes =
          IntMap.map (\(VerifiedTx _ bytes) -> bytes) (IntMap.restrictKeys written (toIntSet (txWriteConfirmed receipt)))
        observed = case Map.lookup point (ebState outstanding) of
          Just (MkEbState _ BodyAcquired{}) ->
            outstanding
              { closureCoverage =
                  Map.update (Coverage.confirmNetwork confirmedSizes) point (closureCoverage outstanding)
              }
          _ -> outstanding
        outstanding' = settleTxRequest peer req (toIntSet (txWriteConfirmed receipt)) observed
    case (Map.lookup point (ebState outstanding), Map.lookup point (ebState outstanding')) of
      _
        | not (IntSet.null wrongSize) ->
            pure (abandonClosure point outstanding', Nothing)
      ( Just (MkEbState _ (BodyAcquired before))
        , Just (MkEbState onset (BodyAcquired after))
        )
          | not (Jobs.nullJobPool before)
          , Jobs.nullJobPool after -> do
              atomically $ lfcNotify $ AcquiredEbTxs point
              let (updated, summary) = finishPointCoverage point False outstanding'
              pure (updated, Just (onset, summary))
      _ -> pure (outstanding', Nothing)
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs.
  setTxLocations
    lfcTxCache
    point
    [ (off, txHash)
    | (off, VerifiedTx txHash _) <- IntMap.toList written
    , IntSet.member off (toIntSet (txWriteConfirmed receipt))
    ]
  atomically $ W.notifyReadiness lfcReady
  forM_ completed $ \(onset, summary) -> do
    traceWith lfcTracer $ TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)
    forM_ summary $ \value -> traceWith lfcTracer (TraceLeiosFetchClientClosureSummary point value)
  -- The receipt owner forwards this protocol fault to the original client,
  -- if it still runs; storage failures instead reach node supervision.
  unless (IntSet.null wrongSize) $
    throwIO $
      ExnLeiosBlockTxsWrongSize point (IntSet.toList wrongSize)
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcNotify
    , lfcTracer
    } = ctx

-- | How long after this onset @now@ is, if there is an onset.
ageAt :: RelativeTime -> StrictMaybe RelativeTime -> Maybe NominalDiffTime
ageAt now = \case
  SJust onset -> Just (diffRelTime now onset)
  SNothing -> Nothing

-- | Cheap validation of one covered job against the commitment the request
-- carries for it: its arriving txs have its total byte size.
--
-- Assumes the reply has as many txs as its request has offsets (see
-- 'ExnLeiosBlockTxsWrongCount'), so every offset of the job has a tx.
--
-- Does /no/ hashing so that redundant\/"hedge" requests don't contend for
-- CPU. A peer that over-sends to create extra work is punished even if we
-- already did the (right amount of) CPU work for a peer that replied earlier,
-- without pointlessly repeating that CPU work.
checkJobSize ::
  LeiosPoint ->
  IntMap.IntMap LeiosTx ->
  Jobs.LeiosJobId ->
  Jobs.LeiosJob ->
  Either ExnLeiosFetchInvalidReply ()
checkJobSize point aligned jid (Jobs.MkLeiosJob offs expectedBytes _commitment)
  | fromIntegral (sum [BS.length (leiosTxBytes tx) | tx <- IntMap.elems sub]) /= expectedBytes =
      Left $ ExnLeiosBlockTxsJobMismatch point jid JobBytesSizeMismatch
  | otherwise = Right ()
 where
  -- just the txs from /this/ job
  sub = IntMap.restrictKeys aligned offs

-- | Content validation of one /pending/ job we intend to ingest: hash its
-- arriving txs and check their hashes against the job's commitment.
-- Gives those txs, with their hashes.
--
-- Only runs for the first reply for a job, in addition to 'checkJobSize'.
checkJobCommitment ::
  LeiosPoint ->
  IntMap.IntMap LeiosTx ->
  Jobs.LeiosJobId ->
  Jobs.LeiosJob ->
  Either ExnLeiosFetchInvalidReply [(Int, TxHash, BS.ByteString)]
checkJobCommitment point aligned jid (Jobs.MkLeiosJob offs _expectedBytes commitment)
  | Jobs.jobCommitmentOfTxs [(h, fromIntegral (BS.length bytes)) | (_, h, bytes) <- hashed] /= commitment =
      Left $ ExnLeiosBlockTxsJobMismatch point jid JobCommitmentMismatch
  | otherwise = Right hashed
 where
  -- Ascending by offset: the order the commitment hashes them in, and the key
  -- the LeiosDb stores the bytes under.
  hashed =
    [ (off, hashLeiosTx tx, leiosTxBytes tx)
    | (off, tx) <- IntMap.toAscList (IntMap.restrictKeys aligned offs)
    ]

-- | Strict, payload-free callback metadata. Retaining a lazy projection from
-- the received vector here would keep its bytes alive through the DB receipt.
data VerifiedTx = VerifiedTx !TxHash !BytesSize

withInitialCoverage ::
  LeiosPoint -> Word64 -> Word64 -> LeiosOutstanding peer -> LeiosOutstanding peer
withInitialCoverage point total local out =
  out
    { closureCoverage = case Coverage.beginCoverage total local of
        Nothing -> Map.delete point (closureCoverage out)
        Just value -> Map.insert point value (closureCoverage out)
    }

finishPointCoverage ::
  LeiosPoint ->
  Bool ->
  LeiosOutstanding peer ->
  (LeiosOutstanding peer, Maybe Coverage.ClosureSummary)
finishPointCoverage point forged out = case Map.lookup point (closureCoverage out) >>= Coverage.finishCoverage forged of
  Nothing -> (out, Nothing)
  Just (summary, updated) -> (out{closureCoverage = Map.insert point updated (closureCoverage out)}, Just summary)

toIntSet :: Set.Set TxOffset -> IntSet.IntSet
toIntSet = IntSet.fromList . Set.toAscList
