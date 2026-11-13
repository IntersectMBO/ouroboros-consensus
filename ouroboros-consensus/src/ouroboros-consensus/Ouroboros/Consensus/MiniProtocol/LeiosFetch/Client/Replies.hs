{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  , acquireForgedEb
  , submitForgedEb
  ) where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (forM_, unless, void, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold, foldl', toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (VersionWord16)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..))
import Ouroboros.Consensus.Leios.TxCache.API
  ( LandedTx (..)
  , LeiosTxCache (..)
  , PackedEndorserBlock
  , TxState (..)
  , insertBody
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , ForgedEndorserBlock (..)
  , LeiosEbByteSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , TxHash
  , TxLocation
  , TxOffset
  , deserialiseEndorserBlock
  , encodeEndorserBlockSize
  , endorserBlockReferences
  , hashLeiosTx
  , hashSerialisedEndorserBlock
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
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse (LocalReuse, reuseMempool)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , FetchPhase (..)
  , FetchRoutine (..)
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
  , LocalFills
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
  , catch
  , evaluate
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

-- | Attempt-scoped ownership of an admitted reply. Accepted work transfers to
-- the node's receipt tasks before any later cache/tracing work can interrupt it.
-- The callbacks contain no payload, and cleanup affects only this attempt.
data ReplyOwnership m = ReplyOwnership
  { claimUsefulReply :: STM m Bool
  -- ^ Exclusively own submission of this still-needed body/job. A competing
  -- accepted receipt can keep ownership even after its peer disappears.
  , acceptedReply :: Time -> BytesSize -> STM m (m ())
  -- ^ Record writer acceptance and release physical request credit. Return
  -- detached observation work to run only after registering the receipt's
  -- node-owned callback, outside STM and the outstanding-state lock.
  , settledReceipt :: Time -> STM m (m ())
  -- ^ Retire only this callback's pending ownership and return its observation;
  -- a late callback must not clear another attempt's claim of the same work.
  , abandonUnsubmitted :: STM m ()
  -- ^ Failure cleanup for a claim not accepted by the writer. Accepted work
  -- remains the receipt callback's responsibility.
  }

requestReplyOwnership ::
  (IOLike m, Ord peer) =>
  LeiosFetchClientContext m peer -> L.AttemptKey peer -> LeiosFetchRequest -> ReplyOwnership m
requestReplyOwnership ctx key req =
  ReplyOwnership
    { claimUsefulReply = R.claimUseful rt key
    , acceptedReply = \at actual -> do
        before <- readTVar (R.runtimeFacts rt)
        let released = Map.member key (L.leases (R.ledger before))
            useful = case Map.lookup (R.workOf req) (R.pending before) of
              Just (R.NetworkOwner owner, False) -> owner == key
              _ -> False
            won = case R.workOf req of
              L.Body{} | useful -> [A.Routine key (FetchBodyWon (A.requestSummaryOf key req) actual)]
              _ -> []
        R.acceptReply rt key (R.workOf req)
        accepted <- A.transition rt key req FetchWriterAccepted at
        refunded <- A.transition rt key req FetchResourceSettled at
        let !events = A.forceObservations (won ++ [accepted] ++ [refunded | released])
        pure (emitContextObservations ctx events)
    , settledReceipt = \at -> do
        R.settleReceipt rt key (R.workOf req)
        event <- A.transition rt key req FetchReceiptSettled at
        pure (emitContextObservations ctx [event])
    , abandonUnsubmitted = R.clearUnsubmitted rt key (R.workOf req)
    }
 where
  rt = lfcRuntime ctx

-- | Emit observations detached from their captured snapshots. Call only after
-- STM and any owning outstanding-state lock.
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
  , lfcTxCache :: !(LeiosTxCache m () VersionWord16 PackedEndorserBlock)
  -- ^ Records each validated tx with the major protocol version it was
  -- validated under. The Mempool and LeiosVoting validate a tx only under the
  -- current major version, so a recorded validation still holds exactly when
  -- its version matches the current one.
  , lfcReceiptTasks :: !(Receipts.ReceiptTasks m)
  -- ^ Node-owned submissions and accepted callbacks, independent of peer lifetime.
  , lfcLocalReuse :: !(LocalReuse m)
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
              pure ([refunded | released] ++ [finished] ++ A.headChanges oldHead (A.headSummary peer following))
            let !captured = A.forceObservations events
            emitContextObservations ctx captured
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
        ( \(MsgLeiosBlockTxs txs) -> processLeiosBlockTxs (requestReplyOwnership ctx key req) ctx peer txReq txs
        )
  point = L.workPoint . R.workOf
  needed req out = case Map.lookup (point req) (ebState out) of
    Just (MkEbState _ state) -> case (req, state) of
      (LeiosBlockRequest{}, NoBody) -> True
      (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs), BodyAcquired pool) -> not (IntMap.null (Jobs.restrictToPending (NEIntMap.toMap jobs) pool))
      _ -> False
    Nothing -> False
  unpick (LeiosBlockTxsRequest req) = settleTxRequest peer req IntSet.empty
  unpick _ = id

-- | Take in an endorser block that arrived in reply to this request.
--
-- The hash is the whole of it: these bytes either are the endorser block we
-- asked for or they are not, and no announcement gets a say. Bytes that are
-- the right block but not the size the peer offered still cost the peer its
-- connection. Bytes larger than the ledger allows, referencing more
-- transaction bytes than it allows, or that do not decode, are never written,
-- so the node never offers them onward.
--
-- The hash is checked before the bytes are decoded. They decode at Dijkstra's
-- ledger version, the only era with endorser blocks.
--
-- Database fills and a bounded local mempool pass run before network jobs are
-- created, so the node fetches only the remaining transactions.
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
  mask_ $
    ( Receipts.withReceiptSubmission (lfcReceiptTasks ctx) $ do
        now <- systemTimeCurrent lfcSystemTime
        when (hashSerialisedEndorserBlock bytes /= ebHash) $ do
          traceWith lfcTracer $
            TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid ebBytesSize)
          throwIO $ ExnLeiosBlockWrongHash point (hashSerialisedEndorserBlock bytes)
        -- Decode and check the immutable wire body before taking the shared lock.
        verdict <- evaluate checkedEb
        delivery <- modifyMVar lfcOutstanding $ \outstanding ->
          case verdict of
            Left exn ->
              pure (acquireEbBody point Jobs.emptyJobPool outstanding, BodyTooBig exn)
            Right eb -> case Map.lookup point (ebState outstanding) of
              Nothing -> pure (outstanding, BodyDiscarded (fetchArrivalEvicted ebBytesSize))
              Just (MkEbState _ BodyAcquired{}) ->
                pure (outstanding, BodyDiscarded (fetchArrivalExtra ebBytesSize))
              Just (MkEbState _ ReceivingFromForge{}) ->
                pure (outstanding, BodyDiscarded (fetchArrivalExtra ebBytesSize))
              Just (MkEbState _ NoBody) -> do
                claimed <- atomically (claimUsefulReply ownership)
                if not claimed
                  then pure (outstanding, BodyDiscarded (fetchArrivalExtra ebBytesSize))
                  else do
                    (_summary, fills) <-
                      insertBodyAndFoldl' lfcTxCache point (packEndorserBlock eb) Map.empty snocLocated
                    pure (outstanding, BodyToWrite eb fills)
        atomically $ W.notifyReadiness lfcReady
        case delivery of
          BodyTooBig exn -> do
            traceWith lfcTracer $
              TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid ebBytesSize)
            throwIO exn
          BodyDiscarded arrival -> traceWith lfcTracer $ TraceLeiosFetchClientBlockArrived peer point arrival
          BodyToWrite eb fills -> do
            written <- writeEbBody lfcWriter point eb fills Map.empty
            acceptedAt <- getMonotonicTime
            emitAccepted <- atomically (acceptedReply ownership acceptedAt ebBytesSize)
            -- Transfer acceptance before any later observation can fail.
            Receipts.forkReceipt
              (lfcReceiptTasks ctx)
              ( settleBody ctx peer point eb now written
                  `finally` finishReplyReceipt ownership
              )
            emitAccepted
        -- Accept a valid body even if its offer understated its size.
        when (ebBytesSize /= lbrOfferedSize req) $
          throwIO $
            ExnLeiosBlockWrongSize point (lbrOfferedSize req) ebBytesSize
    )
      `finally` atomically (abandonUnsubmitted ownership)
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcSystemTime
    , lfcTracer
    } = ctx
  point = lbrPoint req
  ebHash = pointEbHash point
  ebBytesSize = fromIntegral (BSL.length (unSerialised bytes))
  limits = lbrEbBounds req
  checkedEb
    | ebBytesSize > lebBodySize limits =
        Left $ ExnLeiosBlockTooBig point ebBytesSize (lebBodySize limits)
    | otherwise = case deserialiseEndorserBlock (Proxy @DijkstraEra) bytes of
        Left err -> Left $ ExnLeiosBlockUndecodable point err
        Right eb
          | lebTxsSize sizes > lebTxsSize limits ->
              Left $ ExnLeiosClosureTooBig point (lebTxsSize sizes) (lebTxsSize limits)
          | otherwise -> Right eb
         where
          sizes = leiosEbByteSizes eb

-- | What 'processLeiosBlock' does with a body, decided under the lock.
data BodyDelivery
  = -- | Beyond its point's bounds, so never written.
    BodyTooBig !ExnLeiosFetchInvalidReply
  | -- | No longer wanted, its bytes classified so.
    BodyDiscarded !FetchArrivalBytes
  | -- | Wanted, so written, with these fills, and traced once that settles.
    BodyToWrite !EndorserBlock !LocalFills

-- | Record the body as acquired at its point, now that its write is durable,
-- with jobs for the txs the point does not hold yet, and emit what is acquired.
-- Nothing if the point was pruned meanwhile, or another delivery of it settled
-- first. Either way, trace a delivered body's arrival: its bytes were needed
-- only if it settled first. Cache capacity does not invalidate a positive receipt.
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
  peer ->
  LeiosPoint ->
  EndorserBlock ->
  -- | when the body arrived
  RelativeTime ->
  Promise m BodyWriteReceipt ->
  m ()
settleBody ctx peer point eb now bodyWritten = do
  labelThisThread "LeiosFetch.settleBody"
  receipt <- await bodyWritten
  case receipt of
    BodyUnavailable -> pure ()
    BodyPresent confirmed -> do
      let heldAtCommit = toIntSet confirmed
      held <-
        reuseMempool
          (lfcLocalReuse ctx)
          stillNeeded
          point
          heldAtCommit
          (zip [0 ..] (endorserBlockReferences eb))
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
      recordConfirmedBodyTxs lfcTxCache point refs Map.empty (Set.fromList heldOffs)
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
  stillNeeded = do
    outstanding <- readMVar lfcOutstanding
    pure $ case Map.lookup point (ebState outstanding) of
      Just (MkEbState _ NoBody) -> True
      _ -> False
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  arrived bucket =
    TraceLeiosFetchClientBlockArrived peer point (bucket (lebBodySize (leiosEbByteSizes eb)))
  first = arrived fetchArrivalGood
  notFirst bucket = [arrived bucket]

-- | Acquire, at this point, an endorser block the node's own forge produced, as
-- one that arrived with no peer: take its body into the LeiosTxCache, write it
-- together with all its txs, take those into the LeiosTxCache as validated,
-- wait for the write to be durable, and then settle the point
-- ('settleForgedEb').
--
-- Its txs go to 'writeEbBody' with the body, rather than 'writeTxs', since
-- they have not passed the MsgLeiosBlockTxs commitment check. Their bytes are
-- not checked: the forge built the body from these very txs. No tx is filled from another
-- point, since all of them are written.
--
-- So 'AcquiredEb', and with it the node's offer of the body, waits for the txs
-- write as well as the body's.
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
  _ <- insertBody lfcTxCache point (packEndorserBlock eb)
  version <- evaluate (forgedEbTxsProtVer forged)
  written <-
    writeEbBody lfcWriter point eb Map.empty (Map.fromList (zip [0 ..] (toList (forgedEbTxs forged))))
  Receipts.forkReceipt
    (lfcReceiptTasks ctx)
    ( do
        confirmed <- settleForgedEb ctx point eb now version written
        atomically $ S.putTMVar completed confirmed
    )
  pure (S.readTMVar completed)
 where
  MkLeiosFetchClientContext{lfcTxCache, lfcWriter, lfcSystemTime} = ctx
  eb = forgedEbBody forged

-- | Record the forge's endorser block as acquired at this point, with no job
-- left, now that its writes are durable, and emit what of its acquisition the
-- node had not already emitted there ('settleReceivingFromForge').
settleForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  EndorserBlock ->
  -- | when the forge handed it over
  RelativeTime ->
  VersionWord16 ->
  -- | the offsets of the txs the point holds once the writes are durable
  Promise m BodyWriteReceipt ->
  m Bool
settleForgedEb ctx point eb now version written = do
  receipt <- await written
  case receipt of
    BodyUnavailable -> pure False
    BodyPresent confirmed -> do
      labelThisThread "LeiosFetch.settleForgedEb"
      let heldOffs = Set.toAscList confirmed
      let complete = IntMap.keysSet refs `IntSet.isSubsetOf` IntSet.fromList heldOffs
      (current, acquired) <- modifyMVar lfcOutstanding $ \outstanding -> do
        -- A successful submission is not proof that all target rows survived to
        -- the TX commit: GC can run between the body's and TXs' transactions. The
        -- forge supplies every exact TX, so an incomplete receipt cannot establish
        -- acquisition. Its collected point is retired by the ordinary pruner; no
        -- receipt task waits for that bookkeeping entry to disappear.
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
      recordConfirmedBodyTxs
        lfcTxCache
        point
        refs
        (Map.fromList [(h, version) | (h, _size) <- IntMap.elems refs])
        (Set.fromList heldOffs)
      atomically $ W.notifyReadiness lfcReady
      mapM_ (traceWith lfcTracer) acquired
      pure (complete && current)
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcNotify, lfcTracer} = ctx
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))

-- | Publish confirmed held offsets once, selecting validation only from captured
-- facts. Cache eviction can make this a no-op; the receipt still settles work.
-- See @docs/fetch-policy/leiosfetch-improved.md#local-materialization-and-confirmation@.
recordConfirmedBodyTxs ::
  Monad m =>
  LeiosTxCache m () v b ->
  LeiosPoint ->
  IntMap.IntMap (TxHash, BytesSize) ->
  Map TxHash v ->
  Set.Set TxOffset ->
  m ()
recordConfirmedBodyTxs cache point refs validated held = do
  let landed =
        [ MkLandedTx off h size (maybe (TxInserted ()) TxValidated (Map.lookup h validated))
        | off <- Set.toAscList held
        , let (h, size) = refs IntMap.! off
        ]
      -- Force the metadata spine and strict records before the cache takes its lock.
      !() = foldl' (\() !_ -> ()) () landed
  void $ insertLandedTxs cache point landed

snocLocated ::
  LocalFills ->
  TxOffset ->
  TxHash ->
  BytesSize ->
  Maybe TxLocation ->
  LocalFills
snocLocated acc off _txh _sz = \case
  Just loc -> Map.insert off loc acc
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
        arrival <-
          classifyTxs
            lfcTxCache
            point
            [(h, size) | VerifiedTx h size <- IntMap.elems metadata]
        owner <- myThreadId
        txsWritten <-
          writeTxs lfcWriter point (Map.fromList [(off, bytes) | (off, _txHash, bytes) <- toIngest])
        acceptedAt <- getMonotonicTime
        emitAccepted <-
          atomically
            ( acceptedReply
                ownership
                acceptedAt
                (sum [fromIntegral (BS.length bytes) | (_, _, bytes) <- toIngest])
            )
        -- The writer accepted this immutable job. Its node-owned callback survives
        -- subsequent cache/tracer waits and peer death. Protocol faults retain the
        -- original connection owner; storage failures go to node supervision.
        Receipts.forkReceipt (lfcReceiptTasks ctx) $
          ( settleTxs ctx peer req now metadata txsWritten
              `catch` \(fault :: ExnLeiosFetchInvalidReply) -> throwTo owner fault
          )
            `finally` finishReplyReceipt ownership
        emitAccepted
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
          IntMap.map
            (\(VerifiedTx _ bytes) -> bytes)
            (IntMap.restrictKeys written (toIntSet (txWriteConfirmed receipt)))
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
          | txWriteBodyPresent receipt
          , not (Jobs.nullJobPool before)
          , Jobs.nullJobPool after -> do
              atomically $ lfcNotify $ AcquiredEbTxs point
              let (updated, summary) = finishPointCoverage point False outstanding'
              pure (updated, Just (onset, summary))
      _ -> pure (outstanding', Nothing)
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs.
  let landed =
        [ MkLandedTx off h size (TxInserted ())
        | (off, VerifiedTx h size) <-
            IntMap.toAscList (IntMap.restrictKeys written (toIntSet (txWriteConfirmed receipt)))
        ]
      !() = foldl' (\() !_ -> ()) () landed
  void $ insertLandedTxs lfcTxCache point landed
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
  | Jobs.jobCommitmentOfTxs [(h, fromIntegral (BS.length bytes)) | (_, h, bytes) <- hashed]
      /= commitment =
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
