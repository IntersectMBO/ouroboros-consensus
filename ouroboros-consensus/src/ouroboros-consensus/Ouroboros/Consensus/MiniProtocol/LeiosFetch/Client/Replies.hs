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
  , ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , awaitTerminate
  , nextLeiosFetchJob
  , processLeiosBlock
  , processLeiosBlockTxs
  , acquireForgedEb
  ) where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Control.Monad (foldM, void, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold, for_, toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..))
import Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCache (..)
  , PackedEndorserBlock
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlock
  , EndorserBlockHash
  , ForgedEndorserBlock (..)
  , LeiosEbByteSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , Serialised (..)
  , TxHash
  , TxLocation
  , TxOffset
  , deserialiseEndorserBlock
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , hashSerialisedEndorserBlock
  , leiosEbByteSizes
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client (SomeLeiosFetchJob (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , acquireEbBody
  , completeTxRequest
  , ebStateOnset
  , holdForgedEb
  , newLeiosJobPool
  , refundEbRequest
  , refundTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , TraceLeiosFetchClientEvent (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..), offsetsToBitmap)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , LocalFills
  , Promise (..)
  , awaitAll
  )
import Ouroboros.Consensus.Util.IOLike
  ( Exception
  , IOLike
  , STM
  , StrictMVar
  , StrictTVar
  , atomically
  , evaluate
  , modifyMVar
  , modifyMVar_
  , readMVar
  , readTVar
  , retry
  , throwIO
  , tryPutMVar
  , writeTVar
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)

-- | What every LeiosFetch client of the node shares.
data LeiosFetchClientContext m peer = MkLeiosFetchClientContext
  { lfcStaticEnv :: !LeiosFetchStaticEnv
  , lfcOutstanding :: !(StrictMVar m (LeiosOutstanding peer))
  , lfcReady :: !(StrictMVar m ())
  -- ^ Filled by anything that might unblock a new fetch decision.
  , lfcTxCache :: !(LeiosTxCache m () ProtVerParity PackedEndorserBlock)
  , lfcWriter :: !(LeiosDbWriter m)
  , lfcNotify :: !(LeiosAcquisition -> STM m ())
  -- ^ Emit an acquisition to the rest of the node.
  , lfcSystemTime :: !(SystemTime m)
  -- ^ For reporting how long after its point's slot began an endorser block,
  -- or its transactions, were acquired.
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
  | -- | The endorser block has the requested hash, but does not decode, so no
    -- honest peer holds it.
    ExnLeiosBlockUndecodable !LeiosPoint !DecoderError
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
  deriving Show

instance Exception ExnLeiosFetchInvalidReply

-- | How a job's transactions differ from what it covers.
data JobMismatch = JobBytesSizeMismatch | JobCommitmentMismatch
  deriving Show

-----

-- | Blocks until this peer's client should stop.
awaitTerminate :: IOLike m => ControlMessageSTM m -> STM m ()
awaitTerminate controlMessageSTM =
  controlMessageSTM >>= \case
    Terminate -> pure ()
    -- Only warm mini-protocols receive this, and LeiosFetch is hot.
    Quiesce -> retry
    Continue -> retry

-- | The next request the decision logic assigned to this peer, with what to do
-- with its reply. Blocks until there is one.
nextLeiosFetchJob ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  -- | the requests the decision logic assigned to this peer and the client has
  -- not sent yet
  StrictTVar m (Seq LeiosFetchRequest) ->
  STM m (SomeLeiosFetchJob LeiosPoint (Serialised EndorserBlock) LeiosTx m)
nextLeiosFetchJob ctx peer reqsVar =
  readTVar reqsVar >>= \case
    Seq.Empty -> retry
    req Seq.:<| reqs -> toJob req <$ writeTVar reqsVar reqs
 where
  -- Replies are processed right on the pipelined peer's collector thread:
  -- everything they touch is thread-safe.
  toJob = \case
    LeiosBlockRequest req ->
      MkSomeLeiosFetchJob
        (MsgLeiosBlockRequest (lbrPoint req))
        (\(MsgLeiosBlock bytes) -> processLeiosBlock ctx peer req bytes)
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p jobs) ->
      -- The wire request is just the point and bitmap: the union of the
      -- covered jobs' offsets.
      let bitmaps = offsetsToBitmap (foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs)
       in MkSomeLeiosFetchJob
            (MsgLeiosBlockTxsRequest p bitmaps)
            (\(MsgLeiosBlockTxs txs) -> processLeiosBlockTxs ctx peer req txs)

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
processLeiosBlock ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockRequest ->
  Serialised EndorserBlock ->
  m ()
processLeiosBlock ctx peer req bytes = do
  when (hashSerialisedEndorserBlock bytes /= requestedEbHash) $ do
    traceWith lfcTracer $
      TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid ebBytesSize)
    throwIO $ ExnLeiosBlockWrongHash point (hashSerialisedEndorserBlock bytes)
  -- Decoded before taking the lock, which the replies of every peer share.
  verdict <- evaluate checkedEb
  -- Under the lock, decide what to do with this body. The point lacks the body
  -- until a reply's write settles, so another reply meanwhile is written
  -- too.
  disposition <- modifyMVar lfcOutstanding $ \outstanding -> do
    -- Answered, so this request is no longer in flight.
    let cleaned = refundEbRequest peer point (lbrOfferedSize req) outstanding
    case verdict of
      Left exn ->
        -- Held with nothing to fetch, so that neither its closure nor a
        -- re-send of the same bytes costs us anything more at this point.
        -- It is not written, so it is never emitted as acquired.
        pure (acquireEbBody point Jobs.emptyJobPool cleaned, BodyTooBig exn)
      Right eb -> case Map.lookup point (ebState outstanding) of
        -- Pruned meanwhile.
        Nothing -> pure (cleaned, BodyDiscarded (fetchArrivalEvicted ebBytesSize))
        -- Another reply's write settled first.
        Just (MkEbState _onset BodyAcquired{}) ->
          pure (cleaned, BodyDiscarded (fetchArrivalExtra ebBytesSize))
        Just (MkEbState _onset NoBody) -> do
          -- Register the body's transactions in the tx cache, and read where
          -- some recent point durably holds any of them, so that the body
          -- write copies those rather than us fetching them again.
          (_summary, located) <- insertBody lfcTxCache point (packEndorserBlock eb) [] snocLocated
          pure (cleaned, BodyToWrite eb (Map.fromList located))
  void $ tryPutMVar lfcReady ()
  case disposition of
    BodyTooBig exn -> do
      traceWith lfcTracer $
        TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid ebBytesSize)
      throwIO exn
    BodyDiscarded arrival -> traceWith lfcTracer $ TraceLeiosFetchClientBlockArrived peer point arrival
    BodyToWrite eb localFills -> do
      -- The client takes the peer's next reply only once this write lands, so
      -- a slow disk slows the peer down rather than piling up writes.
      heldOffs <- await =<< writeEbBody lfcWriter point eb localFills
      settleBody ctx peer point eb heldOffs
  -- Checked last, so that the body is still taken: throwing it away would let
  -- a peer deny us an endorser block just by lowballing its own offer.
  when (ebBytesSize /= lbrOfferedSize req) $
    throwIO $
      ExnLeiosBlockWrongSize point (lbrOfferedSize req) ebBytesSize
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcTracer
    } = ctx

  point = lbrPoint req
  requestedEbHash = pointEbHash point
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
data BodyDisposition
  = -- | Beyond its point's bounds, or undecodable, so never written.
    BodyTooBig !ExnLeiosFetchInvalidReply
  | -- | No longer wanted, its bytes classified so.
    BodyDiscarded !FetchArrivalBytes
  | -- | Wanted, so written, with these fills, and traced once that settles.
    BodyToWrite !EndorserBlock !LocalFills

-- | Record the body as acquired at its point, now that its write is durable
-- and holds the txs at these offsets,
-- with jobs for the txs the point does not hold yet, and emit what is acquired.
-- Nothing if the point was pruned meanwhile, or another reply with it settled
-- first. Either way, trace the reply's arrival: its bytes were needed only if
-- it settled first.
--
-- The LeiosDB drops the writes of a point older than its promoted slot, which
-- reaches a block's slot once that block is copied to the ImmutableDB. The
-- fetch state normally prunes such a point first, but only timing ensures
-- that. Had the LeiosDB dropped a write first, a settle would emit an
-- acquisition of a point whose writes were dropped, as 'LeiosAcquisition'
-- allows.
settleBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosPoint ->
  EndorserBlock ->
  Set TxOffset ->
  m ()
settleBody ctx peer point eb heldOffs = do
  traces <- modifyMVar lfcOutstanding $ \outstanding ->
    case Map.lookup point (ebState outstanding) of
      Nothing -> pure (outstanding, \_now -> [arrived fetchArrivalEvicted])
      Just (MkEbState _onset BodyAcquired{}) -> pure (outstanding, \_now -> [arrived fetchArrivalExtra])
      Just (MkEbState onset NoBody) -> do
        let
          jobPool =
            newLeiosJobPool lfcStaticEnv (refs `IntMap.withoutKeys` toIntSet heldOffs)
        atomically $ lfcNotify $ AcquiredEb point (encodeEndorserBlockSize eb)
        when (Jobs.nullJobPool jobPool) $
          atomically $
            lfcNotify $
              AcquiredEbTxs point
        pure
          ( acquireEbBody point jobPool outstanding
          , \now ->
              arrived fetchArrivalGood
                : TraceLeiosFetchClientBlockAcquired point (ageAt now onset)
                : [ TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)
                  | Jobs.nullJobPool jobPool
                  ]
          )
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs.
  setTxLocations lfcTxCache point [(off, fst (refs IntMap.! off)) | off <- Set.toList heldOffs]
  void $ tryPutMVar lfcReady ()
  -- Read after emitting, so that the acquisitions' ages run until then.
  now <- systemTimeCurrent lfcSystemTime
  mapM_ (traceWith lfcTracer) (traces now)
 where
  MkLeiosFetchClientContext
    { lfcStaticEnv
    , lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  arrived bucket =
    TraceLeiosFetchClientBlockArrived peer point (bucket (encodeEndorserBlockSize eb))

-- | Acquire an endorser block the node's own forge produced in this election,
-- as one that arrived with no peer: take its body into the LeiosTxCache, write
-- it and all its txs, take those into the LeiosTxCache as validated, and once
-- both writes land, hold it with no job left ('holdForgedEb') and emit the
-- acquisitions not yet emitted at its point.
--
-- Its bytes are not checked: the forge built the body from these very txs. No
-- tx is filled from another point, since all of them are written.
--
-- So 'AcquiredEb', and with it the node's offer of the body, waits for the txs
-- write as well as the body's.
acquireForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  ElectionId ->
  ForgedEndorserBlock ->
  m ()
acquireForgedEb ctx elId forged = do
  _ <-
    insertBody lfcTxCache point (packEndorserBlock eb) () (\w _off _txh _sz _loc -> w)
  bodyWritten <- writeEbBody lfcWriter point eb mempty
  txsWritten <- writeTxs lfcWriter point (Map.fromList (zip [0 ..] (toList (forgedEbTxs forged))))
  withLockedInsertAppliedTx lfcTxCache $ \w0 step ->
    foldM (\w (txHash, _size) -> step w txHash (forgedEbTxsParity forged)) w0 refs
  awaitAll [void bodyWritten, txsWritten]
  (onset, acquired) <- modifyMVar lfcOutstanding $ \outstanding -> do
    let (outstanding', acquired) = holdForgedEb elId ebHash (encodeEndorserBlockSize eb) outstanding
    atomically $ mapM_ lfcNotify acquired
    pure
      (outstanding', (maybe SNothing ebStateOnset (Map.lookup point (ebState outstanding)), acquired))
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs. The point holds them all: the
  -- forge built the body from these very txs, so none is of another size than
  -- it declares.
  setTxLocations lfcTxCache point (zip [0 ..] (map fst refs))
  void $ tryPutMVar lfcReady ()
  -- Read after emitting, so that the acquisitions' ages run until then.
  now <- systemTimeCurrent lfcSystemTime
  for_ acquired $
    traceWith lfcTracer . \case
      AcquiredEb{} -> TraceLeiosFetchClientBlockAcquired point (ageAt now onset)
      AcquiredEbTxs{} -> TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)
 where
  MkLeiosFetchClientContext
    { lfcTxCache
    , lfcWriter
    , lfcOutstanding
    , lfcReady
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx
  eb = forgedEbBody forged
  ebHash = endorserBlockHash eb
  point = MkLeiosPoint (electionSlot elId) ebHash
  refs = endorserBlockReferences eb

-- | Prepend a tx's location, if it has one, as a fill of its offset.
snocLocated ::
  [(TxOffset, TxLocation)] ->
  TxOffset ->
  TxHash ->
  BytesSize ->
  Maybe TxLocation ->
  [(TxOffset, TxLocation)]
snocLocated acc off _txh _sz = \case
  Just loc -> (off, loc) : acc
  Nothing -> acc

-- | Take in transactions that arrived in reply to this request.
processLeiosBlockTxs ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockTxsRequest ->
  V.Vector LeiosTx ->
  m ()
processLeiosBlockTxs ctx peer req@(MkLeiosBlockTxsRequest point jobs) txs = do
  -- Cheap checks, without hashing, of the whole reply and of every covered
  -- job, so that an over-send is caught and punished even for a job we have
  -- since completed.
  when (V.length txs /= IntSet.size offsetsSet) $
    invalidReply $
      ExnLeiosBlockTxsWrongCount point (IntSet.size offsetsSet) (V.length txs)
  either invalidReply (\_ -> pure ()) $
    NEIntMap.traverseWithKey
      (\i job -> checkJobSize point aligned (Jobs.MkLeiosJobId i) job)
      jobs
  -- Only jobs still pending are hashed and ingested; a redundant reply for a
  -- completed job, or a reply for an endorser block pruned meanwhile, is
  -- discarded. The state is read without holding the lock through the hashing
  -- and the write, so it may be stale by then, harmlessly: a completed job
  -- never becomes pending again, and writing a transaction again rewrites the
  -- same bytes.
  outstanding0 <- readMVar lfcOutstanding
  let (pendingJobs, discardedAs) = case Map.lookup point (ebState outstanding0) of
        Just (MkEbState _onset (BodyAcquired jobPool)) ->
          (Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool, fetchArrivalExtra)
        -- Pruned meanwhile, and perhaps referenced again since: txs are
        -- requested only once the body is held.
        Just (MkEbState _onset NoBody) -> (IntMap.empty, fetchArrivalEvicted)
        Nothing -> (IntMap.empty, fetchArrivalEvicted)
      discarded = discardedAs $ jobsBytesSize (IntMap.difference (NEIntMap.toMap jobs) pendingJobs)
  toIngest <-
    either invalidReply (pure . fold) $
      IntMap.traverseWithKey
        (\i job -> checkJobCommitment point aligned (Jobs.MkLeiosJobId i) job)
        pendingJobs
  -- The client takes the peer's next reply only once this write lands, so a
  -- slow disk slows the peer down rather than piling up writes. Until then the
  -- jobs stay picked by this peer, which others may already be asked for too,
  -- and which its disconnection releases.
  await
    =<< writeTxs lfcWriter point (Map.fromList [(off, bytes) | (off, _txHash, bytes) <- toIngest])
  -- After the write, so that the LeiosTxCache holds a tx as inserted only once
  -- the LeiosDB holds it.
  arrival <-
    withLockedInsertUnappliedTx lfcTxCache $ \w0 step ->
      foldM (\w (_off, txHash, bytes) -> step w txHash (fromIntegral (BS.length bytes)) ()) w0 toIngest
  -- A reply that writes nothing is traced now; one that does is instead
  -- traced by 'settleTxs'.
  when (IntMap.null pendingJobs) $
    traceWith lfcTracer $
      TraceLeiosFetchClientTxsArrived peer point discarded
  modifyMVar_ lfcOutstanding (pure . refundTxRequest peer (fromIntegral batchBytes))
  settleTxs
    ctx
    peer
    req
    [(off, txHash) | (off, txHash, _bytes) <- toIngest]
    (arrival <> discarded)
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcTxCache
    , lfcWriter
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
-- 'AcquiredEbTxs' if no job of the point remains. If the reply wrote any txs,
-- trace its arrival, as classified when it arrived.
--
-- See 'settleBody' on the LeiosDB's GC overtaking the fetch state's pruning.
settleTxs ::
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockTxsRequest ->
  -- | the offsets and hashes of the txs this reply wrote
  [(TxOffset, TxHash)] ->
  -- | the reply's bytes, classified when it arrived
  FetchArrivalBytes ->
  m ()
settleTxs ctx peer req@(MkLeiosBlockTxsRequest point _jobs) written arrival = do
  traces <- modifyMVar lfcOutstanding $ \outstanding -> do
    let outstanding' = completeTxRequest peer req outstanding
    case (Map.lookup point (ebState outstanding), Map.lookup point (ebState outstanding')) of
      -- This reply's write completed the closure.
      ( Just (MkEbState _ (BodyAcquired before))
        , Just (MkEbState onset (BodyAcquired after))
        )
          | not (Jobs.nullJobPool before)
          , Jobs.nullJobPool after -> do
              atomically $ lfcNotify $ AcquiredEbTxs point
              pure
                ( outstanding'
                , \now -> [TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)]
                )
      _ -> pure (outstanding', const [])
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs.
  setTxLocations lfcTxCache point written
  void $ tryPutMVar lfcReady ()
  -- Read after emitting, so that the acquisition's age runs until then.
  now <- systemTimeCurrent lfcSystemTime
  mapM_ (traceWith lfcTracer) (arrived <> traces now)
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx

  -- A reply that wrote nothing was traced on arrival, as Extra or Evicted.
  arrived = [TraceLeiosFetchClientTxsArrived peer point arrival | not (null written)]

-- | The total size of these jobs' txs.
jobsBytesSize :: IntMap.IntMap Jobs.LeiosJob -> BytesSize
jobsBytesSize = getSum . foldMap (\(Jobs.MkLeiosJob _ bytes _) -> Sum bytes)

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
-- arriving txs and check their hashes and sizes against the job's commitment.
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

toIntSet :: Set TxOffset -> IntSet.IntSet
toIntSet = IntSet.fromDistinctAscList . Set.toAscList
