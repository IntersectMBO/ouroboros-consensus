{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the node's LeiosFetch clients do: send the requests the decision
-- logic assigned to their peer, and take in the replies.
--
-- Taking in a reply is where the node acquires an endorser block or its
-- transactions, so it is also where it announces each acquisition, once the
-- write is durable: 'AcquiredEb' when the body is, and 'AcquiredEbTxs' when no
-- job of its closure remains.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( LeiosFetchClientContext (..)
  , ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , nextLeiosFetchClientCommand
  , processLeiosBlock
  , processLeiosBlockTxs
  ) where

import Control.Monad (foldM, forM_, void, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCache (..)
  , PackedTxReferences
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  , packTxReferences
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , TxHash
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client (SomeLeiosFetchJob (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( BodySource (..)
  , EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , acquireEbBody
  , completeTxRequest
  , ebStateHasBody
  , offsetsToBitmap
  , refundEbRequest
  , refundTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchClientEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( Exception
  , IOLike
  , STM
  , StrictMVar
  , StrictTVar
  , async
  , atomically
  , link
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
  , lfcTxCache :: !(LeiosTxCache m () ProtVerParity PackedTxReferences)
  , lfcWriter :: !(LeiosDbWriter m)
  , lfcNotify :: !(LeiosEbNotification -> STM m ())
  -- ^ Announce an acquisition to the rest of the node.
  , lfcSystemTime :: !(SystemTime m)
  -- ^ For reporting how long after its announcement an endorser block arrived.
  , lfcTracer :: !(Tracer m TraceLeiosFetchClientEvent)
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
  | -- | The endorser block references more transaction bytes than the ledger
    -- allows.
    ExnLeiosClosureTooBig
      !LeiosPoint
      -- | the bytes it references
      !Word64
      -- | the most the ledger allows
      !BytesSize
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
data JobMismatch = JobTxCountMismatch | JobBytesMismatch | JobRootHashMismatch
  deriving Show

-----

-- | The next thing this peer's client should do: send a request the decision
-- logic assigned to it, or stop. 'Right' if that is known now, else 'Left'
-- with what blocks until it is.
nextLeiosFetchClientCommand ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  ControlMessageSTM m ->
  peer ->
  -- | the requests the decision logic assigned to this peer and the client has
  -- not sent yet
  StrictTVar m (Seq LeiosFetchRequest) ->
  m
    ( Either
        (m (Either () (SomeLeiosFetchJob LeiosPoint EndorserBlock LeiosTx m)))
        (Either () (SomeLeiosFetchJob LeiosPoint EndorserBlock LeiosTx m))
    )
nextLeiosFetchClientCommand ctx controlMessageSTM peer reqsVar =
  atomically checkOrBlock >>= \case
    Just result -> pure $ Right result
    Nothing -> pure $ Left (atomically awaitStopOrRequest)
 where
  checkOrBlock :: STM m (Maybe (Either () (SomeLeiosFetchJob LeiosPoint EndorserBlock LeiosTx m)))
  checkOrBlock =
    controlMessageSTM >>= \case
      Terminate -> pure $ Just $ Left ()
      -- Only warm mini-protocols receive this, and LeiosFetch is hot.
      Quiesce -> next
      Continue -> next
   where
    next =
      readTVar reqsVar >>= \case
        req Seq.:<| reqs -> do
          writeTVar reqsVar reqs
          pure $ Just $ Right $ toJob req
        Seq.Empty -> pure Nothing

  awaitStopOrRequest :: STM m (Either () (SomeLeiosFetchJob LeiosPoint EndorserBlock LeiosTx m))
  awaitStopOrRequest = checkOrBlock >>= maybe retry pure

  -- Replies are processed right on the pipelined peer's collector thread:
  -- everything they touch is thread-safe.
  toJob = \case
    LeiosBlockRequest req ->
      MkSomeLeiosFetchJob
        (MsgLeiosBlockRequest (lbrPoint req))
        (pure $ \(MsgLeiosBlock eb) -> processLeiosBlock ctx peer req eb)
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p jobs) ->
      -- The wire request is just the point and bitmap: the union of the
      -- covered jobs' offsets.
      let bitmaps = offsetsToBitmap (foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs)
       in MkSomeLeiosFetchJob
            (MsgLeiosBlockTxsRequest p bitmaps)
            (pure $ \(MsgLeiosBlockTxs txs) -> processLeiosBlockTxs ctx peer req txs)

-- | Take in an endorser block that arrived in reply to this request.
--
-- The hash is the whole of it: these bytes either are the endorser block we
-- asked for or they are not, and no announcement gets a say. Bytes that are
-- the right block but not the size the peer offered still cost the peer its
-- connection, but only once we have taken the body: throwing them away would
-- let a peer deny us an endorser block just by lowballing its own offer.
processLeiosBlock ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockRequest ->
  EndorserBlock ->
  m ()
processLeiosBlock ctx peer req eb = do
  now <- systemTimeCurrent lfcSystemTime
  when (endorserBlockHash eb /= ebHash) $ do
    traceWith lfcTracer $ TraceLeiosFetchClientBlockArrived point (fetchArrivalInvalid ebBytesSize)
    throwIO $ ExnLeiosBlockWrongHash point (endorserBlockHash eb)
  -- The lock decides whether this is a novel, still relevant body, so two peers
  -- delivering the same body cannot both write it.
  (fills, arrival) <- modifyMVar lfcOutstanding $ \outstanding -> do
    let tooOld = pointSlotNo point < outstandingPrunedSlot outstanding
        novel = not $ maybe False ebStateHasBody (Map.lookup ebHash (ebState outstanding))
        -- This request is no longer in flight, either way.
        cleaned = refundEbRequest peer ebHash (lbrOfferedSize req) outstanding
    if
      | tooBig ->
          -- Held with nothing to fetch, so that neither its closure nor a
          -- re-send of the same bytes costs us anything more. It is not
          -- written, so it is never announced as acquired.
          --
          -- TODO the LeiosDb has no way to record that we are done with an
          -- endorser block whose closure we never fetched, so a restart within
          -- the pruning window forfeits this, costing one more body fetch from
          -- the next peer to serve it.
          pure
            ( acquireEbBody ebHash OversizedBody Jobs.emptyLeiosJobPool cleaned
            , (Nothing, fetchArrivalInvalid ebBytesSize)
            )
      | tooOld -> pure (cleaned, (Nothing, fetchArrivalEvicted ebBytesSize))
      | not novel -> pure (cleaned, (Nothing, fetchArrivalExtra ebBytesSize))
      | otherwise -> do
          -- Register the body's transactions in the tx cache, and read where
          -- some recent endorser block durably holds any of them, so that the
          -- body write copies those rather than us fetching them again.
          mbInserted <-
            insertBody
              lfcTxCache
              ebHash
              (packTxReferences eb)
              []
              ( \acc off _txh _sz -> \case
                  Just loc -> (off, loc) : acc
                  Nothing -> acc
              )
          pure
            ( cleaned
            , case mbInserted of
                Just (_summary, located) -> (Just located, fetchArrivalGood ebBytesSize)
                -- The cache does not track this endorser block's announcement,
                -- perhaps since evicted, so it locates nothing.
                Nothing -> (Just [], fetchArrivalEvicted ebBytesSize)
            )
  void $ tryPutMVar lfcReady ()
  traceWith lfcTracer $ TraceLeiosFetchClientBlockArrived point arrival
  forM_ fills $ \localFills -> do
    -- The LeiosDB writes a body only for a point it holds. The announcement
    -- path writes the point, but not every endorser block fetched has had its
    -- announcement processed by this node with this code.
    --
    -- TODO Remove this once that is so.
    pointWritten <- writeEbPoint lfcWriter point ebBytesSize
    bodyWritten <- writeEbBody lfcWriter point eb localFills
    -- Off this thread, but linked to it, so a failed write still brings the
    -- node down rather than being silently dropped.
    link =<< async (settleBody now pointWritten bodyWritten)
  when (ebBytesSize /= lbrOfferedSize req) $
    throwIO $
      ExnLeiosBlockWrongSize point (lbrOfferedSize req) ebBytesSize
  when tooBig $
    throwIO $
      ExnLeiosClosureTooBig point closureBytesSize (lbrMaxEbTxsSize req)
 where
  MkLeiosFetchClientContext
    { lfcStaticEnv
    , lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx

  point = lbrPoint req
  ebHash = pointEbHash point
  ebBytesSize = encodeEndorserBlockSize eb
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  closureBytesSize :: Word64
  closureBytesSize = sum (fromIntegral . snd <$> refs)
  tooBig = closureBytesSize > fromIntegral (lbrMaxEbTxsSize req)

  -- Claim the body, now that its write is durable, with the jobs that the
  -- write's fills left to fetch, and announce what is acquired.
  settleBody now pointWritten bodyWritten = do
    () <- await pointWritten
    (_completed, filledOffs) <- await bodyWritten
    let jobPool =
          Jobs.mkLeiosJobPool
            (maxJobBytesSize lfcStaticEnv)
            (maxJobTxCount lfcStaticEnv)
            (refs `IntMap.withoutKeys` IntSet.fromList filledOffs)
    acquired <- modifyMVar lfcOutstanding $ \outstanding ->
      case Map.lookup ebHash (ebState outstanding) of
        Just (MkEbState slot onset NoBody) -> do
          let acquiredPoint = MkLeiosPoint slot ebHash
          atomically $ do
            lfcNotify $ AcquiredEb acquiredPoint ebBytesSize
            when (Jobs.nullLeiosJobPool jobPool) $
              lfcNotify $
                AcquiredEbTxs acquiredPoint
          pure
            (acquireEbBody ebHash (FetchedBody ebBytesSize) jobPool outstanding, Just (acquiredPoint, onset))
        -- Pruned meanwhile, or another delivery of it settled first.
        _ -> pure (outstanding, Nothing)
    -- These bytes are durable now: advertise them as fill sources for later
    -- endorser blocks referencing the same txs.
    setTxLocations lfcTxCache ebHash [(off, fst (refs IntMap.! off)) | off <- filledOffs]
    void $ tryPutMVar lfcReady ()
    forM_ acquired $ \(acquiredPoint, onset) -> do
      traceWith lfcTracer $ TraceLeiosFetchClientBlockAcquired acquiredPoint (ageAt now onset)
      when (Jobs.nullLeiosJobPool jobPool) $
        traceWith lfcTracer $
          TraceLeiosFetchClientBlockTxsAcquired acquiredPoint (ageAt now onset)

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
  now <- systemTimeCurrent lfcSystemTime
  when (V.length txs /= IntSet.size offsetsSet) $
    invalidReply $
      ExnLeiosBlockTxsWrongCount point (IntSet.size offsetsSet) (V.length txs)
  -- Cheap checks, without hashing, for every covered job, so that an over-send
  -- is caught and punished even for a job we have since completed.
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
  let pendingJobs = case Map.lookup ebHash (ebState outstanding0) of
        Just (MkEbState _slot _onset (BodyAcquired _source jobPool)) ->
          Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool
        Just (MkEbState _slot _onset NoBody) -> IntMap.empty
        Nothing -> IntMap.empty
      redundantExtra =
        fetchArrivalExtra $
          IntMap.foldr
            (\(Jobs.MkLeiosJob _ bytes _) acc -> bytes + acc)
            0
            (IntMap.difference (NEIntMap.toMap jobs) pendingJobs)
  toIngest <-
    either invalidReply (pure . fold) $
      IntMap.traverseWithKey (\i job -> ingestJob point aligned (Jobs.MkLeiosJobId i) job) pendingJobs
  txsWritten <- writeTxs lfcWriter point [(off, bytes) | (off, _txHash, bytes) <- toIngest]
  arrival <-
    withLockedInsertUnappliedTx lfcTxCache $ \w0 step ->
      foldM (\w (_off, txHash, bytes) -> step w txHash (fromIntegral (BS.length bytes)) ()) w0 toIngest
  traceWith lfcTracer $ TraceLeiosFetchClientTxsArrived point (arrival <> redundantExtra)
  modifyMVar_ lfcOutstanding (pure . refundTxRequest peer (fromIntegral batchBytes))
  void $ tryPutMVar lfcReady ()
  -- Retiring the jobs waits for the write: retiring them drops them from the
  -- pool for every peer, so doing it on arrival would leave the closure
  -- incomplete for good if the write never landed. Until then they stay
  -- picked by this peer, which others may already be asked for too, and
  -- which its disconnection releases.
  --
  -- TODO: the worker's lifetime is unbounded -- 'link' propagates its
  -- exceptions but does not tie it to the peer, so on teardown it is orphaned
  -- and keeps touching shared state. Fork it in the peer's ResourceRegistry
  -- (or move to a writer-run completion callback) so it dies with the peer.
  link =<< async (settleTxs now txsWritten toIngest)
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx

  ebHash = pointEbHash point
  txBytess = V.map leiosTxBytes txs
  batchBytes = V.sum (V.map BS.length txBytess)
  -- The union of the covered jobs' offsets, ascending: the order the peer
  -- decoded our bitmap into, so it aligns position-wise with the arriving txs.
  offsetsSet = foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs
  aligned = IntMap.fromList $ zip (IntSet.toAscList offsetsSet) (V.toList txs)

  invalidReply :: ExnLeiosFetchInvalidReply -> m a
  invalidReply exn = do
    traceWith lfcTracer $
      TraceLeiosFetchClientTxsArrived point (fetchArrivalInvalid (fromIntegral batchBytes))
    throwIO exn

  settleTxs now txsWritten toIngest = do
    _completed <- await txsWritten
    completed <- modifyMVar lfcOutstanding $ \outstanding -> do
      let outstanding' = completeTxRequest peer req outstanding
      case (Map.lookup ebHash (ebState outstanding), Map.lookup ebHash (ebState outstanding')) of
        ( Just (MkEbState _ _ (BodyAcquired _ before))
          , Just (MkEbState slot onset (BodyAcquired _ after))
          )
            | not (Jobs.nullLeiosJobPool before)
            , Jobs.nullLeiosJobPool after -> do
                let acquiredPoint = MkLeiosPoint slot ebHash
                atomically $ lfcNotify $ AcquiredEbTxs acquiredPoint
                pure (outstanding', Just (acquiredPoint, onset))
        _ -> pure (outstanding', Nothing)
    -- These bytes are durable now: advertise them as fill sources for later
    -- endorser blocks referencing the same txs.
    setTxLocations lfcTxCache ebHash [(off, txHash) | (off, txHash, _bytes) <- toIngest]
    void $ tryPutMVar lfcReady ()
    forM_ completed $ \(acquiredPoint, onset) ->
      traceWith lfcTracer $
        TraceLeiosFetchClientBlockTxsAcquired acquiredPoint (ageAt now onset)

-- | How long after this onset @now@ is, if there is an onset.
ageAt :: RelativeTime -> StrictMaybe RelativeTime -> Maybe NominalDiffTime
ageAt now = \case
  SJust onset -> Just (diffRelTime now onset)
  SNothing -> Nothing

-- | Cheap validation of one covered job against the commitment the request
-- carries for it: its arriving txs match its offset count and its total byte
-- size.
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
checkJobSize point aligned jid (Jobs.MkLeiosJob offs expectedBytes _root)
  | IntMap.size sub /= IntSet.size offs =
      Left $ ExnLeiosBlockTxsJobMismatch point jid JobTxCountMismatch
  | fromIntegral (sum [BS.length (leiosTxBytes tx) | tx <- IntMap.elems sub]) /= expectedBytes =
      Left $ ExnLeiosBlockTxsJobMismatch point jid JobBytesMismatch
  | otherwise = Right ()
 where
  -- just the txs from /this/ job
  sub = IntMap.restrictKeys aligned offs

-- | Content validation of one /pending/ job we intend to ingest: hash its
-- arriving txs and check their root hash against the request's commitment.
--
-- Only runs for the first reply for a job, in addition to 'checkJobSize'.
ingestJob ::
  LeiosPoint ->
  IntMap.IntMap LeiosTx ->
  Jobs.LeiosJobId ->
  Jobs.LeiosJob ->
  Either ExnLeiosFetchInvalidReply [(Int, TxHash, BS.ByteString)]
ingestJob point aligned jid (Jobs.MkLeiosJob offs _expectedBytes expectedRoot)
  | Jobs.jobRootHashOfTxHashes [h | (_, h, _) <- hashed] /= expectedRoot =
      Left $ ExnLeiosBlockTxsJobMismatch point jid JobRootHashMismatch
  | otherwise = Right hashed
 where
  -- Ascending by offset: the order the root hash commits to, and the key the
  -- LeiosDb stores the bytes under.
  hashed =
    [ (off, hashLeiosTx tx, leiosTxBytes tx)
    | (off, tx) <- IntMap.toAscList (IntMap.restrictKeys aligned offs)
    ]
