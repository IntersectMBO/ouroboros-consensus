{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the node's LeiosFetch clients do: send the requests the decision
-- logic assigned to their peer, and take in the replies.
--
-- Taking in a reply is where the node acquires an endorser block or its
-- transactions, so it is also where it emits each acquisition, once the write
-- is durable: 'AcquiredEb' when the body is, and 'AcquiredEbTxs' when no job of
-- its closure remains.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( LeiosFetchClientContext (..)
  , ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , nextLeiosFetchClientCommand
  , processLeiosBlock
  , processLeiosBlockTxs
  , acquireCachedBody
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
  , ReferencesTxsByHash (..)
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
  , LeiosEbSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , TxHash
  , TxLocation
  , TxOffset
  , admitsSizes
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , leiosEbSizes
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client (SomeLeiosFetchJob (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbBounds (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , acquireEbBody
  , completeTxRequest
  , newLeiosJobPool
  , offsetsToBitmap
  , refundEbRequest
  , refundTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchClientEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , LocalFill
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
  -- ^ Emit an acquisition to the rest of the node.
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
-- let a peer deny us an endorser block just by lowballing its own offer. Bytes
-- larger than the ledger allows, or referencing more transaction bytes than it
-- allows, are never written, so the node never offers them onward.
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
    -- Answered, so this request is no longer in flight.
    let cleaned = refundEbRequest peer point (lbrOfferedSize req) outstanding
    if
      | tooBig ->
          -- Held with nothing to fetch, so that neither its closure nor a
          -- re-send of the same bytes costs us anything more at this point.
          -- It is not written, so it is never emitted as acquired.
          --
          -- TODO the LeiosDb has no way to record that we are done with an
          -- endorser block whose closure we never fetched, so a restart within
          -- the pruning window forfeits this, costing one more body fetch from
          -- the next peer to serve it.
          pure
            ( acquireEbBody point Jobs.emptyLeiosJobPool cleaned
            , (Nothing, fetchArrivalInvalid ebBytesSize)
            )
      | otherwise -> case Map.lookup point (ebState outstanding) of
          -- Pruned meanwhile.
          Nothing -> pure (cleaned, (Nothing, fetchArrivalEvicted ebBytesSize))
          -- Another delivery got here first.
          Just (MkEbState _onset BodyAcquired{}) -> pure (cleaned, (Nothing, fetchArrivalExtra ebBytesSize))
          Just (MkEbState _onset NoBody) -> do
            -- Register the body's transactions in the tx cache, and read where
            -- some recent point durably holds any of them, so that the body
            -- write copies those rather than us fetching them again.
            mbInserted <- insertBody lfcTxCache ebHash (packTxReferences eb) [] snocLocated
            case mbInserted of
              Just (_summary, located) -> pure (cleaned, (Just located, fetchArrivalGood ebBytesSize))
              -- The cache already holds the body, taken in for another point,
              -- or it does not track this endorser block's announcement,
              -- perhaps since evicted. Either way, its txs may have locations.
              Nothing -> do
                located <- locateTxs lfcTxCache (packTxReferences eb) [] snocLocated
                pure (cleaned, (Just located, fetchArrivalEvicted ebBytesSize))
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
    -- node down rather than being silently dropped. Like the worker that
    -- 'processLeiosBlockTxs' forks, it may outlive the peer.
    link =<< async (settleBody ctx point eb now pointWritten bodyWritten)
  when (ebBytesSize /= lbrOfferedSize req) $
    throwIO $
      ExnLeiosBlockWrongSize point (lbrOfferedSize req) ebBytesSize
  when bodyTooBig $
    throwIO $
      ExnLeiosBlockTooBig point ebBytesSize lebMaxBodySize
  when closureTooBig $
    throwIO $
      ExnLeiosClosureTooBig point closureBytesSize lebMaxTxsSize
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
  sizes = leiosEbSizes eb
  ebBytesSize = lesBodySize sizes
  closureBytesSize = lesTxsSize sizes
  -- TODO The node does not vote yet. Voting must check the endorser block
  -- against the limits of the election it votes in: these are the limits of
  -- whichever election the request was made for, and a later election that
  -- names the same endorser block may have smaller ones.
  MkLeiosEbBounds{lebMaxBodySize, lebMaxTxsSize} = lbrEbBounds req
  bodyTooBig = ebBytesSize > lebMaxBodySize
  closureTooBig = closureBytesSize > fromIntegral lebMaxTxsSize
  tooBig = bodyTooBig || closureTooBig

-- | Hold the body, now that its write is durable, with the jobs that the
-- write's fills left to fetch, and emit what is acquired. Nothing if the point
-- was pruned meanwhile, or another delivery of it settled first.
--
-- TODO The LeiosDB's GC normally collects a point only after the fetch state
-- has pruned it, but nothing enforces that order; checking the LeiosDB's GC
-- frontier here would.
settleBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  EndorserBlock ->
  -- | when the body arrived
  RelativeTime ->
  Promise m () ->
  Promise m [TxOffset] ->
  m ()
settleBody ctx point eb now pointWritten bodyWritten = do
  () <- await pointWritten
  filledOffs <- await bodyWritten
  acquired <- modifyMVar lfcOutstanding $ \outstanding ->
    case Map.lookup point (ebState outstanding) of
      Just (MkEbState onset NoBody) -> do
        let
          jobPool =
            newLeiosJobPool lfcStaticEnv (refs `IntMap.withoutKeys` IntSet.fromList filledOffs)
        atomically $ do
          lfcNotify $ AcquiredEb point (encodeEndorserBlockSize eb)
          when (Jobs.nullLeiosJobPool jobPool) $
            lfcNotify $
              AcquiredEbTxs point
        pure
          ( acquireEbBody point jobPool outstanding
          , Just (onset, jobPool)
          )
      _ -> pure (outstanding, Nothing)
  -- These bytes are durable now: advertise them as fill sources for later
  -- endorser blocks referencing the same txs.
  setTxLocations lfcTxCache point [(off, fst (refs IntMap.! off)) | off <- filledOffs]
  void $ tryPutMVar lfcReady ()
  forM_ acquired $ \(onset, jobPool) -> do
    traceWith lfcTracer $ TraceLeiosFetchClientBlockAcquired point (ageAt now onset)
    when (Jobs.nullLeiosJobPool jobPool) $
      traceWith lfcTracer $
        TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)
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

-- | Acquire the endorser block at this point from the body the LeiosTxCache
-- holds for it, having taken it in for another point, instead of fetching it
-- again: write the point and the body, filling its txs from where they are
-- durably held, and settle them as an arrival would.
-- Nothing if the cache holds no body for it, as after a restart, or if these
-- bounds do not admit it; the point is then fetched like any other.
acquireCachedBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  LeiosEbBounds ->
  m ()
acquireCachedBody ctx point bounds =
  lookupBody lfcTxCache (pointEbHash point) >>= \case
    Nothing -> pure ()
    Just packed
      | not (admitsSizes bounds (leiosEbSizes eb)) -> pure ()
      | otherwise -> do
          now <- systemTimeCurrent lfcSystemTime
          localFills <- locateTxs lfcTxCache packed [] snocLocated
          pointWritten <- writeEbPoint lfcWriter point (lesBodySize (leiosEbSizes eb))
          bodyWritten <- writeEbBody lfcWriter point eb localFills
          link =<< async (settleBody ctx point eb now pointWritten bodyWritten)
     where
      eb = mkEndorserBlock $ reverse $ foldTxReferences (\acc txHash size -> (txHash, size) : acc) [] packed
 where
  MkLeiosFetchClientContext{lfcTxCache, lfcWriter, lfcSystemTime} = ctx

-- | Prepend a tx's location, if it has one, as a fill of its offset.
snocLocated :: [LocalFill] -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> [LocalFill]
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
  let pendingJobs = case Map.lookup point (ebState outstanding0) of
        Just (MkEbState _onset (BodyAcquired jobPool)) ->
          Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool
        Just (MkEbState _onset NoBody) -> IntMap.empty
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
  -- The worker may outlive the peer's registration, harmlessly: it settles
  -- only what is now durable, and the node keys the peer's state by its
  -- registration's @LeiosPeerLifetimeId@, so the worker neither recreates the
  -- state that unregistering removed nor touches a later registration's. It
  -- ends once the write lands, which closing the LeiosDB ensures.
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
    await txsWritten
    completed <- modifyMVar lfcOutstanding $ \outstanding -> do
      let outstanding' = completeTxRequest peer req outstanding
      case (Map.lookup point (ebState outstanding), Map.lookup point (ebState outstanding')) of
        ( Just (MkEbState _ (BodyAcquired before))
          , Just (MkEbState onset (BodyAcquired after))
          )
            | not (Jobs.nullLeiosJobPool before)
            , Jobs.nullLeiosJobPool after -> do
                atomically $ lfcNotify $ AcquiredEbTxs point
                pure (outstanding', Just (point, onset))
        _ -> pure (outstanding', Nothing)
    -- These bytes are durable now: advertise them as fill sources for later
    -- endorser blocks referencing the same txs.
    setTxLocations lfcTxCache point [(off, txHash) | (off, txHash, _bytes) <- toIngest]
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
