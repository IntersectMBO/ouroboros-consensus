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

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad (foldM, forM_, void, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
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
  , LeiosEbSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , SerialisedEndorserBlock (..)
  , TxHash
  , deserialiseEndorserBlock
  , endorserBlockReferences
  , hashEndorserBlockBytes
  , hashLeiosTx
  , leiosEbSizes
  , unwrapEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client (SomeLeiosFetchJob (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( BodySource (..)
  , EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbBounds (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , acquireEbBody
  , completeTxRequest
  , ebStateHasBody
  , newLeiosJobPool
  , offsetsToBitmap
  , refundEbRequest
  , refundTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchClientEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , unwrittenTx
  , withReader
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
  , lfcLeiosDb :: !(LeiosDbHandle m)
  -- ^ What 'lfcWriter' writes to, for reading back what it wrote.
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
  | -- | The bytes are not an endorser block in its envelope, or have the
    -- requested hash but are not an endorser block.
    ExnLeiosBlockUndecodable
      !LeiosPoint
      -- | why they do not decode
      !String
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
        (m (Either () (SomeLeiosFetchJob LeiosPoint (SerialisedEndorserBlock) LeiosTx m)))
        (Either () (SomeLeiosFetchJob LeiosPoint (SerialisedEndorserBlock) LeiosTx m))
    )
nextLeiosFetchClientCommand ctx controlMessageSTM peer reqsVar =
  atomically checkOrBlock >>= \case
    Just result -> pure $ Right result
    Nothing -> pure $ Left (atomically awaitStopOrRequest)
 where
  checkOrBlock ::
    STM m (Maybe (Either () (SomeLeiosFetchJob LeiosPoint (SerialisedEndorserBlock) LeiosTx m)))
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

  awaitStopOrRequest ::
    STM m (Either () (SomeLeiosFetchJob LeiosPoint (SerialisedEndorserBlock) LeiosTx m))
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
-- asked for or they are not, and no announcement gets a say. So the bytes are
-- hashed before they are decoded. Bytes that are
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
  SerialisedEndorserBlock ->
  m ()
processLeiosBlock ctx peer req (MkSerialisedEndorserBlock serialised) =
  case unwrapEndorserBlock serialised of
    Left err -> invalid (BS.length serialised) $ ExnLeiosBlockUndecodable point err
    Right (_version, bytes) -> do
      when (hashEndorserBlockBytes bytes /= ebHash) $
        invalid (BS.length bytes) $
          ExnLeiosBlockWrongHash point (hashEndorserBlockBytes bytes)
      case deserialiseEndorserBlock serialised of
        Left err -> invalid (BS.length bytes) $ ExnLeiosBlockUndecodable point err
        Right eb -> processEndorserBlock ctx peer req eb
 where
  point = lbrPoint req
  ebHash = pointEbHash point

  invalid :: Int -> ExnLeiosFetchInvalidReply -> m ()
  invalid size exn = do
    traceWith (lfcTracer ctx) $
      TraceLeiosFetchClientBlockArrived point (fetchArrivalInvalid (fromIntegral size))
    throwIO exn

-- | 'processLeiosBlock', once the bytes are the requested endorser block.
processEndorserBlock ::
  forall peer m.
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockRequest ->
  EndorserBlock ->
  m ()
processEndorserBlock ctx peer req eb = do
  now <- systemTimeCurrent lfcSystemTime
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
          -- re-send of the same bytes costs us anything more, until an offer
          -- whose bounds admit it. It is not written, so it is never announced
          -- as acquired.
          --
          -- TODO the LeiosDb has no way to record that we are done with an
          -- endorser block whose closure we never fetched, so a restart within
          -- the pruning window forfeits this, costing one more body fetch from
          -- the next peer to serve it.
          pure
            ( acquireEbBody ebHash (OversizedBody sizes) Jobs.emptyLeiosJobPool cleaned
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
    -- node down rather than being silently dropped. Like the worker that
    -- 'processLeiosBlockTxs' forks, it may outlive the peer.
    link =<< async (settleBody now pointWritten bodyWritten)
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
    { lfcStaticEnv
    , lfcOutstanding
    , lfcReady
    , lfcTxCache
    , lfcWriter
    , lfcLeiosDb
    , lfcNotify
    , lfcSystemTime
    , lfcTracer
    } = ctx

  point = lbrPoint req
  ebHash = pointEbHash point
  sizes = leiosEbSizes eb
  ebBytesSize = lesBodySize sizes
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  closureBytesSize = lesTxsSize sizes
  -- TODO The node does not vote yet. Voting must check the endorser block
  -- against the limits of the election it votes in: these are the limits of
  -- whichever election the request was made for, and a later election that
  -- names the same endorser block may have smaller ones.
  MkLeiosEbBounds{lebMaxBodySize, lebMaxTxsSize} = lbrEbBounds req
  bodyTooBig = ebBytesSize > lebMaxBodySize
  closureTooBig = closureBytesSize > fromIntegral lebMaxTxsSize
  tooBig = bodyTooBig || closureTooBig

  -- Claim the body, now that its write is durable, with the jobs that the
  -- write's fills left to fetch, and announce what is acquired.
  settleBody now pointWritten bodyWritten = do
    () <- await pointWritten
    filledOffs <- await bodyWritten
    (filled, acquired) <- claimBody filledOffs (pointSlotNo point)
    -- These bytes are durable now: advertise them as fill sources for later
    -- endorser blocks referencing the same txs.
    setTxLocations lfcTxCache ebHash [(off, fst (refs IntMap.! off)) | off <- filled]
    void $ tryPutMVar lfcReady ()
    forM_ acquired $ \(acquiredPoint, onset, jobPool) -> do
      traceWith lfcTracer $ TraceLeiosFetchClientBlockAcquired acquiredPoint (ageAt now onset)
      when (Jobs.nullLeiosJobPool jobPool) $
        traceWith lfcTracer $
          TraceLeiosFetchClientBlockTxsAcquired acquiredPoint (ageAt now onset)

  -- Claim the body at the slot the endorser block is tracked at, provided the
  -- LeiosDb holds the point of the given slot and the txs at the given offsets.
  -- Returns the offsets whose txs it holds, and what it claimed.
  --
  -- An election may have focused the endorser block at a newer slot while the
  -- body was being written: that point is written first, and then the body and
  -- those txs are read back. Meanwhile pruning may have passed the older point
  -- and the LeiosDb collected the endorser block, after which another delivery
  -- may have written the body again, without those txs. What the reads find
  -- stays, since the LeiosDb holds the newer point.
  claimBody filledOffs writtenSlot = do
    settlement <- modifyMVar lfcOutstanding $ \outstanding ->
      case Map.lookup ebHash (ebState outstanding) of
        Just (MkEbState slot onset NoBody)
          | slot /= writtenSlot -> pure (outstanding, BodyTrackedAt slot)
          | otherwise -> do
              let acquiredPoint = MkLeiosPoint slot ebHash
                  (jobPool, outstanding') =
                    newLeiosJobPool
                      lfcStaticEnv
                      (refs `IntMap.withoutKeys` IntSet.fromList filledOffs)
                      outstanding
              atomically $ do
                lfcNotify $ AcquiredEb acquiredPoint sizes
                when (Jobs.nullLeiosJobPool jobPool) $
                  lfcNotify $
                    AcquiredEbTxs acquiredPoint sizes
              pure
                ( acquireEbBody ebHash (FetchedBody sizes) jobPool outstanding'
                , BodyClaimed (acquiredPoint, onset, jobPool)
                )
        _ -> pure (outstanding, BodyNotWanted)
    case settlement of
      BodyClaimed claimed -> pure (filledOffs, Just claimed)
      BodyNotWanted -> pure (filledOffs, Nothing)
      BodyTrackedAt slot -> do
        await =<< writeEbPoint lfcWriter (MkLeiosPoint slot ebHash) ebBytesSize
        (held, stillFilled) <- withReader lfcLeiosDb $ \reader -> do
          body <- lookupEbBody reader ebHash
          txs <- batchRetrieveTxs reader ebHash filledOffs
          pure (isJust body, [off | (off, bytes) <- txs, not (unwrittenTx bytes)])
        if held then claimBody stillFilled slot else pure ([], Nothing)

-- | What settling a body finds, holding the lock on the fetch state.
data BodySettlement a
  = -- | It claimed the body.
    BodyClaimed a
  | -- | The endorser block is tracked at this slot, whose point the LeiosDb may
    -- not hold yet.
    BodyTrackedAt !SlotNo
  | -- | The endorser block was pruned meanwhile, or another delivery of it
    -- settled first.
    BodyNotWanted

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
    () <- await txsWritten
    completed <- modifyMVar lfcOutstanding $ \outstanding -> do
      let outstanding' = completeTxRequest peer req outstanding
      case (Map.lookup ebHash (ebState outstanding), Map.lookup ebHash (ebState outstanding')) of
        ( Just (MkEbState _ _ (BodyAcquired _ before))
          , Just (MkEbState slot onset (BodyAcquired (FetchedBody sizes) after))
          )
            | not (Jobs.nullLeiosJobPool before)
            , Jobs.nullLeiosJobPool after -> do
                let acquiredPoint = MkLeiosPoint slot ebHash
                atomically $ lfcNotify $ AcquiredEbTxs acquiredPoint sizes
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
