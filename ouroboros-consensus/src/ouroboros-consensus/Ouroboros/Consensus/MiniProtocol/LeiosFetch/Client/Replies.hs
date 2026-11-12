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
  , ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , awaitTerminate
  , nextLeiosFetchJob
  , processLeiosBlock
  , processLeiosBlockTxs
  , acquireForgedEb
  , noMempoolTxsAndVersion0
  ) where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Control.Monad (unless, void, when)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold, foldl', toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
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
  , LeiosMempoolTxs (..)
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
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client (SomeLeiosFetchJob (..))
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
  , newLeiosJobPool
  , refundEbRequest
  , refundTxRequest
  , settleReceivingFromForge
  , settleTxRequest
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , LeiosBlockTxSources (..)
  , LeiosTxCacheInsertBodySummary
  , TraceLeiosFetchClientEvent (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (Message (..), offsetsToBitmap)
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
  , StrictTVar
  , atomically
  , evaluate
  , modifyMVar
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
  , lfcTxCache :: !(LeiosTxCache m () VersionWord16 PackedEndorserBlock)
  -- ^ Records each validated tx with the major protocol version it was
  -- validated under. The Mempool and LeiosVoting validate a tx only under the
  -- current major version, so a recorded validation still holds exactly when
  -- its version matches the current one.
  , lfcWriter :: !(LeiosDbWriter m)
  , lfcNotify :: !(LeiosAcquisition -> STM m ())
  -- ^ Emit an acquisition to the rest of the node.
  , lfcSystemTime :: !(SystemTime m)
  -- ^ For reporting how long after its point's slot began an endorser block,
  -- or its transactions, were acquired.
  , lfcMempoolTxs :: !(Set TxHash -> m (LeiosMempoolTxs ByteString))
  -- ^ For the subset of these txs the node's Mempool currently holds, their
  -- bytes; and the major protocol version the Mempool validated them all under.
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
  | -- | Storage rejected declared sizes at these submitted offsets.
    ExnLeiosBlockTxsWrongSize !LeiosPoint ![TxOffset]
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
--
-- The body is written with those of its transactions the node holds locally
-- ('writeBodyWithLocalTxs'), so it fetches only the rest.
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
        Just (MkEbState _onset ReceivingFromForge{}) ->
          pure (cleaned, BodyDiscarded (fetchArrivalExtra ebBytesSize))
        Just (MkEbState _onset NoBody) -> do
          -- Register the body's transactions in the tx cache, and read where
          -- some recent point holds any of them, so that the body
          -- write copies those rather than us fetching them again.
          (summary, fills) <-
            insertBodyAndFoldl' lfcTxCache point (packEndorserBlock eb) Map.empty snocLocated
          pure (cleaned, BodyToWrite eb fills summary)
  void $ tryPutMVar lfcReady ()
  case disposition of
    BodyTooBig exn -> do
      traceWith lfcTracer $
        TraceLeiosFetchClientBlockArrived peer point (fetchArrivalInvalid ebBytesSize)
      throwIO exn
    BodyDiscarded arrival -> traceWith lfcTracer $ TraceLeiosFetchClientBlockArrived peer point arrival
    BodyToWrite eb localFills summary -> do
      -- Settled only once the write lands, since the settle needs the offsets
      -- the point then holds. The client takes no other reply from this peer
      -- meanwhile.
      (written, validated) <- writeBodyWithLocalTxs ctx point eb localFills summary
      settleBody ctx peer point eb validated written
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
    -- Also the LeiosTxCache's summary of taking it in, if it did.
    BodyToWrite !EndorserBlock !LocalFills !(Maybe LeiosTxCacheInsertBodySummary)

-- | Record the body as acquired at its point, now that its write has settled
-- and holds the txs at these offsets,
-- with jobs for the txs the point does not hold yet, and emit what is acquired.
-- Nothing if the point was pruned meanwhile, or another reply with it settled
-- first. Either way, trace the reply's arrival: its bytes were needed only if
-- it settled first.
--
-- Positive receipts distinguish an unavailable/retired target from a present
-- body whose closure is still missing. Writer completion alone is not proof
-- of possession. A cache publication no-op cannot invalidate a positive
-- receipt or recreate work that fetch pruning already removed.
settleBody ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  -- | the peer that sent it
  peer ->
  LeiosPoint ->
  EndorserBlock ->
  Map TxHash VersionWord16 ->
  Promise m BodyWriteReceipt ->
  m ()
settleBody ctx peer point eb validated bodyWritten = do
  receipt <- await bodyWritten
  case receipt of
    BodyUnavailable -> void $ tryPutMVar lfcReady ()
    BodyPresent heldOffs -> do
      traces <- modifyMVar lfcOutstanding $ \outstanding ->
        case Map.lookup point (ebState outstanding) of
          Nothing -> pure (outstanding, \_now -> [arrived fetchArrivalEvicted])
          Just (MkEbState _onset BodyAcquired{}) -> pure (outstanding, \_now -> [arrived fetchArrivalExtra])
          Just (MkEbState _onset ReceivingFromForge{}) -> pure (outstanding, \_now -> [arrived fetchArrivalExtra])
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
      recordConfirmedBodyTxs lfcTxCache point refs validated heldOffs
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

-- | Write the body at this point together with the txs it references that the
-- node holds locally, so that it fetches none of them: txs the LeiosDB holds at
-- other points (the 'LocalFills'), and txs its mempool holds.
--
-- A tx the mempool holds is written from there rather than filled, which is
-- better in every way: the mempool validated it under a recent major protocol
-- version, which the LeiosTxCache then records, and its bytes are already in
-- memory, whereas 'writeEbBody' would have to copy a fill's bytes from another
-- point, which might have been garbage-collected already.
--
-- The mempool's txs go to 'writeEbBody' alongside the fills, rather than to
-- 'writeTxs', which is for txs that have passed the MsgLeiosBlockTxs commitment
-- check. Like a fill, a tx from the mempool has not been checked against this
-- body: it might not have the length the body declares for it, for example.
-- 'writeEbBody' writes it, as it writes a fill, only at the size the body
-- declares.
--
-- Returns the real write promise and compact captured validation facts.
-- Only the positive receipt establishes held offsets.
--
-- It traces where the txs come from, along with the LeiosTxCache's summary of
-- taking the body in, if it did.
writeBodyWithLocalTxs ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  EndorserBlock ->
  LocalFills ->
  Maybe LeiosTxCacheInsertBodySummary ->
  m (Promise m BodyWriteReceipt, Map TxHash VersionWord16)
writeBodyWithLocalTxs ctx point eb localFills summary = do
  MkLeiosMempoolTxs inMempool protVer <- lfcMempoolTxs (Set.fromList (map fst refs))
  let fromMempool =
        [ (off, txHash, bytes)
        | (off, (txHash, _size)) <- zip [0 ..] refs
        , Just bytes <- [Map.lookup txHash inMempool]
        ]
      mempoolOffs = Set.fromList [off | (off, _, _) <- fromMempool]
      fills = localFills `Map.withoutKeys` mempoolOffs
  traceWith lfcTracer $
    TraceLeiosFetchClientBlockTxSources
      point
      MkLeiosBlockTxSources
        { btsLocalFills = length fills
        , btsMempool = length fromMempool
        , btsRemaining = length refs - length fills - length fromMempool
        }
      summary
  validated <- evaluate $ Map.fromList [(h, protVer) | (_off, h, _bytes) <- fromMempool]
  written <-
    writeEbBody
      lfcWriter
      point
      eb
      fills
      (Map.fromList [(off, bytes) | (off, _txHash, bytes) <- fromMempool])
  pure (written, validated)
 where
  MkLeiosFetchClientContext{lfcMempoolTxs, lfcWriter, lfcTracer} = ctx
  refs = endorserBlockReferences eb

-- | 'lfcMempoolTxs' for a node that takes no txs from a mempool: none, with
-- version 0, which therefore describes no tx.
noMempoolTxsAndVersion0 :: Applicative m => Set TxHash -> m (LeiosMempoolTxs ByteString)
noMempoolTxsAndVersion0 _ = pure (MkLeiosMempoolTxs Map.empty minBound)

-- | Acquire an endorser block the node's own forge produced in this election,
-- as one that arrived with no peer: take its body into the LeiosTxCache, write
-- it together with all its txs, and once the write lands, record those txs in
-- the LeiosTxCache as validated, hold it with no job left ('holdForgedEb') and
-- emit the acquisitions not yet emitted at its point.
--
-- Its txs go to 'writeEbBody' with the body, as the mempool's txs do in
-- 'writeBodyWithLocalTxs', since they have not passed the MsgLeiosBlockTxs
-- commitment check that 'writeTxs' is for. Their bytes are not checked: the
-- forge built the body from these very txs. No tx is filled from another
-- point, since all of them are written.
--
-- So 'AcquiredEb', and with it the node's offer of the body, waits for the write
-- of the txs as well as the body.
acquireForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  ForgedEndorserBlock ->
  m ()
acquireForgedEb ctx point forged = do
  _ <- insertBody lfcTxCache point (packEndorserBlock eb)
  version <- evaluate (forgedEbTxsProtVer forged)
  written <-
    writeEbBody lfcWriter point eb mempty (Map.fromList (zip [0 ..] (toList (forgedEbTxs forged))))
  settleForgedEb ctx point eb version written
 where
  MkLeiosFetchClientContext{lfcTxCache, lfcWriter} = ctx
  eb = forgedEbBody forged

-- | Reconcile the forge only from positive target confirmation, independently
-- of whether the cache still retains its point.
settleForgedEb ::
  IOLike m =>
  LeiosFetchClientContext m peer ->
  LeiosPoint ->
  EndorserBlock ->
  VersionWord16 ->
  Promise m BodyWriteReceipt ->
  m ()
settleForgedEb ctx point eb version written = do
  receipt <- await written
  case receipt of
    BodyUnavailable -> void $ tryPutMVar lfcReady ()
    BodyPresent held -> do
      acquired <- modifyMVar lfcOutstanding $ \outstanding -> do
        let (outstanding', toEmit) = settleReceivingFromForge point outstanding
            onset = maybe SNothing ebStateOnset $ Map.lookup point (ebState outstanding)
            (notifications, traces) = case toEmit of
              Just EmittedNothing ->
                ( [AcquiredEb point (encodeEndorserBlockSize eb), AcquiredEbTxs point]
                , \age -> [TraceLeiosFetchClientBlockAcquired point age, TraceLeiosFetchClientBlockTxsAcquired point age]
                )
              Just EmittedBody -> ([AcquiredEbTxs point], \age -> [TraceLeiosFetchClientBlockTxsAcquired point age])
              Just EmittedBodyAndClosure -> ([], const [])
              Nothing -> ([], const [])
        atomically $ mapM_ lfcNotify notifications
        pure (outstanding', \now -> traces (ageAt now onset))
      recordConfirmedBodyTxs
        lfcTxCache
        point
        refs
        (Map.fromList [(h, version) | (h, _size) <- IntMap.elems refs])
        held
      void $ tryPutMVar lfcReady ()
      now <- systemTimeCurrent lfcSystemTime
      mapM_ (traceWith lfcTracer) (acquired now)
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcNotify, lfcSystemTime, lfcTracer} = ctx
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
  Set TxOffset ->
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
  -- discarded. The lock is not held through the hashing and the write, so the
  -- state may be stale by then, harmlessly: a completed job never becomes
  -- pending again, and writing a transaction again rewrites the same bytes.
  --
  -- The same critical section refunds the request, as 'processLeiosBlock' does:
  -- its credit bounds the bytes the peer may send before the client reads
  -- them, and the client has read this reply. The jobs stay this peer's until
  -- the write lands.
  outstanding0 <-
    modifyMVar lfcOutstanding $ \outstanding ->
      let refunded = refundTxRequest peer (fromIntegral batchBytes) outstanding
       in pure (refunded, refunded)
  void $ tryPutMVar lfcReady ()
  let (pendingJobs, discardedAs) = case Map.lookup point (ebState outstanding0) of
        Just (MkEbState _onset (BodyAcquired jobPool)) ->
          (Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool, fetchArrivalExtra)
        -- Pruned meanwhile, and perhaps referenced again since: txs are
        -- requested only once the body is held.
        Just (MkEbState _onset NoBody) -> (IntMap.empty, fetchArrivalEvicted)
        Just (MkEbState _onset ReceivingFromForge{}) -> (IntMap.empty, fetchArrivalExtra)
        Nothing -> (IntMap.empty, fetchArrivalEvicted)
      discarded = discardedAs $ jobsBytesSize (IntMap.difference (NEIntMap.toMap jobs) pendingJobs)
  toIngest <-
    either invalidReply (pure . fold) $
      IntMap.traverseWithKey
        (\i job -> checkJobCommitment point aligned (Jobs.MkLeiosJobId i) job)
        pendingJobs
  metadata <-
    evaluate $
      IntMap.fromList
        [(off, (h, size)) | (off, !h, bytes) <- toIngest, let !size = fromIntegral (BS.length bytes)]
  arrival <- classifyTxs lfcTxCache point [(h, size) | (h, size) <- IntMap.elems metadata]
  written <- writeTxs lfcWriter point (Map.fromList [(off, bytes) | (off, _h, bytes) <- toIngest])
  settleTxs ctx peer req metadata (arrival <> discarded) written
 where
  MkLeiosFetchClientContext
    { lfcOutstanding
    , lfcReady
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
-- emit 'AcquiredEbTxs' if no job of the point remains, and trace the reply's
-- arrival, as classified when it arrived.
--
-- See 'settleBody' on the LeiosDB's GC overtaking the fetch state's pruning.
settleTxs ::
  (Ord peer, IOLike m) =>
  LeiosFetchClientContext m peer ->
  peer ->
  LeiosBlockTxsRequest ->
  IntMap.IntMap (TxHash, BytesSize) ->
  FetchArrivalBytes ->
  Promise m TxWriteReceipt ->
  m ()
settleTxs ctx peer req@(MkLeiosBlockTxsRequest point _jobs) written arrival txsWritten = do
  receipt <- await txsWritten
  let confirmed = toIntSet (txWriteConfirmed receipt)
      wrongSize = txWriteWrongSize receipt
  traces <- modifyMVar lfcOutstanding $ \outstanding -> do
    let settled = settleTxRequest peer req confirmed outstanding
        outstanding' = if Set.null wrongSize then settled else abandonClosure point settled
    case (Map.lookup point (ebState outstanding), Map.lookup point (ebState outstanding')) of
      (Just (MkEbState _ (BodyAcquired before)), Just (MkEbState onset (BodyAcquired after)))
        | txWriteBodyPresent receipt
        , not (Jobs.nullJobPool before)
        , Jobs.nullJobPool after -> do
            atomically $ lfcNotify $ AcquiredEbTxs point
            pure (outstanding', \now -> [TraceLeiosFetchClientBlockTxsAcquired point (ageAt now onset)])
      _ -> pure (outstanding', const [])
  let landed =
        [ MkLandedTx off h size (TxInserted ())
        | (off, (h, size)) <- IntMap.toAscList (IntMap.restrictKeys written confirmed)
        ]
      !() = foldl' (\() !_ -> ()) () landed
  void $ insertLandedTxs lfcTxCache point landed
  void $ tryPutMVar lfcReady ()
  now <- systemTimeCurrent lfcSystemTime
  mapM_ (traceWith lfcTracer) (TraceLeiosFetchClientTxsArrived peer point arrival : traces now)
  unless (Set.null wrongSize) $
    throwIO $
      ExnLeiosBlockTxsWrongSize point (Set.toAscList wrongSize)
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcNotify, lfcSystemTime, lfcTracer} = ctx

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
