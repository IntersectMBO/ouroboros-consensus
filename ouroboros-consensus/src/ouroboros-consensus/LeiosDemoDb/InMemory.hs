{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  ) where

import Cardano.Prelude (Generic, forM_, maybeToList, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , StrictTVar
  , dupTChan
  , modifyTVar
  , newBroadcastTChan
  , newTVarIO
  , readTVar
  , writeTChan
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , LocalFill
  , Promise (..)
  , TxOffset
  )
import LeiosDemoException
  ( LeiosDbException (LeiosDbWriteException, submittedFrom, writeFailure, writeJob)
  , throwLeiosDbException
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , TxLocation (..)
  , encodeLeiosEbSize
  , leiosEbBodyItems
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks (..)
  , atomically
  , throwIO
  , toException
  , try
  )

-- | In-memory database state.
data InMemoryLeiosDb = InMemoryLeiosDb
  { imEbTxBytes :: !(Map EbHash (IntMap ByteString))
  -- ^ Tx bytes owned by the referencing EB, keyed by offset -- mirrors the
  -- SQLite backend's @ebTxBytes@ table. A tx shared by two EBs is stored
  -- twice, deliberately.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their sizes. The same EB content can be
  -- inserted at multiple slots; each @(slot, hash)@ point has its own entry
  -- and completes independently.
  --
  -- As in the SQL backend's @ebs.ebBytesSize@: the body's actual size once
  -- this point is in 'imEbBodiesDownloaded', and the first size this point was
  -- announced at before that. Nothing reads it --- looking an acquired size up
  -- by hash goes to 'imEbBodies' --- but it is kept faithful to the column.
  --
  -- If multiple announcements /with different elections/ insert the same
  -- 'LeiosPoint' with a different 'BytesSize', then only the first is recorded.
  -- (TODO see the TODO on 'writeEbPoint')
  , imEbBodiesDownloaded :: !(Set LeiosPoint)
  -- ^ Subset of 'imEbPoints' for which 'writeEbBody' has been
  -- called. Only these points are considered for completion.
  , imEbBodies :: !(Map EbHash StoredEbBody)
  -- ^ Keyed by content hash, so shared across every point that references the
  -- same EB.
  --
  -- The SQL backend has no per-hash table to put the size in, so there it
  -- shares the per-point @ebs.ebBytesSize@ column with the announced sizes,
  -- and finding it means a query restricted to the points that have acquired
  -- the body --- 'sql_lookup_stored_eb_size', which the partial index
  -- @idx_ebs_acquired@ accelerates. Here a hash keys it directly.
  , imCompletedEbs :: !(Set LeiosPoint)
  -- ^ Points for which 'AcquiredEbTxs' has already been broadcast.
  -- The completion predicate stays true once an EB is complete, so
  -- any later batch containing one of its txs would re-trigger it
  -- without this guard; downstream consumers (e.g. 'runLeiosVoting')
  -- treat the re-notification as fatal ('AlreadyKnown' from
  -- 'addVote'). Each point carries the EB's hash and its announcer slot,
  -- so this is also the source for
  -- 'scanCompleteEbClosuresNotOlderThanSlot'.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty

-- | A stored EB body: the references it carries, and the size of the encoding
-- of those references, which is what identifies the body (see 'hashLeiosEb').
data StoredEbBody = StoredEbBody
  { sebBytesSize :: !BytesSize
  , sebTxEntries :: !(IntMap {- txOffset -} EbTxEntry)
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | EB transaction entry (references txs by hash, no bytes stored here)
data EbTxEntry = EbTxEntry
  { eteTxHash :: !TxHash
  , eteTxBytesSize :: !BytesSize
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | Create a new in-memory Leios database handle.
-- This is suitable for testing in IOSim.
newLeiosDBInMemory :: IOLike m => m (LeiosDbHandle m)
newLeiosDBInMemory = do
  stateVar <- newTVarIO emptyInMemoryLeiosDb
  newLeiosDBInMemoryWith stateVar

newLeiosDBInMemoryWith :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbHandle m)
newLeiosDBInMemoryWith stateVar = do
  notificationChan <- atomically newBroadcastTChan
  pure $
    LeiosDbHandle
      { -- Nothing to close: the state is a 'StrictTVar'.
        close = pure ()
      , subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , -- No-op for now; see 'leiosDbGarbageCollect'.
        leiosDbGarbageCollect = \_slotNo -> pure ()
      , -- No-op for now; see 'leiosDbPromoteToImmutable'.
        leiosDbPromoteToImmutable = \_points -> pure ()
      , -- The in-memory implementation does not track stats.
        leiosDbSampleStats = pure (LeiosDbStats 0 0 0)
      , openReader = openInMemoryReader stateVar
      , openWriter = openInMemoryWriter stateVar notificationChan
      }

-- | Reads off the 'StrictTVar'. Nothing to open or close.
openInMemoryReader :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbReader m)
openInMemoryReader stateVar =
  pure
    LeiosDbReader
      { close = pure ()
      , lookupEbBody = imLookupEbBody stateVar
      , lookupTrustedEbClosure = imLookupTrustedEbClosure stateVar
      , batchRetrieveTxs = imBatchRetrieveTxs stateVar
      , scanEbPoints = imScanEbPoints stateVar
      , -- ThreadNet persists 'stateVar' across simulated restarts, so on
        -- restart this seeds the restored acquired-EB-closures set.
        scanCompleteEbClosuresNotOlderThanSlot = imScanCompleteEbClosuresSince stateVar
      }

-- | No worker and no queue: the state is a 'StrictTVar', so each write runs
-- at submission and its 'Promise' comes back already resolved. A failure is
-- captured rather than thrown here, so it surfaces at 'await' exactly like
-- the SQLite backend's.
openInMemoryWriter ::
  forall m.
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  m (LeiosDbWriter m)
openInMemoryWriter stateVar notificationChan =
  pure
    LeiosDbWriter
      { close = pure ()
      , writeEbPoint = \point ebBytesSize ->
          resolved
            ("WriteEbPoint " <> show point)
            (imInsertEbPoint stateVar notificationChan point ebBytesSize)
      , writeEbBody = \point eb fills ->
          resolved
            ("WriteEbBody " <> show point)
            (imInsertEbBody stateVar notificationChan point eb fills)
      , writeTxs = \point offBytes ->
          resolved
            ("WriteTxs " <> show point <> " (" <> show (length offBytes) <> " txs)")
            (imInsertTxs stateVar notificationChan point offBytes)
      }
 where
  resolved :: HasCallStack => String -> m a -> m (Promise m a)
  resolved job action = do
    result <- try action
    pure $ Promise $ case result of
      Right x -> pure x
      Left cause ->
        throwIO
          LeiosDbWriteException
            { writeJob = job
            , submittedFrom = GHC.prettyCallStack GHC.callStack
            , writeFailure = toException (cause :: LeiosDbException)
            }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EbHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (p.pointSlotNo, p.pointEbHash)
    | p <- Map.keys (imEbPoints state)
    ]

-- | Implements 'writeEbPoint'.
--
-- The new point is added to 'imEbBodiesDownloaded'. 'imInsertTxs' considers
-- only the points in that set, so adding it there is what lets a later
-- transaction arrival complete this point's closure.
imInsertEbPoint ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  LeiosPoint ->
  BytesSize ->
  m CompletedEbs
imInsertEbPoint stateVar notificationChan point ebBytesSize = atomically $ do
  state <- readTVar stateVar
  if Map.member point (imEbPoints state)
    then pure []
    else do
      modifyTVar stateVar $ \s ->
        s{imEbPoints = Map.insert point ebBytesSize (imEbPoints s)}
      case Map.lookup point.pointEbHash (imEbBodies state) of
        Nothing -> pure []
        Just storedBody -> do
          modifyTVar stateVar $ \s ->
            s
              { -- This point has acquired the body, so its size is the body's
                -- actual size rather than the one just announced.
                imEbPoints =
                  Map.insert point (sebBytesSize storedBody) (imEbPoints s)
              , imEbBodiesDownloaded = Set.insert point (imEbBodiesDownloaded s)
              }
          writeTChan notificationChan $
            AcquiredEb point (sebBytesSize storedBody)
          if hashCompleteIn state point.pointEbHash
            then do
              modifyTVar stateVar $ \s ->
                s{imCompletedEbs = Set.insert point (imCompletedEbs s)}
              writeTChan notificationChan $ AcquiredEbTxs point
              pure [point]
            else pure []

imLookupEbBody :: IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m [(TxHash, BytesSize)]
imLookupEbBody stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  case Map.lookup ebHash (imEbBodies state) of
    Nothing -> pure []
    Just storedBody ->
      pure
        [ (eteTxHash e, eteTxBytesSize e)
        | e <- IntMap.elems (sebTxEntries storedBody)
        ]

imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  LeiosPoint ->
  LeiosEb ->
  [LocalFill] ->
  m (CompletedEbs, [TxOffset])
imInsertEbBody stateVar notificationChan point eb fills = do
  let items = leiosEbBodyItems eb
      ebBytesSize = encodeLeiosEbSize eb
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  atomically $ do
    let entries =
          IntMap.fromList
            [ ( offset
              , EbTxEntry
                  { eteTxHash = txHash
                  , eteTxBytesSize = size
                  }
              )
            | (offset, txHash, size) <- items
            ]
    modifyTVar stateVar $ \s ->
      s
        { -- The tx-hash list is fully determined by the EB content
          -- hash, so a second insertion at the same hash is a no-op.
          imEbBodies =
            Map.insertWith
              (\_ old -> old)
              point.pointEbHash
              (StoredEbBody ebBytesSize entries)
              (imEbBodies s)
        , -- This point has acquired the body, so its size becomes the body's
          -- actual size, whatever size it was announced at.
          imEbPoints =
            Map.insert point ebBytesSize (imEbPoints s)
        , -- Mark this point as downloaded. Other points referencing
          -- the same EB hash are unaffected; each needs its own
          -- 'writeEbBody' to be considered complete.
          imEbBodiesDownloaded =
            Set.insert point (imEbBodiesDownloaded s)
        }
    -- Cross-EB fills: copy locally-held bytes into this EB's closure, before
    -- completion is judged. Same guards as the SQLite backend: the row must be
    -- one the body declared, not yet filled, the source must hold the bytes
    -- and declare the SAME tx hash (a stale location resolving to the wrong
    -- EB fills nothing), and the size must match the declaration.
    st0 <- readTVar stateVar
    let ebHash = point.pointEbHash
        declared = maybe IntMap.empty sebTxEntries (Map.lookup ebHash (imEbBodies st0))
        held = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes st0)
        accepted =
          IntMap.fromList
            [ (dstOff, bytes)
            | (dstOff, MkTxLocation srcEb srcOff) <- fills
            , Just e <- [IntMap.lookup dstOff declared]
            , not (IntMap.member dstOff held)
            , Just srcE <-
                [IntMap.lookup srcOff (maybe IntMap.empty sebTxEntries (Map.lookup srcEb (imEbBodies st0)))]
            , eteTxHash srcE == eteTxHash e
            , Just bytes <-
                [IntMap.lookup srcOff (Map.findWithDefault IntMap.empty srcEb (imEbTxBytes st0))]
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            ]
    modifyTVar stateVar $ \s ->
      s{imEbTxBytes = Map.insertWith IntMap.union ebHash accepted (imEbTxBytes s)}
    writeTChan notificationChan $ AcquiredEb point ebBytesSize
    -- If every tx referenced by this body is already present, the closure
    -- is complete the moment the body lands — no subsequent
    -- 'writeTxs' will fire for this point, so we must notify here.
    -- Only trigger for a novel point ('imCompletedEbs' is our idempotency
    -- guard). This mirrors what 'imInsertTxs' does when the last missing
    -- tx of an already-downloaded body arrives.
    state <- readTVar stateVar
    let allTxsPresent = hashCompleteIn state point.pointEbHash
        alreadyNotified = Set.member point (imCompletedEbs state)
    completed <-
      if allTxsPresent && not alreadyNotified
        then do
          modifyTVar stateVar $ \s ->
            s{imCompletedEbs = Set.insert point (imCompletedEbs s)}
          writeTChan notificationChan $ AcquiredEbTxs point
          pure [point]
        else pure []
    pure (completed, IntMap.keys accepted)

-- | Whether every offset of this content hash's body has its bytes.
hashCompleteIn :: InMemoryLeiosDb -> EbHash -> Bool
hashCompleteIn state h = case Map.lookup h (imEbBodies state) of
  Nothing -> False
  Just storedBody ->
    let bytes = Map.findWithDefault IntMap.empty h (imEbTxBytes state)
     in IntMap.keysSet (sebTxEntries storedBody) `IntSet.isSubsetOf` IntMap.keysSet bytes

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m CompletedEbs
imInsertTxs stateVar notificationChan point offBytes = atomically $ do
  let ebHash = pointEbHash point
  -- Mirrors the SQLite backend's guarded in-place fill: a fill only lands on a
  -- pre-allocated row, i.e. an offset the body declared, once, at the declared
  -- size. Anything else changes nothing -- in particular, bytes arriving
  -- before the body are dropped (production cannot produce them: the writer
  -- queue is FIFO and tx writes follow their body write).
  modifyTVar stateVar $ \s ->
    let declared = maybe IntMap.empty sebTxEntries (Map.lookup ebHash (imEbBodies s))
        held = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes s)
        accepted =
          IntMap.fromList
            [ (off, bytes)
            | (off, bytes) <- offBytes
            , Just e <- [IntMap.lookup off declared]
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            , not (IntMap.member off held)
            ]
     in s{imEbTxBytes = Map.insertWith IntMap.union ebHash accepted (imEbTxBytes s)}
  state <- readTVar stateVar
  -- Candidates: every point of THIS content hash whose body has been
  -- downloaded and whose closure is now complete. Bytes are per-EB, so no
  -- other hash can have been affected.
  --
  -- 'hashCompleteIn' does not depend on the point, so judge the closure once
  -- rather than once per downloaded point. A batch that does not complete the
  -- closure -- the common case while a multi-batch fetch is in flight -- then
  -- scans nothing, which keeps a catching-up node's per-write cost off the
  -- unboundedly-growing 'imEbBodiesDownloaded' set (otherwise every tx write is
  -- O(downloaded EBs), quadratic over a sync).
  let candidates
        | hashCompleteIn state ebHash =
            [ p
            | p <- Set.toList (imEbBodiesDownloaded state)
            , pointEbHash p == ebHash
            ]
        | otherwise = []
      completed =
        filter
          (\p -> not (Set.member p (imCompletedEbs state)))
          candidates
  modifyTVar stateVar $ \s ->
    s
      { imCompletedEbs =
          foldr Set.insert (imCompletedEbs s) completed
      }
  -- Emit a closure-completion notification for each newly-complete EB. The
  -- ChainDB subscribes to these to grow the acquired-EB-closures set it owns.
  forM_ completed $ \p ->
    writeTChan notificationChan $ AcquiredEbTxs p
  pure completed

-- | Implements 'scanCompleteEbClosuresNotOlderThanSlot': the already-completed EBs
-- announced no older than the given slot.
--
-- Derived from 'imCompletedEbs': an EB's greatest announcer slot is no older
-- than @sinceSlot@ exactly when at least one of its completed points has a slot
-- @>= sinceSlot@, so we collect (and dedupe) the hashes of those points. This
-- is precise across all of an EB's announcer slots, as in the SQLite backend.
imScanCompleteEbClosuresSince ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m [LeiosPoint]
imScanCompleteEbClosuresSince stateVar sinceSlot = atomically $ do
  s <- readTVar stateVar
  pure
    [ p
    | p <- Set.toList (imCompletedEbs s)
    , pointSlotNo p >= sinceSlot
    ]

imBatchRetrieveTxs ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
imBatchRetrieveTxs stateVar ebHash offsets = atomically $ do
  state <- readTVar stateVar
  case Map.lookup ebHash (imEbBodies state) of
    Nothing -> pure []
    Just storedBody ->
      pure
        [ (offset, eteTxHash entry, IntMap.lookup offset bytes)
        | offset <- offsets
        , Just entry <- [IntMap.lookup offset (sebTxEntries storedBody)]
        ]
     where
      bytes = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes state)

imLookupTrustedEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe [(TxHash, ByteString)])
imLookupTrustedEbClosure stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  case Map.lookup ebHash (imEbBodies state) of
    Nothing -> pure Nothing
    Just storedBody ->
      let entries = sebTxEntries storedBody
          bytes = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes state)
          txClosure =
            [ (eteTxHash e, tx)
            | (off, e) <- IntMap.toAscList entries
            , tx <- maybeToList (IntMap.lookup off bytes)
            ]
       in if IntMap.size entries == length txClosure
            then pure (Just txClosure)
            else pure Nothing
