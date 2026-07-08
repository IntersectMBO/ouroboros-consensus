{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBodyItems
  , leiosEbBytesSize
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks (..)
  , atomically
  )

-- | In-memory database state.
data InMemoryLeiosDb = InMemoryLeiosDb
  { imTxs :: !(Map TxHash (ByteString, BytesSize))
  -- ^ Global transaction storage.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their expected sizes. The same EB
  -- content can be inserted at multiple slots; each
  -- @(slot, hash)@ point has its own entry and completes
  -- independently.
  , imEbBodiesDownloaded :: !(Set LeiosPoint)
  -- ^ Subset of 'imEbPoints' for which 'leiosDbInsertEbBody' has been
  -- called. Only these points are considered for completion.
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  -- ^ EB tx-hash list, keyed by content hash. Shared across every
  -- point that references the same EB.
  , imCompletedEbs :: !(Set LeiosPoint)
  -- ^ Points for which 'AcquiredEbTxs' has already been broadcast.
  -- The completion predicate stays true once an EB is complete, so
  -- any later batch containing one of its txs would re-trigger it
  -- without this guard; downstream consumers (e.g. 'runLeiosVoting')
  -- treat the re-notification as fatal ('AlreadyKnown' from
  -- 'addVote'). Each point carries the EB's hash and its announcer slot,
  -- so this is also the source for
  -- 'leiosDbScanCompleteEbClosuresNotOlderThanSlot'.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty

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
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , -- This is consulted only once, at ChainDB open, before any mini-protocol
        -- runs (see 'Ouroboros.Consensus.Storage.ChainDB.Impl.openDBInternal').
        -- An in-memory DB has no on-disk persistence, so for a fresh process it
        -- is empty at that point and this could simply be @pure []@. We keep the
        -- real scan because the ThreadNet harness persists the 'stateVar' across
        -- simulated node restarts (it lives in the per-node 'NodeInfo' alongside
        -- the MockFS DBs; see @Test.ThreadNet.Network@), so on a restart the
        -- ChainDB reopens against a non-empty DB and this seeds the restored
        -- acquired-EB-closures set — the restart-recovery path.
        leiosDbScanCompleteEbClosuresNotOlderThanSlot = imScanCompleteEbClosuresSince stateVar
      , -- No-op for now; see 'leiosDbGarbageCollect'.
        leiosDbGarbageCollect = \_slotNo -> pure ()
      , -- No-op for now; see 'leiosDbPromoteToImmutable'.
        leiosDbPromoteToImmutable = \_point -> pure ()
      , open =
          pure $
            LeiosDbConnection
              { close = pure ()
              , leiosDbScanEbPoints = imScanEbPoints stateVar
              , leiosDbInsertEbPoint = imInsertEbPoint stateVar
              , leiosDbLookupEbBody = imLookupEbBody stateVar
              , leiosDbInsertEbBody = imInsertEbBody stateVar notificationChan
              , leiosDbInsertTxs = imInsertTxs stateVar notificationChan
              , leiosDbBatchRetrieveTxs = imBatchRetrieveTxs stateVar
              , leiosDbFilterMissingEbBodies = imFilterMissingEbBodies stateVar
              , leiosDbFilterMissingTxs = imFilterMissingTxs stateVar
              , leiosDbLookupEbClosure = imLookupEbClosure stateVar
              }
      }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EbHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (p.pointSlotNo, p.pointEbHash)
    | p <- Map.keys (imEbPoints state)
    ]

-- | Insert an announced EB point. Idempotent: a second insert at the
-- same point keeps the first-seen size.
imInsertEbPoint :: IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> BytesSize -> m ()
imInsertEbPoint stateVar point ebBytesSize = atomically $
  modifyTVar stateVar $ \s ->
    s{imEbPoints = Map.insertWith (\_ old -> old) point ebBytesSize (imEbPoints s)}

imLookupEbBody :: IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m [(TxHash, BytesSize)]
imLookupEbBody stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  case Map.lookup ebHash (imEbBodies state) of
    Nothing -> pure []
    Just offsetMap ->
      pure
        [ (eteTxHash e, eteTxBytesSize e)
        | e <- IntMap.elems offsetMap
        ]

imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  LeiosPoint ->
  LeiosEb ->
  m ()
imInsertEbBody stateVar notificationChan point eb = do
  let items = leiosEbBodyItems eb
      ebBytesSize = leiosEbBytesSize eb
  when (null items) $
    error "leiosDbInsertEbBody: empty EB body (programmer error)"
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
            Map.insertWith (\_ old -> old) point.pointEbHash entries (imEbBodies s)
        , -- Mark this point as downloaded. Other points referencing
          -- the same EB hash are unaffected; each needs its own
          -- 'leiosDbInsertEbBody' to be considered complete.
          imEbBodiesDownloaded =
            Set.insert point (imEbBodiesDownloaded s)
        }
    writeTChan notificationChan $ AcquiredEb point ebBytesSize

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  [(TxHash, ByteString)] ->
  m CompletedEbs
imInsertTxs stateVar notificationChan txs = atomically $ do
  let insertedTxHashes = [txHash | (txHash, _) <- txs]
  forM_ txs $ \(txHash, txBytes) -> do
    let txBytesSize = fromIntegral $ BS.length txBytes
    modifyTVar stateVar $ \s ->
      if Map.member txHash (imTxs s)
        then s
        else s{imTxs = Map.insert txHash (txBytes, txBytesSize) (imTxs s)}
  state <- readTVar stateVar
  -- Candidates: every point whose body has been downloaded, whose
  -- hash is touched by this batch, and whose closure is now complete.
  -- A point not in 'imEbBodiesDownloaded' is skipped — its body
  -- hasn't been inserted, so it can't be complete. Two points
  -- referencing the same EB hash both light up if both have had
  -- their body inserted.
  let touchedByBatch =
        Set.fromList
          [ ebHash
          | (ebHash, entries) <- Map.toList (imEbBodies state)
          , any (\e -> eteTxHash e `elem` insertedTxHashes) (IntMap.elems entries)
          ]
      hashComplete h = case Map.lookup h (imEbBodies state) of
        Nothing -> False
        Just entries ->
          all (\e -> Map.member (eteTxHash e) (imTxs state)) (IntMap.elems entries)
      candidates =
        [ point
        | point <- Set.toList (imEbBodiesDownloaded state)
        , Set.member (pointEbHash point) touchedByBatch
        , hashComplete (pointEbHash point)
        ]
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
  forM_ completed $ \point ->
    writeTChan notificationChan (AcquiredEbTxs point)
  pure completed

-- | Implements 'leiosDbScanCompleteEbClosuresNotOlderThanSlot': the already-completed EBs
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
    Just offsetMap ->
      pure
        [ (offset, eteTxHash entry, fst <$> Map.lookup (eteTxHash entry) (imTxs state))
        | offset <- offsets
        , Just entry <- [IntMap.lookup offset offsetMap]
        ]

imFilterMissingEbBodies ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> [LeiosPoint] -> m [LeiosPoint]
imFilterMissingEbBodies stateVar points = atomically $ do
  state <- readTVar stateVar
  pure [p | p <- points, not $ Map.member p.pointEbHash (imEbBodies state)]

imFilterMissingTxs :: IOLike m => StrictTVar m InMemoryLeiosDb -> [TxHash] -> m [TxHash]
imFilterMissingTxs stateVar txHashes = atomically $ do
  state <- readTVar stateVar
  pure [txHash | txHash <- txHashes, not $ Map.member txHash (imTxs state)]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe [(TxHash, ByteString)])
imLookupEbClosure stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  case Map.lookup ebHash (imEbBodies state) of
    Nothing -> pure Nothing
    Just entries ->
      let ebTxHashes = [eteTxHash e | e <- IntMap.elems entries]
          txClosure =
            [ (ebTxHash, tx)
            | ebTxHash <- ebTxHashes
            , (tx, _txSize) <- maybeToList (Map.lookup ebTxHash (imTxs state))
            ]
       in if length ebTxHashes == length txClosure
            then pure (Just txClosure)
            else pure Nothing
