{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LeiosDB.Impl.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  ) where

import Cardano.Prelude (Generic, forM_, unless, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , StrictTVar
  , dupTChan
  , modifyTVar
  , newBroadcastTChan
  , newTVarIO
  , readTVar
  , writeTChan
  , writeTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , TxOffset
  , encodeLeiosEbSize
  , leiosEbBodyItems
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (LeiosDbWriteException)
  , LeiosDbWriteFailure (..)
  , throwLeiosDbException
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
  { imEbTxBytes :: !(Map EbHash (IntMap {- txOffset -} ByteString))
  -- ^ Tx bytes owned by the referencing EB, keyed by offset -- mirrors the
  -- SQLite backend's @ebTxBytes@ table. A tx shared by two EBs is stored
  -- twice, deliberately.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their expected sizes. The same EB
  -- content can be inserted at multiple slots; each
  -- @(slot, hash)@ point has its own entry and completes
  -- independently.
  , imEbBodiesDownloaded :: !(Set LeiosPoint)
  -- ^ Subset of 'imEbPoints' for which 'writeEbBody' has been
  -- called. Only these points are considered for completion.
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  -- ^ EB tx-hash list, keyed by EB hash. Shared across every
  -- point that references the same EB.
  , imCompletedEbs :: !(Set LeiosPoint)
  -- ^ Points for which 'AcquiredEbTxs' has already been broadcast.
  -- The completion predicate stays true once an EB is complete, so
  -- any later batch containing one of its txs would re-trigger it
  -- without this guard; downstream consumers (e.g. 'runLeiosVoting')
  -- treat the re-notification as fatal ('AlreadyKnown' from
  -- 'addVote'). Each point carries the EB's hash and its announcer slot,
  -- so this is also the source for
  -- 'scanCompleteEbClosuresNotOlderThanSlot'.
  , imImmutableEbBodies :: !(Map EbHash (IntMap EbTxEntry))
  -- ^ The bodies of the EBs 'leiosDbPromoteToImmutable' promoted. The garbage
  -- collection evicts such an EB from the other fields like any other, and the
  -- reads fall back to this and 'imImmutableEbTxBytes'.
  , imImmutableEbTxBytes :: !(Map EbHash (IntMap {- txOffset -} ByteString))
  -- ^ The tx bytes of the bodies in 'imImmutableEbBodies', by offset.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty mempty mempty

-- | EB transaction entry (no bytes stored here, see 'imEbTxBytes')
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
        closeLeiosDbHandle = pure ()
      , subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect = imGarbageCollect stateVar
      , leiosDbPromoteToImmutable = imPromoteToImmutable stateVar
      , -- The in-memory implementation does not track stats.
        leiosDbSampleStats = pure (LeiosDbStats 0 0 0)
      , openReader = openInMemoryReader stateVar
      , openWriter = openInMemoryWriter stateVar notificationChan
      }

-- | Copy the closures of the given EBs to 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes', unless they are there already. The node pins only what
-- the immutable chain certifies, and only once it holds its closure, so a
-- closure that is incomplete is a caller's bug: this then fails, changing
-- nothing.
imPromoteToImmutable :: IOLike m => StrictTVar m InMemoryLeiosDb -> [LeiosPoint] -> m ()
imPromoteToImmutable stateVar points = do
  incomplete <- atomically $ do
    s <- readTVar stateVar
    let closures =
          Map.fromSet
            ( \ebHash -> do
                entries <- Map.lookup ebHash (imEbBodies s)
                let txBytes = volatileTxBytes s ebHash
                _ <- closureWith txBytes entries
                pure (entries, txBytes)
            )
            ( Set.fromList (map pointEbHash points)
                `Set.difference` Map.keysSet (imImmutableEbBodies s)
            )
        copied = Map.mapMaybe id closures
        incomplete = Map.keys (Map.filter isNothing closures)
    when (null incomplete) $
      writeTVar
        stateVar
        s
          { imImmutableEbBodies = imImmutableEbBodies s <> fmap fst copied
          , imImmutableEbTxBytes = imImmutableEbTxBytes s <> fmap snd copied
          }
    pure incomplete
  unless (null incomplete) $
    throwLeiosDbException $
      "leiosDbPromoteToImmutable: incomplete closure (programmer error): " <> show incomplete

-- | Evict every EB all of whose points are older than the slot, with its body
-- and tx bytes. A promoted EB keeps its copy in 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes'.
imGarbageCollect :: IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m ()
imGarbageCollect stateVar slot = atomically $ modifyTVar stateVar $ \s ->
  let newestPointSlots =
        Map.fromListWith max [(pointEbHash p, pointSlotNo p) | p <- Map.keys (imEbPoints s)]
      evicted = Map.keysSet (Map.filter (< slot) newestPointSlots)
      kept p = pointEbHash p `Set.notMember` evicted
   in s
        { imEbPoints = Map.filterWithKey (\p _ -> kept p) (imEbPoints s)
        , imEbBodiesDownloaded = Set.filter kept (imEbBodiesDownloaded s)
        , imEbBodies = Map.withoutKeys (imEbBodies s) evicted
        , imEbTxBytes = Map.withoutKeys (imEbTxBytes s) evicted
        , imCompletedEbs = Set.filter kept (imCompletedEbs s)
        }

-- | Reads off the 'StrictTVar'. Nothing to open or close.
openInMemoryReader :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbReader m)
openInMemoryReader stateVar =
  pure
    LeiosDbReader
      { closeReader = pure ()
      , lookupEbBody = imLookupEbBody stateVar
      , lookupEbClosure = imLookupEbClosure stateVar
      , batchRetrieveTxs = imBatchRetrieveTxs stateVar
      , testScanEbPoints = imScanEbPoints stateVar
      , -- ThreadNet persists 'stateVar' across simulated restarts, so on
        -- restart this seeds the restored acquired-EB-closures set.
        scanCompleteEbClosuresNotOlderThanSlot = imScanCompleteEbClosuresSince stateVar
      }

-- | No worker and no queue: the state is a 'StrictTVar', so each write runs
-- at submission and its 'Promise' comes back already resolved. A failure is
-- thrown at submission, so a caller that does not await sees it.
openInMemoryWriter ::
  forall m.
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  StrictTChan m LeiosEbNotification ->
  m (LeiosDbWriter m)
openInMemoryWriter stateVar notificationChan =
  pure
    LeiosDbWriter
      { closeWriter = pure ()
      , writeEbPoint = \point ebBytesSize ->
          resolved ("WriteEbPoint " <> show point) (imInsertEbPoint stateVar point ebBytesSize)
      , writeEbBody = \point eb ->
          resolved ("WriteEbBody " <> show point) (imInsertEbBody stateVar notificationChan point eb)
      , writeTxs = \point txs ->
          resolved
            ("WriteTxs " <> show point <> " (" <> show (length txs) <> " txs)")
            (imInsertTxs stateVar notificationChan point txs)
      }
 where
  resolved :: HasCallStack => String -> m a -> m (Promise m a)
  resolved job action = do
    result <- try action
    case result of
      Right x -> pure $ Promise $ pure x
      Left cause ->
        throwIO $
          LeiosDbWriteException
            LeiosDbWriteFailure
              { ldwfWriteJob = job
              , ldwfSubmittedFrom = GHC.prettyCallStack GHC.callStack
              , ldwfWriteFailure = toException (cause :: LeiosDbException)
              }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EbHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (pointSlotNo p, pointEbHash p)
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
  case Map.lookup ebHash (imEbBodies state) <|> Map.lookup ebHash (imImmutableEbBodies state) of
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
  m CompletedEbs
imInsertEbBody stateVar notificationChan point eb = do
  let items = leiosEbBodyItems eb
      ebBytesSize = encodeLeiosEbSize eb
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  known <- Map.member point . imEbPoints <$> atomically (readTVar stateVar)
  unless known $
    throwLeiosDbException "writeEbBody: point not registered (programmer error)"
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
            Map.insertWith (\_ old -> old) (pointEbHash point) entries (imEbBodies s)
        , -- Mark this point as downloaded. Other points referencing
          -- the same EB hash are unaffected; each needs its own
          -- 'writeEbBody' to be considered complete.
          imEbBodiesDownloaded =
            Set.insert point (imEbBodiesDownloaded s)
        }
    writeTChan notificationChan $ AcquiredEb point ebBytesSize
    -- If every tx referenced by this body is already present, the closure
    -- is complete the moment the body lands — no subsequent
    -- 'writeTxs' will fire for this point, so we must notify here.
    -- Only trigger for a novel point ('imCompletedEbs' is our idempotency
    -- guard). This mirrors what 'imInsertTxs' does when the last missing
    -- tx of an already-downloaded body arrives.
    state <- readTVar stateVar
    let allTxsPresent = hashCompleteIn state (pointEbHash point)
        alreadyNotified = Set.member point (imCompletedEbs state)
    if allTxsPresent && not alreadyNotified
      then do
        modifyTVar stateVar $ \s ->
          s{imCompletedEbs = Set.insert point (imCompletedEbs s)}
        writeTChan notificationChan (AcquiredEbTxs point)
        pure [point]
      else pure []

-- | Whether every offset of this EB body has its bytes.
hashCompleteIn :: InMemoryLeiosDb -> EbHash -> Bool
hashCompleteIn state h = case Map.lookup h (imEbBodies state) of
  Nothing -> False
  Just entries ->
    let txBytesByOffset = Map.findWithDefault IntMap.empty h (imEbTxBytes state)
     in IntMap.keysSet entries `IntSet.isSubsetOf` IntMap.keysSet txBytesByOffset

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
  -- size. Anything else changes nothing.
  modifyTVar stateVar $ \s ->
    let declared = Map.findWithDefault IntMap.empty ebHash (imEbBodies s)
        held = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes s)
        accepted =
          IntMap.fromList
            [ (off, bytes)
            | (off, bytes) <- offBytes
            , Just e <- [IntMap.lookup off declared]
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            , not (IntMap.member off held)
            ]
     in if IntMap.null accepted
          then s
          else s{imEbTxBytes = Map.insertWith IntMap.union ebHash accepted (imEbTxBytes s)}
  state <- readTVar stateVar
  -- Candidates: every point of THIS EB whose body has been downloaded and
  -- whose closure is now complete. Tx bytes are not shared among overlapping
  -- EBs, so no other hash can have been affected.
  --
  -- 'hashCompleteIn' does not depend on the point, so judge the closure once
  -- rather than once per downloaded point. A batch that does not complete the
  -- closure -- the common case while a multi-batch fetch is in flight -- then
  -- scans nothing, which keeps the growth of the 'imEbBodiesDownloaded' set
  -- from increasing the per-write cost.
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
    writeTChan notificationChan (AcquiredEbTxs p)
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

-- | This falls back to the immutable copy only when 'imEbBodies' lacks the
-- body, even if that body's closure is incomplete. The SQLite implementation
-- has the same shortcoming (see the TODO on @sqlBatchRetrieveTxs@).
imBatchRetrieveTxs ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
imBatchRetrieveTxs stateVar ebHash offsets = atomically $ do
  state <- readTVar stateVar
  pure $ case Map.lookup ebHash (imEbBodies state) of
    Just offsetMap -> retrieve offsetMap (Map.findWithDefault IntMap.empty ebHash (imEbTxBytes state))
    Nothing -> case Map.lookup ebHash (imImmutableEbBodies state) of
      Just offsetMap -> retrieve offsetMap (Map.findWithDefault IntMap.empty ebHash (imImmutableEbTxBytes state))
      Nothing -> []
 where
  retrieve offsetMap txBytesByOffset =
    [ (offset, eteTxHash entry, IntMap.lookup offset txBytesByOffset)
    | offset <- offsets
    , Just entry <- [IntMap.lookup offset offsetMap]
    ]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe [(TxHash, ByteString)])
imLookupEbClosure stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $ case Map.lookup ebHash (imEbBodies state) >>= closureWith (volatileTxBytes state ebHash) of
    Just closure -> Just closure
    Nothing ->
      Map.lookup ebHash (imImmutableEbBodies state)
        >>= closureWith (Map.findWithDefault IntMap.empty ebHash (imImmutableEbTxBytes state))

-- | The EB's tx bytes by offset, from 'imEbTxBytes'.
volatileTxBytes :: InMemoryLeiosDb -> EbHash -> IntMap ByteString
volatileTxBytes s ebHash = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes s)

-- | The body's closure, if the tx bytes hold each of its transactions.
closureWith :: IntMap ByteString -> IntMap EbTxEntry -> Maybe [(TxHash, ByteString)]
closureWith txBytesByOffset entries =
  traverse
    (\(offset, e) -> (,) (eteTxHash e) <$> IntMap.lookup offset txBytesByOffset)
    (IntMap.toAscList entries)
