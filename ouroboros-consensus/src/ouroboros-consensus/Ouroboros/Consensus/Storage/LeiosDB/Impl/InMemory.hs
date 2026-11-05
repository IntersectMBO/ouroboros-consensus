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

import Cardano.Prelude (Generic, unless, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  , hashLeiosTx
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LocalFill
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
  { imEbTxBytes :: !(Map LeiosPoint (IntMap {- txOffset -} ByteString))
  -- ^ Tx bytes owned by the point, keyed by offset -- the SQLite backend's
  -- @txs@ column, one tx at a time. A tx shared by two EBs, or two points of
  -- an EB, is stored twice, deliberately.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their expected sizes. The same EB
  -- content can be inserted at multiple slots; each
  -- @(slot, hash)@ point has its own entry.
  , imEbBodies :: !(Map EndorserBlockHash StoredBody)
  -- ^ EB bodies, keyed by EB hash. Shared across every point that references
  -- the same EB.
  , imImmutableEbBodies :: !(Map LeiosPoint StoredBody)
  -- ^ The bodies of the EBs 'leiosDbPromoteToImmutable' promoted, at the
  -- points they were promoted at. The garbage collection evicts such an EB from
  -- the other fields like any other.
  , imImmutableEbTxBytes :: !(Map LeiosPoint (IntMap {- txOffset -} ByteString))
  -- ^ The tx bytes of the bodies in 'imImmutableEbBodies', by offset.
  , imGcSlot :: !SlotNo
  -- ^ The slot of the latest garbage collection. The writes of a point before
  -- it are dropped, and its reads go to 'imImmutableEbBodies' and
  -- 'imImmutableEbTxBytes'.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty 0

-- | An EB body: the bytes it arrived in, as the SQLite backend's @body@
-- column, and the txs it lists by offset.
data StoredBody = StoredBody
  { sbBytes :: !ByteString
  , sbTxs :: !(IntMap {- txOffset -} EbTxEntry)
  }
  deriving stock Generic
  deriving anyclass NoThunks

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
newLeiosDBInMemoryWith stateVar =
  pure $
    LeiosDbHandle
      { -- Nothing to close: the state is a 'StrictTVar'.
        closeLeiosDbHandle = pure ()
      , garbageCollect = imGarbageCollect stateVar
      , leiosDbPromoteToImmutable = imPromoteToImmutable stateVar
      , -- The in-memory implementation does not track stats.
        leiosDbSampleStats = pure (LeiosDbStats 0 0 0)
      , openReader = openInMemoryReader stateVar
      , openWriter = openInMemoryWriter stateVar
      }

-- | Copy the closures of the given EBs to 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes', unless they are there already. The node promotes only what
-- the immutable chain certifies, and only once it holds its closure, so a
-- closure that is incomplete is a caller's bug: this then fails, changing
-- nothing.
imPromoteToImmutable :: IOLike m => StrictTVar m InMemoryLeiosDb -> [LeiosPoint] -> m ()
imPromoteToImmutable stateVar points = do
  incomplete <- atomically $ do
    s <- readTVar stateVar
    let promoted = Set.fromList (map pointEbHash (Map.keys (imImmutableEbBodies s)))
        -- The first point given of each EB not promoted yet.
        closures =
          Map.fromList
            [ (pointEbHash point, (point, closureAt point))
            | point <- reverse points
            , Set.notMember (pointEbHash point) promoted
            ]
        closureAt point = do
          body <- Map.lookup (pointEbHash point) (imEbBodies s)
          let txBytes = volatileTxBytes s point
          if hashesMatch txBytes body then Just (body, txBytes) else Nothing
        copied = Map.fromList [(point, closure) | (point, Just closure) <- Map.elems closures]
        incomplete = [point | (point, Nothing) <- Map.elems closures]
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
-- and tx bytes, and drop the older points of every other EB. A promoted EB
-- keeps its copy in 'imImmutableEbBodies' and 'imImmutableEbTxBytes'.
imGarbageCollect :: IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m ()
imGarbageCollect stateVar slot = atomically $ modifyTVar stateVar $ \s ->
  let newestPointSlots =
        Map.fromListWith max [(pointEbHash p, pointSlotNo p) | p <- Map.keys (imEbPoints s)]
      evicted = Map.keysSet (Map.filter (< slot) newestPointSlots)
   in s
        { imEbPoints = Map.filterWithKey (\p _ -> pointSlotNo p >= slot) (imEbPoints s)
        , imEbBodies = Map.withoutKeys (imEbBodies s) evicted
        , imEbTxBytes = Map.filterWithKey (\p _ -> pointSlotNo p >= slot) (imEbTxBytes s)
        , imGcSlot = max slot (imGcSlot s)
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
-- captured rather than thrown here, so it surfaces at 'await' exactly like
-- the SQLite backend's.
openInMemoryWriter ::
  forall m.
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  m (LeiosDbWriter m)
openInMemoryWriter stateVar =
  pure
    LeiosDbWriter
      { closeWriter = pure ()
      , writeEbBody = \point eb fills ->
          resolved ("WriteEbBody " <> show point) (imInsertEbBody stateVar point eb fills)
      , writeTxs = \point txs ->
          resolved
            ("WriteTxs " <> show point <> " (" <> show (length txs) <> " txs)")
            (imInsertTxs stateVar point txs)
      }
 where
  resolved :: HasCallStack => String -> m a -> m (Promise m a)
  resolved job action = do
    result <- try action
    pure $ Promise $ case result of
      Right x -> pure x
      Left cause ->
        throwIO $
          LeiosDbWriteException
            LeiosDbWriteFailure
              { ldwfWriteJob = job
              , ldwfSubmittedFrom = GHC.prettyCallStack GHC.callStack
              , ldwfWriteFailure = toException (cause :: LeiosDbException)
              }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EndorserBlockHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (pointSlotNo p, pointEbHash p)
    | p <- Map.keys (imEbPoints state)
    ]

-- | Insert a point, unless the latest garbage collection collects it.
-- Idempotent: a second insert at the same point keeps the first-seen size.
insertPoint :: LeiosPoint -> BytesSize -> InMemoryLeiosDb -> InMemoryLeiosDb
insertPoint point ebBytesSize s
  | pointSlotNo point < imGcSlot s = s
  | otherwise = s{imEbPoints = Map.insertWith (\_ old -> old) point ebBytesSize (imEbPoints s)}

imLookupEbBody ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe ByteString)
imLookupEbBody stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ sbBytes <$> bodyAt state point

-- | Mirrors the SQLite backend: the point if not written yet, then the body,
-- returning the offsets the fills filled. Nothing, if the latest garbage
-- collection collects the point.
imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  m [TxOffset]
imInsertEbBody stateVar point eb fills = do
  when (null refs) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  atomically $ do
    collected <- (pointSlotNo point <) . imGcSlot <$> readTVar stateVar
    if collected then pure [] else fill
 where
  fill = do
    modifyTVar stateVar $ \s ->
      (insertPoint point (fromIntegral (BS.length (sbBytes body))) s)
        { -- The body is fully determined by the EB content hash, so a second
          -- insertion at the same hash is a no-op.
          imEbBodies =
            Map.insertWith (\_ old -> old) ebHash body (imEbBodies s)
        }
    s <- readTVar stateVar
    let
      -- Mirrors the SQLite backend: a fill lands only on an offset the body
      -- declares, at its declared size, from a point of the source holding
      -- bytes that hash to the tx this body lists there, the first of the
      -- fills winning an offset.
      filled =
        IntMap.fromListWith
          (\_new old -> old)
          [ (off, bytes)
          | (off, MkTxLocation srcHash srcOff) <- fills
          , Just e <- [IntMap.lookup off refs]
          , bytes : _ <-
              [ [ tx
                | (p, txs) <- Map.toList (imEbTxBytes s)
                , pointEbHash p == srcHash
                , Just tx <- [IntMap.lookup srcOff txs >>= nonZero]
                ]
              ]
          , hashLeiosTx (MkLeiosTx bytes) == eteTxHash e
          , fromIntegral (BS.length bytes) == eteTxBytesSize e
          ]
    unless (IntMap.null filled) $
      modifyTVar stateVar $ \s' ->
        s'{imEbTxBytes = Map.insertWith IntMap.union point filled (imEbTxBytes s')}
    pure (IntMap.keys filled)
  ebHash = pointEbHash point
  refs =
    IntMap.fromList
      [ (offset, EbTxEntry{eteTxHash = txHash, eteTxBytesSize = size})
      | (offset, (txHash, size)) <- zip [0 ..] (endorserBlockReferences eb)
      ]
  body = StoredBody{sbBytes = serialiseEndorserBlock eb, sbTxs = refs}

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m [TxOffset]
imInsertTxs stateVar point offBytes = atomically $ do
  let ebHash = pointEbHash point
  -- Mirrors the SQLite backend's in-place fill: a tx only lands at an offset
  -- the body declared at the point, at the declared size, the first of a write
  -- winning.
  declared <-
    ( \s ->
        maybe
          IntMap.empty
          sbTxs
          (guard (Map.member point (imEbPoints s)) *> Map.lookup ebHash (imEbBodies s))
    )
      <$> readTVar stateVar
  let inBody = [(off, bytes, e) | (off, bytes) <- offBytes, Just e <- [IntMap.lookup off declared]]
      sized (_, bytes, e) = fromIntegral (BS.length bytes) == eteTxBytesSize e
      accepted = IntMap.fromListWith (\_new old -> old) [(off, bytes) | tx@(off, bytes, _) <- inBody, sized tx]
  unless (IntMap.null accepted) $
    modifyTVar stateVar $ \s ->
      s{imEbTxBytes = Map.insertWith IntMap.union point accepted (imEbTxBytes s)}
  pure [off | tx@(off, _, _) <- inBody, not (sized tx)]

-- | Implements 'scanCompleteEbClosuresNotOlderThanSlot': each point no older
-- than the given slot whose closure is complete where its reads go
-- ('readsImmutable'). A volatile closure is complete if each of its txs hashes
-- to what the body lists, and a promoted one is, since only complete EBs are
-- promoted.
imScanCompleteEbClosuresSince ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m [LeiosPoint]
imScanCompleteEbClosuresSince stateVar sinceSlot = atomically $ do
  s <- readTVar stateVar
  let isComplete p
        | pointSlotNo p < imGcSlot s = Map.member p (imImmutableEbBodies s)
        | otherwise =
            maybe False (hashesMatch (volatileTxBytes s p)) (Map.lookup (pointEbHash p) (imEbBodies s))
  pure
    [ p
    | p <- Set.toAscList (Map.keysSet (imEbPoints s) <> Map.keysSet (imImmutableEbBodies s))
    , pointSlotNo p >= sinceSlot
    , isComplete p
    ]

-- | Each requested offset the body declares, with its bytes, or 'Nothing' if
-- not written.
imBatchRetrieveTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb -> LeiosPoint -> [Int] -> m [(Int, Maybe ByteString)]
imBatchRetrieveTxs stateVar point offsets = atomically $ do
  state <- readTVar stateVar
  pure
    [ (offset, writtenTx state point offset)
    | Just body <- [bodyAt state point]
    , offset <- IntSet.toAscList (IntSet.fromList offsets)
    , IntMap.member offset (sbTxs body)
    ]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe [ByteString])
imLookupEbClosure stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ do
    body <- bodyAt state point
    traverse (writtenTx state point) (IntMap.keys (sbTxs body))

-- | Whether reads of the point go to the immutable copies: it is older than
-- the latest garbage collection.
readsImmutable :: InMemoryLeiosDb -> LeiosPoint -> Bool
readsImmutable state point = pointSlotNo point < imGcSlot state

-- | The body at the point.
bodyAt :: InMemoryLeiosDb -> LeiosPoint -> Maybe StoredBody
bodyAt state point
  | readsImmutable state point = Map.lookup point (imImmutableEbBodies state)
  | otherwise =
      guard (Map.member point (imEbPoints state)) *> Map.lookup (pointEbHash point) (imEbBodies state)

-- | A tx's bytes at the point, if written.
writtenTx :: InMemoryLeiosDb -> LeiosPoint -> Int -> Maybe ByteString
writtenTx state point offset = IntMap.lookup offset txs >>= nonZero
 where
  txs
    | readsImmutable state point = Map.findWithDefault IntMap.empty point (imImmutableEbTxBytes state)
    | otherwise = volatileTxBytes state point

-- | A tx's bytes, unless they are zeroes, which the SQLite backend reads as not
-- written: a valid tx is never all zeroes.
nonZero :: ByteString -> Maybe ByteString
nonZero bytes
  | BS.all (== 0) bytes = Nothing
  | otherwise = Just bytes

-- | The point's tx bytes by offset, from 'imEbTxBytes'.
volatileTxBytes :: InMemoryLeiosDb -> LeiosPoint -> IntMap ByteString
volatileTxBytes s point = Map.findWithDefault IntMap.empty point (imEbTxBytes s)

-- | Whether each tx of the body is written and hashes to what the body lists.
hashesMatch :: IntMap ByteString -> StoredBody -> Bool
hashesMatch txBytesByOffset body =
  and
    [ maybe
        False
        (\tx -> hashLeiosTx (MkLeiosTx tx) == eteTxHash e)
        (IntMap.lookup off txBytesByOffset >>= nonZero)
    | (off, e) <- IntMap.toList (sbTxs body)
    ]
