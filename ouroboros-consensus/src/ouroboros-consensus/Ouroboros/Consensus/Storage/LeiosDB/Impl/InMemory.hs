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
  , LeiosPoint (..)
  , LeiosTx (..)
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
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (gcBatchSize)
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks (..)
  , atomically
  , throwIO
  , toException
  , try
  )

-- | In-memory database state, as the SQLite backend's: a row per point in
-- each partition, and the promoted slot.
data InMemoryLeiosDb = InMemoryLeiosDb
  { imVolatile :: !(Map LeiosPoint EbRow)
  -- ^ The volatile partition. Each point holds its own row, even when another
  -- point names the same EB.
  , imImmutable :: !(Map LeiosPoint EbRow)
  -- ^ The immutable partition: the rows 'leiosDbPromoteToImmutable' copied, at
  -- the points they were promoted at. Never collected.
  , imPromotedSlot :: !SlotNo
  -- ^ The slot the latest promotion reached. The writes of a point before it
  -- are dropped, its reads go to 'imImmutable', and the garbage collection
  -- deletes its volatile row.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty 0

-- | A row of the SQLite backend's @ebs@ table.
data EbRow = EbRow
  { rowBody :: !ByteString
  -- ^ The body in the bytes it arrived in, as @body@.
  , rowTxSizes :: !(IntMap {- txOffset -} BytesSize)
  -- ^ The size of each tx the body lists, by offset, as @txBounds@. No hashes:
  -- the body lists those.
  , rowTxs :: !(IntMap {- txOffset -} ByteString)
  -- ^ The written txs by offset, as @txs@ and @txWritten@.
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

-- | Copy the rows of the given points to 'imImmutable', unless they are there
-- already, then advance the promoted slot. The node promotes only what the
-- immutable chain certifies, and only once it holds its closure, so a closure
-- that is incomplete is a caller's bug: this then fails, changing nothing.
imPromoteToImmutable ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> [LeiosPoint] -> m ()
imPromoteToImmutable stateVar slot points = do
  incomplete <- atomically $ do
    s <- readTVar stateVar
    let rows =
          Map.fromList
            [ (point, complete =<< Map.lookup point (imVolatile s))
            | point <- points
            , Map.notMember point (imImmutable s)
            ]
        complete row = guard (rowComplete row) *> Just row
        incomplete = [point | (point, Nothing) <- Map.toList rows]
    when (null incomplete) $
      writeTVar
        stateVar
        s
          { imImmutable = imImmutable s <> Map.mapMaybe id rows
          , imPromotedSlot = max slot (imPromotedSlot s)
          }
    pure incomplete
  unless (null incomplete) $
    throwLeiosDbException $
      "leiosDbPromoteToImmutable: incomplete closure (programmer error): " <> show incomplete

-- | Delete the volatile rows before 'imPromotedSlot', 'gcBatchSize' at a
-- time, as the SQLite backend's jobs do, until none is left. A promoted EB
-- keeps its row in 'imImmutable'.
imGarbageCollect :: IOLike m => StrictTVar m InMemoryLeiosDb -> m ()
imGarbageCollect stateVar = do
  more <- atomically $ do
    s <- readTVar stateVar
    let collected =
          take gcBatchSize $
            takeWhile ((< imPromotedSlot s) . pointSlotNo) (Map.keys (imVolatile s))
    writeTVar stateVar s{imVolatile = foldr Map.delete (imVolatile s) collected}
    pure (length collected == gcBatchSize)
  when more $ imGarbageCollect stateVar

-- | Reads off the 'StrictTVar'. Nothing to open or close.
openInMemoryReader :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbReader m)
openInMemoryReader stateVar =
  pure
    LeiosDbReader
      { closeReader = pure ()
      , lookupEbBody = imLookupEbBody stateVar
      , lookupEbClosure = imLookupEbClosure stateVar
      , isImmutable = \point -> Map.member point . imImmutable <$> atomically (readTVar stateVar)
      , batchRetrieveTxs = imBatchRetrieveTxs stateVar
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

-- | Whether the latest promotion passed the point, so that its writes are
-- dropped.
isPromoted :: InMemoryLeiosDb -> LeiosPoint -> Bool
isPromoted s point = pointSlotNo point < imPromotedSlot s

imLookupEbBody ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe ByteString)
imLookupEbBody stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ rowBody <$> rowAt state point

-- | Mirrors the SQLite backend: the row of the point, unless an earlier
-- delivery wrote it, then the fills, returning the offsets the point holds.
-- Nothing, if the latest promotion passed the point.
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
    s <- readTVar stateVar
    if isPromoted s point
      then pure []
      else do
        let row = Map.findWithDefault newRow point (imVolatile s)
            toWrite = filled s `IntMap.withoutKeys` IntMap.keysSet (rowTxs row)
            row' = row{rowTxs = rowTxs row <> toWrite}
        writeTVar stateVar s{imVolatile = Map.insert point row' (imVolatile s)}
        pure (IntMap.keys (rowTxs row'))
 where
  -- Mirrors the SQLite backend: a fill lands only on an offset the body
  -- declares, at its declared size, from a volatile point of the source holding
  -- bytes that hash to the tx this body lists there, the first of the fills
  -- winning an offset.
  filled s =
    IntMap.fromListWith
      (\_new old -> old)
      [ (off, bytes)
      | (off, MkTxLocation srcEbHash srcOff) <- fills
      , Just (txHash, size) <- [IntMap.lookup off refs]
      , bytes : _ <-
          [ [ tx
            | (p, row) <- Map.toList (imVolatile s)
            , pointEbHash p == srcEbHash
            , Just tx <- [IntMap.lookup srcOff (rowTxs row)]
            ]
          ]
      , hashLeiosTx (MkLeiosTx bytes) == txHash
      , fromIntegral (BS.length bytes) == size
      ]
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  newRow =
    EbRow
      { rowBody = serialiseEndorserBlock eb
      , rowTxSizes = fmap snd refs
      , rowTxs = IntMap.empty
      }

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m [TxOffset]
imInsertTxs stateVar point offBytes = atomically $ do
  s <- readTVar stateVar
  -- Mirrors the SQLite backend's in-place fill: a tx only lands at an offset
  -- the body declared at the point, at the declared size, the first of a write
  -- winning.
  case guard (not (isPromoted s point)) *> Map.lookup point (imVolatile s) of
    Nothing -> pure []
    Just row -> do
      let inBody =
            [(off, bytes, size) | (off, bytes) <- offBytes, Just size <- [IntMap.lookup off (rowTxSizes row)]]
          sized (_, bytes, size) = fromIntegral (BS.length bytes) == size
          accepted = IntMap.fromListWith (\_new old -> old) [(off, bytes) | tx@(off, bytes, _) <- inBody, sized tx]
      unless (IntMap.null accepted) $
        writeTVar
          stateVar
          s{imVolatile = Map.insert point row{rowTxs = IntMap.union accepted (rowTxs row)} (imVolatile s)}
      pure [off | tx@(off, _, _) <- inBody, not (sized tx)]

-- | Implements 'scanCompleteEbClosuresNotOlderThanSlot': each point no older
-- than the given slot whose closure is complete where its reads go
-- ('readsImmutable'). A volatile closure is complete if each of its txs is
-- written, and a promoted one is, since only complete EBs are promoted.
imScanCompleteEbClosuresSince ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m [LeiosPoint]
imScanCompleteEbClosuresSince stateVar sinceSlot = atomically $ do
  s <- readTVar stateVar
  let vol = Map.filterWithKey (\p row -> not (readsImmutable s p) && rowComplete row) (imVolatile s)
      imm = Map.filterWithKey (\p _ -> readsImmutable s p) (imImmutable s)
  pure [p | p <- Set.toAscList (Map.keysSet vol <> Map.keysSet imm), pointSlotNo p >= sinceSlot]

-- | Each requested offset the body declares, with its bytes, or 'Nothing' if
-- not written.
imBatchRetrieveTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb -> LeiosPoint -> [TxOffset] -> m [(TxOffset, Maybe ByteString)]
imBatchRetrieveTxs stateVar point offsets = atomically $ do
  state <- readTVar stateVar
  pure
    [ (offset, IntMap.lookup offset (rowTxs row))
    | Just row <- [rowAt state point]
    , offset <- IntSet.toAscList (IntSet.fromList offsets)
    , IntMap.member offset (rowTxSizes row)
    ]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe [ByteString])
imLookupEbClosure stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ do
    row <- rowAt state point
    traverse (`IntMap.lookup` rowTxs row) (IntMap.keys (rowTxSizes row))

-- | Whether reads of the point go to the immutable partition: it is older
-- than the slot the latest promotion reached.
readsImmutable :: InMemoryLeiosDb -> LeiosPoint -> Bool
readsImmutable = isPromoted

-- | The row of the point, in the partition its reads go to.
rowAt :: InMemoryLeiosDb -> LeiosPoint -> Maybe EbRow
rowAt state point
  | readsImmutable state point = Map.lookup point (imImmutable state)
  | otherwise = Map.lookup point (imVolatile state)

-- | Whether each tx of the row is written.
rowComplete :: EbRow -> Bool
rowComplete row = IntMap.keysSet (rowTxSizes row) == IntMap.keysSet (rowTxs row)
