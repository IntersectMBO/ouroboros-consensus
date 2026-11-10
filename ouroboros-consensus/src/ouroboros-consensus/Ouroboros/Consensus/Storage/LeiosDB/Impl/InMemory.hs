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
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockBytes
  , endorserBlockReferences
  , hashLeiosTx
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LocalFills
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
  -- ^ The slot the latest promotion or truncation reached. The writes of a
  -- point before it
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
  , rowTxSizes :: !(Map TxOffset BytesSize)
  -- ^ The size of each tx the body lists, by offset, as @txBounds@. No hashes:
  -- the body lists those.
  , rowTxs :: !(Map TxOffset ByteString)
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
      , leiosDbTruncateImmutable = \slot -> atomically $ modifyTVar stateVar $ \s ->
          s
            { imImmutable = Map.filterWithKey (\p _ -> pointSlotNo p < slot) (imImmutable s)
            , imPromotedSlot = slot
            }
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
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe (Serialised EndorserBlock))
imLookupEbBody stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ Serialised . BSL.fromStrict . rowBody <$> rowAt state point

-- | Mirrors the SQLite backend: the row of the point, unless an earlier
-- delivery wrote it, then the fills, returning the offsets the point holds.
-- Nothing, if the latest promotion passed the point.
imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  EndorserBlock ->
  LocalFills ->
  m (Set TxOffset)
imInsertEbBody stateVar point eb fills = do
  when (null refs) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  atomically $ do
    s <- readTVar stateVar
    if isPromoted s point
      then pure Set.empty
      else do
        let row = Map.findWithDefault newRow point (imVolatile s)
            toWrite = filled s `Map.withoutKeys` Map.keysSet (rowTxs row)
            row' = row{rowTxs = rowTxs row <> toWrite}
        writeTVar stateVar s{imVolatile = Map.insert point row' (imVolatile s)}
        pure (Map.keysSet (rowTxs row'))
 where
  -- Mirrors the SQLite backend: a fill lands only on an offset the body
  -- declares, at its declared size, from the source's volatile row holding
  -- bytes that hash to the tx this body lists there.
  filled s =
    Map.fromList
      [ (off, bytes)
      | (off, MkTxLocation srcPoint srcOff) <- Map.toList fills
      , Just (txHash, size) <- [Map.lookup off refs]
      , Just row <- [Map.lookup srcPoint (imVolatile s)]
      , Just bytes <- [Map.lookup srcOff (rowTxs row)]
      , hashLeiosTx (MkLeiosTx bytes) == txHash
      , fromIntegral (BS.length bytes) == size
      ]
  refs = Map.fromList (zip [0 ..] (endorserBlockReferences eb))
  newRow =
    EbRow
      { rowBody = endorserBlockBytes eb
      , rowTxSizes = fmap snd refs
      , rowTxs = Map.empty
      }

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  Map TxOffset ByteString ->
  m (Set TxOffset)
imInsertTxs stateVar point offBytes = atomically $ do
  s <- readTVar stateVar
  -- Mirrors the SQLite backend's in-place fill: a tx only lands at an offset
  -- the body declared at the point, at the declared size.
  case guard (not (isPromoted s point)) *> Map.lookup point (imVolatile s) of
    Nothing -> pure Set.empty
    Just row -> do
      let inBody = Map.intersectionWith (,) offBytes (rowTxSizes row)
          sized (bytes, size) = fromIntegral (BS.length bytes) == size
          (accepted, missized) = Map.partition sized inBody
      unless (Map.null accepted) $
        writeTVar
          stateVar
          s
            { imVolatile = Map.insert point row{rowTxs = Map.union (fst <$> accepted) (rowTxs row)} (imVolatile s)
            }
      pure (Map.keysSet missized)

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

-- | The bytes of each requested offset the point holds.
imBatchRetrieveTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb -> LeiosPoint -> Set TxOffset -> m (Map TxOffset ByteString)
imBatchRetrieveTxs stateVar point offsets = atomically $ do
  state <- readTVar stateVar
  pure $ case rowAt state point of
    Nothing -> Map.empty
    Just row -> rowTxs row `Map.restrictKeys` offsets

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe [ByteString])
imLookupEbClosure stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ do
    row <- rowAt state point
    traverse (`Map.lookup` rowTxs row) (Map.keys (rowTxSizes row))

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
rowComplete row = Map.keysSet (rowTxSizes row) == Map.keysSet (rowTxs row)
