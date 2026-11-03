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
import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
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

-- | In-memory database state. Like the SQLite backend, it stores a body and
-- its tx bytes per point, even for two points of the same endorser block.
data InMemoryLeiosDb = InMemoryLeiosDb
  { imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted points with their expected sizes.
  , imEbBodies :: !(Map LeiosPoint (IntMap {- txOffset -} EbTxEntry))
  -- ^ The tx-hash list written at each point, by offset.
  , imEbTxBytes :: !(Map LeiosPoint (IntMap {- txOffset -} ByteString))
  -- ^ The tx bytes written at each point, by offset -- mirrors the SQLite
  -- backend's @ebTxBytes@ table. A tx shared by two points is stored twice,
  -- deliberately.
  , imImmutableEbBodies :: !(Map LeiosPoint (IntMap EbTxEntry))
  -- ^ The bodies of the points 'leiosDbPromoteToImmutable' promoted. The
  -- garbage collection evicts such a point from the other fields like any
  -- other, and the reads fall back to this and 'imImmutableEbTxBytes'.
  , imImmutableEbTxBytes :: !(Map LeiosPoint (IntMap {- txOffset -} ByteString))
  -- ^ The tx bytes of the bodies in 'imImmutableEbBodies', by offset.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty

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

-- | Copy the closures of the given points to 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes', unless they are there already. The node pins only
-- what the immutable chain certifies, and only once it holds its closure, so a
-- closure that is incomplete is a caller's bug: this then fails, changing
-- nothing.
imPromoteToImmutable :: IOLike m => StrictTVar m InMemoryLeiosDb -> [LeiosPoint] -> m ()
imPromoteToImmutable stateVar points = do
  incomplete <- atomically $ do
    s <- readTVar stateVar
    let closures =
          Map.fromSet
            ( \point -> do
                entries <- Map.lookup point (imEbBodies s)
                let txBytes = volatileTxBytes s point
                _ <- closureWith txBytes entries
                pure (entries, txBytes)
            )
            (Set.fromList points `Set.difference` Map.keysSet (imImmutableEbBodies s))
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

-- | Evict every point older than the slot, with its body and tx bytes. A
-- promoted point keeps its copy in 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes'.
imGarbageCollect :: IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m ()
imGarbageCollect stateVar slot = atomically $ modifyTVar stateVar $ \s ->
  s
    { imEbPoints = kept (imEbPoints s)
    , imEbBodies = kept (imEbBodies s)
    , imEbTxBytes = kept (imEbTxBytes s)
    }
 where
  old point = pointSlotNo point < slot
  kept :: Map LeiosPoint a -> Map LeiosPoint a
  kept = Map.dropWhileAntitone old

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
      , writeEbPoint = \point ebBytesSize ->
          resolved ("WriteEbPoint " <> show point) (imInsertEbPoint stateVar point ebBytesSize)
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

-- | Insert an announced EB point. Idempotent: a second insert at the
-- same point keeps the first-seen size.
imInsertEbPoint :: IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> BytesSize -> m ()
imInsertEbPoint stateVar point ebBytesSize = atomically $
  modifyTVar stateVar $ \s ->
    s{imEbPoints = Map.insertWith (\_ old -> old) point ebBytesSize (imEbPoints s)}

imLookupEbBody ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m [(TxHash, BytesSize)]
imLookupEbBody stateVar point = atomically $ do
  state <- readTVar stateVar
  case Map.lookup point (imEbBodies state) <|> Map.lookup point (imImmutableEbBodies state) of
    Nothing -> pure []
    Just offsetMap ->
      pure
        [ (eteTxHash e, eteTxBytesSize e)
        | e <- IntMap.elems offsetMap
        ]

imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  m [TxOffset]
imInsertEbBody stateVar point eb fills = do
  let items = zip [0 :: Int ..] (endorserBlockReferences eb)
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
            | (offset, (txHash, size)) <- items
            ]
    modifyTVar stateVar $ \s ->
      s
        { -- A second delivery of the body at the same point is a no-op.
          imEbBodies = Map.insertWith (\_ old -> old) point entries (imEbBodies s)
        }
    filled <- fillLocally <$> readTVar stateVar
    unless (IntMap.null filled) $
      modifyTVar stateVar $ \s ->
        s{imEbTxBytes = Map.insertWith IntMap.union point filled (imEbTxBytes s)}
    pure (IntMap.keys filled)
 where
  -- Mirrors the SQLite backend: a fill lands only on an offset the body
  -- declared and has no bytes for yet, and only from a source holding bytes of
  -- the declared size for the tx its body lists there, which must be the tx
  -- this body lists.
  fillLocally s =
    Foldable.foldl'
      ( \acc (off, MkTxLocation src srcOff) ->
          case wanted s off of
            Just e
              | not (IntMap.member off acc)
              , Just srcEntry <- IntMap.lookup srcOff =<< Map.lookup src (imEbBodies s)
              , eteTxHash srcEntry == eteTxHash e
              , Just bytes <- IntMap.lookup srcOff (volatileTxBytes s src)
              , fromIntegral (BS.length bytes) == eteTxBytesSize e ->
                  IntMap.insert off bytes acc
            _ -> acc
      )
      IntMap.empty
      fills
  -- The entry the body declares at this offset, if it has no bytes yet.
  wanted s off = do
    e <- IntMap.lookup off =<< Map.lookup point (imEbBodies s)
    if IntMap.member off (volatileTxBytes s point) then Nothing else Just e

-- | Whether a body is written at this point and every one of its offsets has
-- its bytes.
pointCompleteIn :: InMemoryLeiosDb -> LeiosPoint -> Bool
pointCompleteIn state point = case Map.lookup point (imEbBodies state) of
  Nothing -> False
  Just entries ->
    IntMap.keysSet entries `IntSet.isSubsetOf` IntMap.keysSet (volatileTxBytes state point)

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m ()
imInsertTxs stateVar point offBytes = atomically $
  -- Mirrors the SQLite backend's guarded in-place fill: a fill only lands on a
  -- pre-allocated row, i.e. an offset the body declared, once, at the declared
  -- size. Anything else changes nothing.
  modifyTVar stateVar $ \s ->
    let declared = Map.findWithDefault IntMap.empty point (imEbBodies s)
        held = volatileTxBytes s point
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
          else s{imEbTxBytes = Map.insertWith IntMap.union point accepted (imEbTxBytes s)}

-- | Implements 'scanCompleteEbClosuresNotOlderThanSlot': each point no older
-- than the given slot whose closure is complete, as in the SQLite backend.
imScanCompleteEbClosuresSince ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m [LeiosPoint]
imScanCompleteEbClosuresSince stateVar sinceSlot = atomically $ do
  s <- readTVar stateVar
  pure
    [ point
    | point <- Map.keys (Map.dropWhileAntitone ((< sinceSlot) . pointSlotNo) (imEbBodies s))
    , pointCompleteIn s point
    ]

-- | The tx bytes at the point, from the immutable copy if the point was
-- promoted and then evicted.
imBatchRetrieveTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [Int] ->
  m [(Int, TxHash, Maybe ByteString)]
imBatchRetrieveTxs stateVar point offsets = atomically $ do
  state <- readTVar stateVar
  let rows entries txBytes =
        [ (offset, eteTxHash entry, IntMap.lookup offset txBytes)
        | offset <- offsets
        , Just entry <- [IntMap.lookup offset entries]
        ]
  pure $ case Map.lookup point (imEbBodies state) of
    Just entries -> rows entries (volatileTxBytes state point)
    Nothing -> case Map.lookup point (imImmutableEbBodies state) of
      Just entries -> rows entries (Map.findWithDefault IntMap.empty point (imImmutableEbTxBytes state))
      Nothing -> []

imLookupEbClosure ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe [(TxHash, ByteString)])
imLookupEbClosure stateVar point = atomically $ do
  state <- readTVar stateVar
  pure $ case Map.lookup point (imEbBodies state) >>= closureWith (volatileTxBytes state point) of
    Just closure -> Just closure
    Nothing ->
      Map.lookup point (imImmutableEbBodies state)
        >>= closureWith (Map.findWithDefault IntMap.empty point (imImmutableEbTxBytes state))

-- | The point's tx bytes by offset, from 'imEbTxBytes'.
volatileTxBytes :: InMemoryLeiosDb -> LeiosPoint -> IntMap ByteString
volatileTxBytes s point = Map.findWithDefault IntMap.empty point (imEbTxBytes s)

-- | The body's closure, if the tx bytes hold each of its transactions.
closureWith :: IntMap ByteString -> IntMap EbTxEntry -> Maybe [(TxHash, ByteString)]
closureWith txBytesByOffset entries =
  traverse
    (\(offset, e) -> (,) (eteTxHash e) <$> IntMap.lookup offset txBytesByOffset)
    (IntMap.toAscList entries)
