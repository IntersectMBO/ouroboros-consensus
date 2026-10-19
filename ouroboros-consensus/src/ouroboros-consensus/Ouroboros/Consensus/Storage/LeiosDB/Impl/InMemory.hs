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

import Cardano.Prelude (Generic, fromMaybe, unless, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Write (toStrictByteString)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , TxOffset
  , encodeLeiosEb
  , leiosEbBodyItems
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
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
  -- ^ Tx bytes owned by the referencing EB, keyed by offset -- the SQLite
  -- backend's @ebClosures@ table, one tx at a time. A tx shared by two EBs is
  -- stored twice, deliberately.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their expected sizes. The same EB
  -- content can be inserted at multiple slots; each
  -- @(slot, hash)@ point has its own entry.
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  -- ^ EB tx-hash list, keyed by EB hash. Shared across every
  -- point that references the same EB.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty

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
      , -- No-op for now; see 'leiosDbGarbageCollect'.
        leiosDbGarbageCollect = \_slotNo -> pure ()
      , -- No-op for now; see 'leiosDbPromoteToImmutable'.
        leiosDbPromoteToImmutable = \_points -> pure ()
      , -- The in-memory implementation does not track stats.
        leiosDbSampleStats = pure (LeiosDbStats 0 0 0)
      , openReader = openInMemoryReader stateVar
      , openWriter = openInMemoryWriter stateVar
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
      , writeEbBody = \point eb ->
          resolved ("WriteEbBody " <> show point) (imInsertEbBody stateVar point eb)
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

imLookupEbBody :: IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe ByteString)
imLookupEbBody stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $
    toStrictByteString
      . encodeLeiosEb
      . MkLeiosEb
      . V.fromList
      . map (\e -> (eteTxHash e, eteTxBytesSize e))
      . IntMap.elems
      <$> Map.lookup ebHash (imEbBodies state)

imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  LeiosEb ->
  m ()
imInsertEbBody stateVar point eb = do
  let items = leiosEbBodyItems eb
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
        }

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m ()
imInsertTxs stateVar point offBytes = atomically $ do
  let ebHash = pointEbHash point
  -- Mirrors the SQLite backend's in-place fill: a tx only lands at an offset
  -- the body declared, at the declared size, the first of a write winning.
  modifyTVar stateVar $ \s ->
    let declared = Map.findWithDefault IntMap.empty ebHash (imEbBodies s)
        accepted =
          IntMap.fromListWith
            (\_new old -> old)
            [ (off, bytes)
            | (off, bytes) <- offBytes
            , Just e <- [IntMap.lookup off declared]
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            ]
     in if IntMap.null accepted
          then s
          else s{imEbTxBytes = Map.insertWith IntMap.union ebHash accepted (imEbTxBytes s)}

imBatchRetrieveTxs ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> [Int] -> m [(Int, ByteString)]
imBatchRetrieveTxs stateVar ebHash offsets = atomically $ do
  state <- readTVar stateVar
  pure $ case Map.lookup ebHash (imEbBodies state) of
    Nothing -> []
    Just entries ->
      [ (offset, txBytesOrZeroes state ebHash offset e)
      | offset <- IntSet.toAscList (IntSet.fromList offsets)
      , Just e <- [IntMap.lookup offset entries]
      ]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe [ByteString])
imLookupEbClosure stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $
    fmap
      (\entries -> [txBytesOrZeroes state ebHash off e | (off, e) <- IntMap.toAscList entries])
      (Map.lookup ebHash (imEbBodies state))

-- | A tx's bytes, or zeroes of its size if not written, as the SQLite
-- backend's closure is allocated.
txBytesOrZeroes :: InMemoryLeiosDb -> EbHash -> Int -> EbTxEntry -> ByteString
txBytesOrZeroes state ebHash offset e =
  fromMaybe
    (BS.replicate (fromIntegral (eteTxBytesSize e)) 0)
    (IntMap.lookup offset =<< Map.lookup ebHash (imEbTxBytes state))
