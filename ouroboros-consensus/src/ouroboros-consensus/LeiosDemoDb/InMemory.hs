{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosFetchWork (..)
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBodyItems
  , leiosEbBytesSize
  , trustNoVerifyLeiosCertificate
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks (..)
  , atomically
  )

-- | In-memory database state
data InMemoryLeiosDb = InMemoryLeiosDb
  { imTxs :: !(Map TxHash (ByteString, BytesSize))
  -- ^ Global transaction storage (normalized)
  , imEbPoints :: !(IntMap {- SlotNo -} (EbHash, BytesSize))
  -- ^ Announced EB points with their expected sizes
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  , imEbSlots :: !(Map EbHash SlotNo)
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty

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
      , open =
          pure $
            LeiosDbConnection
              { close = pure ()
              , leiosDbScanEbPoints = imScanEbPoints stateVar
              , leiosDbLookupEbPoint = imLookupEbPoint stateVar
              , leiosDbInsertEbPoint = imInsertEbPoint stateVar
              , leiosDbLookupEbBody = imLookupEbBody stateVar
              , leiosDbInsertEbBody = imInsertEbBody stateVar notificationChan
              , leiosDbInsertTxs = imInsertTxs stateVar notificationChan
              , leiosDbBatchRetrieveTxs = imBatchRetrieveTxs stateVar
              , leiosDbFilterMissingEbBodies = imFilterMissingEbBodies stateVar
              , leiosDbFilterMissingTxs = imFilterMissingTxs stateVar
              , leiosDbQueryFetchWork = imQueryFetchWork stateVar
              , leiosDbQueryCompletedEbByPoint = imQueryCompletedEbByPoint stateVar
              , leiosDbQueryCertificateByPoint = return . Just . trustNoVerifyLeiosCertificate
              }
      }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EbHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (SlotNo (fromIntegral slot), hash)
    | (slot, (hash, _size)) <- IntMap.toAscList (imEbPoints state)
    ]

imLookupEbPoint :: IOLike m => StrictTVar m InMemoryLeiosDb -> EbHash -> m (Maybe SlotNo)
imLookupEbPoint stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $
    foldr
      (\(slot, (h, _)) acc -> if h == ebHash then Just (SlotNo (fromIntegral slot)) else acc)
      Nothing
      (IntMap.toList (imEbPoints state))

imInsertEbPoint :: IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> BytesSize -> m ()
imInsertEbPoint stateVar point ebBytesSize = atomically $
  modifyTVar stateVar $ \s ->
    s
      { imEbPoints =
          IntMap.insert
            (fromIntegral $ unSlotNo point.pointSlotNo)
            (point.pointEbHash, ebBytesSize)
            (imEbPoints s)
      }

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
        { imEbBodies = Map.insert point.pointEbHash entries (imEbBodies s)
        , imEbSlots = Map.insert point.pointEbHash point.pointSlotNo (imEbSlots s)
        }
    writeTChan notificationChan $ AcquiredEb point (leiosEbBytesSize eb)

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
  let completed =
        [ MkLeiosPoint slot ebHash
        | (ebHash, entries) <- Map.toList (imEbBodies state)
        , any (\e -> eteTxHash e `elem` insertedTxHashes) (IntMap.elems entries)
        , all (\e -> Map.member (eteTxHash e) (imTxs state)) (IntMap.elems entries)
        , slot <- maybeToList $ Map.lookup ebHash (imEbSlots state)
        ]
  forM_ completed $ writeTChan notificationChan . AcquiredEbTxs
  pure completed

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

imQueryFetchWork :: IOLike m => StrictTVar m InMemoryLeiosDb -> m LeiosFetchWork
imQueryFetchWork stateVar = atomically $ do
  state <- readTVar stateVar
  let missingEbBodies =
        Map.fromList
          [ (MkLeiosPoint (SlotNo (fromIntegral slot)) ebHash, ebBytesSize)
          | (slot, (ebHash, ebBytesSize)) <- IntMap.toAscList (imEbPoints state)
          , not $ Map.member ebHash (imEbBodies state)
          ]
  let missingEbTxs =
        Map.fromList
          [ (MkLeiosPoint slot ebHash, missingEntries)
          | (ebHash, entries) <- Map.toList (imEbBodies state)
          , slot <- maybeToList $ Map.lookup ebHash (imEbSlots state)
          , let missingEntries =
                  [ (offset, eteTxHash e, eteTxBytesSize e)
                  | (offset, e) <- IntMap.toList entries
                  , not $ Map.member (eteTxHash e) (imTxs state)
                  ]
          , not $ null missingEntries
          ]
  pure LeiosFetchWork{missingEbBodies, missingEbTxs}

imQueryCompletedEbByPoint ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> m (Maybe [(TxHash, ByteString)])
imQueryCompletedEbByPoint stateVar ebPoint = atomically $ do
  state <- readTVar stateVar
  case Map.lookup (pointEbHash ebPoint) (imEbBodies state) of
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
