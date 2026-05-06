{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module LeiosDemoDb.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  , snapshotInMemoryLeiosDb
  , InMemoryLeiosDbF (..)
  , inMemoryLeiosDbFromSnapshot
  ) where

import Cardano.Prelude (Generic, maybeToList, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , StrictTChan
  , StrictTVar
  , dupTChan
  , modifyTVar
  , newBroadcastTChan
  , newTVarIO
  , readTVar
  , writeTChan
  )
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosFetchWork (..)
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosNotification (..)
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

-- | HKD in-memory LeiosDb
data InMemoryLeiosDbF f = InMemoryLeiosDbF
  { imTxs :: f (Map TxHash (ByteString, BytesSize))
  -- ^ Global transaction storage (normalized)
  , imEbs :: f (Map LeiosPoint BytesSize)
  -- ^ Announced EBs
  , imEbBodies :: f (Map EbHash (IntMap {- txOffset -} EbTxEntry))
  -- ^ EB bodies
  }
  deriving stock Generic

-- | STM based in-memory LeiosDb
newtype InMemoryLeiosDb m = InMemoryLeiosDb
  { unInMemoryLeiosDb :: InMemoryLeiosDbF (StrictTVar m)
  }

emptyInMemoryLeiosDb :: MonadSTM m => m (InMemoryLeiosDb m)
emptyInMemoryLeiosDb =
  InMemoryLeiosDb
    <$> ( InMemoryLeiosDbF
            <$> newTVarIO mempty
            <*> newTVarIO mempty
            <*> newTVarIO mempty
        )

snapshotInMemoryLeiosDb :: MonadSTM m => InMemoryLeiosDb m -> m (InMemoryLeiosDbF Identity)
snapshotInMemoryLeiosDb (InMemoryLeiosDb (InMemoryLeiosDbF{..})) =
  atomically
    ( InMemoryLeiosDbF
        <$> (Identity <$> readTVar imTxs)
        <*> (Identity <$> readTVar imEbs)
        <*> (Identity <$> readTVar imEbBodies)
    )

inMemoryLeiosDbFromSnapshot :: MonadSTM m => InMemoryLeiosDbF Identity -> m (InMemoryLeiosDb m)
inMemoryLeiosDbFromSnapshot (InMemoryLeiosDbF{..}) =
  InMemoryLeiosDb
    <$> ( InMemoryLeiosDbF
            <$> newTVarIO (runIdentity imTxs)
            <*> newTVarIO (runIdentity imEbs)
            <*> newTVarIO (runIdentity imEbBodies)
        )

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
  state <- emptyInMemoryLeiosDb
  newLeiosDBInMemoryWith state

newLeiosDBInMemoryWith :: IOLike m => InMemoryLeiosDb m -> m (LeiosDbHandle m)
newLeiosDBInMemoryWith state = do
  notificationChan <- atomically newBroadcastTChan
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , open =
          pure $
            LeiosDbConnection
              { close = pure ()
              , leiosDbScanEbPoints = imScanEbPoints state
              , leiosDbLookupEbPoint = imLookupEbPoint state
              , leiosDbInsertEbPoint = imInsertEbPoint state
              , leiosDbLookupEbBody = imLookupEbBody state
              , leiosDbInsertEbBody = imInsertEbBody state notificationChan
              , leiosDbInsertTxs = imInsertTxs state notificationChan
              , leiosDbBatchRetrieveTxs = imBatchRetrieveTxs state
              , leiosDbFilterMissingEbBodies = imFilterMissingEbBodies state
              , leiosDbFilterMissingTxs = imFilterMissingTxs state
              , leiosDbQueryFetchWork = imQueryFetchWork state
              , leiosDbQueryCompletedEbByPoint = imQueryCompletedEbByPoint state
              , leiosDbQueryCertificateByPoint = return . Just . trustNoVerifyLeiosCertificate
              }
      }

-- * Top-level implementations

imScanEbPoints :: IOLike m => InMemoryLeiosDb m -> m [(SlotNo, EbHash)]
imScanEbPoints (InMemoryLeiosDb state) = do
  ebs <- atomically $ readTVar (imEbs state)
  pure
    [ (slot, hash)
    | (MkLeiosPoint slot hash) <- Set.toList (Map.keysSet ebs)
    ]

imLookupEbPoint :: IOLike m => InMemoryLeiosDb m -> LeiosPoint -> m (Maybe BytesSize)
imLookupEbPoint (InMemoryLeiosDb state) ebPoint = do
  ebs <- atomically $ readTVar (imEbs state)
  return $ Map.lookup ebPoint ebs

imInsertEbPoint :: IOLike m => InMemoryLeiosDb m -> LeiosPoint -> BytesSize -> m ()
imInsertEbPoint (InMemoryLeiosDb state) ebPoint ebBytesSize =
  atomically $
    modifyTVar (imEbs state) $
      Map.insert
        ebPoint
        ebBytesSize

imLookupEbBody :: IOLike m => InMemoryLeiosDb m -> EbHash -> m [(TxHash, BytesSize)]
imLookupEbBody (InMemoryLeiosDb state) ebHash = do
  ebBodies <- atomically $ readTVar (imEbBodies state)
  case Map.lookup ebHash ebBodies of
    Nothing -> pure []
    Just offsetMap ->
      pure
        [ (eteTxHash e, eteTxBytesSize e)
        | e <- IntMap.elems offsetMap
        ]

imInsertEbBody ::
  IOLike m =>
  InMemoryLeiosDb m ->
  StrictTChan m LeiosNotification ->
  LeiosPoint ->
  LeiosEb ->
  m ()
imInsertEbBody (InMemoryLeiosDb state) notificationChan point eb = do
  let items = leiosEbBodyItems eb
  when (null items) $
    error "leiosDbInsertEbBody: empty EB body (programmer error)"
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

  atomically $ modifyTVar (imEbBodies state) $ Map.insert point.pointEbHash entries
  atomically $ writeTChan notificationChan $ LeiosOfferBlock point (leiosEbBytesSize eb)

notifyEbCompleted ::
  IOLike m =>
  InMemoryLeiosDb m ->
  StrictTChan m LeiosNotification ->
  m CompletedEbs
notifyEbCompleted (InMemoryLeiosDb state) notificationChan = do
  (txs, ebs, ebBodies) <-
    atomically $
      (,,)
        <$> readTVar (imTxs state)
        <*> readTVar (imEbs state)
        <*> readTVar (imEbBodies state)

  let completed =
        [ ebPoint
        | (ebHash, entries) <- Map.toList ebBodies
        , all (\e -> Map.member (eteTxHash e) txs) (IntMap.elems entries)
        , ebPoint <- Set.toDescList (Map.keysSet ebs)
        , ebPoint.pointEbHash == ebHash -- TODO(bladyjoker): This needs a Map EbHash (Set SlotNo) index on Ebs
        ]

  atomically $
    forM_ completed (writeTChan notificationChan . LeiosOfferBlockTxs)

  return completed

imInsertTxs ::
  IOLike m =>
  InMemoryLeiosDb m ->
  StrictTChan m LeiosNotification ->
  [(TxHash, ByteString)] ->
  m CompletedEbs
imInsertTxs db@(InMemoryLeiosDb state) notificationChan newTxs = do
  let newTxsMap =
        Map.fromList [(txHash, (txBytes, fromIntegral $ BS.length txBytes)) | (txHash, txBytes) <- newTxs]

  atomically $ do
    modifyTVar (imTxs state) $
      Map.union newTxsMap

  notifyEbCompleted db notificationChan

imBatchRetrieveTxs ::
  IOLike m => InMemoryLeiosDb m -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
imBatchRetrieveTxs (InMemoryLeiosDb state) ebHash offsets = do
  (txs, ebBodies) <- atomically $ (,) <$> readTVar (imTxs state) <*> readTVar (imEbBodies state)
  case Map.lookup ebHash ebBodies of
    Nothing -> pure []
    Just offsetMap ->
      pure
        [ (offset, eteTxHash entry, fst <$> Map.lookup (eteTxHash entry) txs)
        | offset <- offsets
        , Just entry <- [IntMap.lookup offset offsetMap]
        ]

imFilterMissingEbBodies ::
  IOLike m => InMemoryLeiosDb m -> [LeiosPoint] -> m [LeiosPoint]
imFilterMissingEbBodies (InMemoryLeiosDb state) points = do
  ebBodies <- atomically $ readTVar (imEbBodies state)
  pure [p | p <- points, not $ Map.member p.pointEbHash ebBodies]

imFilterMissingTxs :: IOLike m => InMemoryLeiosDb m -> [TxHash] -> m [TxHash]
imFilterMissingTxs (InMemoryLeiosDb state) txHashes = do
  txs <- atomically $ readTVar (imTxs state)
  pure [txHash | txHash <- txHashes, not $ Map.member txHash txs]

imQueryFetchWork :: IOLike m => InMemoryLeiosDb m -> m LeiosFetchWork
imQueryFetchWork (InMemoryLeiosDb state) = do
  (txs, ebs, ebBodies) <-
    atomically $
      (,,)
        <$> readTVar (imTxs state)
        <*> readTVar (imEbs state)
        <*> readTVar (imEbBodies state)

  -- Eb without EbBody
  let missingEbBodies =
        Map.fromList
          [ (ebPoint, ebBytesSize)
          | (ebPoint, ebBytesSize) <- Map.toList ebs
          , not $ Map.member ebPoint.pointEbHash ebBodies
          ]
  -- Eb with EbBody but without Txs
  let missingEbTxs =
        Map.fromList
          [ (ebPoint, missingEntries)
          | (ebHash, entries) <- Map.toList ebBodies
          , ebPoint <- Set.toDescList (Map.keysSet ebs)
          , ebPoint.pointEbHash == ebHash -- TODO(bladyjoker): This needs a Map EbHash (Set SlotNo) index on Ebs
          , let missingEntries =
                  [ (offset, eteTxHash e, eteTxBytesSize e)
                  | (offset, e) <- IntMap.toList entries
                  , not $ Map.member (eteTxHash e) txs
                  ]
          , not $ null missingEntries
          ]
  pure LeiosFetchWork{missingEbBodies, missingEbTxs}

imQueryCompletedEbByPoint ::
  IOLike m => InMemoryLeiosDb m -> LeiosPoint -> m (Maybe [(TxHash, ByteString)])
imQueryCompletedEbByPoint (InMemoryLeiosDb state) ebPoint = do
  (txs, ebBodies) <-
    atomically $
      (,)
        <$> readTVar (imTxs state)
        <*> readTVar (imEbBodies state)

  case Map.lookup (pointEbHash ebPoint) ebBodies of
    Nothing -> pure Nothing
    Just entries ->
      let ebTxHashes = [eteTxHash e | e <- IntMap.elems entries]
          txClosure =
            [ (ebTxHash, tx)
            | ebTxHash <- ebTxHashes
            , (tx, _txSize) <- maybeToList (Map.lookup ebTxHash txs)
            ]
       in if length ebTxHashes == length txClosure
            then pure (Just txClosure)
            else pure Nothing
