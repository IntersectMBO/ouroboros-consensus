{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An implementation of a 'BackingStore' using a TVar. This is the
-- implementation known as \"InMemory\".
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory (
    -- * Constructor
    newInMemoryBackingStore
    -- * Errors
  , InMemoryBackingStoreExn (..)
  , InMemoryBackingStoreInitExn (..)
  ) where

import           Cardano.Binary as CBOR
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join, unless, void, when)
import           Control.Monad.Class.MonadThrow (catch)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String (fromString)
import           GHC.Generics
import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import           Ouroboros.Consensus.Util.IOLike (Exception, IOLike,
                     MonadSTM (STM, atomically), MonadThrow (throwIO), NoThunks,
                     StrictTVar, newTVarIO, readTVar, throwSTM, writeTVar)
import           Prelude hiding (lookup)
import           System.FS.API
                     (HasFS (createDirectory, doesDirectoryExist, doesFileExist, mkFsErrorPath),
                     SomeHasFS (SomeHasFS), withFile)
import           System.FS.API.Lazy (hGetAll, hPutAll)
import           System.FS.API.Types (AllowExisting (MustBeNew), FsErrorPath,
                     FsPath (fsPathToList), OpenMode (ReadMode, WriteMode),
                     fsPathFromList)

{-------------------------------------------------------------------------------
  An in-memory backing store
-------------------------------------------------------------------------------}

data BackingStoreContents m l =
    BackingStoreContentsClosed
  | BackingStoreContents
      !(WithOrigin SlotNo)
      !(LedgerTables l ValuesMK)
  deriving (Generic)

deriving instance ( NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  ) => NoThunks (BackingStoreContents m l)

-- | Use a 'TVar' as a trivial backing store
newInMemoryBackingStore ::
     forall l m.
     ( IOLike m
     , HasLedgerTables l
     )
  => Tracer m BackingStoreTrace
  -> SnapshotsFS m
  -> InitFrom (LedgerTables l ValuesMK)
  -> m (LedgerBackingStore m l)
newInMemoryBackingStore tracer (SnapshotsFS (SomeHasFS fs)) initialization = do
    traceWith tracer BSOpening
    ref <- do
      (slot, values) <- case initialization of
        InitFromCopy path -> do
          traceWith tracer $ BSInitialisingFromCopy path
          tvarFileExists <- doesFileExist fs (extendPath path)
          unless tvarFileExists $
            throwIO . StoreDirIsIncompatible $ mkFsErrorPath fs path
          withFile fs (extendPath path) ReadMode $ \h -> do
            bs <- hGetAll fs h
            case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> valuesMKDecoder) bs of
              Left  err        -> throwIO $ InMemoryBackingStoreDeserialiseExn err
              Right (extra, x) -> do
                unless (BSL.null extra) $ throwIO InMemoryIncompleteDeserialiseExn
                traceWith tracer $ BSInitialisedFromCopy path
                pure x
        InitFromValues slot values -> do
          traceWith tracer $ BSInitialisingFromValues slot
          pure (slot, values)
      newTVarIO $ BackingStoreContents slot values
    traceWith tracer $ BSOpened Nothing
    pure BackingStore {
        bsClose = do
            traceWith tracer BSClosing
            catch
              (atomically $ do
                  guardClosed ref
                  writeTVar ref BackingStoreContentsClosed
              )
              (\case
                InMemoryBackingStoreClosedExn -> traceWith tracer BSAlreadyClosed
                e -> throwIO e
              )
            traceWith tracer BSClosed
      , bsCopy = \path -> do
          traceWith tracer $ BSCopying path
          join $ atomically $ do
            readTVar ref >>= \case
              BackingStoreContentsClosed       ->
                throwSTM InMemoryBackingStoreClosedExn
              BackingStoreContents slot values -> pure $ do
                exists <- doesDirectoryExist fs path
                when exists $ throwIO InMemoryBackingStoreDirectoryExists
                createDirectory fs path
                withFile fs (extendPath path) (WriteMode MustBeNew) $ \h ->
                  void $ hPutAll fs h
                       $ CBOR.toLazyByteString
                       $ CBOR.toCBOR slot <> valuesMKEncoder values
          traceWith tracer $ BSCopied path
      , bsValueHandle = do
          traceWith tracer BSCreatingValueHandle
          vh <- join $ atomically $ do
           readTVar ref >>= \case
             BackingStoreContentsClosed       ->
               throwSTM InMemoryBackingStoreClosedExn
             BackingStoreContents slot values -> pure $ do
               refHandleClosed <- newTVarIO False
               pure $ BackingStoreValueHandle {
                   bsvhAtSlot    = slot
                 , bsvhClose     = do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHClosing
                     catch
                      (atomically $ do
                        guardClosed ref
                        guardHandleClosed refHandleClosed
                        writeTVar refHandleClosed True
                      )
                      (\case
                        InMemoryBackingStoreClosedExn            ->
                          traceWith tracer BSAlreadyClosed
                        InMemoryBackingStoreValueHandleClosedExn ->
                          traceWith tracer (BSValueHandleTrace Nothing BSVHAlreadyClosed)
                        e                                    ->
                          throwIO e
                      )
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHClosed
                 , bsvhRangeRead = \rq -> do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRangeReading
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ rangeRead rq values
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRangeRead
                     pure r
                 , bsvhReadAll   =
                    atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure values
                 , bsvhRead      = \keys -> do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHReading
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ lookup keys values
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRead
                     pure r
                 , bsvhStat = do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHStatting
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ Statistics slot (count values)
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHStatted
                     pure r
                 }
          traceWith tracer BSCreatedValueHandle
          pure vh
      , bsWrite = \slot2 diff -> do
         traceWith tracer $ BSWriting slot2
         slot1 <- atomically $ do
          readTVar ref >>= \case
            BackingStoreContentsClosed        ->
              throwSTM InMemoryBackingStoreClosedExn
            BackingStoreContents slot1 values -> do
              unless (slot1 <= At slot2) $
                throwSTM $ InMemoryBackingStoreNonMonotonicSeq (At slot2) slot1
              writeTVar ref $
                BackingStoreContents
                  (At slot2)
                  (forwardValues values diff)
              pure slot1
         traceWith tracer $ BSWritten slot1 slot2
      }
  where
    extendPath path =
      fsPathFromList $ fsPathToList path <> [fromString "tvar"]

    lookup :: LedgerTables l KeysMK
           -> LedgerTables l ValuesMK
           -> LedgerTables l ValuesMK
    lookup = ltliftA2 lookup'

    lookup' ::
         Ord k
      => KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    lookup' (KeysMK ks) (ValuesMK vs) =
      ValuesMK (Map.restrictKeys vs ks)


    rangeRead :: RangeQuery (LedgerTables l KeysMK)
              -> LedgerTables l ValuesMK
              -> LedgerTables l ValuesMK
    rangeRead rq values = case rqPrev rq of
      Nothing   ->
        ltmap (rangeRead0' (rqCount rq))      values
      Just keys ->
        ltliftA2 (rangeRead'  (rqCount rq)) keys values

    rangeRead0' ::
         Int
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead0' n (ValuesMK vs) =
      ValuesMK $ Map.take n vs

    rangeRead' ::
         Ord k
      => Int
      -> KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead' n (KeysMK ks) (ValuesMK vs) =
        case Set.lookupMax ks of
          Nothing -> ValuesMK Map.empty
          Just  k -> ValuesMK  $ Map.take n $ snd $ Map.split k vs

    forwardValues :: LedgerTables l ValuesMK
                  -> LedgerTables l DiffMK
                  -> LedgerTables l ValuesMK
    forwardValues = ltliftA2 applyDiff_

    applyDiff_ ::
         Ord k
      => ValuesMK k v
      -> DiffMK   k v
      -> ValuesMK k v
    applyDiff_ (ValuesMK values) (DiffMK diff) =
      ValuesMK (Diff.applyDiff values diff)

    count :: LedgerTables l ValuesMK -> Int
    count = ltcollapse . ltmap (K2 . count')

    count' :: ValuesMK k v -> Int
    count' (ValuesMK values) = Map.size values

guardClosed ::
     IOLike m
  => StrictTVar m (BackingStoreContents ks vs)
  -> STM m ()
guardClosed ref = readTVar ref >>= \case
  BackingStoreContentsClosed -> throwSTM InMemoryBackingStoreClosedExn
  BackingStoreContents _ _   -> pure ()

guardHandleClosed ::
     IOLike m
  => StrictTVar m Bool
  -> STM m ()
guardHandleClosed refHandleClosed = do
  isClosed <- readTVar refHandleClosed
  when isClosed $ throwSTM InMemoryBackingStoreValueHandleClosedExn

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors that the InMemory backing store can throw on runtime.
--
-- __WARNING__: these errors will be thrown in IO as having a corrupt database
-- is critical for the functioning of Consensus.
data InMemoryBackingStoreExn =
    InMemoryBackingStoreClosedExn
  | InMemoryBackingStoreValueHandleClosedExn
  | InMemoryBackingStoreDirectoryExists
  | InMemoryBackingStoreNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | InMemoryBackingStoreDeserialiseExn CBOR.DeserialiseFailure
  | InMemoryIncompleteDeserialiseExn
  deriving anyclass (Exception)
  deriving stock    (Show)

-- | Errors that the InMemory backing store can throw on initialization.
--
-- __WARNING__: these errors will be thrown in IO as having a corrupt database
-- is critical for the functioning of Consensus.
newtype InMemoryBackingStoreInitExn =
  StoreDirIsIncompatible FsErrorPath
  deriving anyclass (Exception)

instance Show InMemoryBackingStoreInitExn where
  show (StoreDirIsIncompatible p) =
       "In-Memory database not found in the database directory: "
    <> show p
    <> ".\nPre-UTxO-HD and LMDB implementations are incompatible with the In-Memory \
       \ implementation. Please delete your ledger database directory."
