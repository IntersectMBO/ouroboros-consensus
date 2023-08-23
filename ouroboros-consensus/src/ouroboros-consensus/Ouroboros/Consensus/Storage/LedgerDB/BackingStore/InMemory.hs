{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

-- | An implementation of a 'BackingStore' using a TVar. This is the
-- implementation known as "InMemory".
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory (
    -- * Constructor
    newTVarBackingStoreInitialiser
    -- * Errors
  , StoreDirIsIncompatible (..)
  , TVarBackingStoreExn (..)
  ) where

import           Cardano.Binary as CBOR
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (join, unless, void, when)
import           Control.Monad.Class.MonadThrow (catch)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import           Data.String (fromString)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Util.IOLike (Exception, IOLike,
                     MonadSTM (STM, atomically), MonadThrow (throwIO), NoThunks,
                     StrictTVar, newTVarIO, readTVar, throwSTM, writeTVar)
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

data TVarBackingStoreContents m values =
    TVarBackingStoreContentsClosed
  | TVarBackingStoreContents
      !(WithOrigin SlotNo)
      !values
  deriving (Generic, NoThunks)

data TVarBackingStoreExn =
    TVarBackingStoreClosedExn
  | TVarBackingStoreValueHandleClosedExn
  | TVarBackingStoreDirectoryExists
  | TVarBackingStoreNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | TVarBackingStoreDeserialiseExn CBOR.DeserialiseFailure
  | TVarIncompleteDeserialiseExn
  deriving anyclass (Exception)
  deriving stock    (Show)

-- | Use a 'TVar' as a trivial backing store
newTVarBackingStoreInitialiser ::
     (IOLike m, NoThunks values)
  => Tracer m BackingStoreTrace
  -> (keys -> values -> values)
  -> (RangeQuery keys -> values -> values)
  -> (values -> diff -> values)
  -> (values -> Int)
  -> (values -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s values)
  -> SomeHasFS m
  -> InitFrom values
  -> m (BackingStore m keys values diff)
newTVarBackingStoreInitialiser tracer lookup_ rangeRead_ forwardValues_ count_ enc dec (SomeHasFS fs0) initialization = do
    traceWith tracer BSOpening
    ref <- do
      (slot, values) <- case initialization of
        InitFromCopy (BackingStorePath path) -> do
          traceWith tracer $ BSInitialisingFromCopy path
          tvarFileExists <- doesFileExist fs0 (extendPath path)
          unless tvarFileExists $
            throwIO . StoreDirIsIncompatible $ mkFsErrorPath fs0 path
          withFile fs0 (extendPath path) ReadMode $ \h -> do
            bs <- hGetAll fs0 h
            case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> dec) bs of
              Left  err        -> throwIO $ TVarBackingStoreDeserialiseExn err
              Right (extra, x) -> do
                unless (BSL.null extra) $ throwIO TVarIncompleteDeserialiseExn
                traceWith tracer $ BSInitialisedFromCopy path
                pure x
        InitFromValues slot values -> do
          traceWith tracer $ BSInitialisingFromValues slot
          pure (slot, values)
      newTVarIO $ TVarBackingStoreContents slot values
    traceWith tracer $ BSOpened Nothing
    pure BackingStore {
        bsClose    = do
            traceWith tracer BSClosing
            catch
              (atomically $ do
                  guardClosed ref
                  writeTVar ref TVarBackingStoreContentsClosed
              )
              (\case
                TVarBackingStoreClosedExn -> traceWith tracer BSAlreadyClosed
                e -> throwIO e
              )
            traceWith tracer BSClosed
      , bsCopy = \(SomeHasFS fs) (BackingStorePath path) -> do
          traceWith tracer $ BSCopying path
          join $ atomically $ do
            readTVar ref >>= \case
              TVarBackingStoreContentsClosed                ->
                throwSTM TVarBackingStoreClosedExn
              TVarBackingStoreContents slot values -> pure $ do
                exists <- doesDirectoryExist fs path
                when exists $ throwIO TVarBackingStoreDirectoryExists
                createDirectory fs path
                withFile fs (extendPath path) (WriteMode MustBeNew) $ \h -> do
                  void $ hPutAll fs h $ CBOR.toLazyByteString $ CBOR.toCBOR slot <> enc values
          traceWith tracer $ BSCopied path
      , bsValueHandle = do
          traceWith tracer BSCreatingValueHandle
          vh <- join $ atomically $ do
           readTVar ref >>= \case
             TVarBackingStoreContentsClosed                ->
               throwSTM TVarBackingStoreClosedExn
             TVarBackingStoreContents slot values -> pure $ do
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
                        TVarBackingStoreClosedExn            -> traceWith tracer BSAlreadyClosed
                        TVarBackingStoreValueHandleClosedExn -> traceWith tracer (BSValueHandleTrace Nothing BSVHAlreadyClosed)
                        e                                    -> throwIO e
                      )
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHClosed
                 , bsvhRangeRead = \rq -> do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRangeReading
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ rangeRead_ rq values
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRangeRead
                     pure r
                 , bsvhRead      = \keys -> do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHReading
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ lookup_ keys values
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHRead
                     pure r
                 , bsvhStat = do
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHStatting
                     r <- atomically $ do
                       guardClosed ref
                       guardHandleClosed refHandleClosed
                       pure $ Statistics slot (count_ values)
                     traceWith tracer $ BSValueHandleTrace Nothing BSVHStatted
                     pure r
                 }
          traceWith tracer BSCreatedValueHandle
          pure vh
      , bsWrite    = \slot2 diff -> do
         traceWith tracer $ BSWriting slot2
         slot1 <- atomically $ do
          readTVar ref >>= \case
            TVarBackingStoreContentsClosed        ->
              throwSTM TVarBackingStoreClosedExn
            TVarBackingStoreContents slot1 values -> do
              unless (slot1 <= At slot2) $
                throwSTM $ TVarBackingStoreNonMonotonicSeq (At slot2) slot1
              writeTVar ref $
                TVarBackingStoreContents
                  (At slot2)
                  (forwardValues_ values diff)
              pure slot1
         traceWith tracer $ BSWritten slot1 slot2
      }
  where
    extendPath path =
      fsPathFromList $ fsPathToList path <> [fromString "tvar"]

guardClosed ::
     IOLike m
  => StrictTVar m (TVarBackingStoreContents ks vs)
  -> STM m ()
guardClosed ref = readTVar ref >>= \case
  TVarBackingStoreContentsClosed -> throwSTM TVarBackingStoreClosedExn
  TVarBackingStoreContents _ _   -> pure ()

guardHandleClosed ::
     IOLike m
  => StrictTVar m Bool
  -> STM m ()
guardHandleClosed refHandleClosed = do
  isClosed <- readTVar refHandleClosed
  when isClosed $ throwSTM TVarBackingStoreValueHandleClosedExn

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

newtype StoreDirIsIncompatible = StoreDirIsIncompatible FsErrorPath
  deriving anyclass (Exception)

instance Show StoreDirIsIncompatible where
  show (StoreDirIsIncompatible p) =
       "In-Memory database not found in the database directory: "
    <> show p
    <> ".\nPre-UTxO-HD and LMDB implementations are incompatible with the In-Memory \
       \ implementation. Please delete your ledger database directory."
