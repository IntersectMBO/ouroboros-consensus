{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory (
    -- * LedgerTablesHandle
    newInMemoryLedgerTablesHandle
    -- * Snapshots
  , loadSnapshot
  , takeSnapshot
  , snapshotToStatePath
  , snapshotToTablePath
  ) where

import Data.Maybe
import           Cardano.Binary as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (decode)
import           Control.Monad (unless, void)
import           Control.Monad.Except (runExceptT)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Prelude hiding (read)
import           System.FS.API
import           System.FS.API.Lazy


{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandleState l =
    LedgerTablesHandleOpen !(LedgerTables l ValuesMK)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTables l ValuesMK) => NoThunks (LedgerTablesHandleState l)

data InMemoryClosedExn = InMemoryClosedExn
  deriving (Show, Exception)

guardClosed :: LedgerTablesHandleState l -> (LedgerTables l ValuesMK -> a) -> a
guardClosed LedgerTablesHandleClosed    _ = error $ show InMemoryClosedExn
guardClosed (LedgerTablesHandleOpen st) f = f st

newInMemoryLedgerTablesHandle ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle someFS@(SomeHasFS hasFS) l = do
  !ioref <- newTVarIO (LedgerTablesHandleOpen l)
  pure LedgerTablesHandle {
      close =
        atomically $ modifyTVar ioref (\_ -> LedgerTablesHandleClosed)
    , duplicate = do
        hs <- readTVarIO ioref
        !x <- guardClosed hs $ newInMemoryLedgerTablesHandle someFS
        pure x
    , read = \keys -> do
        hs <- readTVarIO ioref
        guardClosed hs (\st -> pure $ ltliftA2 rawRestrictValues st keys)
    , readRange = \(f, t) -> do
        hs <- readTVarIO ioref
        guardClosed hs (\(LedgerTables (ValuesMK m)) ->
                          pure . LedgerTables . ValuesMK . Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m)
    , write = \(!diffs) ->
        atomically
        $ modifyTVar ioref
        (\r -> guardClosed r (\st -> LedgerTablesHandleOpen (ltliftA2 rawApplyDiffs st diffs)))
    , writeToDisk = \snapshotName -> do
        createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName, "tables"]
        h <- readTVarIO ioref
        guardClosed h $
          \values ->
            withFile hasFS (mkFsPath [snapshotName, "tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
              void $ hPutAll hasFS hf
                   $ CBOR.toLazyByteString
                   $ valuesMKEncoder values
    , tablesSize = do
        hs <- readTVarIO ioref
        guardClosed hs (\(getLedgerTables -> ValuesMK m) -> pure $ Just $ Map.size m)
    , isOpen = do
        hs <- readTVarIO ioref
        case hs of
          LedgerTablesHandleOpen{}   -> pure True
          LedgerTablesHandleClosed{} -> pure False
    }

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

snapshotToTablePath :: DiskSnapshot -> FsPath
snapshotToTablePath = mkFsPath . (\x -> [x, "tables", "tvar"]) . snapshotToDirName

writeSnapshot ::
     MonadThrow m
  => SomeHasFS m
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> StateRef m (ExtLedgerState blk)
  -> m ()
writeSnapshot fs@(SomeHasFS hasFs) encLedger ds st = do
    createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
    writeExtLedgerState fs encLedger (snapshotToStatePath ds) $ state st
    writeToDisk (tables st) $ snapshotToDirName ds

takeSnapshot ::
     ( MonadThrow m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     )
  => CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> Maybe DiskSnapshot
  -> StateRef m (ExtLedgerState blk)
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot ccfg tracer hasFS dsOverride st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number   = unSlotNo (realPointSlot t)
          snapshot = fromMaybe (DiskSnapshot number Nothing) dsOverride
      diskSnapshots <- listSnapshots hasFS
      if List.any ((== number) . dsNumber) diskSnapshots then
        return Nothing
        else do
        writeSnapshot hasFS (encodeExtLedgerState' ccfg) snapshot st
        traceWith tracer $ TookSnapshot snapshot t
        return $ Just (snapshot, t)

loadSnapshot ::
    ( LedgerDbSerialiseConstraints blk
    , LedgerSupportsProtocol blk
    , IOLike m
    )
    => ResourceRegistry m
    -> CodecConfig blk
    -> SomeHasFS m
    -> DiskSnapshot
    -> m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
loadSnapshot _rr ccfg fs@(SomeHasFS hasFS) ds = do
  eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeExtLedgerState' ccfg) decode (snapshotToStatePath ds)
  case eExtLedgerSt of
    Left err -> pure (Left $ InitFailureRead err)
    Right extLedgerSt -> do
      traceMarkerIO "Loaded state"
      case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
        Origin        -> pure (Left InitFailureGenesis)
        NotOrigin pt -> do
          values <- withFile hasFS ( fsPathFromList
                                   $ fsPathToList (snapshotToDirPath ds)
                                   <> [fromString "tables", fromString "tvar"]) ReadMode $ \h -> do
            bs <- hGetAll hasFS h
            case CBOR.deserialiseFromBytes valuesMKDecoder bs of
              Left  err        -> error $ show err
              Right (extra, x) -> do
                unless (BSL.null extra) $ error "Trailing bytes in snapshot"
                pure x
          Right . (,pt) <$> empty extLedgerSt values (newInMemoryLedgerTablesHandle fs)
