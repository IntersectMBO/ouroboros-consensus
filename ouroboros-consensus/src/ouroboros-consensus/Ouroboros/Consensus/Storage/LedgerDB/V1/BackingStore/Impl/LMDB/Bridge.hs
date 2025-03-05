{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| Alternatives to LMDB operations that do not rely on @'Serialise'@ instances

  We cannot (easily and without runtime overhead) satisfy the @'Serialise'@
  constraints that the @lmdb-simple@ operations require. We have access to the
  codification and decodification functions provided in @'CodecMK'@, thus, we
  redefine parts of the internal @LMDB.Simple@ operations here. The
  redefinitions are largely analogous to their counterparts, though they thread
  through explicit CBOR encoders and decoders.
-}
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Bridge (
    -- * Cursor
    fromCodecMK
  , runCursorAsTransaction'
    -- * Internal: get and put
  , delete
  , deleteBS
  , get
  , getBS
  , getBS'
  , indexedGet
  , indexedPut
  , put
  , putBS
  ) where

import           Control.Exception (assert)
import           Control.Monad ((>=>))
import qualified Control.Monad as Monad
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import           Data.MemPack
import           Data.MemPack.Buffer
import           Database.LMDB.Raw (MDB_val (..), mdb_reserve')
import           Database.LMDB.Simple (Database, Mode (ReadWrite), Transaction)
import           Database.LMDB.Simple.Cursor (CursorM)
import qualified Database.LMDB.Simple.Cursor as Cursor
import qualified Database.LMDB.Simple.Internal as Internal
import           Foreign (Storable (peek, poke), castPtr)
import           GHC.Ptr (Ptr (..))
import           Ouroboros.Consensus.Util.IndexedMemPack

instance Buffer MDB_val where
  bufferByteCount = fromIntegral . mv_size
  {-# INLINE bufferByteCount #-}

  buffer (MDB_val _ (Ptr addr#)) _ f = f addr#
  {-# INLINE buffer #-}

{-------------------------------------------------------------------------------
  Internal: peek and poke
-------------------------------------------------------------------------------}

peekMDBValMemPack :: MemPack a => Ptr MDB_val -> IO a
peekMDBValMemPack = peek >=> pure . unpackError

pokeMDBValMemPack :: MemPack a => Ptr MDB_val -> a -> IO ()
pokeMDBValMemPack ptr x = Internal.marshalOutBS (packByteString x) (poke ptr)

indexedPeekMDBValMemPack :: IndexedMemPack idx a => idx -> Ptr MDB_val -> IO a
indexedPeekMDBValMemPack idx = peek >=> pure . indexedUnpackError idx

indexedPokeMDBValMemPack :: IndexedMemPack idx a => idx -> Ptr MDB_val -> a -> IO ()
indexedPokeMDBValMemPack idx ptr x = Internal.marshalOutBS (indexedPackByteString idx x) (poke ptr)

{-------------------------------------------------------------------------------
  Cursor
-------------------------------------------------------------------------------}

fromCodecMK :: (IndexedMemPack idx v, MemPack k) => idx -> Cursor.PeekPoke k v
fromCodecMK idx = Cursor.PeekPoke {
    Cursor.kPeek = peekMDBValMemPack
  , Cursor.vPeek = indexedPeekMDBValMemPack idx
  , Cursor.kPoke = pokeMDBValMemPack
  , Cursor.vPoke = indexedPokeMDBValMemPack idx
  }

-- | Wrapper around @'Cursor.runCursorAsTransaction''@ that requires a
-- @'CodecMK'@ instead of a @'PeekPoke'@.
runCursorAsTransaction' ::
     (MemPack k, IndexedMemPack idx v)
  => idx
  -> CursorM k v mode a
  -> Database k v
  -> Transaction mode a
runCursorAsTransaction' idx cm db =
  Cursor.runCursorAsTransaction' cm db (fromCodecMK idx)

{-------------------------------------------------------------------------------
  Internal: get, put and delete
-------------------------------------------------------------------------------}

get ::
     (MemPack k, MemPack v)
  => Database k v
  -> k
  -> Transaction mode (Maybe v)
get db = getBS db . packByteString

getBS ::
     MemPack v
  => Database k v
  -> BS.ByteString
  -> Transaction mode (Maybe v)
getBS db k = getBS' db k >>=
    maybe (return Nothing) (liftIO . fmap Just . pure . unpackError)

indexedGet ::
     (IndexedMemPack idx v, MemPack k)
  => idx
  -> Database k v
  -> k
  -> Transaction mode (Maybe v)
indexedGet idx db = indexedGetBS idx db . packByteString

indexedGetBS ::
     IndexedMemPack idx v
  => idx
  -> Database k v
  -> BS.ByteString
  -> Transaction mode (Maybe v)
indexedGetBS idx db k = getBS' db k >>=
    maybe (return Nothing) (liftIO . fmap Just . pure . indexedUnpackError idx)

getBS' :: Database k v -> BS.ByteString -> Transaction mode (Maybe MDB_val)
getBS' = Internal.getBS'

put ::
     (MemPack v, MemPack k)
  => Database k v
  -> k
  -> v
  -> Transaction ReadWrite ()
put db = putBS db . packByteString

putBS ::
     MemPack v
  => Database k v
  -> BS.ByteString
  -> v
  -> Transaction ReadWrite ()
putBS (Internal.Db _ dbi) keyBS value = Internal.Txn $ \txn ->
  Internal.marshalOutBS keyBS $ \kval -> do
    let valueBS = packByteString value
        sz = BS.length valueBS
    MDB_val len ptr <- mdb_reserve' Internal.defaultWriteFlags txn dbi kval sz
    let len' = fromIntegral len
    Monad.void $ assert (len' == sz) $ Internal.copyBS (castPtr ptr, len') valueBS

indexedPut ::
     (IndexedMemPack idx v, MemPack k)
  => idx
  -> Database k v
  -> k
  -> v
  -> Transaction ReadWrite ()
indexedPut idx db = indexedPutBS idx db . packByteString

indexedPutBS ::
     IndexedMemPack idx v
  => idx
  -> Database k v
  -> BS.ByteString
  -> v
  -> Transaction ReadWrite ()
indexedPutBS idx (Internal.Db _ dbi) keyBS value = Internal.Txn $ \txn ->
  Internal.marshalOutBS keyBS $ \kval -> do
    let valueBS = indexedPackByteString idx value
        sz = BS.length valueBS
    MDB_val len ptr <- mdb_reserve' Internal.defaultWriteFlags txn dbi kval sz
    let len' = fromIntegral len
    Monad.void $ assert (len' == sz) $ Internal.copyBS (castPtr ptr, len') valueBS

delete ::
     MemPack k
  => Database k v
  -> k
  -> Transaction ReadWrite Bool
delete db = deleteBS db . packByteString

deleteBS :: Database k v -> BS.ByteString -> Transaction ReadWrite Bool
deleteBS = Internal.deleteBS
