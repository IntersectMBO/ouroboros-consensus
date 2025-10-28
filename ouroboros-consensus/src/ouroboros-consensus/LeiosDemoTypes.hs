{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module LeiosDemoTypes (module LeiosDemoTypes) where

import           Cardano.Binary (enforceSize)
import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import           Data.Int (Int64)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64)
import qualified Database.SQLite3.Direct as DB
import qualified Numeric
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.Exit (die)

type BytesSize = Word32

newtype EbId = MkEbId Int
  deriving (Eq, Ord)

prettyEbId :: EbId -> String
prettyEbId (MkEbId i) = show i

fromIntegralEbId :: Integral a => EbId -> a
fromIntegralEbId (MkEbId x) = fromIntegral x

newtype PeerId a = MkPeerId a
  deriving (Eq, Ord)

newtype EbHash = MkEbHash ByteString
  deriving (Eq, Ord, Show)

newtype TxHash = MkTxHash ByteString
  deriving (Eq, Ord, Show)

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

data LeiosPoint = MkLeiosPoint SlotNo EbHash
  deriving (Show)

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) (MkEbHash bytes)) =
    "(" ++ show slotNo ++ ", " ++ BS8.unpack (BS16.encode bytes) ++ ")"

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot (MkEbHash ebHash)) =
    CBOR.encodeListLen 2
 <> encode ebSlot
 <> CBOR.encodeBytes ebHash

decodeLeiosPoint :: Decoder s LeiosPoint
decodeLeiosPoint = do
    enforceSize (fromString "LeiosPoint") 2
    MkLeiosPoint <$> decode <*> (MkEbHash <$> decode)

-----

data LeiosFetchRequest =
    LeiosBlockRequest LeiosBlockRequest
  |
    LeiosBlockTxsRequest LeiosBlockTxsRequest

data LeiosBlockRequest =
    -- |
    --
    -- The size isn't sent to the peer, but it's used to validate the reponse
    -- when it arrives.
    MkLeiosBlockRequest
        !LeiosPoint
        !BytesSize

data LeiosBlockTxsRequest =
    -- |
    --
    -- The hashes aren't sent to the peer, but they are used to validate the
    -- response when it arrives.
  MkLeiosBlockTxsRequest
        !LeiosPoint
        [(Word16, Word64)]
        !(V.Vector TxHash)

prettyLeiosBlockTxsRequest :: LeiosBlockTxsRequest -> String
prettyLeiosBlockTxsRequest (MkLeiosBlockTxsRequest p bitmaps _txHashes) =
    unwords
  $ "MsgLeiosBlockTxs" : prettyLeiosPoint p : map prettyBitmap bitmaps

prettyBitmap :: (Word16, Word64) -> String
prettyBitmap (idx, bitmap) =
    show idx ++ ":0x" ++ Numeric.showHex bitmap ""

-----

--
-- Compare the following data types to the @LeiosFetchDynamicEnv@ and
-- @LeiosFetchState@ types in the Leios model exe
--
-- These data types are organized differently because they are organized by the
-- patterns of access to the "Ouroboros.Consensus.NodeKernel"'s shared state.
--

data LeiosPeerVars m = MkLeiosPeerVars {
    -- written to only by the LeiosNotify client (TODO and eviction)
    offerings :: !(MVar m (Set EbId, Set EbId))
  ,
    -- | written to by the fetch logic and the LeiosFetch client
    --
    -- These are the requests the fetch logic assumes will be sent, but have
    -- not already been sent.
    --
    -- Each client also maintains its own queue of requests that were
    -- actually sent (ie dequeued from this sequence but their reply
    -- hasn't yet arrived).
    --
    -- Note that @requestedPerPeer@ is the list maintained per client,
    -- whereas this list is not present in the model exe.
    --
    -- This is a 'TVar' so that the LeiosFetch client can wait on either it or
    -- the Diffusion Layer's control message to be actionable.
    requestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  }

newLeiosPeerVars :: IOLike m => m (LeiosPeerVars m)
newLeiosPeerVars = do
    offerings <- MVar.newMVar (Set.empty, Set.empty)
    requestsToSend <- StrictSTM.newTVarIO Seq.empty
    pure MkLeiosPeerVars {offerings, requestsToSend}

data LeiosEbBodies = MkLeiosEbBodies {
    acquiredEbBodies :: !(Set EbId)
  ,
    missingEbBodies :: !(Map EbId BytesSize)
  ,
    ebPoints :: !(IntMap {- SlotNo -} (Map EbHash EbId))
  ,
    ebPointsInverse :: !(IntMap {- EbId -} EbHash)
  }

emptyLeiosEbBodies :: LeiosEbBodies
emptyLeiosEbBodies =
    MkLeiosEbBodies
        Set.empty
        Map.empty
        IntMap.empty
        IntMap.empty

prettyLeiosEbBodies :: LeiosEbBodies -> String
prettyLeiosEbBodies x =
  unwords
  [
        "LeiosEbBodies:"
      , 
        "acquiredEbBodies = " ++ show (Set.size acquiredEbBodies)
      ,
        "missingEbBodies = " ++ show (Map.size missingEbBodies)
   ]
  where
    MkLeiosEbBodies {
        acquiredEbBodies
      ,
        missingEbBodies
      } = x

data LeiosOutstanding pid = MkLeiosOutstanding {
    requestedEbPeers :: !(Map EbId (Set (PeerId pid)))
  ,
    requestedTxPeers :: !(Map TxHash (Set (PeerId pid)))
  ,
    requestedBytesSizePerPeer :: !(Map (PeerId pid) BytesSize)
  ,
    requestedBytesSize :: !BytesSize
  ,
    -- TODO this might be far too big for the heap
    cachedTxs :: !(Map TxHash BytesSize)
  ,
    -- | The txs that still need to be sourced
    --
    -- * A @MsgLeiosBlock@ inserts into 'missingEbTxs' if that EB has never
    --   been received before.
    --
    -- * Every @MsgLeiosBlockTxs@ deletes from 'missingEbTxs', but that delete
    --   will be a no-op for all except the first to arrive carrying this EbTx.
    --
    -- * EbTxs are deleted from 'missingEbTxs' when a 'toCopy' is scheduled
    --   (b/c we can immediately stop requesting it from any peer). This delete
    --   will never be a no-op (except maybe in a race?).
    --
    -- TODO this is far too big for the heap
    missingEbTxs :: !(Map EbId (IntMap (TxHash, BytesSize)))
  ,
    -- TODO this is far too big for the heap
    --
    -- inverse of missingEbTxs
    txOffsetss :: !(Map TxHash (Map EbId Int))
  ,
    -- | How many txs of each EB are not yet in the @ebTxs@ table
    --
    -- These NULLs are blocking the node from sending @MsgLeiosBlockTxsOffer@
    -- to its downstream peers.
    --
    -- It's different from 'missingEbTxs' in two ways.
    --
    -- * The heap footprint of 'blockingPerEb' doesn't scale with the number of
    --   EbTxs.
    --
    -- * 'blockingPerEb' is only updated when a 'toCopy' /finishes/ instead of as
    --   soon as it's /scheduled/.
    --
    -- We need to be careful not to double-count arrivals. 'blockingPerEb'
    --  should only be decremented by the arrival of a @MsgLeiosBlockTx@ if
    --
    -- * The EbTx is in 'missingEbTxs'.
    --
    -- * The EbTx is in 'toCopy' (and therefore not in 'missingEbTxs'). The
    --   handler shoulder also remove it from 'toCopy'.
    blockingPerEb :: !(Map EbId Int)
  ,
    toCopy :: !(Map EbId (IntMap BytesSize))
  ,
    toCopyBytesSize :: !BytesSize
  ,
    toCopyCount :: !Int
  }

emptyLeiosOutstanding :: LeiosOutstanding pid
emptyLeiosOutstanding =
    MkLeiosOutstanding
        Map.empty
        Map.empty
        Map.empty
        0
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        0
        0

prettyLeiosOutstanding :: LeiosOutstanding pid -> String
prettyLeiosOutstanding x =
  unlines $ map ("    [leios] " ++) $
  [ 
        "requestedEbPeers = " ++ unwords (map prettyEbId (Map.keys requestedEbPeers))
      ,
        "requestedTxPeers = " ++ unwords (map prettyTxHash (Map.keys requestedTxPeers))
      ,
        "requestedBytesSizePerPeer = " ++ show (Map.elems requestedBytesSizePerPeer)
      ,
        "requestedBytesSize = " ++  show requestedBytesSize
      ,
        "missingEbTxs = " ++ unwords [ (prettyEbId k ++ "__" ++ show (IntMap.size v)) | (k, v) <- Map.toList missingEbTxs ]
      ,
        "blockingPerEb = " ++ unwords [ (prettyEbId k ++ "__" ++ show c) | (k, c) <- Map.toList blockingPerEb ]
      ,
        "toCopy = " ++ unwords [ (prettyEbId k ++ "__" ++ show (IntMap.size v)) | (k, v) <- Map.toList toCopy ]
      ,
        "toCopyBytesSize = " ++  show toCopyBytesSize
      ,
        "toCopyCount = " ++  show toCopyCount
      ,
        ""
   ]
  where
    MkLeiosOutstanding {
        requestedEbPeers
      ,
        requestedTxPeers
      ,
        requestedBytesSizePerPeer
      ,
        requestedBytesSize
      ,
        missingEbTxs
      ,
        blockingPerEb
      ,
        toCopy
      ,
        toCopyBytesSize
      ,
        toCopyCount
      } = x

-----

newtype LeiosTx = MkLeiosTx ByteString
  deriving (Show)

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx (MkLeiosTx bytes) = CBOR.encodeBytes bytes

decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx = MkLeiosTx <$> CBOR.decodeBytes

data LeiosEb = MkLeiosEb !(V.Vector (TxHash, BytesSize))
  deriving (Show)

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb v) =
    V.foldl
        (\acc (MkTxHash bytes, txBytesSize) ->
            acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
        )
        (CBOR.encodeMapLen $ fromIntegral $ V.length v)
        v

decodeLeiosEb :: Decoder s LeiosEb
decodeLeiosEb = do
    n <- CBOR.decodeMapLen
    -- TODO does V.generateM allocate exacly one buffer, via the hint?
    --
    -- If not, we could do so manually by relying on the fact that Decoder is
    -- ultimate in ST.
    fmap MkLeiosEb $ V.generateM n $ \_i -> do
        (,) <$> (fmap MkTxHash CBOR.decodeBytes) <*> CBOR.decodeWord32

-----

maxMsgLeiosBlockBytesSize :: BytesSize
maxMsgLeiosBlockBytesSize = 500 * 10^(3 :: Int)   -- from CIP-0164's recommendations

minEbItemBytesSize :: BytesSize
minEbItemBytesSize = (32 - hashOverhead) + minSizeOverhead
  where
    hashOverhead = 1 + 1   -- bytestring major byte + a length = 32
    minSizeOverhead = 1 + 1   -- int major byte + a value at low as 55

maxEbItems :: Int
maxEbItems =
    fromIntegral
  $         (maxMsgLeiosBlockBytesSize - msgOverhead - sequenceOverhead)
    `div`
            minEbItemBytesSize
  where
    msgOverhead = 1 + 1   -- short list len + small word
    sequenceOverhead = 1 + 2   -- sequence major byte + a length > 255

-----

data SomeLeiosDb m = forall stmt. MkSomeLeiosDb (LeiosDb stmt m)

data LeiosDb stmt m = MkLeiosDb {
    dbBindBlob :: !(stmt -> DB.ParamIndex -> ByteString -> m ())
  ,
    dbBindInt64 :: !(stmt -> DB.ParamIndex -> Int64 -> m ())
  ,
    dbColumnBlob :: !(stmt -> DB.ColumnIndex -> m ByteString)
  ,
    dbColumnInt64 :: !(stmt -> DB.ColumnIndex -> m Int64)
  ,
    dbExec :: !(DB.Utf8 -> m ())
  ,
    dbFinalize :: !(stmt -> m ())
  ,
    dbPrepare :: !(DB.Utf8 -> m stmt)
  ,
    dbReset :: !(stmt -> m ())
  ,
    dbStep :: !(stmt -> m DB.StepResult)
  ,
    dbStep1 :: !(stmt -> m ())
  }

leiosDbFromSqliteDirect :: DB.Database -> LeiosDb DB.Statement IO
leiosDbFromSqliteDirect db = MkLeiosDb {
    dbBindBlob = \stmt p v -> withDie $ DB.bindBlob stmt p v
  ,
    dbBindInt64 = \stmt p v -> withDie $ DB.bindInt64 stmt p v
  ,
    dbColumnBlob = \stmt c -> DB.columnBlob stmt c
  ,
    dbColumnInt64 = \stmt c -> DB.columnInt64 stmt c
  ,
    dbExec = \q -> withDieMsg $ DB.exec db q
  ,
    dbFinalize = \stmt -> withDie $ DB.finalize stmt
  ,
    dbPrepare = \q -> withDieJust $ DB.prepare db q
  ,
    dbReset = \stmt -> withDie $ DB.reset stmt
  ,
    dbStep = \stmt -> withDie $ DB.stepNoCB stmt
  ,
    dbStep1 = \stmt -> withDieDone $ DB.stepNoCB stmt
  }

withDiePoly :: Show b => (e -> b) -> IO (Either e a) -> IO a
withDiePoly f io =
    io >>= \case
        Left e -> die $ "LeiosDb: " ++ show (f e)
        Right x -> pure x

withDieMsg :: IO (Either (DB.Error, DB.Utf8) a) -> IO a
withDieMsg = withDiePoly snd

withDie :: IO (Either DB.Error a) -> IO a
withDie = withDiePoly id

withDieJust :: IO (Either DB.Error (Maybe a)) -> IO a
withDieJust io =
    withDie io >>= \case
        Nothing -> die $ "LeiosDb: [Just] " ++ "impossible!"
        Just x -> pure x

withDieDone :: IO (Either DB.Error DB.StepResult) -> IO ()
withDieDone io =
    withDie io >>= \case
        DB.Row -> die $ "LeiosDb: [Done] " ++ "impossible!"
        DB.Done -> pure ()

-----

demoNewLeiosDbConnectionIO :: IO (SomeLeiosDb IO)
demoNewLeiosDbConnectionIO = do
    dbPath <- lookupEnv "LEIOS_DB_PATH" >>= \case
        Nothing -> die "You must define the LEIOS_DB_PATH variable for this demo."
        Just x -> pure x
    newLeiosDbConnectionIO dbPath

newLeiosDbConnectionIO :: FilePath -> IO (SomeLeiosDb IO)
newLeiosDbConnectionIO dbPath = do
    doesFileExist dbPath >>= \case
        False -> die $ "No such LeiosDb file: " ++ dbPath
        True -> do
            db <- withDieMsg $ DB.open (fromString dbPath)
            let db' = leiosDbFromSqliteDirect db
            dbExec db' (fromString sql_attach_memTxPoints)
            pure $ MkSomeLeiosDb db'

sql_attach_memTxPoints :: String
sql_attach_memTxPoints =
    "ATTACH DATABASE ':memory:' AS mem;\n\
    \\n\
    \CREATE TABLE mem.txPoints (\n\
    \    ebId INTEGER NOT NULL\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebId ASC, txOffset ASC)\n\
    \  ) WITHOUT ROWID;\n\
    \"

-----

-- TODO which of these limits are allowed to be exceeded by at most one
-- request?
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv {
    -- | At most this many outstanding bytes requested from all peers together
    maxRequestedBytesSize :: BytesSize
  ,
    -- | At most this many outstanding bytes requested from each peer
    maxRequestedBytesSizePerPeer :: BytesSize
  ,
    -- | At most this many outstanding bytes per request
    maxRequestBytesSize :: BytesSize
  ,
    -- | At most this many outstanding requests for each EB body
    maxRequestsPerEb :: Int
  ,
    -- | At most this many outstanding requests for each individual tx
    maxRequestsPerTx :: Int
  ,
    -- | At most this many bytes are scheduled to be copied from the TxCache to the EbStore
    maxToCopyBytesSize :: BytesSize
  ,
    -- | At most this many txs are scheduled to be copied from the TxCache to the EbStore
    maxToCopyCount :: Int
  ,
    -- | @maximumIngressQueue@ for LeiosNotify
    maxLeiosNotifyIngressQueue :: BytesSize
  ,
    -- | @maximumIngressQueue@ for LeiosFetch
    maxLeiosFetchIngressQueue :: BytesSize
  }

demoLeiosFetchStaticEnv :: LeiosFetchStaticEnv
demoLeiosFetchStaticEnv =
    MkLeiosFetchStaticEnv {
        maxRequestedBytesSize = 50 * million
      ,
        maxRequestedBytesSizePerPeer = 5 * million
      ,
        maxRequestBytesSize = 500 * thousand
      ,
        maxRequestsPerEb = 2
      ,
        maxRequestsPerTx = 2
      ,
        maxToCopyBytesSize = 100 * millionBase2
      ,
        maxToCopyCount = 100 * thousand
      ,
        maxLeiosNotifyIngressQueue = 1 * millionBase2
      ,
        maxLeiosFetchIngressQueue = 50 * millionBase2
      }
  where
    million :: Num a => a
    million = 10^(6 :: Int)
    millionBase2 :: Num a => a
    millionBase2 = 2^(20 :: Int)
    thousand :: Num a => a
    thousand = 10^(3 :: Int)

-----

data LeiosNotification =
    LeiosOfferBlock EbId BytesSize
  |
    LeiosOfferBlockTxs EbId
