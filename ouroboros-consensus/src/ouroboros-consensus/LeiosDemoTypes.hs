{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}

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
import           Control.Monad.Class.MonadThrow (MonadThrow, bracket,
                     generalBracket)
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack
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

prettyEbHash :: EbHash -> String
prettyEbHash (MkEbHash bytes) = BS8.unpack (BS16.encode bytes)

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
    show idx ++ ":0x" ++ padding ++ Numeric.showHex bitmap ""
  where
    n = Bits.countLeadingZeros bitmap

    padding = replicate (n `div` 4) '0'

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
    offerings      :: !(MVar m (Set EbId, Set EbId))
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
    missingEbBodies  :: !(Map EbId BytesSize)
  ,
    ebPoints         :: !(IntMap {- SlotNo -} (Map EbHash EbId))
  ,
    ebPointsInverse  :: !(IntMap {- EbId -} EbHash)
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
    requestedEbPeers          :: !(Map EbId (Set (PeerId pid)))
  ,
    requestedTxPeers          :: !(Map TxHash (Set (PeerId pid)))
  ,
    requestedBytesSizePerPeer :: !(Map (PeerId pid) BytesSize)
  ,
    requestedBytesSize        :: !BytesSize
  ,
    -- TODO this might be far too big for the heap
    cachedTxs                 :: !(Map TxHash BytesSize)
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
    missingEbTxs              :: !(Map EbId (IntMap (TxHash, BytesSize)))
  ,
    -- TODO this is far too big for the heap
    --
    -- inverse of missingEbTxs
    txOffsetss                :: !(Map TxHash (Map EbId Int))
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
    blockingPerEb             :: !(Map EbId Int)
  ,
    toCopy                    :: !(Map EbId (IntMap BytesSize))
  ,
    toCopyBytesSize           :: !BytesSize
  ,
    toCopyCount               :: !Int
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

leiosTxBytesSize:: LeiosTx -> BytesSize
leiosTxBytesSize (MkLeiosTx bytes) =
    majorByte + argument + fromIntegral n
  where
    majorByte = 1
    -- ASSUMPTION: greater than 55 and at most 2^14
    argument = 1 + (if n >= 2^(8::Int) then 1 else 0)
    n = BS.length bytes

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx (MkLeiosTx bytes) = CBOR.encodeBytes bytes

decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx = MkLeiosTx <$> CBOR.decodeBytes

data LeiosEb = MkLeiosEb !(V.Vector (TxHash, BytesSize))
  deriving (Show)

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

leiosEbBytesSize:: LeiosEb -> BytesSize
leiosEbBytesSize (MkLeiosEb items) =
    majorByte + argument + (V.sum $ V.map (each . snd) items)
  where
    majorByte = 1
    -- ASSUMPTION: less than 14000
    argument = 1 + (if V.length items >= 2^(8::Int) then 1 else 0)

    -- ASSUMPTION: greater than 55 and at most 2^14
    each sz = 1 + 32 + 1 + 1 + (if sz >= 2^(8::Int) then 1 else 0)

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
    dbBindBlob_ :: !(HasCallStack => stmt -> DB.ParamIndex -> ByteString -> m ())
  ,
    dbBindInt64_ :: !(HasCallStack => stmt -> DB.ParamIndex -> Int64 -> m ())
  ,
    dbColumnBlob_ :: !(HasCallStack => stmt -> DB.ColumnIndex -> m ByteString)
  ,
    dbColumnInt64_ :: !(HasCallStack => stmt -> DB.ColumnIndex -> m Int64)
  ,
    dbExec_ :: !(HasCallStack => DB.Utf8 -> m ())
  ,
    dbFinalize_ :: !(HasCallStack => stmt -> m ())
  ,
    dbPrepare_ :: !(HasCallStack => DB.Utf8 -> m stmt)
  ,
    dbReset_ :: !(HasCallStack => stmt -> m ())
  ,
    dbStep_ :: !(HasCallStack => stmt -> m DB.StepResult)
  ,
    dbStep1_ :: !(HasCallStack => stmt -> m ())
  }

dbBindBlob :: HasCallStack => LeiosDb stmt m -> stmt -> DB.ParamIndex -> ByteString -> m ()
dbBindBlob = dbBindBlob_

dbBindInt64 :: HasCallStack => LeiosDb stmt m -> stmt -> DB.ParamIndex -> Int64 -> m ()
dbBindInt64 = dbBindInt64_

dbColumnBlob :: HasCallStack => LeiosDb stmt m -> stmt -> DB.ColumnIndex -> m ByteString
dbColumnBlob = dbColumnBlob_

dbColumnInt64 :: HasCallStack => LeiosDb stmt m -> stmt -> DB.ColumnIndex -> m Int64
dbColumnInt64 = dbColumnInt64_

dbExec :: HasCallStack => LeiosDb stmt m -> DB.Utf8 -> m ()
dbExec = dbExec_

dbFinalize :: HasCallStack => LeiosDb stmt m -> stmt -> m ()
dbFinalize = dbFinalize_

dbPrepare :: HasCallStack => LeiosDb stmt m -> DB.Utf8 -> m stmt
dbPrepare = dbPrepare_

dbWithPrepare ::
    (HasCallStack, MonadThrow m)
 =>
    LeiosDb stmt m -> DB.Utf8 -> (stmt -> m r) -> m r
dbWithPrepare db q k = bracket (dbPrepare db q) (dbFinalize db) k

dbWithBEGIN ::
    (HasCallStack, IOLike m)
 =>
    LeiosDb stmt m -> m r -> m r
dbWithBEGIN db k = do
     fmap fst
   $ generalBracket
        (dbExec db (fromString "BEGIN"))
        (\() -> \case
            MonadThrow.ExitCaseSuccess _ -> dbExec db (fromString "COMMIT")
            MonadThrow.ExitCaseException _ -> dbExec db (fromString "ROLLBACK")
            MonadThrow.ExitCaseAbort -> dbExec db (fromString "ROLLBACK")
        )
        (\() -> k)

dbReset :: HasCallStack => LeiosDb stmt m -> stmt -> m ()
dbReset = dbReset_

dbStep :: HasCallStack => LeiosDb stmt m -> stmt -> m DB.StepResult
dbStep = dbStep_

dbStep1 :: HasCallStack => LeiosDb stmt m -> stmt -> m ()
dbStep1 = dbStep1_

leiosDbFromSqliteDirect :: DB.Database -> LeiosDb DB.Statement IO
leiosDbFromSqliteDirect db = MkLeiosDb {
    dbBindBlob_ = \stmt p v -> withDie $ DB.bindBlob stmt p v
  ,
    dbBindInt64_ = \stmt p v -> withDie $ DB.bindInt64 stmt p v
  ,
    dbColumnBlob_ = \stmt c -> DB.columnBlob stmt c
  ,
    dbColumnInt64_ = \stmt c -> DB.columnInt64 stmt c
  ,
    dbExec_ = \q -> withDieMsg $ DB.exec db q
  ,
    dbFinalize_ = \stmt -> withDie $ DB.finalize stmt
  ,
    dbPrepare_ = \q -> withDieJust $ DB.prepare db q
  ,
    dbReset_ = \stmt -> withDie $ DB.reset stmt
  ,
    dbStep_ = \stmt -> withDie $ DB.stepNoCB stmt
  ,
    dbStep1_ = \stmt -> withDieDone $ DB.stepNoCB stmt
  }

withDiePoly :: (HasCallStack, Show b) => (e -> b) -> IO (Either e a) -> IO a
withDiePoly f io =
    io >>= \case
        Left e -> dieStack $ "LeiosDb: " ++ show (f e)
        Right x -> pure x

withDieMsg :: HasCallStack => IO (Either (DB.Error, DB.Utf8) a) -> IO a
withDieMsg = withDiePoly snd

withDie :: HasCallStack => IO (Either DB.Error a) -> IO a
withDie = withDiePoly id

withDieJust :: HasCallStack => IO (Either DB.Error (Maybe a)) -> IO a
withDieJust io =
    withDie io >>= \case
        Nothing -> dieStack $ "LeiosDb: [Just] " ++ "impossible!"
        Just x -> pure x

withDieDone :: HasCallStack => IO (Either DB.Error DB.StepResult) -> IO ()
withDieDone io =
    withDie io >>= \case
        DB.Row -> dieStack $ "LeiosDb: [Done] " ++ "impossible!"
        DB.Done -> pure ()

dieStack :: HasCallStack => String -> IO a
dieStack s = die $ s ++ "\n\n" ++ GHC.Stack.prettyCallStack GHC.Stack.callStack

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
    maxRequestedBytesSize        :: BytesSize
  ,
    -- | At most this many outstanding bytes requested from each peer
    maxRequestedBytesSizePerPeer :: BytesSize
  ,
    -- | At most this many outstanding bytes per request
    maxRequestBytesSize          :: BytesSize
  ,
    -- | At most this many outstanding requests for each EB body
    maxRequestsPerEb             :: Int
  ,
    -- | At most this many outstanding requests for each individual tx
    maxRequestsPerTx             :: Int
  ,
    -- | At most this many bytes are scheduled to be copied from the TxCache to the EbStore
    maxToCopyBytesSize           :: BytesSize
  ,
    -- | At most this many txs are scheduled to be copied from the TxCache to the EbStore
    maxToCopyCount               :: Int
  ,
    -- | @maximumIngressQueue@ for LeiosNotify
    maxLeiosNotifyIngressQueue   :: BytesSize
  ,
    -- | @maximumIngressQueue@ for LeiosFetch
    maxLeiosFetchIngressQueue    :: BytesSize
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

-----

data TraceLeiosKernel =
    MkTraceLeiosKernel String
  |
    TraceLeiosBlockAcquired LeiosPoint
  |
    TraceLeiosBlockTxsAcquired LeiosPoint
  deriving (Show)

traceLeiosKernelToObject :: TraceLeiosKernel -> Aeson.Object
traceLeiosKernelToObject = \case
    MkTraceLeiosKernel s -> fromString "msg" Aeson..= Aeson.String (fromString s)
    TraceLeiosBlockAcquired p ->
        let MkLeiosPoint (SlotNo ebSlot) ebHash = p
        in
        (fromString "kind" Aeson..= Aeson.String (fromString "LeiosBlockAcquired"))
        <>
        (fromString "ebHash" Aeson..= Aeson.String (fromString $ prettyEbHash ebHash))
        <>
        (fromString "ebSlot" Aeson..= Aeson.String (fromString $ show ebSlot))
    TraceLeiosBlockTxsAcquired p ->
        let MkLeiosPoint (SlotNo ebSlot) ebHash = p
        in
        (fromString "kind" Aeson..= Aeson.String (fromString "LeiosBlockTxsAcquired"))
        <>
        (fromString "ebHash" Aeson..= Aeson.String (fromString $ prettyEbHash ebHash))
        <>
        (fromString "ebSlot" Aeson..= Aeson.String (fromString $ show ebSlot))

newtype TraceLeiosPeer = MkTraceLeiosPeer String
  deriving (Show)

traceLeiosPeerToObject :: TraceLeiosPeer -> Aeson.Object
traceLeiosPeerToObject (MkTraceLeiosPeer s) = fromString "msg" Aeson..= Aeson.String (fromString s)
