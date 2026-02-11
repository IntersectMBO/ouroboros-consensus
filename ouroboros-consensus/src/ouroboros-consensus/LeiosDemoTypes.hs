{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LeiosDemoTypes (module LeiosDemoTypes) where

import Cardano.Binary (enforceSize, serialize', toCBOR)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Core (EraTx, Tx)
import Cardano.Prelude (NonEmpty, toList, toString, (&))
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (decode, encode)
import Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64)
import Debug.Trace (trace)
import LeiosDemoOnlyTestFetch (LeiosFetch, Message (..))
import qualified Numeric
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , TxMeasureMetrics
  , txMeasureMetricTxSizeBytes
  )
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Text.Pretty.Simple (pShow)

type BytesSize = Word32

newtype PeerId a = MkPeerId a
  deriving (Eq, Ord)

-- Hash algorithm used in leios for EBs and txs
type HASH = Hash.Blake2b_256

newtype EbHash = MkEbHash {ebHashBytes :: ByteString}
  deriving (Eq, Ord, Show)

prettyEbHash :: EbHash -> String
prettyEbHash (MkEbHash bytes) = BS8.unpack (BS16.encode bytes)

newtype TxHash = MkTxHash ByteString
  deriving (Eq, Ord, Show)

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

-- | Uniquely identifies an endorser block in Leios. Could use 'Block SlotNo
-- EbHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EbHash}
  deriving (Show, Eq, Ord)

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

data LeiosFetchRequest
  = LeiosBlockRequest LeiosBlockRequest
  | LeiosBlockTxsRequest LeiosBlockTxsRequest

data LeiosBlockRequest
  = -- |
    --
    -- The size isn't sent to the peer, but it's used to validate the reponse
    -- when it arrives.
    MkLeiosBlockRequest
      !LeiosPoint
      !BytesSize

data LeiosBlockTxsRequest
  = -- |
    --
    -- The hashes aren't sent to the peer, but they are used to validate the
    -- response when it arrives.
    MkLeiosBlockTxsRequest
      !LeiosPoint
      [(Word16, Word64)]
      !(V.Vector TxHash)

prettyLeiosBlockTxsRequest :: LeiosBlockTxsRequest -> String
prettyLeiosBlockTxsRequest (MkLeiosBlockTxsRequest p bitmaps _txHashes) =
  unwords $
    "MsgLeiosBlockTxs" : prettyLeiosPoint p : map prettyBitmap bitmaps

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

data LeiosPeerVars m = MkLeiosPeerVars
  { -- written to only by the LeiosNotify client (TODO and eviction)
    offerings :: !(MVar m (Set EbHash, Set EbHash))
  , requestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  -- ^ written to by the fetch logic and the LeiosFetch client
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
  }

newLeiosPeerVars :: IOLike m => m (LeiosPeerVars m)
newLeiosPeerVars = do
  offerings <- MVar.newMVar (Set.empty, Set.empty)
  requestsToSend <- StrictSTM.newTVarIO Seq.empty
  pure MkLeiosPeerVars{offerings, requestsToSend}

-- REVIEW: Is the acquired/missing here redundant to the maps in LeiosOutstanding?
data LeiosEbBodies = MkLeiosEbBodies
  { acquiredEbBodies :: !(Set EbHash)
  , missingEbBodies :: !(Map LeiosPoint BytesSize)
  , ebPoints :: !(IntMap {- SlotNo -} EbHash)
  }

emptyLeiosEbBodies :: LeiosEbBodies
emptyLeiosEbBodies =
  MkLeiosEbBodies
    Set.empty
    Map.empty
    IntMap.empty

prettyLeiosEbBodies :: LeiosEbBodies -> String
prettyLeiosEbBodies x =
  unwords
    [ "LeiosEbBodies:"
    , "acquiredEbBodies = " ++ show (Set.size acquiredEbBodies)
    , "missingEbBodies = " ++ show (Map.size missingEbBodies)
    ]
 where
  MkLeiosEbBodies
    { acquiredEbBodies
    , missingEbBodies
    } = x

-- | Main data structure used in the Leios fetching logic.
data LeiosOutstanding pid = MkLeiosOutstanding
  { requestedEbPeers :: !(Map EbHash (Set (PeerId pid)))
  , requestedTxPeers :: !(Map TxHash (Set (PeerId pid)))
  , requestedBytesSizePerPeer :: !(Map (PeerId pid) BytesSize)
  , requestedBytesSize :: !BytesSize
  , -- TODO this might be far too big for the heap
    cachedTxs :: !(Map TxHash BytesSize)
  , missingEbTxs :: !(Map LeiosPoint (IntMap (TxHash, BytesSize)))
  -- ^ The txs that still need to be sourced
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
  , -- TODO this is far too big for the heap
    --
    -- inverse of missingEbTxs
    txOffsetss :: !(Map TxHash (Map EbHash Int))
  , blockingPerEb :: !(Map LeiosPoint Int)
  -- ^ How many txs of each EB are not yet in the @ebTxs@ table
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
  , toCopy :: !(Map LeiosPoint (IntMap BytesSize))
  , toCopyBytesSize :: !BytesSize
  , toCopyCount :: !Int
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
  unlines $
    map ("    [leios] " ++) $
      [ "requestedEbPeers = " ++ unwords (map prettyEbHash (Map.keys requestedEbPeers))
      , "requestedTxPeers = " ++ unwords (map prettyTxHash (Map.keys requestedTxPeers))
      , "requestedBytesSizePerPeer = " ++ show (Map.elems requestedBytesSizePerPeer)
      , "requestedBytesSize = " ++ show requestedBytesSize
      , "missingEbTxs = "
          ++ unwords [(prettyLeiosPoint k ++ "__" ++ show (IntMap.size v)) | (k, v) <- Map.toList missingEbTxs]
      , "blockingPerEb = "
          ++ unwords [(prettyLeiosPoint k ++ "__" ++ show c) | (k, c) <- Map.toList blockingPerEb]
      , "toCopy = "
          ++ unwords [(prettyLeiosPoint k ++ "__" ++ show (IntMap.size v)) | (k, v) <- Map.toList toCopy]
      , "toCopyBytesSize = " ++ show toCopyBytesSize
      , "toCopyCount = " ++ show toCopyCount
      , ""
      ]
 where
  MkLeiosOutstanding
    { requestedEbPeers
    , requestedTxPeers
    , requestedBytesSizePerPeer
    , requestedBytesSize
    , missingEbTxs
    , blockingPerEb
    , toCopy
    , toCopyBytesSize
    , toCopyCount
    } = x

-----

newtype LeiosTx = MkLeiosTx ByteString
  deriving Show

leiosTxBytesSize :: LeiosTx -> BytesSize
leiosTxBytesSize (MkLeiosTx bytes) =
  majorByte + argument + fromIntegral n
 where
  majorByte = 1
  -- ASSUMPTION: greater than 55 and at most 2^14
  argument = 1 + (if n >= 2 ^ (8 :: Int) then 1 else 0)
  n = BS.length bytes

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx (MkLeiosTx bytes) = CBOR.encodeBytes bytes

decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx = MkLeiosTx <$> CBOR.decodeBytes

-- TODO: Keep track of the slot of an EB?
data LeiosEb = MkLeiosEb {leiosEbTxs :: !(V.Vector (TxHash, BytesSize))}
  deriving (Show, Eq)

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

mkLeiosEb :: EraTx era => NonEmpty (Tx era) -> LeiosEb
mkLeiosEb txs =
  MkLeiosEb . V.fromList . map go $ toList txs
 where
  go tx =
    let hash = Hash.hashWithSerialiser @HASH toCBOR tx
        byteSize = fromIntegral $ BS.length $ serialize' tx
     in (MkTxHash $ Hash.hashToBytes hash, byteSize)

leiosEbBodyItems :: LeiosEb -> [(Int, TxHash, BytesSize)]
leiosEbBodyItems eb =
  leiosEbTxs eb
    & V.imap (\ix (txh, size) -> (ix, txh, size))
    & toList

leiosEbBytesSize :: LeiosEb -> BytesSize
leiosEbBytesSize (MkLeiosEb items) =
  cborUintSize (V.length items) + V.sum (V.map (each . snd) items)
 where
  each sz = cborBytesSize 32 + cborUintSize sz

  cborBytesSize len = cborUintSize len + len

  cborUintSize n
    | n < 24 = 1
    | n < 0x100 = 2
    | n < 0x10000 = 3
    | otherwise = 5

hashLeiosEb :: LeiosEb -> EbHash
hashLeiosEb =
  MkEbHash . Hash.hashToBytes . Hash.hashWith @HASH id . serialize' . encodeLeiosEb

encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb v) =
  V.foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
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
maxMsgLeiosBlockBytesSize = 500 * 10 ^ (3 :: Int) -- from CIP-0164's recommendations

minEbItemBytesSize :: BytesSize
minEbItemBytesSize = (32 - hashOverhead) + minSizeOverhead
 where
  hashOverhead = 1 + 1 -- bytestring major byte + a length = 32
  minSizeOverhead = 1 + 1 -- int major byte + a value at low as 55

maxTxsPerEb :: Int
maxTxsPerEb =
  fromIntegral $
    (maxMsgLeiosBlockBytesSize - msgOverhead - sequenceOverhead)
      `div` minEbItemBytesSize
 where
  msgOverhead = 1 + 1 -- short list len + small word
  sequenceOverhead = 1 + 2 -- sequence major byte + a length > 255

-----

-- TODO which of these limits are allowed to be exceeded by at most one
-- request?
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv
  { maxRequestedBytesSize :: BytesSize
  -- ^ At most this many outstanding bytes requested from all peers together
  , maxRequestedBytesSizePerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each peer
  , maxRequestBytesSize :: BytesSize
  -- ^ At most this many outstanding bytes per request
  , maxRequestsPerEb :: Int
  -- ^ At most this many outstanding requests for each EB body
  , maxRequestsPerTx :: Int
  -- ^ At most this many outstanding requests for each individual tx
  , maxToCopyBytesSize :: BytesSize
  -- ^ At most this many bytes are scheduled to be copied from the TxCache to the EbStore
  , maxToCopyCount :: Int
  -- ^ At most this many txs are scheduled to be copied from the TxCache to the EbStore
  , maxLeiosNotifyIngressQueue :: BytesSize
  -- ^ @maximumIngressQueue@ for LeiosNotify
  , maxLeiosFetchIngressQueue :: BytesSize
  -- ^ @maximumIngressQueue@ for LeiosFetch
  }

demoLeiosFetchStaticEnv :: LeiosFetchStaticEnv
demoLeiosFetchStaticEnv =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSize = 50 * million
    , maxRequestedBytesSizePerPeer = 5 * million
    , maxRequestBytesSize = 500 * thousand
    , maxRequestsPerEb = 2
    , maxRequestsPerTx = 2
    , maxToCopyBytesSize = 100 * millionBase2
    , maxToCopyCount = 100 * thousand
    , maxLeiosNotifyIngressQueue = 1 * millionBase2
    , maxLeiosFetchIngressQueue = 50 * millionBase2
    }
 where
  million :: Num a => a
  million = 10 ^ (6 :: Int)
  millionBase2 :: Num a => a
  millionBase2 = 2 ^ (20 :: Int)
  thousand :: Num a => a
  thousand = 10 ^ (3 :: Int)

-----

data LeiosNotification
  = LeiosOfferBlock LeiosPoint BytesSize
  | LeiosOfferBlockTxs LeiosPoint

-----

messageLeiosFetchToObject ::
  Message (LeiosFetch LeiosPoint LeiosEb LeiosTx) st st' ->
  Aeson.Object
messageLeiosFetchToObject = \case
  MsgLeiosBlockRequest (MkLeiosPoint ebSlot ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockRequest"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      ]
  MsgLeiosBlock eb ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlock"
      , "ebHash" .= prettyEbHash (hashLeiosEb eb)
      , "ebBytesSize" .= Aeson.Number (fromIntegral $ leiosEbBytesSize eb)
      ]
  MsgLeiosBlockTxsRequest (MkLeiosPoint ebSlot ebHash) bitmaps ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxsRequest"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "numTxs" .= Aeson.Number (fromIntegral $ sum $ map (Bits.popCount . snd) bitmaps)
      , "bitmaps" .= map prettyBitmap bitmaps
      ]
  MsgLeiosBlockTxs txs ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxs"
      , "numTxs" .= Aeson.Number (fromIntegral (V.length txs))
      , "txsBytesSize" .= Aeson.Number (fromIntegral $ V.sum $ V.map leiosTxBytesSize txs)
      , "txs" .= Aeson.String "<elided>"
      ]
  MsgDone ->
    "kind" .= Aeson.String "MsgDone"

data TraceLeiosKernel
  = MkTraceLeiosKernel String
  | TraceLeiosBlockAcquired LeiosPoint
  | TraceLeiosBlockTxsAcquired LeiosPoint
  | forall m. (Show m, TxMeasureMetrics m) => TraceLeiosBlockForged
      { slot :: SlotNo
      , eb :: LeiosEb
      , ebMeasure :: m
      , mempoolRestMeasure :: m
      }
  | TraceLeiosBlockStored {slot :: SlotNo, eb :: LeiosEb}

deriving instance Show TraceLeiosKernel

traceLeiosKernelToObject :: TraceLeiosKernel -> Aeson.Object
traceLeiosKernelToObject = \case
  MkTraceLeiosKernel s ->
    mconcat
      [ "kind" .= Aeson.String "LeiosKernelMsg"
      , "msg" .= s
      ]
  TraceLeiosBlockAcquired (MkLeiosPoint (SlotNo ebSlot) ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockAcquired"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      ]
  TraceLeiosBlockTxsAcquired (MkLeiosPoint (SlotNo ebSlot) ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockTxsAcquired"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      ]
  TraceLeiosBlockForged{slot, eb, ebMeasure, mempoolRestMeasure} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockForged"
      , "slot" .= slot
      , "hash" .= prettyEbHash (hashLeiosEb eb)
      , "numTxs" .= V.length (leiosEbTxs eb)
      , "ebSize" .= leiosEbBytesSize eb
      , "closureSize" .= unByteSize32 (txMeasureMetricTxSizeBytes ebMeasure)
      , "mempoolRestSize" .= unByteSize32 (txMeasureMetricTxSizeBytes mempoolRestMeasure)
      ]
  TraceLeiosBlockStored{slot, eb} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockStored"
      , "slot" .= slot
      , "hash" .= prettyEbHash (hashLeiosEb eb)
      ]

newtype TraceLeiosPeer = MkTraceLeiosPeer String
  deriving Show

traceLeiosPeerToObject :: TraceLeiosPeer -> Aeson.Object
traceLeiosPeerToObject (MkTraceLeiosPeer s) = fromString "msg" .= Aeson.String (fromString s)

leiosMempoolSize :: ByteSize32
leiosMempoolSize = ByteSize32 24_090_112 -- 2 * (leiosEBMaxClosureSize + RB block size (mainnet = 90112))

-- TODO: dry with maxMsgLeiosBlockBytesSize
leiosEBMaxSize :: ByteSize32
leiosEBMaxSize = ByteSize32 512_000

leiosEBMaxClosureSize :: ByteSize32
leiosEBMaxClosureSize = ByteSize32 12_000_000

-- * Utilities for prototyping

-- | Like 'traceShow', but with pretty printing of the value.
{-# WARNING spy "Use for debugging purposes only" #-}
spy :: Show a => a -> a
spy a = trace (toString $ pShow a) a

-- | Like 'spy' but prefixed with a label.
{-# WARNING spy' "Use for debugging purposes only" #-}
spy' :: Show a => String -> a -> a
spy' msg a = trace (msg <> ": " <> toString (pShow a)) a
