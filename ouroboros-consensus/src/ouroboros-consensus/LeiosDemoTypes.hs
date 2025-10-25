module LeiosDemoTypes (module LeiosDemoTypes) where

import           Cardano.Binary (enforceSize)
import           Cardano.Slotting.Slot (SlotNo)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)
import           Control.Concurrent.Class.MonadMVar (MVar)
import           Data.ByteString (ByteString)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64)
import           Ouroboros.Consensus.Util (ShowProxy (..))

type BytesSize = Word32

newtype EbId = MkEbId Int
  deriving (Eq, Ord)

newtype PeerId a = MkPeerId a
  deriving (Eq, Ord)

newtype EbHash = MkEbHash ByteString
  deriving (Eq, Ord, Show)

newtype TxHash = MkTxHash ByteString
  deriving (Eq, Ord)

data LeiosPoint = MkLeiosPoint SlotNo EbHash
  deriving (Show)

instance ShowProxy LeiosPoint where
    showProxy _ = "LeiosPoint"

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot (MkEbHash ebHash)) =
    encodeListLen 2
 <> encode ebSlot
 <> encode ebHash

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
    -- | ebSlot, ebHash
    MkLeiosBlockRequest
        !SlotNo
        !ByteString

data LeiosBlockTxsRequest =
    -- | ebSlot, ebHash, bitmaps, txHashes
    --
    -- The hashes aren't sent to the peer, but they are used to validate the
    -- reply when it arrives.
    MkLeiosBlockTxsRequest
        !SlotNo
        !ByteString
        [(Word16, Word64)]
        !(V.Vector TxHash)

-----

--
-- Compare the following data types to the @LeiosFetchDynamicEnv@ and
-- @LeiosFetchState@ types in the Leios model exe
--
-- These data types are organized differently because they are organized by the
-- patterns of access to the "Ouroboros.Consensus.NodeKernel"'s shared state.
--

data LeiosPeerMVars m = MkLeiosPeerMVars {
    -- written to only by the LeiosNotify client (TODO and eviction)
    offerings :: !(MVar m (Set EbId, Set EbId))
  ,
    -- written to by the fetch logic and the LeiosFetch client
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
    requestsToSend :: !(MVar m (Seq LeiosFetchRequest))
  }

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
    -- TODO this is far too big for the heap
    missingTxBodies :: !(Set TxHash)
  ,
    -- TODO this is far too big for the heap
    ebBodies :: !(Map EbId (IntMap (TxHash, BytesSize)))
  ,
    -- TODO this is far too big for the heap
    txOffsetss :: !(Map TxHash (Map EbId Int))
  }

emptyLeiosOutstanding :: LeiosOutstanding pid
emptyLeiosOutstanding =
    MkLeiosOutstanding
        Map.empty
        Map.empty
        Map.empty
        0
        Map.empty
        Set.empty
        Map.empty
        Map.empty

data LeiosToCopy = MkLeiosToCopy {
    toCopy :: !(Map EbId (IntMap BytesSize))
  ,
    toCopyBytesSize :: !BytesSize
  ,
    toCopyCount :: !Int
  }

emptyLeiosToCopy :: LeiosToCopy
emptyLeiosToCopy = MkLeiosToCopy Map.empty 0 0
