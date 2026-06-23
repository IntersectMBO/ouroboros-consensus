{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LeiosDemoTypes (module LeiosDemoTypes, module Cardano.Crypto.Leios) where

import Cardano.Binary
  ( Decoder
  , Encoding
  , FromCBOR (fromCBOR)
  , ToCBOR
  , enforceSize
  , serialize'
  , toCBOR
  , toStrictByteString
  )
import qualified Cardano.Binary as CBOR
import Cardano.Crypto.DSIGN
  ( decodeSigDSIGN
  , encodeSigDSIGN
  , signDSIGN
  , verifyDSIGN
  )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Leios
  ( AggregationError (..)
  , LeiosCert (..)
  , LeiosCommittee (..)
  , LeiosDSIGN
  , LeiosSignature
  , LeiosSigningKey
  , LeiosVerificationKey
  , LeiosVoter (..)
  , LeiosVoterId (..)
  , VerificationError
  , Weight
  , aggregateLeiosCert
  , leiosCommitteeSize
  , decodeLeiosVoterId
  , encodeLeiosVoterId
  , getLeiosVoterId
  , leiosSignContext
  , resolveLeiosVoter
  , verifyLeiosCert
  )
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Core (EraTx, Tx, TxLevel (TopTx))
import Cardano.Prelude (NFData, NonEmpty, toList, toString, (&))
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Codec.Serialise (Serialise, decode, encode)
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
import Data.Fixed (Pico)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nubBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word32, Word64)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import LeiosDemoDb.Trace (TraceLeiosDb (..))
import LeiosDemoException (LeiosDbException (..), jsonLeiosDbException)
import LeiosDemoOnlyTestFetch (LeiosFetch, Message (..))
import qualified LeiosDemoOnlyTestFetch as LeiosFetch
import LeiosDemoOnlyTestNotify (LeiosNotify, Message (..))
import qualified LeiosDemoOnlyTestNotify as LeiosNotify
import qualified Numeric
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , TxMeasureMetrics
  , txMeasureMetricTxSizeBytes
  )
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, NoThunks)
import Text.Pretty.Simple (pShow)

-- * Hashes and identities

newtype PeerId a = MkPeerId a
  deriving (Eq, Ord)

-- Hash algorithm used in leios for EBs and txs
type HASH = Hash.Blake2b_256

-- | Hash of an Endorser Block
newtype EbHash = MkEbHash {ebHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise, DecCBOR, EncCBOR, ToCBOR, FromCBOR)
  deriving stock Generic

instance Show EbHash where
  show = prettyEbHash

encodeEbHash :: EbHash -> Encoding
encodeEbHash (MkEbHash bytes) = CBOR.encodeBytes bytes

decodeEbHash :: Decoder s EbHash
decodeEbHash = MkEbHash <$> CBOR.decodeBytes

prettyEbHash :: EbHash -> String
prettyEbHash (MkEbHash bytes) = BS8.unpack (BS16.encode bytes)

-- | Hash of a Ranking Block
--
-- A Ranking Block is the Praos Block. While the regular Praos headers are parameterised
-- over 'blk', we choose to keep 'RbHash' monomorphic. Use the 'ConvertRawHash' type class
-- to convert between this type and 'HeaderHash'.
newtype RbHash = MkRbHash {rbHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise, DecCBOR, EncCBOR, ToCBOR, FromCBOR)
  deriving stock Generic

instance Show RbHash where
  show = prettyRbHash

encodeRbHash :: RbHash -> Encoding
encodeRbHash (MkRbHash bytes) = CBOR.encodeBytes bytes

decodeRbHash :: Decoder s RbHash
decodeRbHash = MkRbHash <$> CBOR.decodeBytes

prettyRbHash :: RbHash -> String
prettyRbHash (MkRbHash bytes) = BS8.unpack (BS16.encode bytes)

instance SignableRepresentation RbHash where
  getSignableRepresentation point =
    toStrictByteString $
      encodeRbHash point

newtype TxHash = MkTxHash ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Show TxHash where
  show = prettyTxHash

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

-- | Uniquely identifies an endorser block in Leios. Could use 'Block SlotNo
-- EbHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EbHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

-- TODO: prettyprinter instance Pretty?
instance Show LeiosPoint where
  show = prettyLeiosPoint

instance SignableRepresentation LeiosPoint where
  getSignableRepresentation point =
    toStrictByteString $
      -- REVIEW: Flat concatenation expected as what is signed?
      encode point.pointSlotNo
        <> encodeEbHash point.pointEbHash

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) (MkEbHash bytes)) =
  "(" ++ show slotNo ++ ", " ++ BS8.unpack (BS16.encode bytes) ++ ")"

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot ebHash) =
  CBOR.encodeListLen 2
    <> encode ebSlot
    <> encodeEbHash ebHash

decodeLeiosPoint :: Decoder s LeiosPoint
decodeLeiosPoint = do
  enforceSize (fromString "LeiosPoint") 2
  MkLeiosPoint <$> decode <*> decodeEbHash

-- | Types used in Praos headers
data EbAnnouncement = EbAnnouncement
  { ebAnnouncementHash :: EbHash
  , ebAnnouncementSize :: BytesSize
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass NoThunks

instance ToCBOR EbAnnouncement where
  toCBOR ebAnn = CBOR.encodeListLen 2 <> encode (ebAnnouncementHash ebAnn) <> encode (ebAnnouncementSize ebAnn)

instance FromCBOR EbAnnouncement where
  fromCBOR = do
    enforceSize "EbAnnouncement" 2
    EbAnnouncement <$> decode <*> decode

instance EncCBOR EbAnnouncement where
  encCBOR ebAnn =
    Binary.encodeListLen 2
      <> encCBOR (ebAnnouncementHash ebAnn)
      <> encCBOR (ebAnnouncementSize ebAnn)

instance DecCBOR EbAnnouncement where
  decCBOR =
    Binary.decodeRecordNamed "EbAnnouncement" (const 2) $
      EbAnnouncement <$> decCBOR <*> decCBOR

-- * Fetch logic types

type BytesSize = Word32

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
      !(Vector TxHash)

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

-- | Main data structure used in the Leios fetching logic.
--
-- Tracks both EB-level state (what EBs we have/need) and TX-level state
-- (what TXs we need for each EB), along with request tracking for bandwidth
-- management.
--
-- TODO: Potential simplifications once we have better test coverage:
--
-- 1. With filterMissingWork now querying the DB before each fetch iteration,
--    we could simplify this structure to only track "offers" from peers rather
--    than "missing" items. The DB would be the source of truth for what we have,
--    and we'd filter offers against DB to find what to fetch.
--
-- 2. The acquiredEbBodies set is now redundant with DB - we update it in
--    filterMissingWork but could remove it entirely once we trust DB filtering.
--
-- 3. The reverseEbIndexByTx inverse index could be computed on-demand from missingEbTxs
--    rather than maintained incrementally, simplifying state updates.
--
-- 4. Consider separating "offer tracking" from "request tracking" into distinct
--    data structures for clarity.
data LeiosOutstanding pid = MkLeiosOutstanding
  { -- EB-level tracking
    acquiredEbBodies :: !(Set EbHash)
  -- ^ EB bodies we've successfully received/stored
  , missingEbBodies :: !(Map LeiosPoint BytesSize)
  -- ^ EB bodies still needed to be fetched (indexed by point and size)
  -- Request tracking
  , requestedEbPeers :: !(Map EbHash (Set (PeerId pid)))
  -- ^ Which peers we've requested each EB from
  , requestedTxPeers :: !(Map TxHash (Set (PeerId pid)))
  -- ^ Which peers we've requested each TX from
  , requestedBytesSizePerPeer :: !(Map (PeerId pid) BytesSize)
  -- ^ Running total of bytes requested from each peer
  , requestedBytesSize :: !BytesSize
  -- ^ Total bytes requested across all peers
  -- TX-level tracking
  , missingEbTxs :: !(Map LeiosPoint (IntMap (TxHash, BytesSize)))
  -- ^ The txs that still need to be sourced
  --
  -- * A @MsgLeiosBlock@ inserts into 'missingEbTxs' if that EB has never
  --   been received before.
  --
  -- * Every @MsgLeiosBlockTxs@ deletes from 'missingEbTxs', but that delete
  --   will be a no-op for all except the first to arrive carrying this EbTx.
  --
  -- TODO this is far too big for the heap
  , reverseEbIndexByTx :: !(Map TxHash (Map EbHash (Int, BytesSize)))
  -- ^ Inverse of missingEbTxs - for each TX, which EBs (and offsets) need it
  --
  -- TODO this is far too big for the heap
  , blockingPerEb :: !(Map LeiosPoint Int)
  -- ^ How many txs of each EB are not yet in the @txs@ table
  --
  -- These missing txs are blocking the node from sending @MsgLeiosBlockTxsOffer@
  -- to its downstream peers.
  --
  -- It's different from 'missingEbTxs' in two ways.
  --
  -- * The heap footprint of 'blockingPerEb' doesn't scale with the number of
  --   EbTxs.
  --
  -- * 'blockingPerEb' is only decremented when txs are actually inserted
  --   into the DB (via @MsgLeiosBlockTxs@ handling).
  }

emptyLeiosOutstanding :: LeiosOutstanding pid
emptyLeiosOutstanding =
  MkLeiosOutstanding
    { acquiredEbBodies = Set.empty
    , missingEbBodies = Map.empty
    , requestedEbPeers = Map.empty
    , requestedTxPeers = Map.empty
    , requestedBytesSizePerPeer = Map.empty
    , requestedBytesSize = 0
    , missingEbTxs = Map.empty
    , reverseEbIndexByTx = Map.empty
    , blockingPerEb = Map.empty
    }

prettyLeiosOutstanding :: LeiosOutstanding pid -> String
prettyLeiosOutstanding x =
  unlines $
    map ("    [leios] " ++) $
      [ "acquiredEbBodies = " ++ show (Set.size acquiredEbBodies)
      , "missingEbBodies = " ++ show (Map.size missingEbBodies)
      , "requestedEbPeers = " ++ unwords (map prettyEbHash (Map.keys requestedEbPeers))
      , "requestedTxPeers = " ++ unwords (map prettyTxHash (Map.keys requestedTxPeers))
      , "requestedBytesSizePerPeer = " ++ show (Map.elems requestedBytesSizePerPeer)
      , "requestedBytesSize = " ++ show requestedBytesSize
      , "missingEbTxs = "
          ++ unwords [(prettyLeiosPoint k ++ "__" ++ show (IntMap.size v)) | (k, v) <- Map.toList missingEbTxs]
      , "blockingPerEb = "
          ++ unwords [(prettyLeiosPoint k ++ "__" ++ show c) | (k, c) <- Map.toList blockingPerEb]
      , ""
      ]
 where
  MkLeiosOutstanding
    { acquiredEbBodies
    , missingEbBodies
    , requestedEbPeers
    , requestedTxPeers
    , requestedBytesSizePerPeer
    , requestedBytesSize
    , missingEbTxs
    , blockingPerEb
    } = x

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

-- * LeiosTx newtype

-- | A wrapper around transaction bytes for the simple purpose of serving them.
-- This typically contains a CBOR-encoded 'Tx era'.
newtype LeiosTx = MkLeiosTx {cbor :: ByteString}
  deriving Show

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

-- | Uses cbor-in-cbor to allow for not needing to decode into a 'Tx era'.
encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx MkLeiosTx{cbor} =
  CBOR.encodeBytes cbor

-- | Relies on cbor-in-cbor to allow for not needing to decode into a 'Tx era'.
decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx =
  MkLeiosTx <$> CBOR.decodeBytes

hashLeiosTx :: LeiosTx -> TxHash
hashLeiosTx =
  MkTxHash . Hash.hashToBytes . Hash.hashWith @HASH cbor

-- * Endorser Block

-- | An Endorser Block as it is submitted through the network.
-- TODO: Keep track of the slot of an EB?
data LeiosEb = MkLeiosEb
  { leiosEbTxs :: !(Vector (TxHash, BytesSize))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | A newly forged 'LeiosEb' that includes the whole closure of endorsed
-- transactions.
data ForgedLeiosEb = ForgedLeiosEb
  { point :: !LeiosPoint
  , body :: !LeiosEb
  , txClosure :: ![(TxHash, ByteString)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

forgeLeiosEb :: EraTx era => SlotNo -> NonEmpty (Tx TopTx era) -> ForgedLeiosEb
forgeLeiosEb slot txs =
  ForgedLeiosEb{point, body, txClosure}
 where
  point = MkLeiosPoint slot (hashLeiosEb body)

  body =
    serializedTxs
      & map (\(hash, size, _) -> (hash, size))
      & V.fromList
      & MkLeiosEb

  txClosure =
    serializedTxs
      & map (\(hash, _, bytes) -> (hash, bytes))
      & toList

  hashTx =
    MkTxHash . Hash.hashToBytes . Hash.hashWithSerialiser @HASH toCBOR

  serializedTxs =
    [ (hashTx tx, byteSize, bytes)
    | tx <- toList txs
    , let bytes = serialize' tx
    , let byteSize = fromIntegral $ BS.length bytes
    ]

leiosEbBodyItems :: LeiosEb -> [(Int, TxHash, BytesSize)]
leiosEbBodyItems eb =
  leiosEbTxs eb
    & V.imap (\ix (txh, size) -> (ix, txh, size))
    & toList

leiosEbBytesSize :: LeiosEb -> BytesSize
leiosEbBytesSize (MkLeiosEb items) =
  cborIntBytesSize (length items) + sum (fmap (each . snd) items)
 where
  each sz = cborBytesSize 32 + cborIntBytesSize sz

  cborBytesSize len = cborIntBytesSize len + len

-- | Length of a unsigned integer if it were encoded in a "flattened format".
-- See 'encodeInteger'.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5

hashLeiosEb :: LeiosEb -> EbHash
hashLeiosEb =
  MkEbHash . Hash.hashToBytes . Hash.hashWith @HASH id . serialize' . encodeLeiosEb

encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb v) =
  foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length v)
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

-- * Voting

-- | Create a 'LeiosCommittee' from a mapping of verification keys and some
-- associated weight. Duplicate entries by verification key are ignored. The
-- final 'Weight' in the committee is normalized by the total of the input map.
-- TODO: The total can only be calculated here in "everyone votes" scheme.
mkCommitteeEveryoneVotes :: Real w => [(LeiosVerificationKey, w)] -> LeiosCommittee
mkCommitteeEveryoneVotes inputs =
  LeiosCommittee
    . V.fromList
    . sortOn voterWeight
    $ [ LeiosVoter{voterWeight = toRational weight / totalWeight, voterVKey = vk}
      | (vk, weight) <- nubBy ((==) `on` fst) inputs
      ]
 where
  totalWeight = toRational . sum $ snd <$> inputs

-- ** Vote

-- | A vote in the Leios protocol.
data LeiosVote = MkLeiosVote
  { announcingRbHash :: RbHash
  -- ^ The message that gets signed, the hash of the ranking block
  --   that announced an endorser block.
  , voterId :: LeiosVoterId
  -- ^ Identity within a 'LeiosCommittee' who signed this vote.
  , voteSignature :: LeiosSignature
  -- ^ The cryptographic signature of the vote.
  }
  deriving (Generic, Eq, Show)

instance Ord LeiosVote where
  compare v1 v2 =
    compare v1.announcingRbHash v2.announcingRbHash
      <> compare v1.voterId v2.voterId

instance ShowProxy LeiosVote where showProxy _ = "LeiosVote"

-- | Encode a 'LeiosVote' into CBOR.
-- NOTE: Encodes points flat into the vote for smaller votes.
encodeLeiosVote :: LeiosVote -> Encoding
encodeLeiosVote MkLeiosVote{announcingRbHash, voterId, voteSignature} =
  CBOR.encodeListLen 4
    <> encodeRbHash announcingRbHash
    <> encodeLeiosVoterId voterId
    <> encodeSigDSIGN voteSignature

-- | Dedoe a 'LeiosVote' from CBOR.
decodeLeiosVote :: Decoder s LeiosVote
decodeLeiosVote = do
  enforceSize (fromString "LeiosVote") 4
  pointRbHash <- decodeRbHash
  voterId <- decodeLeiosVoterId
  voteSignature <- decodeSigDSIGN
  pure
    MkLeiosVote
      { announcingRbHash = pointRbHash
      , voterId
      , voteSignature
      }

voteToObject :: LeiosVote -> Aeson.Object
voteToObject MkLeiosVote{announcingRbHash, voterId} =
  mconcat
    [ "rbHash" .= prettyRbHash announcingRbHash
    , "voterId" .= voterId.voterIndex
    ]

-- | Create a vote for given 'LeiosPoint' and signing key.
signLeiosVote :: LeiosSigningKey -> LeiosVoterId -> RbHash -> LeiosVote
signLeiosVote sk voterId announcingRbHash =
  MkLeiosVote
    { announcingRbHash
    , voterId
    , voteSignature = signDSIGN leiosSignContext announcingRbHash sk
    }

-- | Validate a 'LeiosVote' against a selected 'Commitee'.
validateLeiosVote :: LeiosCommittee -> LeiosVote -> Either VoteInvalid Weight
validateLeiosVote committee MkLeiosVote{announcingRbHash, voterId, voteSignature} =
  case resolveLeiosVoter committee voterId of
    Nothing -> Left SignerNotInCommittee
    Just voter ->
      case verifyDSIGN leiosSignContext voter.voterVKey announcingRbHash voteSignature of
        Left _ -> Left InvalidSignature
        Right () -> Right voter.voterWeight

data VoteInvalid
  = InvalidSignature
  | SignerNotInCommittee
  deriving (Eq, Show)

-- * Era-level Leios dispatch

-- | Per-era hooks for Leios voting and CertRB admission. Default
-- methods make this a no-op for non-Leios eras.
--
-- Lives here rather than in 'LeiosVoting' so it can be referenced from
-- the LedgerDB layer ('applyBlock') without pulling 'ChainDB' (which
-- 'runLeiosVoting' depends on) into scope.
class HasLeiosVoting blk where
  -- | The voting committee for the given (pre-tick) ledger state, or
  -- 'Nothing' if the era does not participate in Leios voting.
  getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe LeiosCommittee
  getLeiosCommittee _ = Nothing

-- * Tracing

messageLeiosNotifyToObject ::
  Message (LeiosNotify LeiosPoint () LeiosVote) st st' ->
  Aeson.Object
messageLeiosNotifyToObject = \case
  MsgLeiosNotificationRequestNext ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosNotificationRequestNext"
      ]
  MsgLeiosBlockAnnouncement{} ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockAnnouncement"
      ]
  MsgLeiosBlockOffer (MkLeiosPoint ebSlot ebHash) ebBytesSize ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockOffer"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "ebBytesSize" .= ebBytesSize
      ]
  MsgLeiosBlockTxsOffer (MkLeiosPoint ebSlot ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxsOffer"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      ]
  MsgLeiosVotes votes ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosVotes"
      , "votes" .= fmap voteToObject votes
      ]
  LeiosNotify.MsgDone ->
    mconcat
      [ "kind" .= Aeson.String "MsgDone"
      ]

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
  MsgLeiosBlockTxs (MkLeiosPoint ebSlot ebHash) bitmaps txs ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxs"
      , "numTxs" .= Aeson.Number (fromIntegral (length txs))
      , "txsBytesSize" .= Aeson.Number (fromIntegral $ sum $ fmap (BS.length . cbor) txs)
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "bitmaps" .= map prettyBitmap bitmaps
      ]
  LeiosFetch.MsgDone ->
    "kind" .= Aeson.String "MsgDone"

data TraceLeiosKernel
  = MkTraceLeiosKernel String
  | TraceLeiosBlockAcquired LeiosPoint
  | -- | The EB body was received but the point was not in the database. This is
    -- unexpected as the point should have been inserted during announcement handling.
    TraceLeiosBlockPointMissing LeiosPoint
  | TraceLeiosBlockTxsAcquired LeiosPoint
  | forall m. (Show m, TxMeasureMetrics m) => TraceLeiosBlockForged
      { slot :: SlotNo
      , eb :: LeiosEb
      , ebMeasure :: m
      , mempoolRestMeasure :: m
      }
  | TraceLeiosBlockStored {slot :: SlotNo, eb :: LeiosEb}
  | -- | An RB header announces a freshly-forged EB on this chain.
    -- Lets downstream consumers (e.g. the visualizer) attach the EB to
    -- the announcing RB without having to correlate by timing.
    TraceLeiosBlockAnnounced
      { announcingRbHashBytes :: ByteString
      , announcedEbPoint :: LeiosPoint
      }
  | -- NOTE: We avoid 'Header blk' or 'Point blk' here and a slot should be
    -- sufficient because it the certying block must be directly succeeding the
    -- forging/announcing anyways.
    TraceLeiosBlockCertified {atSlot :: SlotNo, certifiedPoint :: LeiosPoint}
  | TraceLeiosVoted {vote :: LeiosVote, weight :: Weight}
  | TraceLeiosVoteAcquired {vote :: LeiosVote}
  | TraceLeiosCertified {rbHash :: RbHash}
  | TraceLeiosDbException LeiosDbException
  | TraceLeiosDb TraceLeiosDb
  | -- | A CertRB was admitted to the staging area because its certified
    -- EB closure isn't locally available. This is a critical event: it
    -- means the node would have crashed in 'resolveLeiosBlock' (issue
    -- #890) and the staging-area / Phase-2 emergency-fetch path is
    -- compensating. Carries the staged block's point, the missing EB
    -- point, and the number of peers whose ChainSync candidate
    -- contained the block (they're treated as implicit offerers of the
    -- EB by the fetch loop).
    TraceLeiosCertRBStaged
      { stagedBlockPoint :: String
      , stagedEbPoint :: LeiosPoint
      , stagedKnownPeers :: Int
      }
  | -- | A staged CertRB has been released back into ChainSel because
    -- the EB closure (body + txs) is now locally available.
    TraceLeiosCertRBReleased {releasedEbPoint :: LeiosPoint}

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
  TraceLeiosBlockPointMissing (MkLeiosPoint (SlotNo ebSlot) ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockPointMissing"
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
      , "numTxs" .= length (leiosEbTxs eb)
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
  TraceLeiosBlockAnnounced{announcingRbHashBytes, announcedEbPoint} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockAnnounced"
      , "rbHash" .= BS8.unpack (BS16.encode announcingRbHashBytes)
      , "ebSlot" .= announcedEbPoint.pointSlotNo
      , "ebHash" .= prettyEbHash announcedEbPoint.pointEbHash
      ]
  TraceLeiosBlockCertified{atSlot, certifiedPoint} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockCertified"
      , "atSlot" .= atSlot
      , "ebSlot" .= certifiedPoint.pointSlotNo
      , "ebHash" .= prettyEbHash certifiedPoint.pointEbHash
      ]
  TraceLeiosVoted{vote, weight} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosVoted"
      , "vote" .= voteToObject vote
      , -- NOTE: 1 ADA delegation is 2.2 × 10^-11 of the total stake. So 10^-12
        -- is reasonable precision here.
        "weight" .= fromRational @Pico weight
      ]
  TraceLeiosVoteAcquired{vote} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosVoteAcquired"
      , "vote" .= voteToObject vote
      ]
  TraceLeiosCertified{rbHash = announcingRbHash} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosCertified"
      , "rbHash" .= prettyRbHash announcingRbHash
      ]
  TraceLeiosDbException e ->
    jsonLeiosDbException e
  TraceLeiosDb (TraceLeiosDbInsertCollision table key) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosDbInsertCollision"
      , "table" .= table
      , "key" .= key
      ]
  TraceLeiosCertRBStaged{stagedBlockPoint, stagedEbPoint, stagedKnownPeers} ->
    let MkLeiosPoint (SlotNo ebSlot) ebHash = stagedEbPoint
     in mconcat
          [ "kind" .= Aeson.String "LeiosCertRBStaged"
          , "blockPoint" .= stagedBlockPoint
          , "ebHash" .= prettyEbHash ebHash
          , "ebSlot" .= ebSlot
          , "knownPeers" .= stagedKnownPeers
          ]
  TraceLeiosCertRBReleased{releasedEbPoint} ->
    let MkLeiosPoint (SlotNo ebSlot) ebHash = releasedEbPoint
     in mconcat
          [ "kind" .= Aeson.String "LeiosCertRBReleased"
          , "ebHash" .= prettyEbHash ebHash
          , "ebSlot" .= ebSlot
          ]

data TraceLeiosPeer
  = MkTraceLeiosPeer String
  | TraceLeiosPeerDbException LeiosDbException
  deriving Show

traceLeiosPeerToObject :: TraceLeiosPeer -> Aeson.Object
traceLeiosPeerToObject = \case
  MkTraceLeiosPeer s -> fromString "msg" .= Aeson.String (fromString s)
  TraceLeiosPeerDbException e -> jsonLeiosDbException e

-- * Protocol parameters

maxMsgLeiosBlockBytesSize :: BytesSize
maxMsgLeiosBlockBytesSize = 500 * 10 ^ (3 :: Int) -- from CIP-0164's recommendations

minEbItemBytesSize :: BytesSize
minEbItemBytesSize = 32 + hashOverhead + minSizeOverhead
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

minCertificationGap :: Word64
minCertificationGap = 10

-- | Minimum fraction of stake to create a valid 'LeiosCertificate'.
minCertificationThreshold :: Rational
minCertificationThreshold = 3 % 4

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
