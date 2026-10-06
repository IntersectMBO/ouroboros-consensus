{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Leios types that the storage layer needs: hashes, points, endorser
-- block bodies and their sizes.
--
-- This is the slice of the Leios prototype's types that
-- "Ouroboros.Consensus.Storage.LeiosDB" depends on. The rest of the Leios
-- machinery (announcements, fetch, voting, certificates) extends this module
-- as it lands.
module Ouroboros.Consensus.Leios.Types
  ( -- * Hashes
    EbHash (..)
  , encodeEbHash
  , decodeEbHash
  , prettyEbHash
  , RbHash (..)
  , encodeRbHash
  , decodeRbHash
  , prettyRbHash
  , TxHash (..)
  , prettyTxHash

    -- * Transactions
  , LeiosTx (..)
  , encodeLeiosTx
  , decodeLeiosTx

    -- * Points
  , LeiosPoint (..)
  , encodeLeiosPoint
  , decodeLeiosPoint
  , prettyLeiosPoint

    -- * Elections
  , ElectionId (..)

    -- * Votes
  , LeiosVote (..)
  , encodeLeiosVote
  , decodeLeiosVote

    -- * Endorser blocks
  , BytesSize
  , LeiosEb (..)
  , TxOffset
  , leiosEbBodyItems
  , encodeLeiosEb
  , decodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , encodeLeiosEbSize
  , leiosReferencesCapacity
  , maxLeiosEbBytesSize
  , maxTxsPerEb

    -- * Header checks
  , milliseconds32ToNominalDiffTime
  , minCertificationSlot
  ) where

import Cardano.Binary (decodeNestedCborBytes, encodeNestedCborBytes)
import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.Leios (LeiosSeatId (..), LeiosSignature)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Cardano.Ledger.Binary.Plain (decodeRecordNamed)
import Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SlotLength, slotLengthToMillisec)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.CBOR (decodeMapUpTo)

-- * Hashes

-- | Hash of an Endorser Block
newtype EbHash = MkEbHash {ebHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise)
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
  deriving newtype (Eq, Ord, NoThunks)
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

-- | Hash of a Leios transaction (the Blake2b-256 hash of its bytes).
newtype TxHash = MkTxHash ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Show TxHash where
  show = prettyTxHash

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

-- * Transactions

-- | A Leios transaction, as the CBOR bytes of the ledger's transaction.
--
-- Its codec wraps those bytes as CBOR-in-CBOR, so receiving a transaction does
-- not decode it.
--
-- TODO ChainSel and Leios voting will need a way to actually decode the
-- transaction: a per-block-type decoder that takes the ledger state resulting
-- from the announcing ranking block, whose era is the one the endorser block
-- was announced in.
newtype LeiosTx = MkLeiosTx {leiosTxBytes :: ByteString}
  deriving stock (Eq, Show)

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx (MkLeiosTx bytes) = encodeNestedCborBytes (BSL.fromStrict bytes)

decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx = MkLeiosTx <$> decodeNestedCborBytes

-- * Points

-- | Uniquely identifies an endorser block in Leios. Could use 'Block SlotNo
-- EbHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EbHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

instance Show LeiosPoint where
  show = prettyLeiosPoint

instance SignableRepresentation LeiosPoint where
  getSignableRepresentation point =
    toStrictByteString $
      -- REVIEW: Flat concatenation expected as what is signed?
      encode (pointSlotNo point)
        <> encodeEbHash (pointEbHash point)

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot ebHash) =
  CBOR.encodeListLen 2
    <> encode ebSlot
    <> encodeEbHash ebHash

decodeLeiosPoint :: Decoder s LeiosPoint
decodeLeiosPoint =
  decodeRecordNamed (T.pack "LeiosPoint") (const 2) $
    MkLeiosPoint <$> decode <*> decodeEbHash

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) (MkEbHash bytes)) =
  "(" ++ show slotNo ++ ", " ++ BS8.unpack (BS16.encode bytes) ++ ")"

-- * Elections

-- | A ranking block leadership election: a slot and the pool it elected.
--
-- This is unrelated to the 'Ouroboros.Consensus.Committee.Crypto.ElectionId'
-- type family, which identifies an election of a voting committee. We do not
-- anticipate a module using both.
--
-- The derived 'Ord' orders by slot first, which pruning relies on.
data ElectionId = ElectionId
  { electionSlot :: !SlotNo
  , electionElectee :: !(KeyHash BlockIssuer)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NoThunks

-- * Votes

-- | A vote for a Leios announcement.
data LeiosVote = MkLeiosVote
  { voteAnnouncingRbHash :: !RbHash
  -- ^ the 'HeaderHash' of the ranking block ('s header) that carries the
  -- announcement
  , voteSeatId :: !LeiosSeatId
  -- ^ the voter's seat in the committee
  , voteSignature :: !LeiosSignature
  -- ^ the voter's signature of 'voteAnnouncingRbHash'
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosVote where showProxy _ = "LeiosVote"

encodeLeiosVote :: LeiosVote -> Encoding
encodeLeiosVote (MkLeiosVote rbHash seat signature) =
  CBOR.encodeListLen 3
    <> encodeRbHash rbHash
    <> CBOR.encodeWord16 (leiosSeatIndex seat)
    <> encodeFixedSized signature

decodeLeiosVote :: Decoder s LeiosVote
decodeLeiosVote =
  decodeRecordNamed (T.pack "LeiosVote") (const 3) $
    MkLeiosVote <$> decodeRbHash <*> (LeiosSeatId <$> CBOR.decodeWord16) <*> decodeFixedSized

-- * Endorser blocks

type BytesSize = Word32

-- | An Endorser Block as it is submitted through the network.
-- TODO: Keep track of the slot of an EB?
data LeiosEb = MkLeiosEb
  { leiosEbTxs :: !(Vector (TxHash, BytesSize))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

-- | The position of a transaction in its EB's body.
type TxOffset = Int

leiosEbBodyItems :: LeiosEb -> [(TxOffset, TxHash, BytesSize)]
leiosEbBodyItems eb =
  leiosEbTxs eb
    & V.imap (\ix (txh, size) -> (ix, txh, size))
    & toList

-- | The bytes one reference occupies for a transaction of the given size: the
-- hash and the size itself, exactly as the EB's CBOR encoding writes them.
--
-- Also used on the Dijkstra transaction measure, so the mempool charges a
-- transaction what it will actually cost in the reference list rather than an
-- approximation of it.
encodeLeiosEbItemSize :: ByteSize32 -> ByteSize32
encodeLeiosEbItemSize (ByteSize32 txSize) =
  ByteSize32 $ cborBytesSize 32 + cborIntBytesSize txSize
 where
  cborBytesSize len = cborIntBytesSize len + len

-- | Encode a 'LeiosEb' as a CBOR map from each transaction hash to the size
-- of that transaction. Must not add more overhead than
-- 'encodeLeiosEbMaxFramingSize', and the encoding of each reference must match
-- 'encodeLeiosEbItemSize'.
--
-- The mempool charges each transaction 'encodeLeiosEbItemSize', and the
-- endorser-block capacity subtracts 'encodeLeiosEbMaxFramingSize'. If the
-- encoding changes, both sizes must change with it.
--
-- Endorser blocks are on chain, so peers on every node-to-node version must
-- write the same bytes. This encoding has no version yet. When an era after
-- Dijkstra changes it, the @ProtVer@ of that era will select the encoding.
encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb references) =
  foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length references)
    references

-- | The inverse of 'encodeLeiosEb', though it also accepts an indefinite-length
-- map. It rejects an empty map of transaction references, and one with more
-- than 'maxTxsPerEb'.
decodeLeiosEb :: Decoder s LeiosEb
decodeLeiosEb = do
  references <-
    decodeMapUpTo "LeiosEb" maxTxsPerEb $
      (,) <$> (MkTxHash <$> CBOR.decodeBytes) <*> CBOR.decodeWord32
  when (null references) $
    fail "LeiosEb: empty map of transaction references"
  pure $ MkLeiosEb $ V.fromList references

-- | The widest the framing 'encodeLeiosEb' writes around the references can
-- get: the map header. CBOR map headers have the same widths as
-- 'cborIntBytesSize', and the reference count fits 'BytesSize'.
--
-- A capacity expressed in references ('encodeLeiosEbItemSize' each)
-- subtracts this once at the block level; no single transaction can be
-- charged for it.
encodeLeiosEbMaxFramingSize :: ByteSize32
encodeLeiosEbMaxFramingSize = ByteSize32 $ cborIntBytesSize (maxBound :: BytesSize)

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter minus the framing 'encodeLeiosEb' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
leiosReferencesCapacity :: BytesSize -> BytesSize
leiosReferencesCapacity paramLimit = paramLimit - min paramLimit framing
 where
  ByteSize32 framing = encodeLeiosEbMaxFramingSize

-- | The design ceiling on an endorser block's encoding: what the node will
-- forge, serve and size its buffers for.
--
-- TODO Nothing yet keeps the @maxEndorserBlockReferencesSize@ protocol
-- parameter within it, so a larger value would let an honest peer send an
-- endorser block that
-- 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.byteLimitsLeiosFetch'
-- rejects. Make it the constitution's guardrail on that parameter, and refuse
-- to run with a parameter above it.
maxLeiosEbBytesSize :: BytesSize
maxLeiosEbBytesSize = 600 * 1000

-- | The most transactions an endorser block may name, given
-- 'maxLeiosEbBytesSize'.
--
-- It derives from the design ceiling and not from the protocol parameter,
-- since the buffers it sizes are allocated before any ledger state is in reach.
maxTxsPerEb :: Int
maxTxsPerEb =
  fromIntegral $ (maxLeiosEbBytesSize - framing) `div` minItemSize
 where
  ByteSize32 framing = encodeLeiosEbMaxFramingSize
  ByteSize32 minItemSize = encodeLeiosEbItemSize (ByteSize32 0)

-- | Compute the size of a 'LeiosEb' in its CBOR encoding.
encodeLeiosEbSize :: LeiosEb -> BytesSize
encodeLeiosEbSize (MkLeiosEb items) =
  cborIntBytesSize (length items)
    + sum (fmap (unByteSize32 . encodeLeiosEbItemSize . ByteSize32 . snd) items)

-- | Length of a unsigned integer if it were encoded in a "flattened format".
-- See 'encodeInteger'.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5

-- * Header checks

-- | A duration the ledger gives in milliseconds.
milliseconds32ToNominalDiffTime :: Milliseconds32 -> NominalDiffTime
milliseconds32ToNominalDiffTime (Milliseconds32 ms) = fromIntegral ms / 1000

-- | The earliest slot at which a block may certify an endorser block announced
-- in the given slot.
--
-- The announcement, voting and diffusion periods must all have elapsed. They
-- are wall-clock durations, so the gap rounds up to whole slots: a block is
-- forged at its slot's onset, so the answer is the first slot whose onset is far
-- enough after the announcement.
minCertificationSlot ::
  SlotLength ->
  -- | Announcement period length
  Milliseconds32 ->
  -- | Vote period length
  Milliseconds32 ->
  -- | Diffusion period length
  Milliseconds32 ->
  -- | Slot of the announcing block
  SlotNo ->
  SlotNo
minCertificationSlot slotLength announcement vote diffusion announcingSlot =
  announcingSlot + SlotNo (fromIntegral ((totalMs + slotMs - 1) `div` slotMs))
 where
  totalMs = 3 * ms announcement + ms vote + ms diffusion
  ms = toInteger . unMilliseconds32
  slotMs = case slotLengthToMillisec slotLength of
    0 -> error "minCertificationSlot: zero slot length"
    n -> n
