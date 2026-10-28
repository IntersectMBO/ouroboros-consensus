{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | The Leios types that the storage layer needs: hashes, points, endorser
-- block bodies and their sizes.
--
-- This is the slice of the Leios prototype's types that
-- "Ouroboros.Consensus.Storage.LeiosDB" depends on. The rest of the Leios
-- machinery (announcements, fetch, voting, certificates) extends this module
-- as it lands.
module Ouroboros.Consensus.Leios.Types
  ( -- * Hashes
    EndorserBlockHash
  , RankingBlockHash (..)
  , toRankingBlockHash
  , TxHash

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
  , EndorserBlock -- opaque
  , TxOffset
  , mkEndorserBlock
  , endorserBlockFromTxBytes
  , ForgedLeiosEb
  , forgedEbBody
  , forgedEbTxs
  , forgeLeiosEb
  , endorserBlockHash
  , endorserBlockReferences
  , encodeEndorserBlock
  , decodeEndorserBlock
  , sizeEndorserBlockReference
  , encodeEndorserBlockSize
  , referencesCapacity
  , maxEndorserBlockBytesSize
  , maxTxsPerEndorserBlock

    -- * Header checks
  , milliseconds32ToNominalDiffTime
  , minCertificationSlot
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeNestedCborBytes, encodeNestedCborBytes)
import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.Hash
  ( Hash
  , castHash
  , hashFromBytesShort
  , hashSize
  , hashToBytes
  , hashToStringAsHex
  , hashWith
  )
import Cardano.Crypto.Leios (LeiosSeatId (..), LeiosSignature)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Cardano.Ledger.Binary
  ( Annotator (..)
  , DecCBOR (..)
  , EncCBOR (..)
  , FullByteString (..)
  , Version
  , toPlainDecoder
  , toPlainEncoding
  )
import Cardano.Ledger.Binary.Plain (decodeRecordNamed)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Cardano.Ledger.Dijkstra.Block.Endorser as Ledger
import Cardano.Ledger.Hashes
  ( EraIndependentBlockHeader
  , EraIndependentEbReferences
  , EraIndependentTx
  , HASH
  , SafeToHash (originalBytesSize)
  , extractHash
  , hashAnnotated
  , unsafeMakeSafeHash
  )
import Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SlotLength, slotLengthToMillisec)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified GHC.IsList as IsList
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (toShortRawHash), HeaderHash)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Util (ShowProxy (..))

-- * Hashes

-- | The hash of an endorser block, as ranking blocks announce it.
type EndorserBlockHash = Hash HASH EraIndependentEbReferences

-- | The hash of a ranking block's header, which votes sign.
--
-- Ranking blocks are Praos blocks, whose 'HeaderHash' depends on 'blk'. Use
-- 'toRankingBlockHash' to convert it to this monomorphic type.
newtype RankingBlockHash = MkRankingBlockHash
  { unRankingBlockHash :: Hash HASH EraIndependentBlockHeader
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, NoThunks, ToCBOR, FromCBOR)

-- | Used by Leios voting, which signs the hash's bytes.
instance SignableRepresentation RankingBlockHash where
  getSignableRepresentation = hashToBytes . unRankingBlockHash

-- | Convert a ranking block's 'HeaderHash'. It must be the same length as a
-- 'RankingBlockHash', ie 32 bytes.
--
-- TODO Rule out a 'HeaderHash' of another size at compile time, as Peras does
-- with @HashSize blk ~ 32@, without requiring it of every 'RunNode' block.
toRankingBlockHash :: ConvertRawHash blk => Proxy blk -> HeaderHash blk -> RankingBlockHash
toRankingBlockHash p h =
  MkRankingBlockHash $
    fromMaybe (error "toRankingBlockHash: header hash of the wrong size") $
      hashFromBytesShort (toShortRawHash p h)

-- | The hash of a transaction an endorser block references. It hashes all of the
-- transaction's bytes, witnesses included, unlike the ledger's @TxId@ and so
-- the mempool's 'Ouroboros.Consensus.Ledger.SupportsMempool.GenTxId', which
-- hash only the transaction's body.
type TxHash = Hash HASH EraIndependentTx

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
-- EndorserBlockHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EndorserBlockHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

instance Show LeiosPoint where
  show = prettyLeiosPoint

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot ebHash) =
  CBOR.encodeListLen 2
    <> encode ebSlot
    <> toCBOR ebHash

decodeLeiosPoint :: Decoder s LeiosPoint
decodeLeiosPoint =
  decodeRecordNamed (T.pack "LeiosPoint") (const 2) $
    MkLeiosPoint <$> decode <*> fromCBOR

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) ebHash) =
  "(" ++ show slotNo ++ ", " ++ hashToStringAsHex ebHash ++ ")"

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
  { voteAnnouncingRbHash :: !RankingBlockHash
  -- ^ the 'HeaderHash' of the ranking block ('s header) that carries the
  -- announcement
  , voteSeatId :: !LeiosSeatId
  -- ^ the voter's seat in the committee
  , voteSignature :: !LeiosSignature
  -- ^ The voter's signature of 'voteAnnouncingRbHash'.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosVote where showProxy _ = "LeiosVote"

encodeLeiosVote :: LeiosVote -> Encoding
encodeLeiosVote (MkLeiosVote rbHash seat signature) =
  CBOR.encodeListLen 3
    <> toCBOR rbHash
    <> CBOR.encodeWord16 (leiosSeatIndex seat)
    <> encodeFixedSized signature

decodeLeiosVote :: Decoder s LeiosVote
decodeLeiosVote =
  decodeRecordNamed (T.pack "LeiosVote") (const 3) $
    MkLeiosVote <$> fromCBOR <*> (LeiosSeatId <$> CBOR.decodeWord16) <*> decodeFixedSized

-- * Endorser blocks

type BytesSize = Word32

-- | An endorser block in the Leios overlay protocol. It endorses transactions
-- by naming a list of 'TxHash' and 'BytesSize' pairs. Construct using
-- 'mkEndorserBlock'.
--
-- A compatibility layer over the ledger's 'Ledger.EbReferences': consensus
-- sees only this module's functions, never the ledger's representation.
newtype EndorserBlock = MkEndorserBlock Ledger.EbReferences
  deriving stock Generic
  deriving newtype (Eq, Show, NFData, NoThunks)

instance ShowProxy EndorserBlock where showProxy _ = "EndorserBlock"

-- | The protocol version endorser blocks are encoded at.
--
-- NOTE: We do not intend to use endorser blocks across eras, encoded or not.
endorserBlockVersion :: Version
endorserBlockVersion = eraProtVerLow @DijkstraEra

-- | An endorser block naming these transactions, in this order.
mkEndorserBlock :: [(TxHash, BytesSize)] -> EndorserBlock
mkEndorserBlock =
  MkEndorserBlock
    . Ledger.mkEbReferences endorserBlockVersion
    . IsList.fromList
    . map (\(txHash, txSize) -> (unsafeMakeSafeHash txHash, txSize))

-- | The endorser block naming the transactions with these bytes, in this
-- order. It names each by the 'TxHash' and the length of its bytes.
endorserBlockFromTxBytes :: NonEmpty ByteString -> EndorserBlock
endorserBlockFromTxBytes =
  mkEndorserBlock . map reference . toList
 where
  reference :: ByteString -> (TxHash, BytesSize)
  reference bytes = (castHash $ hashWith @HASH id bytes, fromIntegral $ BS.length bytes)

-- | The position of a transaction in its endorser block.
type TxOffset = Int

-- | An endorser block that this node forged, with the bytes of its
-- transactions. Only 'forgeLeiosEb' builds it, so that the bytes always match
-- the references.
data ForgedLeiosEb = ForgedLeiosEb
  { forgedEbBody :: !EndorserBlock
  , forgedEbTxs :: !(NonEmpty ByteString)
  -- ^ The bytes of the transactions that 'forgedEbBody' references, in the
  -- same order.
  }
  deriving stock (Show, Eq)

-- | The endorser block that references the given transactions, in the given
-- order. The reference to a transaction holds the Blake2b-256 hash and the
-- length of its bytes.
forgeLeiosEb :: NonEmpty ByteString -> ForgedLeiosEb
forgeLeiosEb txBytes =
  ForgedLeiosEb
    { forgedEbBody = endorserBlockFromTxBytes txBytes
    , forgedEbTxs = txBytes
    }

-- | The hash ranking blocks announce for the endorser block: that of its
-- encoding.
endorserBlockHash :: EndorserBlock -> EndorserBlockHash
endorserBlockHash (MkEndorserBlock references) = extractHash $ hashAnnotated references

-- | The transactions an endorser block names, in order.
endorserBlockReferences :: EndorserBlock -> [(TxHash, BytesSize)]
endorserBlockReferences (MkEndorserBlock references) =
  map (\(txHash, txSize) -> (extractHash txHash, txSize))
    . IsList.toList
    $ Ledger.getEbReferences references

-- | The bytes one reference occupies for a transaction of the given size: the
-- hash and the size, exactly as 'encodeEndorserBlock' writes them.
--
-- Also used on the Dijkstra transaction measure, so the mempool charges a
-- transaction what it will actually cost in the reference list rather than an
-- approximation of it.
sizeEndorserBlockReference :: ByteSize32 -> ByteSize32
sizeEndorserBlockReference (ByteSize32 txSize) =
  ByteSize32 $ cborBytesSize txHashSize + cborIntBytesSize txSize
 where
  txHashSize = fromIntegral (hashSize (Proxy @HASH))

  cborBytesSize len = cborIntBytesSize len + len

-- | Encode an endorser block as the ledger does: a CBOR map from each
-- transaction hash to the size of that transaction.
--
-- The mempool charges each transaction 'sizeEndorserBlockReference', and
-- 'referencesCapacity' subtracts the framing around the references. If the
-- encoding changes, both must change with it.
encodeEndorserBlock :: EndorserBlock -> Encoding
encodeEndorserBlock (MkEndorserBlock references) =
  toPlainEncoding endorserBlockVersion $ encCBOR references

-- | The inverse of 'encodeEndorserBlock', given the whole input it decoded
-- from, not just the endorser block's bytes: the ledger's annotator slices
-- them by the byte offsets of this decoder's run. Like the ledger, it rejects
-- an empty map of transaction references and one naming a transaction twice.
-- It also rejects one whose bytes exceed 'maxEndorserBlockBytesSize'. It can
-- tell only after decoding them (if endorser blocks were wrapped as
-- CBOR-in-CBOR, it would know their length before decoding them, even from an
-- indefinite-length byte string), since a CBOR map gives its number of entries
-- rather than of bytes. So the byte limit of the message carrying it must keep
-- small the work it does before then, eg
-- 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.byteLimitsLeiosFetch'.
--
-- The endorser block keeps its slice of those bytes, so its hash and encoding
-- are of the bytes received.
decodeEndorserBlock :: Decoder s (BSL.ByteString -> EndorserBlock)
decodeEndorserBlock = do
  start <- CBOR.peekByteOffset
  eb <-
    toPlainDecoder Nothing endorserBlockVersion $
      (. Full)
        -- The annotator only slices the bytes, which cannot fail.
        . (either (\e -> error ("Impossible, endorser block decoder failed: " <> show e)) MkEndorserBlock .)
        . runAnnotator
        <$> decCBOR
  end <- CBOR.peekByteOffset
  -- Nothing currently bounds the number of references more tightly than this
  -- bound on the bytes does, ie by 'maxTxsPerEndorserBlock'.
  when (end - start > fromIntegral maxEndorserBlockBytesSize) $
    fail $
      "endorser block of "
        <> show (end - start)
        <> " bytes exceeds "
        <> show maxEndorserBlockBytesSize
  pure eb

-- | The most bytes 'encodeEndorserBlock' writes besides the references, for an
-- endorser block that 'mkEndorserBlock' built: its map header. Each
-- reference's own CBOR headers count towards 'sizeEndorserBlockReference'
-- instead. The map header widens with the reference count as
-- 'cborIntBytesSize' does, never narrowing, and the count fits 'BytesSize', so
-- the header of 'maxBound' references is the widest.
maxFramingSize :: BytesSize
maxFramingSize = cborIntBytesSize (maxBound :: BytesSize)

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter minus the framing 'encodeEndorserBlock' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
referencesCapacity :: BytesSize -> BytesSize
referencesCapacity paramLimit = paramLimit - min paramLimit maxFramingSize

-- | The design ceiling on an endorser block's encoding: what the node will
-- forge, serve and size its buffers for.
--
-- TODO Nothing yet keeps the @maxEndorserBlockReferencesSize@ protocol
-- parameter within it, so a larger value would let an honest peer send an
-- endorser block that 'decodeEndorserBlock' rejects. Make it the
-- constitution's guardrail on that parameter, and refuse to run with a
-- parameter above it.
maxEndorserBlockBytesSize :: BytesSize
maxEndorserBlockBytesSize = 600 * 1000

-- | The most transactions an endorser block may name, given
-- 'maxEndorserBlockBytesSize'. Each reference takes at least as many bytes as
-- 'sizeEndorserBlockReference' charges a transaction of size 0, however it is
-- encoded.
--
-- It derives from the design ceiling and not from the protocol parameter,
-- since the buffers it sizes are allocated before any ledger state is in reach.
maxTxsPerEndorserBlock :: Int
maxTxsPerEndorserBlock =
  fromIntegral $ maxEndorserBlockBytesSize `div` minItemSize
 where
  ByteSize32 minItemSize = sizeEndorserBlockReference (ByteSize32 0)

-- | The size of 'encodeEndorserBlock': of the bytes the endorser block was
-- received in, or of the ledger's encoding of one this node built.
encodeEndorserBlockSize :: EndorserBlock -> BytesSize
encodeEndorserBlockSize (MkEndorserBlock references) = fromIntegral $ originalBytesSize references

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
