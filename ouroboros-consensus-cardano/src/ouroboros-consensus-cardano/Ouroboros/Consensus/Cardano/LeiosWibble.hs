{-# LANGUAGE LambdaCase #-}

-- | TEMPORARY workaround for mishapen Leios/Dijkstra blocks diffused on the
-- testnet.
--
-- Some blocks were re-encoded with an /indefinite/-length transaction sequence
-- (@0x9f … 0xff@) whereas their header committed to the /definite/-length
-- encoding. The received body therefore no longer hashes to the header's
-- @hbBodyHash@, so the block is rejected and a sync gets stuck.
--
-- On the block-fetch (off-the-wire) path we detect such a block purely from its
-- own contents — rewriting its transaction sequence to definite length makes
-- the body hash match the header's committed body hash — and rewrite it so the
-- body matches the header again. This works for any transaction count, unlike a
-- size-based test (for < 256 transactions the definite and indefinite framings
-- have the same or smaller length, so only the hash distinguishes them).
--
-- Delete this module once the network no longer diffuses such blocks.
module Ouroboros.Consensus.Cardano.LeiosWibble
  ( wibbleMishapenDijkstraBlock
  ) where

import Cardano.Crypto.Hash.Blake2b (Blake2b_256)
import Cardano.Crypto.Hash.Class (Hash, hashToBytes, hashWith)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word64)

-- | Given the on-the-wire bytes of a Cardano block: if it is a Dijkstra block
-- whose (indefinite-length) transaction sequence, once rewritten to definite
-- length, makes the body hash equal the header's committed body hash, return
-- the block with that rewrite applied. Otherwise return 'Nothing'.
--
-- 'Nothing' covers every well-formed block (its definite reframing would /not/
-- match its committed hash) as well as blocks whose sequence is already
-- definite, so rewriting is a fixed point: re-decoding a rewritten block does
-- not rewrite it again.
--
-- Byte fidelity: we only rewrite when re-encoding the /unmodified/ 'Term'
-- reproduces the input bytes exactly (see 'roundTrips'). That guarantees the
-- only byte-level difference we introduce is the transaction-sequence framing,
-- so the header — and thus its hash — is preserved verbatim, and the
-- hash-verified body is exactly the definite encoding the header committed to.
-- If 'encodeTerm' is not faithful for some block, we leave it untouched.
wibbleMishapenDijkstraBlock :: LBS.ByteString -> Maybe LBS.ByteString
wibbleMishapenDijkstraBlock lbs = do
  (leftover, term) <- hush (deserialiseFromBytes decodeTerm lbs)
  guard (LBS.null leftover)
  -- A Cardano block Term is @[eraTag, [header, body]]@.
  (eraTag, header, body) <- case term of
    TList [t, TList [h, b]] -> Just (t, h, b)
    _ -> Nothing
  guard (asWord64 eraTag == Just 8) -- Dijkstra
  committedBodyHash <- headerBodyHash header
  body' <- makeTxSeqDefinite body -- only proceeds if the tx sequence is indefinite
  -- The definite reframing hashes to what the header committed to: this is the
  -- mishapen block, and 'body'' is the encoding it should have had.
  guard (blake2b256 (LBS.toStrict (encode body')) == committedBodyHash)
  -- Only now (a genuine candidate) pay for the full-block fidelity check.
  guard (roundTrips term)
  Just (encode (TList [eraTag, TList [header, body']]))
 where
  encode = toLazyByteString . encodeTerm
  roundTrips t = encode t == lbs

-- | The header is @[header_body, kes_sig]@ and @header_body@ is
-- @[blockNo, slot, prevHash, issuerVKey, vrfVKey, vrfResult, bodySize,
-- bodyHash, …]@, so @hbBodyHash@ is element 7.
headerBodyHash :: Term -> Maybe ByteString
headerBodyHash term = do
  fields <- case term of
    TList (TList fields : _) -> Just fields
    _ -> Nothing
  case drop 7 fields of
    hashT : _ -> asBytes hashT
    _ -> Nothing

-- | The body is @[invalidIndices, txs, leiosCert, perasCert]@. Make @txs@
-- definite-length; if it is already definite, fail (leave the block untouched).
makeTxSeqDefinite :: Term -> Maybe Term
makeTxSeqDefinite = \case
  TList (invalidIndices : txs : rest) -> do
    txs' <- case txs of
      TListI xs -> Just (TList xs)
      _ -> Nothing
    Just (TList (invalidIndices : txs' : rest))
  _ -> Nothing

-- | Blake2b-256 of the given bytes, matching the ledger's block-body hash.
blake2b256 :: ByteString -> ByteString
blake2b256 bs = hashToBytes (hashWith id bs :: Hash Blake2b_256 ByteString)

asWord64 :: Term -> Maybe Word64
asWord64 = \case
  TInt n | n >= 0 -> Just (fromIntegral n)
  TInteger n | n >= 0 && n <= toInteger (maxBound :: Word64) -> Just (fromIntegral n)
  _ -> Nothing

asBytes :: Term -> Maybe ByteString
asBytes = \case
  TBytes b -> Just b
  TBytesI b -> Just (LBS.toStrict b)
  _ -> Nothing

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
