{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Allocation-free cross-era comparison of Cardano transaction ids.
--
-- Both 'Ouroboros.Consensus.HardFork.Combinator.AcrossEras.OneEraGenTxId'
-- instances order by the txid hash, ignoring the era. We read each id's 32-byte
-- hash (Blake2b-256) as four big-endian 'Word64#' and compare those in
-- registers. The words are unboxed, so no hash is boxed on the heap on any
-- era's path:
--
--   * Shelley-based eras store the hash as 'PackedBytes32' (four words
--     already); we unbox its fields.
--   * Byron stores it as a 'ShortByteString'; we read four big-endian words
--     out.
--
-- The extraction walks the era sum with a single dictionary used on every
-- branch, so it is strict in the dictionary and allocates no per-level thunk.
module Ouroboros.Consensus.Cardano.CanHardFork.OptimizedTxIdComparison
  ( ToTxIdWords (..)
  , compareCardanoGenTxId
  ) where

import Cardano.Crypto (abstractHashToShort)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Class (PackedBytes (PackedBytes32))
import Cardano.Crypto.PackedBytes (unpackBytes)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.SOP.Constraint (All)
import qualified Data.SOP.Strict as SOP
import GHC.Exts (Word64#, gtWord64#, ltWord64#)
import GHC.Word (Word64 (W64#))
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.TypeFamilyWrappers (WrapGenTxId, unwrapGenTxId)

-- | Order two Cardano transaction ids by their txid hash, ignoring the era.
--
-- 'txIdWords' reads each hash as four unboxed words and 'compareW64' orders
-- them in registers. Two requirements keep this allocation-free:
--
-- * Both branches of 'txIdWords' force the @All ToTxIdWords ys@ dictionary
--   ('Z' reads its head, 'S' its tail), so GHC compiles the tail-dictionary
--   read as a strict field access rather than a per-step thunk.
--
-- * The compared words are unboxed ('Word64#'), so no hash is boxed on the
--   heap.
compareCardanoGenTxId ::
  All ToTxIdWords xs =>
  SOP.NS WrapGenTxId xs -> SOP.NS WrapGenTxId xs -> Ordering
compareCardanoGenTxId l r =
  case txIdWords l of
    (# a0, a1, a2, a3 #) -> case txIdWords r of
      (# b0, b1, b2, b3 #) ->
        compareW64 a0 b0 <> compareW64 a1 b1 <> compareW64 a2 b2 <> compareW64 a3 b3

-- | The four big-endian 64-bit words of an era's 32-byte txid hash
-- (Blake2b-256), unboxed so no hash is materialised on the heap. An era whose
-- txid hash is not four words is rejected by the 'Shelley' instance's
-- 'PackedBytes32' match rather than miscompared.
class ToTxIdWords blk where
  toTxIdWords :: GenTxId blk -> (# Word64#, Word64#, Word64#, Word64# #)

instance ToTxIdWords ByronBlock where
  toTxIdWords (ByronTxId i) = sbsWords (abstractHashToShort i)
  toTxIdWords (ByronDlgId i) = sbsWords (abstractHashToShort i)
  toTxIdWords (ByronUpdateProposalId i) = sbsWords (abstractHashToShort i)
  toTxIdWords (ByronUpdateVoteId i) = sbsWords (abstractHashToShort i)

instance ShelleyBasedEra era => ToTxIdWords (ShelleyBlock proto era) where
  toTxIdWords (ShelleyTxId i) =
    case Hash.hashToPackedBytes (SL.extractHash (SL.unTxId i)) of
      PackedBytes32 (W64# w0) (W64# w1) (W64# w2) (W64# w3) -> (# w0, w1, w2, w3 #)
      -- The ledger hash is Blake2b-256, always 'PackedBytes32'; this arm is
      -- unreachable and pays a copy only if it ever runs.
      pb -> sbsWords (unpackBytes pb)

-- | The four big-endian 'Word64#' of a 32-byte 'ShortByteString', assembled by
-- shifting its bytes. Portable (no byte swap) and allocation-free: the words go
-- straight into an unboxed tuple. This is the byte order the raw-hash reference
-- compares by, so it agrees with the oracle (checked by the property test across
-- word boundaries).
sbsWords :: ShortByteString -> (# Word64#, Word64#, Word64#, Word64# #)
sbsWords sbs =
  (#
    unbox (word64BigEndianAt 0)
    , unbox (word64BigEndianAt 8)
    , unbox (word64BigEndianAt 16)
    , unbox (word64BigEndianAt 24)
  #)
 where
  -- Inline so the four calls don't share one heap-allocated closure.
  {-# INLINE word64BigEndianAt #-}
  word64BigEndianAt byteOffset =
    -- with bytes b0 b1 … b7 starting at 'byteOffset' the result looks like:
    --    (b0 << 56) | (b1 << 48) | (b2 << 40) | (b3 << 32)
    --  | (b4 << 24) | (b5 << 16) | (b6 << 8)  | b7
    (byte (byteOffset + 0) `unsafeShiftL` 56)
      .|. (byte (byteOffset + 1) `unsafeShiftL` 48)
      .|. (byte (byteOffset + 2) `unsafeShiftL` 40)
      .|. (byte (byteOffset + 3) `unsafeShiftL` 32)
      .|. (byte (byteOffset + 4) `unsafeShiftL` 24)
      .|. (byte (byteOffset + 5) `unsafeShiftL` 16)
      .|. (byte (byteOffset + 6) `unsafeShiftL` 8)
      .|. byte (byteOffset + 7)
  byte k = fromIntegral (SBS.index sbs k) :: Word64
  unbox (W64# w) = w

-- | The txid hash of whichever era the id sits in, as four big-endian 'Word64#'.
-- Direct recursion down the sum, building no intermediate 'NS'. Uses its single
-- dictionary on every branch, so it is strict in it and allocates no per-level
-- thunk.
txIdWords ::
  All ToTxIdWords ys =>
  SOP.NS WrapGenTxId ys -> (# Word64#, Word64#, Word64#, Word64# #)
txIdWords (SOP.Z x) = toTxIdWords (unwrapGenTxId x)
txIdWords (SOP.S y) = txIdWords y

-- | Order two 64-bit words as unsigned, in registers.
compareW64 :: Word64# -> Word64# -> Ordering
compareW64 a b = case a `ltWord64#` b of
  1# -> LT
  _ -> case a `gtWord64#` b of
    1# -> GT
    _ -> EQ
