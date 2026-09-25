{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Block.Abstract
  ( -- * Protocol
    BlockProtocol

    -- * Configuration
  , BlockConfig
  , CodecConfig
  , StorageConfig

    -- * Previous hash
  , GetPrevHash (..)
  , blockPrevHash

    -- * Working with headers
  , GetHeader (..)
  , GetHeader1 (..)
  , Header
  , blockIsEBB
  , blockToIsEBB
  , getBlockHeaderFields
  , headerHash
  , headerPoint
  , headerToIsEBB

    -- * Raw hash
  , ConvertRawHash (..)
  , decodeRawHash
  , encodeRawHash

    -- * Utilities for working with WithOrigin
  , succWithOrigin

    -- * Ouroboros Genesis window
  , GenesisWindow (..)

    -- * Re-export basic definitions from @ouroboros-network@
  , ChainHash (..)
  , HasHeader (..)
  , HeaderFields (..)
  , HeaderHash
  , Point (GenesisPoint, BlockPoint)
  , StandardHash
  , blockHash
  , blockNo
  , blockPoint
  , blockSlot
  , castHash
  , castHeaderFields
  , castPoint
  , pointHash
  , pointSlot

    -- * Re-export basic definitions from @cardano-base@
  , BlockNo (..)
  , EpochNo (..)
  , EpochSize (..)
  , SlotNo (..)
  , WithOrigin (Origin, NotOrigin)
  , fromWithOrigin
  , withOrigin
  , withOriginFromMaybe
  , withOriginToMaybe
  ) where

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot
  ( EpochNo (..)
  , EpochSize (..)
  , SlotNo (..)
  , WithOrigin (Origin)
  , fromWithOrigin
  , withOrigin
  , withOriginFromMaybe
  , withOriginToMaybe
  )
import qualified Cardano.Slotting.Slot as Cardano
import qualified Codec.Serialise as Serialise
import Codec.Serialise.Decoding (Decoder)
import Codec.Serialise.Encoding (Encoding)
import qualified Data.ByteString as Strict
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import GHC.TypeNats (KnownNat, Nat, natVal)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.EBB
import Ouroboros.Network.Block
  ( ChainHash (..)
  , HasHeader (..)
  , HeaderFields (..)
  , HeaderHash
  , Point
  , StandardHash
  , blockHash
  , blockNo
  , blockPoint
  , blockSlot
  , castHash
  , castHeaderFields
  , castPoint
  , pointHash
  , pointSlot
  , pattern BlockPoint
  , pattern GenesisPoint
  )

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

-- | Map block to consensus protocol
type family BlockProtocol blk :: Type

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Static configuration required to work with this type of blocks
data family BlockConfig blk :: Type

-- | Static configuration required for serialisation and deserialisation of
-- types pertaining to this type of block.
--
-- Data family instead of type family to get better type inference.
data family CodecConfig blk :: Type

-- | Config needed for the
-- 'Ouroboros.Consensus.Node.InitStorage.NodeInitStorage' class. Defined here to
-- avoid circular dependencies.
data family StorageConfig blk :: Type

{-------------------------------------------------------------------------------
  Get hash of previous block
-------------------------------------------------------------------------------}

class (HasHeader blk, GetHeader blk) => GetPrevHash blk where
  -- | Get the hash of the predecessor of this block
  headerPrevHash :: Header blk -> ChainHash blk

blockPrevHash :: GetPrevHash blk => blk -> ChainHash blk
blockPrevHash = castHash . headerPrevHash . getHeader

{-------------------------------------------------------------------------------
  Link block to its header
-------------------------------------------------------------------------------}

data family Header blk :: Type

class HasHeader (Header blk) => GetHeader blk where
  getHeader :: blk -> Header blk

  -- | Check whether the header is the header of the block.
  --
  -- For example, by checking whether the hash of the body stored in the
  -- header matches that of the block.
  blockMatchesHeader :: Header blk -> blk -> Bool

  -- | When the given header is the header of an Epoch Boundary Block, returns
  -- its epoch number.
  headerIsEBB :: Header blk -> Maybe EpochNo

headerToIsEBB :: GetHeader blk => Header blk -> IsEBB
headerToIsEBB = toIsEBB . isJust . headerIsEBB

blockIsEBB :: GetHeader blk => blk -> Maybe EpochNo
blockIsEBB = headerIsEBB . getHeader

blockToIsEBB :: GetHeader blk => blk -> IsEBB
blockToIsEBB = headerToIsEBB . getHeader

type instance BlockProtocol (Header blk) = BlockProtocol blk

class GetHeader1 t where
  getHeader1 :: t blk -> Header blk

instance GetHeader1 Header where getHeader1 = id

{-------------------------------------------------------------------------------
  Some automatic instances for 'Header'
-------------------------------------------------------------------------------}

type instance HeaderHash (Header blk) = HeaderHash blk

instance StandardHash blk => StandardHash (Header blk)

-- | Get the 'HeaderFields' of a block, without requiring 'HasHeader blk'
--
-- This is primarily useful as a a simple definition of 'HasHeader' for
-- block types:
--
-- > instance HasHeader SomeBlock where
-- >   getHeaderFields = getBlockHeaderFields
--
-- provided that there is a 'HasHeader' instance for the header.
--
-- Unfortunately we cannot give a 'HasHeader' instance once and for all; if we
-- mapped from a header to a block instead we could do
--
-- > instance HasHeader hdr => HasHeader (Block hdr) where
-- >  ..
--
-- but we can't do that when we do things this way around.
getBlockHeaderFields :: GetHeader blk => blk -> HeaderFields blk
getBlockHeaderFields = castHeaderFields . getHeaderFields . getHeader

{-------------------------------------------------------------------------------
  Convenience wrappers around 'HasHeader' that avoids unnecessary casts
-------------------------------------------------------------------------------}

headerHash :: HasHeader (Header blk) => Header blk -> HeaderHash blk
headerHash = blockHash

headerPoint :: HasHeader (Header blk) => Header blk -> Point blk
headerPoint = castPoint . blockPoint

{-------------------------------------------------------------------------------
  Raw hash
-------------------------------------------------------------------------------}

-- | Convert a hash from/to raw bytes
--
-- Variants of 'toRawHash' and 'fromRawHash' for 'ShortByteString' are
-- included. Override the default implementations to avoid an extra step in
-- case the 'HeaderHash' is a 'ShortByteString' under the hood.
--
-- The associated type 'HashSize' is the size of the hash in bytes, expressed at
-- the type level. The 'hashSize' method returns the same value at the term
-- level.
--
-- The hash size is enforced by default: 'fromRawHash' and 'fromShortRawHash'
-- return 'Nothing' when the input does not have the announced 'hashSize'. One
-- can opt out of this check through the 'unsafeFromRawHash' and
-- 'unsafeFromShortRawHash' variants, which must only be used at call sites
-- where there is no easy way to handle failure and the size is known to be
-- correct.
class KnownNat (HashSize blk) => ConvertRawHash blk where
  -- | The size of the hash in number of bytes, at the type level.
  --
  -- See 'hashSize' for the term-level counterpart.
  type HashSize blk :: Nat

  -- | Get the raw bytes from a hash
  toRawHash :: proxy blk -> HeaderHash blk -> Strict.ByteString
  toRawHash p = Short.fromShort . toShortRawHash p

  -- | Variant of 'toRawHash' for 'ShortByteString'
  toShortRawHash :: proxy blk -> HeaderHash blk -> ShortByteString
  toShortRawHash p = Short.toShort . toRawHash p

  -- | Construct the hash from a raw hash, enforcing that the input has the
  -- announced 'hashSize'.
  --
  -- Returns 'Nothing' on a size mismatch.
  fromRawHash :: proxy blk -> Strict.ByteString -> Maybe (HeaderHash blk)
  fromRawHash p bs
    | fromIntegral (Strict.length bs) == hashSize p = Just (unsafeFromRawHash p bs)
    | otherwise = Nothing

  -- | Construct the hash from a raw hash, enforcing that the input has the
  -- announced 'hashSize'.
  --
  -- Returns 'Nothing' on a size mismatch.
  fromShortRawHash :: proxy blk -> ShortByteString -> Maybe (HeaderHash blk)
  fromShortRawHash p sbs
    | fromIntegral (Short.length sbs) == hashSize p = Just (unsafeFromShortRawHash p sbs)
    | otherwise = Nothing

  -- | Construct the hash from a raw hash /without/ enforcing the size.
  --
  -- PRECONDITION: the input's size must match 'hashSize'.
  unsafeFromRawHash :: proxy blk -> Strict.ByteString -> HeaderHash blk
  unsafeFromRawHash p = unsafeFromShortRawHash p . Short.toShort

  -- | Construct the hash from a raw hash /without/ enforcing the size.
  --
  -- PRECONDITION: the input's size must match 'hashSize'.
  unsafeFromShortRawHash :: proxy blk -> ShortByteString -> HeaderHash blk
  unsafeFromShortRawHash p = unsafeFromRawHash p . Short.fromShort

  -- | The size of the hash in number of bytes
  hashSize :: proxy blk -> Word32
  hashSize _ = fromIntegral (natVal (Proxy @(HashSize blk)))

  {-# MINIMAL
    (toRawHash | toShortRawHash)
    , (unsafeFromRawHash | unsafeFromShortRawHash)
    #-}

encodeRawHash ::
  ConvertRawHash blk =>
  proxy blk -> HeaderHash blk -> Encoding
encodeRawHash p = Serialise.encode . toShortRawHash p

decodeRawHash ::
  ConvertRawHash blk =>
  proxy blk -> forall s. Decoder s (HeaderHash blk)
decodeRawHash p = do
  sbs <- Serialise.decode
  case fromShortRawHash p sbs of
    Just h -> pure h
    Nothing -> fail "decodeRawHash: hash has unexpected size"

{-------------------------------------------------------------------------------
  Utilities for working with WithOrigin
-------------------------------------------------------------------------------}

{-# COMPLETE Origin, NotOrigin #-}

-- | Custom pattern for 'WithOrigin'
--
-- This avoids clashing with our (extensive) use of 'At' for testing.
pattern NotOrigin :: t -> WithOrigin t
pattern NotOrigin t = Cardano.At t

-- | Return the successor of a 'WithOrigin' value. Useful in combination with
-- 'SlotNo' and 'BlockNo'.
succWithOrigin :: (Bounded t, Enum t) => WithOrigin t -> t
succWithOrigin = withOrigin minBound succ

{-------------------------------------------------------------------------------
  Ouroboros Genesis window
-------------------------------------------------------------------------------}

-- | Size of the Genesis window, in number of slots.
--
-- This is the number of slots that the GDD Governor (Genesis Density
-- Disconnection Governor -- see 'Ouroboros.Consensus.Genesis.Governor') will
-- consider when deciding whether to disconnect from a peer. It has to be
-- smaller or equal to the stability window. For instance, for Shelley-based
-- eras, this will be equal to a stability window, that is @3k/f@.
newtype GenesisWindow = GenesisWindow {unGenesisWindow :: Word64}
  deriving stock (Show, Eq, Ord)
  deriving newtype (NoThunks, Num)
