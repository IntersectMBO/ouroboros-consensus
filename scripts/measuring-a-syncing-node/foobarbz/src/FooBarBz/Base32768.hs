{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module FooBarBz.Base32768 (encode) where

import Data.Bits ((.&.))
import Data.Bits qualified as Bit
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as A
import Data.Text.Lazy.Builder qualified as T
import Data.Word (Word32, Word8)

-- TODO 'Text' is UTF-8, not UTF-16

encode :: ByteString -> T.Builder
encode bytes = B.foldr pushByte eof bytes 0 0

-- | @z@ and @numZBits@
--
-- The exact types don't matter very much here, but it's safe for the two to be
-- different types.
--
-- @0 <= numZBits 15@, @0 <= z < 2^numZBits@
type Sink = Word32 -> Int -> T.Builder

pushByte :: Word8 -> Sink -> Sink
pushByte byte k z numZBits =
    if after < 0 then k raw $! (numZBits + 8) else
    let !c  = index fifteenE $ Bit.unsafeShiftR raw after
        !z' = fromIntegral byte .&. lsbs after
    in
    T.singleton c <> k z' after
  where
    -- How many bits beyond 15 is @raw@?
    after = numZBits - 7   --- ie numZBits + 8 - 15

    !raw = shift byte z

eof :: Sink
eof z numZBits
  | 0 == numZBits = mempty
  | numZBits <= 7 = pad sevenE    7
  | otherwise     = pad fifteenE 15
  where
    -- append 1s
    pad arr x =
        let n = x - numZBits
        in
            T.singleton
          $ index arr
          $ Bit.unsafeShiftL z (fromIntegral n) + lsbs n

-----

-- | Shift the @n@ most significant bits of @b@ into the lowest @n@ bits of
-- @z@, preserving their order.
shift :: Word8 -> Word32 -> Word32
shift byte z = Bit.unsafeShiftL z 8 + fromIntegral byte

-- | @2^n - 1@ has 1s in the @n@ least significant bits and 0s elsewhere
lsbs :: Int -> Word32
lsbs n = Bit.bit n - 1

-----

index :: PrimArray Char -> Word32 -> Char
index arr i = A.indexPrimArray arr (fromIntegral i)

repertoireE :: String -> PrimArray Char
repertoireE =
    A.primArrayFromList
  . concat
  . go
  where
    go = \case
        []      -> []
        [_]     -> error "impossible!"   -- even length by construction
        c1:c2:s ->
            -- 'Char' is very much the code point itself, so we don't need the
            -- explicit conversions that qntm needed within JavaScript.
            [c1 .. c2] : go s

fifteenE :: PrimArray Char
fifteenE = repertoireE "ҠҿԀԟڀڿݠޟ߀ߟကဟႠႿᄀᅟᆀᆟᇠሿበቿዠዿጠጿᎠᏟᐠᙟᚠᛟកសᠠᡟᣀᣟᦀᦟ᧠᧿ᨠᨿᯀᯟᰀᰟᴀᴟ⇠⇿⋀⋟⍀⏟␀␟─❟➀➿⠀⥿⦠⦿⨠⩟⪀⪿⫠⭟ⰀⰟⲀⳟⴀⴟⵀⵟ⺠⻟㇀㇟㐀䶟䷀龿ꀀꑿ꒠꒿ꔀꗿꙀꙟꚠꛟ꜀ꝟꞀꞟꡀꡟ"

sevenE :: PrimArray Char
sevenE = repertoireE "ƀƟɀʟ"
