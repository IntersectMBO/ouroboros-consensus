{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Ouroboros.Consensus.ChainGenerator.Params
  ( Asc (Asc, UnsafeAsc)
  , Delta (Delta)
  , Kcp (Kcp)
  , Len (Len)
  , Scg (Scg)
  , ascFromBits
  , ascFromDouble
  , ascVal
  , genAsc
  , genKSD
  ) where

import qualified Data.Bits as B
import Data.Word (Word8)
import Test.Ouroboros.Consensus.QuickCheck.Extras (sized1)
import qualified Test.QuickCheck as QC

-----

-- | The Δ parameter of the Praos theorems and so also of the Praos Race
-- Assumption
--
-- ASSUMPTION: If an honest block @b@ is minted at the start of slot @x@, then
-- every (healthy) honest node will have selected a chain no worse than @b@ by
-- the onset of slot @x + Δ + 1@.
--
-- NOTE: If @Δ=0@, then the best block minted in each slot is selected by every
-- (healthy) honest node before the onset of the next slot.
--
-- NOTE: If the honest block @k+1@ after its intersection with an alternative
-- chain was minted in slot @x@, then the alternative block @k+1@ after the
-- intersection can be minted no sooner than slot @x + Δ + 1@. Thus @x + Δ@ is
-- the youngest slot in the Praos Race Window.
newtype Delta = Delta Int
  deriving (Eq, Ord, Show, Read)

-- | The maximum length of any leader schedule
--
-- This can be interpreted as the /end of time/, the final moment simulated
-- during a test.
newtype Len = Len Int
  deriving (Eq, Ord, Show, Read)

-- | The @k@ parameter of the Praos Common Prefix property
--
-- Also known as the 'Ouroboros.Consensus.Config.SecurityParam.SecurityParam'.
newtype Kcp = Kcp Int
  deriving (Eq, Ord, Show, Read)

-- | The @s@ parameter of the Praos Chain Growth property
--
-- Also known as the width of the /stability window/. In particular, we assume
-- that an adversarial stake holder cannot drastically increase their rate of
-- election until at least @s@ many slots after the first block on an
-- adversarial chain.
--
-- In other words: we're assuming that any serious attempt to corrupt the leader
-- schedule would be isolated to a private adversarial chain.
newtype Scg = Scg Int
  deriving (Eq, Ord, Show, Read)

-----

-- | The /active slot coefficient/
--
-- INVARIANT: 0 < x < 1
--
-- It's as precise as 'Double', which likely suffices for all of our needs.
newtype Asc = UnsafeAsc Double
  deriving (Eq, Read, Show)

pattern Asc :: Double -> Asc
pattern Asc d <- UnsafeAsc d

{-# COMPLETE Asc #-}

ascFromDouble :: Double -> Asc
ascFromDouble d
  | d <= 0 = error "Asc must be > 0"
  | 1 <= d = error "Asc must be < 1"
  | otherwise = UnsafeAsc d

-- | PRECONDITION: the bits aren't all the same
--
-- The 'Asc' that equals the fraction @w \/ 2^widthW@.
ascFromBits :: (Enum w, B.FiniteBits w) => w -> Asc
ascFromBits w = ascFromDouble $ toEnum (fromEnum w) / (2 ^ B.finiteBitSize w)

-- | Interpret 'Asc' as a 'Double'
ascVal :: Asc -> Double
ascVal (Asc x) = x

genAsc :: QC.Gen Asc
genAsc = ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

genKSD :: QC.Gen (Kcp, Scg, Delta)
genKSD = sized1 $ \sz -> do
  -- k > 0 so we can ensure an alternative schema loses the density comparison
  -- without having to deactivate the first active slot
  k <- QC.choose (1, sz + 1)
  s <- (+ (k + 1)) <$> QC.choose (0, 2 * sz) -- ensures @(k+1) / s <= 1@
  d <- QC.choose (0, max 0 $ min (div sz 4) (s - 1)) -- ensures @d < s@
  pure (Kcp k, Scg s, Delta d)
