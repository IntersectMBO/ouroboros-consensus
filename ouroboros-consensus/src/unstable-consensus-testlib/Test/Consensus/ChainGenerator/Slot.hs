{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Consensus.ChainGenerator.Slot (
    -- * Counting
    E (ActiveSlotE, EmptySlotE, SlotE)
  , complementActive
  , complementEmpty
    -- * Slot
  , S
  , Test.Consensus.ChainGenerator.Slot.showS
  , genS
    -- * Reuse
  , POL (mkActive, test)
  , Pol (Inverted, NotInverted)
  , PreImage
  , inverted
  , notInverted
  ) where

import           Data.Coerce (coerce)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as MVG
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Random.Stateful as R
import qualified Test.Consensus.ChainGenerator.Counting as C
import           Test.Consensus.ChainGenerator.Params (Asc, ascVal)
import qualified Test.QuickCheck as QC

-- | The activeness of some slot
newtype S = S Bool
  deriving (QC.Arbitrary, Eq, Ord, Read, Show)

newtype instance MV.MVector s S = MV_S (MV.MVector s Bool)
newtype instance V.Vector     S = V_S (V.Vector Bool)
deriving newtype instance MVG.MVector MV.MVector S
deriving newtype instance VG.Vector   V.Vector   S
instance V.Unbox S

-----

genS :: R.RandomGen g => Asc -> g -> (S, g)
genS asc g =
    bool `seq` (S bool, g')
  where
    (q, g') = R.random g   -- note 0 <= q <= 1

    bool = q < ascVal asc

showS :: S -> ShowS
showS (S bool) = showChar $ if bool then '1' else '0'

-----

-- | Type-level names for the different kinds of slots counted in this library
--
-- The data constructors of this type are used in promoted form with
-- @-XDataKinds@.
--
data E =
     -- | Active slots must be filled on the honest chain and may be filled on an alternative chain.
     ActiveSlotE
     -- | Empty slots may be filled on the honest chain and must not be filled on an alternative chain.
   | EmptySlotE
     -- | @SlotE@ is the union of 'ActiveSlotE' and 'EmptySlotE'
   | SlotE

inverted :: Proxy Inverted
inverted = Proxy

notInverted :: Proxy NotInverted
notInverted = Proxy

-- | The polarity of a type
--
-- This is used to toggle how functions in this library interact with vectors
-- of 'S' values.
--
-- If the polarity is 'Inverted', then the function will treat all 'S' values as
-- if they were first complemented.
--
-- The 'PreImage' type family does the corresponding parameterization of
-- 'ActiveSlotE' and 'EmptySlotE' at the type level.
--
-- NOTE: No 'S' value is ever actually complemented because of 'Inverted', but
-- the functions parameterized by 'pol' will treat them as if they were.
data Pol = Inverted | NotInverted

-- | Overloaded slot operations for the two polarities
class POL (pol :: Pol) where
    -- | Make an active slot
    mkActive :: proxy pol -> S
    -- | Test whether @pol@ maps the given bit to one
    test :: proxy pol -> S -> Bool

-- Both 'complementActive' and 'complementEmpty' are offered for simplicity
-- instead of a generalized function that works in both cases (it would need
-- another proxy parameter for the element type).

-- | Every slot is either active or empty
complementActive ::
     proxy pol
  -> C.Size base SlotE
  -> C.Count base (PreImage pol ActiveSlotE) which
  -> C.Count base (PreImage pol EmptySlotE ) which
complementActive _pol (C.Count n) (C.Count i) = C.Count (n - i)

-- | Every slot is either active or empty
complementEmpty ::
     proxy pol
  -> C.Size base SlotE
  -> C.Count base (PreImage pol EmptySlotE ) which
  -> C.Count base (PreImage pol ActiveSlotE) which
complementEmpty _pol (C.Count n) (C.Count i) = C.Count (n - i)

instance POL Inverted where
    mkActive _pol = coerce False
    test     _pol = coerce not

instance POL NotInverted where
    mkActive _pol = coerce True
    test     _pol = coerce

-- | @PreImage pol e@ is the complement of @e@ if @pol@ is 'Inverted' and simply @e@ if it's 'NotInverted'
type family PreImage (pol :: Pol) (e :: E) where
    PreImage Inverted    EmptySlotE  = ActiveSlotE
    PreImage Inverted    ActiveSlotE = EmptySlotE
    PreImage NotInverted e           = e
