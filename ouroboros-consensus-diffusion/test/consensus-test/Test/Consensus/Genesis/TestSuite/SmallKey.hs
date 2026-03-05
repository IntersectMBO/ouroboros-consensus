{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

-- | Internal module defining the 'SmallKey' utiliy class for 'TestSuite'
-- construction. Exposed through 'Test.Consensus.Genesis.TestSuite' re-exports.
module Test.Consensus.Genesis.TestSuite.SmallKey (SmallKey (allKeys)) where

import Data.Kind
import Data.Proxy
import Data.Word
import GHC.Generics
import GHC.TypeError
import Type.Reflection

-- | Creating an instance of this class is a declaration that the type has a
-- /small/ finite number of values and that 'allKeys' constains them all.
-- \"Small\" here is a safety requirement, meaning that it is feasible to
-- drive an exhaustive construction from its values.
--
-- /Laws:/
--
-- @
-- 'elem' x 'allKeys'                         -- Any inhabitant has a finite index
-- 'length' ('filter' (== x) 'allKeys') == 1  -- Should terminate
-- @
class SmallKey a where
  allKeys :: [a]

instance
  ( Generic a
  , AssertNotRecursive a (Rep a)
  , GSmallKey (Rep a)
  ) =>
  SmallKey (Generically a)
  where
  allKeys = fmap (Generically . to) $ gAllKeys @(Rep a)

--------------------------------------------------------------------------------
-- TODO [Blacklist]
--
-- If @base ^>=4.19.0.0@, then `GHC.TypeError.Unsatisfiable` +
-- `GHC.TypeError.unsatisfiable` can be used to black-list types instead of
-- 'TypeError' + 'error', allowing finer triggering of the error that
-- could be used to improve the 'SmallKey.Tests' (at least in principle).
--
-- See https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst
--------------------------------------------------------------------------------

type family AssertNotRecursive (a :: Type) (f :: Type -> Type) :: Constraint where
  -- Throw type error if a direct recursive occurrence of the data type is found
  -- in its generic representation.
  AssertNotRecursive a (Rec0 a) =
    TypeError
      ( 'Text "Recursive data types are not allowed "
          ':<>: 'Text "to have a SmallKey instance: "
          ':<>: 'ShowType a
      )
  AssertNotRecursive _ (Rec0 _) = ()
  AssertNotRecursive a (l :+: r) =
    (AssertNotRecursive a l, AssertNotRecursive a r)
  AssertNotRecursive a (l :*: r) =
    (AssertNotRecursive a l, AssertNotRecursive a r)
  AssertNotRecursive a (M1 _ _ f) =
    AssertNotRecursive a f
  AssertNotRecursive _ _ = ()

-- * Generic instance of 'SmallKey'

class GSmallKey f where
  gAllKeys :: [f a]

instance GSmallKey V1 where
  gAllKeys = []

instance GSmallKey U1 where
  gAllKeys = [U1]

instance (GSmallKey f, GSmallKey g) => GSmallKey (f :+: g) where
  gAllKeys = fmap L1 gAllKeys <> fmap R1 gAllKeys

instance
  TypeError
    ( 'Text "Product types are not allowed "
        ':<>: 'Text "to have a SmallKey instance"
    ) =>
  GSmallKey (f :*: g)
  where
  gAllKeys = error "unreachable: product type"

instance GSmallKey f => GSmallKey (M1 i c f) where
  gAllKeys = fmap M1 gAllKeys

instance SmallKey a => GSmallKey (K1 r a) where
  gAllKeys = fmap K1 allKeys

-- * Bundled instances

instance SmallKey () where
  allKeys = [()]

instance SmallKey Bool where
  allKeys = [False, True]

-- * Black-listed instances of large types.

type family NoSmallKey ty :: Constraint where
  NoSmallKey ty =
    TypeError
      ( 'ShowType ty ':<>: 'Text " doesn't have a SmallKey instance"
          ':$$: 'Text "because it is too large for exhaustive construction"
      )

unreachableSK :: forall a. Typeable a => Proxy a -> String
unreachableSK _ = "unreachable: NoSmallKey " <> show (typeRep @a)

instance NoSmallKey Integer => SmallKey Integer where
  allKeys = error $ unreachableSK (Proxy :: Proxy Integer)

instance NoSmallKey Int => SmallKey Int where
  allKeys = error $ unreachableSK (Proxy :: Proxy Int)

instance NoSmallKey Word => SmallKey Word where
  allKeys = error $ unreachableSK (Proxy :: Proxy Word)

instance NoSmallKey Word16 => SmallKey Word16 where
  allKeys = error $ unreachableSK (Proxy :: Proxy Word16)

instance NoSmallKey Word32 => SmallKey Word32 where
  allKeys = error $ unreachableSK (Proxy :: Proxy Word32)

instance NoSmallKey Word64 => SmallKey Word64 where
  allKeys = error $ unreachableSK (Proxy :: Proxy Word64)

instance NoSmallKey Char => SmallKey Char where
  allKeys = error $ unreachableSK (Proxy :: Proxy Char)
