{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

module Data.Bifunctor.Barbie.Constraints (
    -- * Instance dictionaries
    Dict2 (..)
  , requiringDict2
    -- * Getting constraints
  , ClassB2
  ) where

import           Data.Kind (Constraint, Type)

{-------------------------------------------------------------------------------
  Instance dictionaries
-------------------------------------------------------------------------------}

-- | @'Dict2' c a b@ is evidence that there exists an instance of @c a b@.
type Dict2 :: (k1 -> k2 -> Constraint) -> k1 -> k2 -> Type
data Dict2 c a b where
  Dict2 :: c a b => Dict2 c a b

-- | Turn a constrained-function into an unconstrained one that uses the packed
-- instance dictionary instead.
requiringDict2 :: (c a b => r) -> Dict2 c a b -> r
requiringDict2 r Dict2 = r

{-------------------------------------------------------------------------------
  Getting constraints
-------------------------------------------------------------------------------}

-- | Create a bifunctor constraint from two functor constraints.
type ClassB2 ::
     (k1 -> Constraint)
  -> (k2 -> Constraint)
  -> (k1 -> k2 -> Constraint)
class (c1 k, c2 v) => ClassB2 c1 c2 k v
instance (c1 k, c2 v) => ClassB2 c1 c2 k v
