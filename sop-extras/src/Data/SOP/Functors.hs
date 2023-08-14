{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.SOP.Functors (Product2 (..)) where

import           Data.Kind (Type)
import           GHC.Generics (Generic)

type Product2 :: (Type -> Type -> Type)
              -> (Type -> Type -> Type)
              -> Type -> Type -> Type
data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)
