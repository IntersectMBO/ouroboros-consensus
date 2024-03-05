{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

module Data.SOP.Functors (
    Flip (..)
  , K1 (..)
  , Product2 (..)
  , product2Snd
  ) where

import           Data.Kind (Type)
import           GHC.Generics (Generic)
import           NoThunks.Class

type Product2 :: (Type -> Type -> Type)
              -> (Type -> Type -> Type)
              -> Type -> Type -> Type
data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)

product2Snd :: Product2 f g x y -> g x y
product2Snd (Pair2 _ g) = g

type Flip :: (x -> y -> Type) -> y -> x -> Type
newtype Flip f x y = Flip {unFlip :: f y x}
  deriving (Eq, Generic, NoThunks, Show)

type K1 :: Type -> x -> y -> Type
newtype K1 a b c = K1 a
