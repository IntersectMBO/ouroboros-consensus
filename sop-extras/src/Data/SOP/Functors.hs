{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

module Data.SOP.Functors (
    Flip (..)
  , K2 (..)
  , Product2 (..)
  , snd2
  ) where

import           Data.Kind (Type)
import           GHC.Generics (Generic)
import           NoThunks.Class

type Product2 :: (Type -> Type -> Type)
              -> (Type -> Type -> Type)
              -> Type -> Type -> Type
data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)

snd2 :: Product2 f g x y -> g x y
snd2 (Pair2 _ g) = g

type Flip :: (x -> y -> Type) -> y -> x -> Type
newtype Flip f x y = Flip {unFlip :: f y x}
  deriving (Eq, Generic, NoThunks, Show)

type K2 :: Type -> x -> y -> Type
newtype K2 a b c = K2 a
