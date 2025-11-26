{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ouroboros.Consensus.Util.TypeLevel
  ( Fst
  , Snd
  , KV
  , Unions
  ) where

import Data.Kind (Type)
import Data.List.Singletons
import Data.SOP.Strict

type KV {- key -} = Type {- value -} -> Type -> Type

type Fst :: (Type, Type) -> Type
type family Fst kv where
  Fst '(k, v) = k

type Snd :: (Type, Type) -> Type
type family Snd kv where
  Snd '(k, v) = k

type Unions :: [[k]] -> [k]
type family Unions xxs where
  Unions '[x] = x
  Unions (x ': y ': xs) = Unions (Union x y  ': xs)
