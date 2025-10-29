{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Consensus.Util.TypeLevel
  ( Fst
  , Snd
  , KV
  ) where

import Data.Kind (Type)

type KV {- key -} = Type {- value -} -> Type -> Type

type Fst :: (Type, Type) -> Type
type family Fst kv where
  Fst '(k, v) = k

type Snd :: (Type, Type) -> Type
type family Snd kv where
  Snd '(k, v) = k
