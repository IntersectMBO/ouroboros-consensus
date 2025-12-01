{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Util.TypeLevel
  ( Unions
  , PrefixI (..)
  , prefixAll
  , withAllDict
  , ToAllDict (..)
  , AllDict (..)
  ) where

import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.SOP.Constraint (All)
import qualified Data.SOP.Dict as Dict

type Unions :: [[k]] -> [k]
type family Unions xxs where
  Unions '[x] = x
  Unions (x ': y ': xs) = Unions (Union x y ': xs)

data Prefix :: [k] -> [k] -> Type where
  PNil :: Prefix '[] ys
  PCons :: Prefix xs ys -> Prefix (x ': xs) (x ': ys)

data AllDict c xs where
  ANil :: AllDict c '[]
  ACons :: Dict.Dict c x -> AllDict c xs -> AllDict c (x ': xs)

class All c xs => ToAllDict c xs where
  toAllDict :: AllDict c xs

instance ToAllDict c '[] where
  toAllDict = ANil

instance (c x, ToAllDict c xs) => ToAllDict c (x ': xs) where
  toAllDict = ACons Dict.Dict toAllDict

prefixAll :: Prefix xs ys -> AllDict c ys -> AllDict c xs
prefixAll PNil _ = ANil
prefixAll (PCons p) (ACons d rest) =
  ACons d (prefixAll p rest)

withAllDict :: AllDict c xs -> (All c xs => r) -> r
withAllDict ANil k = k
withAllDict (ACons Dict.Dict rest) k = withAllDict rest k

class PrefixI xs ys where
  prefix :: Prefix xs ys

instance PrefixI '[] ys where
  prefix = PNil

instance PrefixI xs ys => PrefixI (x ': xs) (x ': ys) where
  prefix = PCons prefix
