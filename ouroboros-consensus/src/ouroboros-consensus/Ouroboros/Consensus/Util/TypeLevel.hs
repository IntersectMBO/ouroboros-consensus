{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Util.TypeLevel
  ( Unions
  , withAllDict
  , ToAllDict (..)
  , AllDict
  , getNPByTag
  , findIndex
  ) where

import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.SOP.Constraint (All, SListI)
import qualified Data.SOP.Dict as Dict
import Data.SOP.Index
import Data.SOP.Strict
import Data.Singletons (SingI, sing)
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))

type Unions :: [[k]] -> [k]
type family Unions xxs where
  Unions '[x] = x
  Unions (x ': y ': xs) = Unions (Union x y ': xs)

type AllDict c xs = NP (Dict.Dict c) xs

class All c xs => ToAllDict c xs where
  toAllDict :: AllDict c xs

instance ToAllDict c '[] where
  toAllDict = Nil

instance (c x, ToAllDict c xs) => ToAllDict c (x ': xs) where
  toAllDict = Dict.Dict :* toAllDict

withAllDict :: AllDict c xs -> (All c xs => r) -> r
withAllDict Nil k = k
withAllDict (Dict.Dict :* rest) k = withAllDict rest k

getNPByTag ::
  forall k (table :: k) f (tables :: [k]).
  ( TestEquality (Sing :: k -> Type)
  , SingI tables
  , SListI tables
  ) =>
  Sing table ->
  NP f tables ->
  Maybe (f table)
getNPByTag stag ad =
  let mem = findIndex (sing @tables) stag
   in fmap (flip projectNP ad) mem

findIndex ::
  forall k (xs :: [k]) (x :: k).
  TestEquality (Sing :: k -> Type) =>
  SList xs ->
  Sing x ->
  Maybe (Index xs x)
findIndex SNil _ = Nothing
findIndex (SCons sy sxs) sx =
  case testEquality sx sy of
    Just Refl -> Just IZ
    Nothing -> IS <$> findIndex sxs sx
