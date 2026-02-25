{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Strict variant of NP
--
-- See [@sop-core@'s
-- NP](https://hackage.haskell.org/package/sop-core/docs/Data-SOP-NP.html).
module Data.SOP.Strict.NP
  ( -- * NP
    NP (..)
  , hd
  , map_NP'
  , npToSListI
  , singletonNP
  , tl
  ) where

import Data.Coerce
import Data.Kind (Type)
import Data.SOP hiding (NP (..), hd, tl)
import Data.SOP.Classes
import Data.SOP.Constraint
import NoThunks.Class

type NP :: (k -> Type) -> [k] -> Type
data NP f xs where
  Nil :: NP f '[]
  (:*) :: !(f x) -> !(NP f xs) -> NP f (x ': xs)

infixr 5 :*

type instance CollapseTo NP a = [a]
type instance AllN NP c = All c
type instance AllZipN NP c = AllZip c
type instance Prod NP = NP
type instance SListIN NP = SListI
type instance Same NP = NP

singletonNP :: f x -> NP f '[x]
singletonNP fx = fx :* Nil

hd :: NP f (x ': xs) -> f x
hd (x :* _) = x

tl :: NP f (x ': xs) -> NP f xs
tl (_ :* xs) = xs

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil Nil = Nil
ap_NP (Fn f :* fs) (x :* xs) = f x :* ap_NP fs xs

pure_NP :: SListI xs => (forall a. f a) -> NP f xs
pure_NP = cpure_NP (Proxy @Top)

cpure_NP ::
  forall c xs proxy f.
  All c xs =>
  proxy c -> (forall a. c a => f a) -> NP f xs
cpure_NP _ f = go sList
 where
  go :: All c xs' => SList xs' -> NP f xs'
  go SNil = Nil
  go SCons = f :* go sList

collapse_NP :: NP (K a) xs -> [a]
collapse_NP = go
 where
  go :: NP (K a) xs -> [a]
  go Nil = []
  go (K x :* xs) = x : go xs

ctraverse'_NP ::
  forall c proxy xs f f' g.
  (All c xs, Applicative g) =>
  proxy c -> (forall a. c a => f a -> g (f' a)) -> NP f xs -> g (NP f' xs)
ctraverse'_NP _ f = go
 where
  go :: All c ys => NP f ys -> g (NP f' ys)
  go Nil = pure Nil
  go (x :* xs) = (:*) <$> f x <*> go xs

ctraverse__NP ::
  forall c proxy xs f g.
  (All c xs, Applicative g) =>
  proxy c -> (forall a. c a => f a -> g ()) -> NP f xs -> g ()
ctraverse__NP _ f = go
 where
  go :: All c ys => NP f ys -> g ()
  go Nil = pure ()
  go (x :* xs) = f x *> go xs

traverse__NP ::
  forall xs f g.
  (SListI xs, Applicative g) =>
  (forall a. f a -> g ()) -> NP f xs -> g ()
traverse__NP = ctraverse__NP (Proxy @Top)

trans_NP ::
  AllZip c xs ys =>
  proxy c ->
  (forall x y. c x y => f x -> g y) ->
  NP f xs ->
  NP g ys
trans_NP _ _t Nil = Nil
trans_NP p t (x :* xs) = t x :* trans_NP p t xs

coerce_NP ::
  forall f g xs ys.
  AllZip (LiftedCoercible f g) xs ys =>
  NP f xs -> NP g ys
coerce_NP = trans_NP (Proxy @(LiftedCoercible f g)) coerce

-- | Version of 'map_NP' that does not require a singleton
map_NP' :: forall f g xs. (forall a. f a -> g a) -> NP f xs -> NP g xs
map_NP' f = go
 where
  go :: NP f xs' -> NP g xs'
  go Nil = Nil
  go (x :* xs) = f x :* go xs

-- | Conjure up an 'SListI' constraint from an 'NP'
npToSListI :: NP a xs -> (SListI xs => r) -> r
npToSListI np = sListToSListI $ npToSList np
 where
  sListToSListI :: SList xs -> (SListI xs => r) -> r
  sListToSListI SNil k = k
  sListToSListI SCons k = k

  npToSList :: NP a xs -> SList xs
  npToSList Nil = SNil
  npToSList (_ :* xs) = sListToSListI (npToSList xs) SCons

instance HPure NP where
  hpure = pure_NP
  hcpure = cpure_NP

instance HAp NP where
  hap = ap_NP

instance HCollapse NP where
  hcollapse = collapse_NP

instance HSequence NP where
  hctraverse' = ctraverse'_NP
  htraverse' = hctraverse' (Proxy @Top)
  hsequence' = htraverse' unComp

instance HTraverse_ NP where
  hctraverse_ = ctraverse__NP
  htraverse_ = traverse__NP

instance HTrans NP NP where
  htrans = trans_NP
  hcoerce = coerce_NP

-- | Copied from sop-core
--
-- Not derived, since derived instance ignores associativity info
instance All (Show `Compose` f) xs => Show (NP f xs) where
  showsPrec _ Nil = showString "Nil"
  showsPrec d (f :* fs) =
    showParen (d > 5) $
      showsPrec (5 + 1) f
        . showString " :* "
        . showsPrec 5 fs

deriving instance All (Eq `Compose` f) xs => Eq (NP f xs)
deriving instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NP f xs)

instance
  All (Compose NoThunks f) xs =>
  NoThunks (NP (f :: k -> Type) (xs :: [k]))
  where
  showTypeOf _ = "NP"
  wNoThunks ctxt = \case
    Nil -> return Nothing
    x :* xs ->
      allNoThunks
        [ noThunks ("fst" : ctxt) x
        , noThunks ("snd" : ctxt) xs
        ]
