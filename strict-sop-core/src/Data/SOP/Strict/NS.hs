{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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

-- | Strict variant of NS
--
-- See [@sop-core@'s
-- NS](https://hackage.haskell.org/package/sop-core/docs/Data-SOP-NS.html).
module Data.SOP.Strict.NS (
    -- * NS
    NS (..)
  , index_NS
  , partition_NS
  , sequence_NS'
  , unZ
    -- * Injections
  , Injection
  , injections
  ) where

import           Data.Coerce
import           Data.Kind
import           Data.SOP hiding (Injection, NP (..), NS (..), injections,
                     shiftInjection, unZ)
import           Data.SOP.Classes
import           Data.SOP.Constraint
import           Data.SOP.Strict.NP
import           NoThunks.Class

type NS :: (k -> Type) -> [k] -> Type
data NS f xs where
  Z :: !(f x) -> NS f (x ': xs)
  S :: !(NS f xs) -> NS f (x ': xs)

type instance CollapseTo NS a = a
type instance AllN       NS c = All c
type instance Prod       NS   = NP
type instance SListIN    NS   = SListI
type instance Same       NS   = NS

unZ :: NS f '[x] -> f x
unZ (Z x) = x

index_NS :: forall f xs . NS f xs -> Int
index_NS = go 0
  where
    go :: forall ys . Int -> NS f ys -> Int
    go !acc (Z _) = acc
    go !acc (S x) = go (acc + 1) x

expand_NS :: SListI xs => (forall x. f x) -> NS f xs -> NP f xs
expand_NS = cexpand_NS (Proxy @Top)

cexpand_NS :: forall c proxy f xs. All c xs
           => proxy c -> (forall x. c x => f x) -> NS f xs -> NP f xs
cexpand_NS p d = go
  where
    go :: forall xs'. All c xs' => NS f xs' -> NP f xs'
    go (Z x) = x :* hcpure p d
    go (S i) = d :* go i

ap_NS :: NP (f -.-> g) xs -> NS f xs -> NS g xs
ap_NS = go
  where
    go :: NP (f -.-> g) xs -> NS f xs -> NS g xs
    go (f :* _)  (Z x)  = Z $ apFn f x
    go (_ :* fs) (S xs) = S $ go fs xs
    go Nil       x      = case x of {}

collapse_NS :: NS (K a) xs -> a
collapse_NS = go
  where
    go :: NS (K a) xs  -> a
    go (Z (K x)) = x
    go (S xs)    = go xs

ctraverse'_NS  :: forall c proxy xs f f' g. (All c xs,  Functor g)
               => proxy c
               -> (forall a. c a => f a -> g (f' a))
               -> NS f xs  -> g (NS f' xs)
ctraverse'_NS _ f = go
  where
    go :: All c ys => NS f ys -> g (NS f' ys)
    go (Z x)  = Z <$> f x
    go (S xs) = S <$> go xs

trans_NS :: forall proxy c f g xs ys. AllZip c xs ys
         => proxy c
         -> (forall x y . c x y => f x -> g y)
         -> NS f xs -> NS g ys
trans_NS _ t = go
  where
    go :: AllZip c xs' ys' => NS f xs' -> NS g ys'
    go (Z x) = Z (t x)
    go (S x) = S (go x)

-- NOTE: @sop-core@ defines this in terms of @unsafeCoerce@. Currently we
-- don't make much use of this function, so I prefer this more strongly
-- typed version.
coerce_NS :: forall f g xs ys. AllZip (LiftedCoercible f g) xs ys
          => NS f xs -> NS g ys
coerce_NS = trans_NS (Proxy @(LiftedCoercible f g)) coerce

-- | Version of 'sequence_NS' that requires only 'Functor'
--
-- The version in the library requires 'Applicative', which is unnecessary.
sequence_NS' :: forall xs f g. Functor f
             => NS (f :.: g) xs -> f (NS g xs)
sequence_NS' = go
  where
    go :: NS (f :.: g) xs' -> f (NS g xs')
    go (Z (Comp fx)) = Z <$> fx
    go (S r)         = S <$> go r

partition_NS :: forall xs f. SListI xs => [NS f xs] -> NP ([] :.: f) xs
partition_NS =
  foldr (hzipWith append . hexpand nil . hmap singleton) (hpure nil)
  where
    nil :: ([] :.: f) a
    nil = Comp []

    singleton :: f a -> ([] :.: f) a
    singleton = Comp . (:[])

    append :: ([] :.: f) a -> ([] :.: f) a -> ([] :.: f) a
    append (Comp as) (Comp as') = Comp (as ++ as')

instance HExpand NS where
  hexpand  = expand_NS
  hcexpand = cexpand_NS

instance HAp NS where
  hap = ap_NS

instance HCollapse NS where
  hcollapse = collapse_NS

instance HSequence NS where
  hctraverse' = ctraverse'_NS
  htraverse'  = hctraverse' (Proxy @Top)
  hsequence'  = htraverse' unComp

instance HTrans NS NS where
  htrans  = trans_NS
  hcoerce = coerce_NS

deriving instance All (Show `Compose` f) xs => Show (NS f xs)
deriving instance All (Eq   `Compose` f) xs => Eq   (NS f xs)
deriving instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NS f xs)

instance All (Compose NoThunks f) xs
      => NoThunks (NS (f :: k -> Type) (xs :: [k])) where
  showTypeOf _ = "NS"
  wNoThunks ctxt = \case
      Z l -> noThunks ("Z" : ctxt) l
      S r -> noThunks ("S" : ctxt) r

{-------------------------------------------------------------------------------
  Injections
-------------------------------------------------------------------------------}

type Injection (f :: k -> Type) (xs :: [k]) = f -.-> K (NS f xs)

injections :: forall xs f. SListI xs => NP (Injection f xs) xs
injections = go sList
  where
    go :: SList xs' -> NP (Injection f xs') xs'
    go SNil  = Nil
    go SCons = fn (K . Z) :* hmap shiftInjection (go sList)

shiftInjection :: Injection f xs a -> Injection f (x ': xs) a
shiftInjection (Fn f) = Fn $ K . S . unK . f
