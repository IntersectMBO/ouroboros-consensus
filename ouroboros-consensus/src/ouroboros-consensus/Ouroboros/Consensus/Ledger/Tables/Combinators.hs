{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Ledger tables are barbie-types (see @barbies@ package), though unfortunately
-- we can not implement classes like 'FunctorB' for ledger tables because the
-- class expects a type that is indexed over a /(uni-)functor/. Ledger tables
-- are indexed over /bifunctors/ (mapkinds), so the kinds do not match. To cut
-- on boilerplate, we do not define variants of 'FunctorB' (and similar classes)
-- for types that are indexed over bifunctors. Instead, we define specialised
-- variants of class functions and utility functions. For example:
--
-- * 'ltmap' instead of 'bmap' or 'bmapC'
--
-- * 'lttraverse' instead of 'btraverse' or 'btraverseC'
--
-- * 'ltsequence' instead of 'bsequence'.
--
-- TODO: if we make mapkinds of kind @(k1, k2) -> Type@ instead of @k1 -> k2 ->
-- Type@, then we could reuse most of the @barbies@ machinery.
module Ouroboros.Consensus.Ledger.Tables.Combinators
  ( -- * Common constraints
    LedgerTableConstraints
  , KVConstraints
    -- * Functor
  , ltmap

    -- * Traversable
  , lttraverse

    -- ** Utility functions
  , ltsequence

    -- * Applicative
  , ltprod
  , ltpure

    -- ** Utility functions
  , ltap
  , ltliftA
  , ltliftA2
  , ltliftA3
  , ltliftA4

    -- * Applicative and Traversable
  , ltzipWith2A

    -- * Collapsing
  , ltcollapse

    -- * Lifted functions
  , fn2_1
  , fn2_2
  , fn2_3
  , fn2_4
  , type (-..->) (..)

    -- ** Re-exports of utils
  , (...:)
  , (..:)
  , (.:)

    -- * Basic bifunctors
  , K2 (..)
  , type (:..:) (..)

  , onUTxOTable
  , onInstantStakeTable
  , onTable
  , onAllTables
  , onAllRawTables
  ) where

import Data.Bifunctor
import Data.Kind
import Data.List.Singletons (SList (..))
import Data.Proxy
import Data.SOP.BasicFunctors ((:.:) (..), K (..))
import Data.SOP.Constraint
import Data.SOP.Functors
import Data.SOP.Strict
import Data.Singletons
import Lens.Micro
import Ouroboros.Consensus.Ledger.Tables.Basics
import Ouroboros.Consensus.Ledger.Tables.MapKind
import Ouroboros.Consensus.Util ((...:), (..:), (.:))
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Common constraints
-------------------------------------------------------------------------------}

-- | The @Eq (TxOut l)@ constraint is here only because of
-- 'Ouroboros.Consensus.Ledger.Tables.Diff.diff'. Once the ledger provides
-- deltas instead of us being the ones that compute them, we can probably drop
-- this constraint.
type LedgerTableConstraints blk = LedgerTableConstraintsMK blk EmptyMK
type KVConstraints blk = KVConstraintsMK blk EmptyMK

type RawLedgerTableConstraints blk k v =
  ( Ord k
  , Eq v
  , MemPack k
  , IndexedMemPack (MemPackIdx blk EmptyMK) v
  )

onUTxOTable ::
  SingI (TablesForBlock blk) =>
  Proxy blk -> ASetter' (LedgerTables blk mk) (Table mk blk UTxOTable)
onUTxOTable p = onTable p (Proxy @UTxOTable)

onInstantStakeTable ::
  SingI (TablesForBlock blk) =>
  Proxy blk -> ASetter' (LedgerTables blk mk) (Table mk blk InstantStakeTable)
onInstantStakeTable p = onTable p (Proxy @InstantStakeTable)

onTable ::
  forall blk (tag :: TAG) mk.
  (SingI tag, SingI (TablesForBlock blk)) =>
  Proxy blk ->
  Proxy tag ->
  ASetter' (LedgerTables blk mk) (Table mk blk tag)
onTable _ _ =
  case setterForSing (sing @(TablesForBlock blk)) (sing @tag) of
    Nothing -> \_ s -> pure s
    Just setter -> setter

onAllTables ::
  forall blk mk mk'.
  LedgerTableConstraints blk =>
  Proxy blk ->
  (forall k v. RawLedgerTableConstraints blk k v => mk k v -> mk' k v) ->
  LedgerTables blk mk ->
  LedgerTables blk mk'
onAllTables p f (LedgerTables tbs) =
  LedgerTables (onAllRawTables p f tbs)

onAllRawTables ::
  forall blk mk mk'.
  LedgerTableConstraints blk =>
  Proxy blk ->
  (forall k v. RawLedgerTableConstraints blk k v => mk k v -> mk' k v) ->
  NP (Table mk blk) (TablesForBlock blk) ->
  NP (Table mk' blk) (TablesForBlock blk)
onAllRawTables _ f tbs = go (sing @(TablesForBlock blk)) tbs
 where
  go :: All (KVConstraints blk) xs => SList xs -> NP (Table mk blk) xs -> NP (Table mk' blk) xs
  go SNil Nil = Nil
  go (SCons _ sn) (Table mk :* next) = Table (f mk) :* go sn next

onAllRawTablesF ::
  forall blk f mk mk'.
  (Applicative f, LedgerTableConstraints blk) =>
  Proxy blk ->
  (forall k v. RawLedgerTableConstraints blk k v => mk k v -> f (mk' k v)) ->
  NP (Table mk blk) (TablesForBlock blk) ->
  f (NP (Table mk' blk) (TablesForBlock blk))
onAllRawTablesF _ f tbs = hsequence' $ go (sing @(TablesForBlock blk)) tbs
 where
  go :: All (KVConstraints blk) xs => SList xs -> NP (Table mk blk) xs -> NP (f :.: Table mk' blk) xs
  go SNil Nil = Nil
  go (SCons _ sn) (Table mk :* next) = Comp (Table <$> f mk) :* go sn next

{-------------------------------------------------------------------------------
  Functor
-------------------------------------------------------------------------------}

-- | Like 'bmap', but for ledger tables.
ltmap ::
  forall blk mk1 mk2.
  LedgerTableConstraints blk =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> mk2 k v) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2
ltmap = onAllTables (Proxy @blk)

{-------------------------------------------------------------------------------
  Traversable
-------------------------------------------------------------------------------}

-- | Like 'btraverse', but for ledger tables.
lttraverse ::
  forall blk f mk1 mk2.
  (Applicative f, LedgerTableConstraints blk) =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> f (mk2 k v)) ->
  LedgerTables blk mk1 ->
  f (LedgerTables blk mk2)
lttraverse f (LedgerTables x) = LedgerTables <$> onAllRawTablesF (Proxy @blk) f x

--
-- Utility functions
--

ltsequence ::
  (Applicative f, LedgerTableConstraints blk) =>
  LedgerTables blk (f :..: mk) ->
  f (LedgerTables blk mk)
ltsequence = lttraverse unComp2

{-------------------------------------------------------------------------------
  Applicative
-------------------------------------------------------------------------------}

-- | Like 'bpure', but for ledger tables.
ltpure ::
  forall blk mk. LedgerTableConstraints blk =>
  (forall k v. RawLedgerTableConstraints blk k v => mk k v) ->
  LedgerTables blk mk
ltpure f = LedgerTables $ hcpure (Proxy @(KVConstraints blk)) (Table f)

-- | Like 'bprod', but for ledger tables.
ltprod :: forall blk f g.  LedgerTableConstraints blk => LedgerTables blk f -> LedgerTables blk g -> LedgerTables blk (f `Product2` g)
ltprod (LedgerTables x) (LedgerTables y) = LedgerTables $ hczipWith (Proxy @(KVConstraints blk)) (\(Table tx) (Table ty) -> Table $ Pair2 tx ty) x y

--
-- Utility functions
--

ltap ::
  LedgerTableConstraints blk =>
  LedgerTables blk (mk1 -..-> mk2) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2
ltap f x = ltmap g $ ltprod f x
 where
  g (Pair2 f' x') = apFn2 f' x'

ltliftA ::
  LedgerTableConstraints blk =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> mk2 k v) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2
ltliftA f x = ltpure (fn2_1 f) `ltap` x

ltliftA2 ::
  LedgerTableConstraints blk =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> mk2 k v -> mk3 k v) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2 ->
  LedgerTables blk mk3
ltliftA2 f x x' = ltpure (fn2_2 f) `ltap` x `ltap` x'

ltliftA3 ::
  LedgerTableConstraints blk =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> mk2 k v -> mk3 k v -> mk4 k v) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2 ->
  LedgerTables blk mk3 ->
  LedgerTables blk mk4
ltliftA3 f x x' x'' = ltpure (fn2_3 f) `ltap` x `ltap` x' `ltap` x''

ltliftA4 ::
  LedgerTableConstraints blk =>
  ( forall k v.
    RawLedgerTableConstraints blk k v =>
    mk1 k v -> mk2 k v -> mk3 k v -> mk4 k v -> mk5 k v
  ) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2 ->
  LedgerTables blk mk3 ->
  LedgerTables blk mk4 ->
  LedgerTables blk mk5
ltliftA4 f x x' x'' x''' =
  ltpure (fn2_4 f) `ltap` x `ltap` x' `ltap` x'' `ltap` x'''

{-------------------------------------------------------------------------------
  Applicative and Traversable
-------------------------------------------------------------------------------}

ltzipWith2A ::
  (Applicative f, LedgerTableConstraints blk) =>
  (forall k v. RawLedgerTableConstraints blk k v => mk1 k v -> mk2 k v -> f (mk3 k v)) ->
  LedgerTables blk mk1 ->
  LedgerTables blk mk2 ->
  f (LedgerTables blk mk3)
ltzipWith2A f = ltsequence .: ltliftA2 (Comp2 .: f)

{-------------------------------------------------------------------------------
  Collapsing
-------------------------------------------------------------------------------}

ltcollapse :: SListI (TablesForBlock blk) => LedgerTables blk (K2 a) -> NP (K a) (TablesForBlock blk)
ltcollapse (LedgerTables tbs) = hmap (\(Table (K2 t)) -> K t) tbs

{-------------------------------------------------------------------------------
  Semigroup and Monoid
-------------------------------------------------------------------------------}

instance
  ( forall k v. RawLedgerTableConstraints blk k v => Semigroup (mk k v)
  , LedgerTableConstraints blk
  ) =>
  Semigroup (LedgerTables blk mk)
  where
  (<>) :: LedgerTables blk mk -> LedgerTables blk mk -> LedgerTables blk mk
  (<>) = ltliftA2 (<>)

instance
  ( forall k v. RawLedgerTableConstraints blk k v => Monoid (mk k v)
  , LedgerTableConstraints blk
  ) =>
  Monoid (LedgerTables blk mk)
  where
  mempty :: LedgerTables blk mk
  mempty = ltpure mempty

{-------------------------------------------------------------------------------
  Lifted functions
-------------------------------------------------------------------------------}

-- | Lifted functions
--
-- Similar to '(-.->)', but for @f@ and @g@ that are bifunctors.
type (-..->) :: (k1 -> k2 -> Type) -> (k1 -> k2 -> Type) -> k1 -> k2 -> Type
newtype (f -..-> g) a b = Fn2 {apFn2 :: f a b -> g a b}

infixr 1 -..->

-- | Construct a lifted function.
fn2_1 :: (f a b -> g a b) -> (f -..-> g) a b
fn2_1 = Fn2

-- | Construct a binary lifted function
fn2_2 :: (f a b -> f' a b -> f'' a b) -> (f -..-> f' -..-> f'') a b
fn2_2 f = Fn2 $ \x -> Fn2 $ \x' -> f x x'

-- | Construct a ternary lifted function.
fn2_3 ::
  (f a b -> f' a b -> f'' a b -> f''' a b) ->
  (f -..-> f' -..-> f'' -..-> f''') a b
fn2_3 f = Fn2 $ \x -> Fn2 $ \x' -> Fn2 $ \x'' -> f x x' x''

-- | Construct a quaternary lifted function.
fn2_4 ::
  (f a b -> f' a b -> f'' a b -> f''' a b -> f'''' a b) ->
  (f -..-> f' -..-> f'' -..-> f''' -..-> f'''') a b
fn2_4 f = Fn2 $ \x -> Fn2 $ \x' -> Fn2 $ \x'' -> Fn2 $ \x''' -> f x x' x'' x'''

{-------------------------------------------------------------------------------
  Basic bifunctors
-------------------------------------------------------------------------------}

-- | The constant type bifunctor.
type K2 :: Type -> k1 -> k2 -> Type
newtype K2 a b c = K2 {unK2 :: a}
  deriving stock (Show, Eq)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Monoid, Semigroup)

instance Bifunctor (K2 a) where
  bimap _ _ (K2 x) = K2 x

-- | Composition of functor after bifunctor.
--
-- Example: @Comp2 (Just (17, True)) :: (Maybe :..: (,)) Int Bool@
type (:..:) :: (k3 -> Type) -> (k1 -> k2 -> k3) -> k1 -> k2 -> Type
newtype (:..:) f g a b = Comp2 {unComp2 :: f (g a b)}
  deriving stock (Show, Eq)
  deriving stock (Functor, Foldable)
  deriving newtype (Monoid, Semigroup)

infixr 7 :..:

deriving stock instance
  (Traversable f, Traversable (g a)) =>
  Traversable ((f :..: g) a)

instance (Functor f, Bifunctor g) => Bifunctor (f :..: g) where
  bimap f g (Comp2 x) = Comp2 $ fmap (bimap f g) x
