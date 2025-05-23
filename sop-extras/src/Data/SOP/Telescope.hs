{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'Telescope'
--
-- Intended for qualified import
--
-- > import           Data.SOP.Telescope (Telescope(..))
-- > import qualified Data.SOP.Telescope as Telescope
module Data.SOP.Telescope
  ( -- * Telescope
    Telescope (..)
  , sequence

    -- ** Utilities
  , fromTZ
  , fromTip
  , tip
  , toAtMost

    -- ** Bifunctor analogues of SOP functions
  , bihap
  , bihczipWith
  , bihmap
  , bihzipWith

    -- * Extension, retraction, alignment
  , Extend (..)
  , Retract (..)
  , align
  , extend
  , retract

    -- ** Simplified API
  , alignExtend
  , alignExtendNS
  , extendIf
  , retractIf

    -- * Additional API
  , ScanNext (..)
  , SimpleTelescope (..)
  , scanl
  ) where

import Data.Coerce (coerce)
import Data.Functor.Product
import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Counting
import Data.SOP.InPairs (InPairs (..), Requiring (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Strict
import Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
import GHC.Stack
import NoThunks.Class (NoThunks (..), allNoThunks)
import Prelude hiding (scanl, sequence, zipWith)

{-------------------------------------------------------------------------------
  Telescope
-------------------------------------------------------------------------------}

-- | Telescope
--
-- A telescope is an extension of an 'NS', where every time we "go right" in the
-- sum we have an additional value.
--
-- The 'Telescope' API mostly follows @sop-core@ conventions, supporting
-- functor ('hmap', 'hcmap'), applicative ('hap', 'hpure'), foldable
-- ('hcollapse') and traversable ('hsequence''). However, since 'Telescope'
-- is a bi-functor, it cannot reuse the @sop-core@ classes. The naming scheme
-- of the functions is adopted from @sop-core@ though; for example:
--
-- > bi h (c) zipWith
-- > |  |  |    |
-- > |  |  |    \ zipWith: the name from base
-- > |  |  |
-- > |  |  \ constrained: version of the function with a constraint parameter
-- > |  |
-- > |  \ higher order: 'Telescope' (like 'NS'/'NP') is a /higher order/ functor
-- > |
-- > \ bifunctor: 'Telescope' (unlike 'NS'/'NP') is a higher order /bifunctor/
--
-- In addition to the standard SOP operators, the new operators that make
-- a 'Telescope' a telescope are 'extend', 'retract' and 'align'; see their
-- documentation for details.
type Telescope :: (k -> Type) -> (k -> Type) -> [k] -> Type
data Telescope g f xs where
  TZ :: !(f x) -> Telescope g f (x ': xs)
  TS :: !(g x) -> !(Telescope g f xs) -> Telescope g f (x ': xs)

{-------------------------------------------------------------------------------
  SOP class instances for 'Telescope'
-------------------------------------------------------------------------------}

type instance Prod (Telescope g) = NP
type instance SListIN (Telescope g) = SListI
type instance AllN (Telescope g) c = All c

instance HAp (Telescope g) where
  hap = flip go
   where
    -- We could define this in terms of 'bihap' but we lack 'SListI'
    go :: Telescope g f xs -> NP (f -.-> f') xs -> Telescope g f' xs
    go (TZ fx) (f :* _) = TZ (apFn f fx)
    go (TS gx t) (_ :* fs) = TS gx (go t fs)

instance HTraverse_ (Telescope g) where
  hctraverse_ p = bihctraverse_ p (\_ -> pure ())
  htraverse_ = bihtraverse_ (\_ -> pure ())

instance HSequence (Telescope g) where
  hsequence' = bihsequence' . bihmap (Comp . pure) id
  hctraverse' p = bihctraverse' p pure
  htraverse' = bihtraverse' pure

-- | Specialization of 'hsequence'' with weaker constraints
-- ('Functor' rather than 'Applicative')
sequence ::
  forall m g f xs.
  Functor m =>
  Telescope g (m :.: f) xs -> m (Telescope g f xs)
sequence = go
 where
  go :: Telescope g (m :.: f) xs' -> m (Telescope g f xs')
  go (TZ (Comp fx)) = TZ <$> fx
  go (TS gx t) = TS gx <$> go t

instance
  (forall x y. LiftedCoercible g g x y) =>
  HTrans (Telescope g) (Telescope g)
  where
  htrans p transf = \case
    TZ fx -> TZ $ transf fx
    TS gx t -> TS (coerce gx) $ htrans p transf t
  hcoerce = \case
    TZ fx -> TZ $ coerce fx
    TS gx t -> TS (coerce gx) $ hcoerce t

type instance Same (Telescope g) = Telescope g

{-------------------------------------------------------------------------------
  Bifunctor analogues of class methods
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hap'
bihap ::
  NP (g -.-> g') xs ->
  NP (f -.-> f') xs ->
  Telescope g f xs ->
  Telescope g' f' xs
bihap = \gs fs t -> go t gs fs
 where
  go ::
    Telescope g f xs ->
    NP (g -.-> g') xs ->
    NP (f -.-> f') xs ->
    Telescope g' f' xs
  go (TZ fx) _ (f :* _) = TZ (apFn f fx)
  go (TS gx t) (g :* gs) (_ :* fs) = TS (apFn g gx) (go t gs fs)

-- | Bifunctor analogue of 'hctraverse''
bihctraverse' ::
  forall proxy c m g g' f f' xs.
  (All c xs, Applicative m) =>
  proxy c ->
  (forall x. c x => g x -> m (g' x)) ->
  (forall x. c x => f x -> m (f' x)) ->
  Telescope g f xs ->
  m (Telescope g' f' xs)
bihctraverse' _ g f = go
 where
  go :: All c xs' => Telescope g f xs' -> m (Telescope g' f' xs')
  go (TZ fx) = TZ <$> f fx
  go (TS gx t) = TS <$> g gx <*> go t

-- | Bifunctor analogue of 'htraverse''
bihtraverse' ::
  (SListI xs, Applicative m) =>
  (forall x. g x -> m (g' x)) ->
  (forall x. f x -> m (f' x)) ->
  Telescope g f xs ->
  m (Telescope g' f' xs)
bihtraverse' = bihctraverse' (Proxy @Top)

-- | Bifunctor analogue of 'hsequence''
bihsequence' ::
  forall m g f xs.
  (SListI xs, Applicative m) =>
  Telescope (m :.: g) (m :.: f) xs -> m (Telescope g f xs)
bihsequence' = bihtraverse' unComp unComp

-- | Bifunctor analogue of 'hctraverse_'
bihctraverse_ ::
  forall proxy c xs m g f.
  (All c xs, Applicative m) =>
  proxy c ->
  (forall x. c x => g x -> m ()) ->
  (forall x. c x => f x -> m ()) ->
  Telescope g f xs ->
  m ()
bihctraverse_ _ g f = go
 where
  go :: All c xs' => Telescope g f xs' -> m ()
  go (TZ fx) = f fx
  go (TS gx t) = g gx *> go t

bihtraverse_ ::
  (SListI xs, Applicative m) =>
  (forall x. g x -> m ()) ->
  (forall x. f x -> m ()) ->
  Telescope g f xs ->
  m ()
bihtraverse_ = bihctraverse_ (Proxy @Top)

{-------------------------------------------------------------------------------
  Bifunctor analogues of derived functions
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hmap'
bihmap ::
  SListI xs =>
  (forall x. g x -> g' x) ->
  (forall x. f x -> f' x) ->
  Telescope g f xs ->
  Telescope g' f' xs
bihmap = bihcmap (Proxy @Top)

-- | Bifunctor analogue of 'hcmap'
bihcmap ::
  All c xs =>
  proxy c ->
  (forall x. c x => g x -> g' x) ->
  (forall x. c x => f x -> f' x) ->
  Telescope g f xs ->
  Telescope g' f' xs
bihcmap p g f = bihap (hcpure p (fn g)) (hcpure p (fn f))

-- | Bifunctor equivalent of 'hzipWith'
bihzipWith ::
  SListI xs =>
  (forall x. h x -> g x -> g' x) ->
  (forall x. h x -> f x -> f' x) ->
  NP h xs ->
  Telescope g f xs ->
  Telescope g' f' xs
bihzipWith g f ns = bihap (hmap (fn . g) ns) (hmap (fn . f) ns)

-- | Bifunctor equivalent of 'hczipWith'
bihczipWith ::
  All c xs =>
  proxy c ->
  (forall x. c x => h x -> g x -> g' x) ->
  (forall x. c x => h x -> f x -> f' x) ->
  NP h xs ->
  Telescope g f xs ->
  Telescope g' f' xs
bihczipWith p g f ns = bihap (hcmap p (fn . g) ns) (hcmap p (fn . f) ns)

{-------------------------------------------------------------------------------
  Simple telescope

  This is an internal type that is useful primarily useful as a sanity check
  of our bifunctor generalizations.
-------------------------------------------------------------------------------}

-- | 'Telescope' with both functors set to the same @f@
newtype SimpleTelescope f xs = SimpleTelescope
  { getSimpleTelescope :: Telescope f f xs
  }

type instance Prod SimpleTelescope = NP
type instance SListIN SimpleTelescope = SListI
type instance AllN SimpleTelescope c = All c
type instance CollapseTo SimpleTelescope a = [a]

instance HAp SimpleTelescope where
  hap fs = SimpleTelescope . bihap fs fs . getSimpleTelescope

instance HTraverse_ SimpleTelescope where
  hctraverse_ p f = bihctraverse_ p f f . getSimpleTelescope
  htraverse_ f = bihtraverse_ f f . getSimpleTelescope

instance HSequence SimpleTelescope where
  hsequence' = fmap SimpleTelescope . bihsequence' . getSimpleTelescope
  hctraverse' p f = fmap SimpleTelescope . bihctraverse' p f f . getSimpleTelescope
  htraverse' f = fmap SimpleTelescope . bihtraverse' f f . getSimpleTelescope

instance HCollapse SimpleTelescope where
  hcollapse (SimpleTelescope t) = go t
   where
    go :: Telescope (K a) (K a) xs -> [a]
    go (TZ (K x)) = [x]
    go (TS (K x) xs) = x : go xs

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

tip :: Telescope g f xs -> NS f xs
tip (TZ l) = Z l
tip (TS _ r) = S (tip r)

fromTip :: NS f xs -> Telescope (K ()) f xs
fromTip (Z l) = TZ l
fromTip (S r) = TS (K ()) (fromTip r)

toAtMost :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
toAtMost = go
 where
  go :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
  go (TZ (K ma)) = maybe AtMostNil atMostOne ma
  go (TS (K a) t) = AtMostCons a (go t)

fromTZ :: Telescope g f '[x] -> f x
fromTZ (TZ fx) = fx

{-------------------------------------------------------------------------------
  Extension and retraction
-------------------------------------------------------------------------------}

newtype Extend m g f x y = Extend {extendWith :: f x -> m (g x, f y)}

-- | Extend the telescope
--
-- We will not attempt to extend the telescope past its final segment.
extend ::
  forall m h g f xs.
  Monad m =>
  -- | How to extend
  InPairs (Requiring h (Extend m g f)) xs ->
  -- | Where to extend /from/
  NP (f -.-> Maybe :.: h) xs ->
  Telescope g f xs ->
  m (Telescope g f xs)
extend = go
 where
  go ::
    InPairs (Requiring h (Extend m g f)) xs' ->
    NP (f -.-> Maybe :.: h) xs' ->
    Telescope g f xs' ->
    m (Telescope g f xs')
  go PNil _ (TZ fx) =
    return (TZ fx)
  go (PCons e es) (p :* ps) (TZ fx) =
    case unComp $ apFn p fx of
      Nothing ->
        return (TZ fx)
      Just hx -> do
        (gx, fy) <- extendWith (provide e hx) fx
        TS gx <$> go es ps (TZ fy)
  go (PCons _ es) (_ :* ps) (TS gx fx) =
    TS gx <$> go es ps fx

newtype Retract m g f x y = Retract {retractWith :: g x -> f y -> m (f x)}

-- | Retract a telescope
retract ::
  forall m h g f xs.
  Monad m =>
  -- | How to retract
  Tails (Requiring h (Retract m g f)) xs ->
  -- | Where to retract /to/
  NP (g -.-> Maybe :.: h) xs ->
  Telescope g f xs ->
  m (Telescope g f xs)
retract =
  \tails np ->
    npToSListI np $ go tails np
 where
  go ::
    SListI xs' =>
    Tails (Requiring h (Retract m g f)) xs' ->
    NP (g -.-> Maybe :.: h) xs' ->
    Telescope g f xs' ->
    m (Telescope g f xs')
  go _ _ (TZ fx) = return $ TZ fx
  go (TCons r rs) (p :* ps) (TS gx t) =
    case unComp (apFn p gx) of
      Just hx ->
        fmap (TZ . hcollapse) $
          hsequence' $
            hzipWith (retractAux hx gx) r (tip t)
      Nothing ->
        TS gx <$> go rs ps t

-- | Internal auxiliary to 'retract' and 'alignWith'
retractAux ::
  Functor m =>
  h x -> -- Proof that we need to retract
  g x -> -- Era we are retracting to
  Requiring h (Retract m g f) x z ->
  f z -> -- Current tip (what we are retracting from)
  (m :.: K (f x)) z
retractAux hx gx r fz = Comp $ K <$> retractWith (provide r hx) gx fz

-- | Align a telescope with another, then apply a function to the tips
--
-- Aligning is a combination of extension and retraction, extending or
-- retracting the telescope as required to match up with the other telescope.
align ::
  forall m g' g f' f f'' xs.
  Monad m =>
  -- | How to extend
  InPairs (Requiring g' (Extend m g f)) xs ->
  -- | How to retract
  Tails (Requiring f' (Retract m g f)) xs ->
  -- | Function to apply at the tip
  NP (f' -.-> f -.-> f'') xs ->
  -- | Telescope we are aligning with
  Telescope g' f' xs ->
  Telescope g f xs ->
  m (Telescope g f'' xs)
align = \es rs atTip ->
  npToSListI atTip $ go es rs atTip
 where
  go ::
    SListI xs' =>
    InPairs (Requiring g' (Extend m g f)) xs' ->
    Tails (Requiring f' (Retract m g f)) xs' ->
    NP (f' -.-> f -.-> f'') xs' ->
    Telescope g' f' xs' ->
    Telescope g f xs' ->
    m (Telescope g f'' xs')
  go _ _ (f :* _) (TZ f'x) (TZ fx) =
    return $ TZ (f `apFn` f'x `apFn` fx)
  go (PCons _ es) (TCons _ rs) (_ :* fs) (TS _ f'x) (TS gx fx) =
    TS gx <$> go es rs fs f'x fx
  go _ (TCons r _) (f :* _) (TZ f'x) (TS gx fx) =
    fmap (TZ . (\fx' -> f `apFn` f'x `apFn` fx') . hcollapse) $
      hsequence' $
        hzipWith (retractAux f'x gx) r (tip fx)
  go (PCons e es) (TCons _ rs) (_ :* fs) (TS g'x t'x) (TZ fx) = do
    (gx, fy) <- extendWith (provide e g'x) fx
    TS gx <$> go es rs fs t'x (TZ fy)

{-------------------------------------------------------------------------------
  Derived API
-------------------------------------------------------------------------------}

-- | Version of 'extend' where the evidence is a simple 'Bool'
extendIf ::
  Monad m =>
  -- | How to extend
  InPairs (Extend m g f) xs ->
  -- | Where to extend /from/
  NP (f -.-> K Bool) xs ->
  Telescope g f xs ->
  m (Telescope g f xs)
extendIf es ps =
  npToSListI ps $
    extend
      (InPairs.hmap (Require . const) es)
      (hmap (\f -> fn $ fromBool . apFn f) ps)

-- | Version of 'retract' where the evidence is a simple 'Bool'
retractIf ::
  Monad m =>
  -- | How to retract
  Tails (Retract m g f) xs ->
  -- | Where to retract /to/
  NP (g -.-> K Bool) xs ->
  Telescope g f xs ->
  m (Telescope g f xs)
retractIf rs ps =
  npToSListI ps $
    retract
      (Tails.hmap (Require . const) rs)
      (hmap (\f -> fn $ fromBool . apFn f) ps)

-- | Version of 'align' that never retracts, only extends
--
-- PRE: The telescope we are aligning with cannot be behind us.
alignExtend ::
  (Monad m, HasCallStack) =>
  -- | How to extend
  InPairs (Requiring g' (Extend m g f)) xs ->
  -- | Function to apply at the tip
  NP (f' -.-> f -.-> f'') xs ->
  -- | Telescope we are aligning with
  Telescope g' f' xs ->
  Telescope g f xs ->
  m (Telescope g f'' xs)
alignExtend es atTip =
  npToSListI atTip $
    align es (Tails.hpure $ Require $ \_ -> error precondition) atTip
 where
  precondition :: String
  precondition = "alignExtend: precondition violated"

-- | Version of 'alignExtend' that extends with an NS instead
alignExtendNS ::
  (Monad m, HasCallStack) =>
  -- | How to extend
  InPairs (Extend m g f) xs ->
  -- | Function to apply at the tip
  NP (f' -.-> f -.-> f'') xs ->
  -- | NS we are aligning with
  NS f' xs ->
  Telescope g f xs ->
  m (Telescope g f'' xs)
alignExtendNS es atTip ns =
  npToSListI atTip $
    alignExtend
      (InPairs.hmap (Require . const) es)
      atTip
      (fromTip ns)

-- | Internal auxiliary to 'extendIf' and 'retractIf'
fromBool :: K Bool x -> (Maybe :.: K ()) x
fromBool (K True) = Comp $ Just $ K ()
fromBool (K False) = Comp Nothing

{-------------------------------------------------------------------------------
  Additional API
-------------------------------------------------------------------------------}

newtype ScanNext h g x y = ScanNext {getNext :: h x -> g x -> h y}

-- | Telescope analogue of 'scanl' on lists
--
-- This function is modelled on
--
-- > scanl :: (b -> a -> b) -> b -> [a] -> [b]
--
-- but there are a few differences:
--
-- * Since every seed has a different type, we must be given a function
--   for each transition.
-- * Unlike 'scanl', we preserve the length of the telescope
--   ('scanl' prepends the initial seed)
-- * Instead of generating a telescope containing only the seeds, we
--   instead pair the seeds with the elements.
scanl ::
  InPairs (ScanNext h g) (x ': xs) ->
  h x ->
  Telescope g f (x ': xs) ->
  Telescope (Product h g) (Product h f) (x ': xs)
scanl = go
 where
  go ::
    InPairs (ScanNext h g) (x' ': xs') ->
    h x' ->
    Telescope g f (x' ': xs') ->
    Telescope (Product h g) (Product h f) (x' ': xs')
  go _ hx (TZ fx) = TZ (Pair hx fx)
  go (PCons f fs) hx (TS gx t) = TS (Pair hx gx) $ go fs (getNext f hx gx) t

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

deriving instance
  ( All (Compose Eq g) xs
  , All (Compose Eq f) xs
  ) =>
  Eq (Telescope g f xs)

deriving instance
  ( All (Compose Eq g) xs
  , All (Compose Ord g) xs
  , All (Compose Eq f) xs
  , All (Compose Ord f) xs
  ) =>
  Ord (Telescope g f xs)

deriving instance
  ( All (Compose Show g) xs
  , All (Compose Show f) xs
  ) =>
  Show (Telescope g f xs)

instance
  ( All (Compose NoThunks g) xs
  , All (Compose NoThunks f) xs
  ) =>
  NoThunks (Telescope g f xs)
  where
  showTypeOf _ = "Telescope"
  wNoThunks ctxt = \case
    TZ f -> noThunks ("TZ" : ctxt) f
    TS g t ->
      allNoThunks
        [ noThunks ("g" : "TS" : ctxt) g
        , noThunks ("t" : "TS" : ctxt) t
        ]
