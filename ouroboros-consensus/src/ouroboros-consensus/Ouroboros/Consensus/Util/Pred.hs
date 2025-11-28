{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Self-explaining boolean predicates
--
-- These can be used to provide detailed counterexamples or witnesses for
-- boolean predicates that evaluate to 'False' or 'True', respectively.
--
-- NOTE: to keep this as simple as possible, we do not perform any boolean
-- simplifications (e.g., double negations, or De Morgan's laws) on the
-- predicates while evauating them. This can be added later if needed.
module Ouroboros.Consensus.Util.Pred
  ( Pred (..)
  , Evidence
  , evalPred
  , Explainable (..)
  , ExplanationMode (..)
  , ShowExplain (..)
  , explainShallow
  , explainDeep
  )
where

import Data.Bifunctor (bimap)
import Data.Typeable (Typeable, cast)

{-------------------------------------------------------------------------------
  Self-explaining boolean predicates
-------------------------------------------------------------------------------}

data Pred tag where
  -- | Tag a predicate with some metadata
  (:=) :: !tag -> !(Pred tag) -> Pred tag
  -- | A concrete boolean value
  Bool :: !Bool -> Pred tag
  -- | Boolean negation
  Not :: !(Pred tag) -> Pred tag
  -- | Greater-than comparison
  (:>:) :: (Typeable a, Ord a, Show a) => !a -> !a -> Pred tag
  -- | Less-than-or-equal comparison
  (:<=:) :: (Typeable a, Ord a, Show a) => !a -> !a -> Pred tag
  -- | Equality comparison
  (:==:) :: (Typeable a, Eq a, Show a) => !a -> !a -> Pred tag
  -- | Conjunction
  (:/\:) :: !(Pred tag) -> !(Pred tag) -> Pred tag
  -- | Disjunction
  (:\/:) :: !(Pred tag) -> !(Pred tag) -> Pred tag

deriving instance Show tag => Show (Pred tag)

instance Eq tag => Eq (Pred tag) where
  (t1 := p1) == (t2 := p2) =
    t1 == t2 && p1 == p2
  Bool b1 == Bool b2 =
    b1 == b2
  Not p1 == Not p2 =
    p1 == p2
  (a1 :>: b1) == (a2 :>: b2)
    | Just (a2', b2') <- cast (a2, b2) =
        a1 == a2' && b1 == b2'
  (a1 :<=: b1) == (a2 :<=: b2)
    | Just (a2', b2') <- cast (a2, b2) =
        a1 == a2' && b1 == b2'
  (a1 :==: b1) == (a2 :==: b2)
    | Just (a2', b2') <- cast (a2, b2) =
        a1 == a2' && b1 == b2'
  (a1 :/\: b1) == (a2 :/\: b2) =
    a1 == a2 && b1 == b2
  (a1 :\/: b1) == (a2 :\/: b2) =
    a1 == a2 && b1 == b2
  _ == _ =
    False

infixr 2 :=

infixr 3 :\/:

infixr 4 :/\:

infixr 5 `Not`

infix 5 :>:

infix 5 :<=:

infix 5 :==:

-- | Sufficient evidence to show that a predicate is either true or false
type Evidence a = Pred a

-- | Evaluate a predicate, yielding either a counterexample or a witness.
--
-- The returned value contains the minimum (modulo conjunction/disjunction
-- short circuiting) evidence needed to explain the outcome.
--
-- Some examples:
--
-- >>> data P = A | B | C  deriving Show
-- >>> a = A := Bool True   -- a ~ True
-- >>> b = B := 2+2 :==: 5  -- b ~ False
-- >>> c = C := 10 :>: 5    -- c ~ True
--
-- >>> evalPred $ a :/\: c  -- success because both a~True and c~True
-- Right ((A := Bool True) :/\: (C := 10 :>: 5))
--
-- >>> evalPred $ a :\/: b -- success because a~True, short-circuits
-- Right (A := Bool True)
--
-- >>> evalPred $ a :/\: b :/\: c -- failure because b~False, short-circuits
-- Left (B := 4 :==: 5)
--
-- >>> evalPred $ (b :\/: a) :/\: (b :\/: c) -- success because of a~True and c~True
-- Right ((A := Bool True) :/\: (C := 10 :>: 5))
--
-- >>> evalPred $ b :\/: (Not c) -- failure because both b~False and c~True
-- Left ((B := 4 :==: 5) :\/: Not (C := 10 :>: 5))
--
-- >>> evalPred $ Not (a :/\: b) -- success because b~False
-- Right (Not (B := 4 :==: 5))
--
-- >>> evalPred $ Not (a :/\: c) -- failure because both a~True and c~True
-- Left (Not ((A := Bool True) :/\: (C := 10 :>: 5)))
evalPred :: Pred tag -> Either (Evidence tag) (Evidence tag)
evalPred = \case
  tag := p' ->
    lift (tag :=) id p'
  p@(Bool b) ->
    boolean b p
  Not p' ->
    lift Not negation p'
  p@(a :>: b) ->
    boolean (a > b) p
  p@(a :<=: b) ->
    boolean (a <= b) p
  p@(a :==: b) ->
    boolean (a == b) p
  a :/\: b ->
    case evalPred a of
      Left a' -> Left a' -- short-circuit
      Right a' ->
        case evalPred b of
          Right b' -> Right (a' :/\: b')
          Left b' -> Left b'
  a :\/: b ->
    case evalPred a of
      Right a' -> Right a' -- short-circuit
      Left a' ->
        case evalPred b of
          Right b' -> Right b'
          Left b' -> Left (a' :\/: b')
 where
  boolean b p
    | b = Right p
    | otherwise = Left p

  lift f g p = bimap f f (g (evalPred p))

  negation = either Right Left

{-------------------------------------------------------------------------------
  Explainable type class
-------------------------------------------------------------------------------}

-- | Explanation mode
--
-- Used to control whether we want to continue explaining terms beyond tags
-- * Shallow: only explain tags
-- * Deep: explain full predicates
data ExplanationMode = Shallow | Deep
  deriving (Show, Eq)

-- | Provides a human-readable explanation for a value
class Explainable a where
  explain :: ExplanationMode -> a -> String
  explain = explainPrec 0

  explainPrec :: Int -> ExplanationMode -> a -> String
  explainPrec _ = explain

  {-# MINIMAL (explain | explainPrec) #-}

-- | Shallow explanation
explainShallow :: Explainable a => a -> String
explainShallow = explain Shallow

-- | Deep explanation
explainDeep :: Explainable a => a -> String
explainDeep = explain Deep

-- | Default 'Explainable' instance via 'Show' to be used with 'deriving via'
newtype ShowExplain a = ShowExplain a
  deriving stock Show

instance Show a => Explainable (ShowExplain a) where
  explain _ (ShowExplain a) = show a

deriving via ShowExplain Bool instance Explainable Bool

instance Explainable a => Explainable (Pred a) where
  explainPrec prec mode = \case
    tag := p ->
      case mode of
        Shallow ->
          explainShallow tag
        Deep ->
          parensIf (prec > 1) $
            explainShallow tag <> " := " <> explainPrec 2 mode p
    Bool b ->
      explain mode b
    Not p ->
      parensIf (prec > 4) $
        "not " <> explainPrec 5 mode p
    a :>: b ->
      parensIf (prec > 4) $
        show a <> " > " <> show b
    a :<=: b ->
      parensIf (prec > 4) $
        show a <> " <= " <> show b
    a :==: b ->
      parensIf (prec > 4) $
        show a <> " == " <> show b
    a :/\: b ->
      parensIf (prec > 3) $
        explainPrec 4 mode a <> " and " <> explainPrec 3 mode b
    a :\/: b ->
      parensIf (prec > 2) $
        explainPrec 3 mode a <> " or " <> explainPrec 2 mode b
   where
    parensIf :: Bool -> String -> String
    parensIf True s = "(" <> s <> ")"
    parensIf False s = s
