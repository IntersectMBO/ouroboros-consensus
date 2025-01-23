module Spec.Foreign.HSTypes where

{-# FOREIGN GHC
  {-# LANGUAGE DeriveGeneric #-}
  {-# LANGUAGE DeriveFunctor #-}
#-}

open import Prelude

open import Foreign.Haskell
open import Foreign.Haskell.Coerce
open import Foreign.Haskell.Either
open import Data.Rational.Base

{-# FOREIGN GHC
  import GHC.Generics (Generic)
  import Data.Void (Void)
  import Prelude hiding (Rational)
  import GHC.Real (Ratio(..))
#-}

-- * The empty type

data Empty : Type where
{-# COMPILE GHC Empty = data Void () #-}

-- * Rational

data Rational : Type where
  _,_ : ℤ → ℕ → Rational
{-# COMPILE GHC Rational = data Rational ((:%)) #-}

-- We'll generate code with qualified references to Rational in this
-- module, so make sure to define it.
{-# FOREIGN GHC type Rational = Ratio Integer #-}

-- * Maps and Sets

record HSMap K V : Type where
  constructor MkHSMap
  field assocList : List (Pair K V)

record HSSet A : Type where
  constructor MkHSSet
  field elems : List A

instance
  Show-HSPair : ∀ {a b} {A : Set a} {B : Set b} → ⦃ Show A ⦄ → ⦃ Show B ⦄ → Show (Pair A B)
  Show-HSPair .show (x , y) = Show-× .show (x , y)

  Show-HSMap : ∀ {K V} → ⦃ Show K ⦄ → ⦃ Show V ⦄ → Show (HSMap K V)
  Show-HSMap .show (record { assocList = al }) = "HSMap " ◇ (Show-List .show al)

  Show-HSSet : ∀ {A} → ⦃ Show A ⦄ → Show (HSSet A)
  Show-HSSet .show (record { elems = es }) = "HSSet " ◇ (Show-List .show es)

{-# FOREIGN GHC
  newtype HSMap k v = MkHSMap [(k, v)]
    deriving (Generic, Show, Eq, Ord)
  newtype HSSet a = MkHSSet [a]
    deriving (Generic, Show, Eq, Ord)
#-}
{-# COMPILE GHC HSMap = data HSMap (MkHSMap) #-}
{-# COMPILE GHC HSSet = data HSSet (MkHSSet) #-}

-- * ComputationResult

data ComputationResult E A : Type where
  Success : A → ComputationResult E A
  Failure : E → ComputationResult E A

instance
  Show-ComputationResult : ∀ {E} {A} → ⦃ Show E ⦄ → ⦃ Show A ⦄ → Show (ComputationResult E A)
  Show-ComputationResult .show (Success x) = "Success " ◇ show x
  Show-ComputationResult .show (Failure e) = "Failure " ◇ show e

{-# FOREIGN GHC
  data ComputationResult e a = Success a | Failure e
    deriving (Functor, Eq, Show, Generic)

  instance Applicative (ComputationResult e) where
    pure = Success
    (Success f) <*> x = f <$> x
    (Failure e) <*> _ = Failure e

  instance Monad (ComputationResult e) where
    return = pure
    (Success a) >>= m = m a
    (Failure e) >>= _ = Failure e
#-}
{-# COMPILE GHC ComputationResult = data ComputationResult (Success | Failure) #-}
