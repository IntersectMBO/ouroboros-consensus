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

-- * ComputationResult

data ComputationResult E A : Type where
  Success : A → ComputationResult E A
  Failure : E → ComputationResult E A

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
