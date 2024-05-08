{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Utilities for arguments record with defaults
--
-- Useful for when you want to define a default value of an arguments record
-- consisting of a mix of arguments with/without defaults.
--
-- The following code example explains it best:
--
-- > data Args f = Args {
-- >       hasNoDefault :: HKD f Int
-- >     , hasDefault   :: Bool
-- >     }
-- >
-- > defaultArgs :: Incomplete Args
-- > defaultArgs = Args {
-- >       hasNoDefault = noDefault
-- >     , hasDefault   = False
-- >     }
-- >
-- > theArgs :: Complete Args
-- > theArgs = defaultArgs {
-- >       hasNoDefault = 0
-- >     }
-- >
-- > useArgs :: Complete Args -> (Int, Bool)
-- > useArgs (Args a b) = (a, b)
--
-- Leaving out the 'hasNoDefault' field from 'theArgs' will result in a type
-- error.
module Ouroboros.Consensus.Util.Args (
    Defaults (..)
  , HKD
  , MapHKD (..)
    -- * Types
  , Complete
  , Incomplete
  , noDefault
  ) where

import           Data.Functor.Identity (Identity (..))
import           Data.Kind

data Defaults t = NoDefault
  deriving (Functor)

noDefault :: Defaults t
noDefault = NoDefault

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

type Incomplete (args :: (Type -> Type) -> k) = args Defaults
type Complete   (args :: (Type -> Type) -> k) = args Identity

class MapHKD f where
  mapHKD :: proxy (f b) -> (a -> b) -> HKD f a -> HKD f b

instance MapHKD Identity where
  mapHKD _ = id

instance MapHKD Defaults where
  mapHKD _ _ = const NoDefault
