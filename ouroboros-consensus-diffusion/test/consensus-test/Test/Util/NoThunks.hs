{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.NoThunks () where

import qualified Data.Strict.Either as Strict
import qualified Data.Strict.Maybe as Strict
import           NoThunks.Class (NoThunks)

instance NoThunks x => NoThunks (Strict.Maybe x)

instance (NoThunks x, NoThunks y) => NoThunks (Strict.Either x y)
