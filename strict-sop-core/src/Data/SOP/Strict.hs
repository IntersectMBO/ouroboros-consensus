{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Strict variant of SOP
module Data.SOP.Strict (
    module Data.SOP.Strict.NP
  , module Data.SOP.Strict.NS
    -- * fn
  , fn_5
    -- * Reexports
  , module Data.SOP.Classes
  ) where

import           Data.SOP.Classes
import           Data.SOP.Strict.NP
import           Data.SOP.Strict.NS

{-------------------------------------------------------------------------------
  fn
-------------------------------------------------------------------------------}

fn_5 :: (f0 a -> f1 a -> f2 a -> f3 a -> f4 a -> f5 a)
     -> (f0 -.-> f1 -.-> f2 -.-> f3 -.-> f4 -.-> f5) a
fn_5 f = Fn $ \x0 ->
         Fn $ \x1 ->
         Fn $ \x2 ->
         Fn $ \x3 ->
         Fn $ \x4 ->
         f x0 x1 x2 x3 x4
