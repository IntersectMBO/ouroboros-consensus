{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A simple permutation type catered to uses of 'queue' in "Test.CsjModel"
module Test.CsjModel.Perm (
    module Test.CsjModel.Perm
  ) where

import qualified Data.Maybe as L (Maybe (Just, Nothing))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | A permutation
newtype Perm pid = UnsafePerm (Seq pid)
  deriving (Foldable, Read, Show)

-- | PREREQUISITE: the @pid@ is already in the permutation
indexPerm :: Eq pid => pid -> Perm pid -> Int
indexPerm pid (UnsafePerm pids) = case Seq.elemIndexL pid pids of
    L.Nothing -> error "impossible!"
    L.Just i  -> i

deletePerm :: Eq pid => pid -> Perm pid -> Perm pid
deletePerm pid perm@(UnsafePerm pids) =
    UnsafePerm $ Seq.deleteAt (indexPerm pid perm) pids

-- | Insert the @pid@ at the end of the permutation
--
-- PREREQUISITE: the @pid@ is not already in the permutation.
snocPerm :: pid -> Perm pid -> Perm pid
snocPerm pid (UnsafePerm pids) = UnsafePerm $ pids Seq.|> pid
