{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A simple permutation type catered to uses of
-- 'Test.CsjModel.StateTypes.queue' in "Test.CsjModel"
module Test.CsjModel.Perm (module Test.CsjModel.Perm) where

import qualified Data.Maybe as L (Maybe (Just, Nothing))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

-- | A permutation
newtype Perm pid = UnsafePerm (Seq pid)
  deriving stock (Generic, Read, Show)
  deriving anyclass (NoThunks)
  deriving newtype (Foldable)

-- | PREREQUISITE: the @pid@ is already in the permutation
indexPerm :: (HasCallStack, Eq pid, Show pid) => pid -> Perm pid -> Int
indexPerm pid (UnsafePerm pids) = case Seq.elemIndexL pid pids of
    L.Nothing -> error $ "impossible! " ++ show (pid, pids)
    L.Just i  -> i

-- | PREREQUISITE: the @pid@ is already in the permutation
deletePerm :: (HasCallStack, Eq pid, Show pid) => pid -> Perm pid -> Perm pid
deletePerm pid perm@(UnsafePerm pids) =
    UnsafePerm $ Seq.deleteAt (indexPerm pid perm) pids

-- | Insert the @pid@ at the end of the permutation
--
-- PREREQUISITE: the @pid@ is not already in the permutation.
snocPerm :: pid -> Perm pid -> Perm pid
snocPerm pid (UnsafePerm pids) = UnsafePerm $ pids Seq.|> pid
