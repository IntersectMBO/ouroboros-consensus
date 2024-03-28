{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- Execute these lines at the bash prompt.
--
-- > $ bash do-ticky MicroReadTVarIO2
-- > $ bash do-timing MicroReadTVarIO2
--
-- And refer to
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/ticky-ticky> for the
-- semantics.

-- This file is a follow-up to @MicroReadTVarIO.hs@. It addresses the
-- discrepancies between that mircobenchmark---which is /extremely/ focused on
-- the @atomically#@ primpop---and the actual Ouroboros Consensus code change
-- (at
-- <https://github.com/input-output-hk/ouroboros-consensus/commit/ea76c4662743e129bf56d206fd212e2fe45685c9>)
-- that we're trying to assess.
--
-- In particular, it introduces ad-hoc polymorphism in the monad, as well as an
-- @fmap@-ish context within the transaction.

-- My interpretation:
--
-- In constrast to @MicroReadTVarIO.hs@, this microbenchmark does show an
-- reduction in heap allocation. But it's exactly 48 bytes per call. Slightly
-- more time is saved, but it's the same order of magnitude: approximately 40
-- nanoseconds per call (70 nanoseconds unoptimized, 29 nanoseconds optimized).
--
-- One caveat: I disabled garbage collection during the ticky run by passing
-- -A2.5G. On the other head, I did not pass -A2.5G in the batch of 30 runs,
-- since both versions were significantly faster /without/ that flag. (I'm
-- assuming it was OS overhead in fielding such a large allocation all at
-- once?)

module MicroReadTVarIO2 (main) where

import qualified Control.Concurrent.STM as Real (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO)
import           Data.Kind (Type)
import           GHC.Exts (Int(I#), Int#)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

-----

main :: IO ()
main = getArgs >>= \case
    [[c]]
      | 'O' <- c -> run   optimized
      | 'U' <- c -> run unoptimized
    _    -> fail "$1 must be O or U"

-- I chose ten million because it causes the faster one to take more than 99ms
-- on my machine, since the RTS MUT time is not more granular than 1ms. Please
-- increase it if your machine is faster than mine.
--
-- Furthermore, powers of 10 make the absolute durations and the ticky counts
-- more legible than do eg powers of 2.

numRepetitions :: Int
numRepetitions = 10^7

-----

{-# NOINLINE unoptimized #-}
unoptimized :: MonadSTM m => Int# -> TVar m Dummy -> m Dummy
unoptimized = \i# var -> atomically (foo i# <$> readTVar var)

{-# NOINLINE optimized #-}
optimized :: MonadSTM m => Int# -> TVar m Dummy -> m Dummy
optimized = \i# var -> foo i# <$> readTVarIO var

-----

data Dummy = Dummy | Dummy2

foo :: Int# -> Dummy -> Dummy   -- an analog of @mkCurrentBlockContext@
foo 999# d = d
foo _i#  d = d `seq` Dummy2

{-# INLINE run #-}
run :: (Int# -> Real.TVar Dummy -> IO Dummy) -> IO ()
run = \f -> do
    var <- Real.newTVarIO Dummy
    let
        go 0 = return ()
        go i@(I# i#) = do _ <- f i# var; go (i - 1)
    go numRepetitions

-----

class (Monad m, Monad (STM m)) => MonadSTM m where
  type STM  m = (stm :: Type -> Type) | stm -> m
  type TVar m :: Type -> Type
  atomically  :: STM m a -> m a
  readTVar    :: TVar m a -> STM m a
  readTVarIO  :: TVar m a -> m a

instance MonadSTM IO where
  type STM  IO = Real.STM
  type TVar IO = Real.TVar
  atomically   = Real.atomically
  readTVar     = Real.readTVar
  readTVarIO   = Real.readTVarIO
