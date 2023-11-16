{-# LANGUAGE LambdaCase #-}

-- Execute these lines at the bash prompt.
--
-- > $ bash do-ticky MicroReadTVarIO
-- > $ bash do-timing MicroReadTVarIO
--
-- And refer to
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/ticky-ticky> for the
-- semantics.

-- My interpretation:
--
-- o The only meaningful difference is the MUT time.
--
-- o The ticky results indicates that's the result of eliminating one call to a
--   dynamic target (aka ENT_DYN_FUN_DIRECT) and two stack checks (aka STK_CHK)
--   per iteration.
--
-- o I /assume/ those are essentially unavoidable when using the @atomically#@
--   primop.
--
-- o In the batch of 30 reps, we interleaved them so that cache warmness is
--   less likely to matter.
--
-- o My averages at 10^2 were both less than 0, so the binary execution
--   overhead is less than a millisecond. My averages at 10^8 were
--   0.13653333333333337 seconds optimized and 2.1664333333333334 seconds
--   unoptimized. So---if binary execution overhead is were actually 0---that'd
--   imply 1.37 nanoseconds per call optimized and 21.7 nanoseconds per call
--   unoptimized. That's a difference of merely 20 extra nanoseconds per
--   call---and no allocation difference whatsoever!
--
-- This microbenchmark assesses removing an occurrence of @atomically@.
-- HOWEVER, this microbenchmark compiles witwh the concrete 'IO' monad
-- explicitly manifest, and so it's measuring exactly the @atomically#@ primop
-- itself. In contrast, the actual Ouroboros Consensus code (at
-- <https://github.com/input-output-hk/ouroboros-consensus/commit/ea76c4662743e129bf56d206fd212e2fe45685c9>),
-- differs in two ways.
--
-- o It involves a layer of ad-hoc polymorphism. Thus removing the @atomically@
--   call in that that context also eliminates a class method call
--   (@atomically@ there is a method of the @io-sim-classes:MonadSTM@ class).
--
--   I think that approximately doubles the benefit, since I think a
--   unspecialized class method call is an additional ENT_DYN_FUN_DIRECT. (The
--   same @atomically@ is being used elsewhere in the real code's scope, so the
--   optimization does not additionally eliminate the dictionary passing nor
--   the method lookup.)
--
-- o The real code also has the shape @atomically (foo i <$> readTVar var)@.
--   The @var@ could float out to be shared by each call to @atomically@, but
--   the @i@ cannot. Moreover, because of the ad-hoc polymorphism, GHC cannot
--   float the entire @(foo i <$> _)@ wrapper outside of the @atomically@, even
--   though that'd be sound for the concrete 'IO'/'STM' monad (TODO I'm
--   /assuming/ GHC does it in that case, but I have not checked).
--
--   This microbenchmark shared the argument of 'atomically' among all calls to
--   it, but the real code cannot. Thus, I think the optimization also saves
--   one heap allocation per call in the real code. HOWEVER, there are still
--   some heap allocations per call after the optimization in the real code, so
--   perhaps eg the heap check count wouldn't change.
--
-- Thus, in summary, this microbenchmark is essentially assessing exactly the
-- cost of the @atomically#@ primop. I do think the diff has benefits beyond
-- elminated that primop call in the real code. However, eliminating that
-- primop call was the most mysterious to me, so this microbenchmark at least
-- helps approximates its benefit: "hundreds of nanoseconds, no inherent change
-- to heap allocation".

module MicroReadTVarIO (main) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-----

-- We choose at run-time in order to avoid having two different binary files.
-- Same reason for using argument strings of the same length.

main :: IO ()
main = getArgs >>= \case
    [[c]]
      | 'O' <- c ->   optimized
      | 'U' <- c -> unoptimized
    _    -> fail "$1 must be O or U"

-- We do not parse 'numRepetitions' at run-time, because different input
-- strings lead to variations in the +RTS -s bytes allocated metric, which is
-- confusing/distracting.

-- I chose one hundred million because it causes the faster one to take more
-- than 99ms on my machine, since the RTS MUT time is not more granular than
-- 1ms. Please increase it if your machine is faster than mine.
--
-- Furthermore, powers of 10 make the absolute durations and the ticky counts
-- more legible than do eg powers of 2.

numRepetitions :: Int
numRepetitions = 10^8

-----

-- We use NOINLINE to prevent duplication in the resulting GHC Core.

{-# NOINLINE unoptimized #-}
unoptimized :: IO ()
unoptimized = run (atomically . readTVar)

{-# NOINLINE optimized #-}
optimized :: IO ()
optimized = run readTVarIO

-----

data Dummy = Dummy

-- We did a Static-Argument Transform so that 'run' can be inlined.

-- We use explicit lambdas so that it will definitely inline, since the GHC's
-- inlining behavior directly depends on the number of arguments to the left of
-- the equals sign.

-- GHC's defaults do already inline 'run', but moreover: it is our explicit
-- intention to do so.

-- We are strict in f to eliminate noise in the resulting GHC Core. Same for
-- -dsuppress-all.

{-# INLINE run #-}
run :: (TVar Dummy -> IO Dummy) -> IO ()
run = \f -> f `seq` do
    var <- newTVarIO Dummy
    let
        -- merely repeat the call to f n times
        go 0 = pure ()
        go i = do _ <- f var; go (i - 1)
    go numRepetitions
