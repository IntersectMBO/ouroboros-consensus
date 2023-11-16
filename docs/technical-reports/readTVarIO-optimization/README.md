# Introduction

The goal of the microbenchmarks in this directory is to demystify the consequences of replacing an `atomically . readTVar` with `readTVarIO`.
In particular, a benchmarking run reported in [this #perf-announce message](https://input-output-rnd.slack.com/archives/C4Q7MF25U/p1698842470166369) on the IOG Slack shows that the system-level benchmarks indicate that [making that change in the `forkBlockForging` function](https://github.com/input-output-hk/ouroboros-consensus/commit/ea76c4662743e129bf56d206fd212e2fe45685c9) yields an improvement on the order of 10% CPU usage and 10% allocation compare to the baseline of `8.5.0-pre` (circa 2023 Nov 1).
Those improvements are far greater than the Consensus Team was expecting, so we've begun investigating.
These microbenchmarks are a first step.

# Simple Case

In the simplest possible case, the `atomically . readTVar` variant costs about 20 nanoseconds more to call (21.7 nanoseconds unoptimized, 1.37 nanoseconds optimized) than does the equivalent `readTVarIO`.
It does not incur any extra allocation---which is unexpected.
Duncan Coutts looked at the RTS code and remarked that the transaction log data structure has its own free-list, so I presume its allocator is working perfectly for these microbenchmarks, thus avoiding any extra heap allocation.
I don't know whether that is happening in the real code or not (which does involve at least two STM transactions in sequence, in the typical case).

# More Realistic Case

The real code does not match that simplest case, for at least two reasons.

- It's part of a top-level declaration that will neither be specialized nor inlined into a context in which the monad is monomorphic.
  Hence its run-time behavior involves dictionary-passing.

- The transaction of interset is actually of the shape `atomically (foo i <$> readTVar var)`, where the `i` cannot be floated out of the loop.
  This means the closure allocated as the argument to `atomically` must be allocated fresh each time.
  The optimized `foo i <$> readTVarIO var` avoids at least that closure.

The second benchmark file is intended to replicate those two aspects of the real code.
In contrast to the simplest case, this more realistic microbenchmark does show a reduction in heap allocation.
It's exactly 48 bytes per call (as determined via [ticky ticky profiling](https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/ticky-ticky)).
Slightly more time is saved per call, but it's the same order of magnitude: approximately 40 nanoseconds per call (70 nanoseconds unoptimized, 29 nanoseconds optimized).

# Conclusion

For a system-level benchmark that runs for a total of 54000 seconds, these microbenchmarks indicate that this particular manual optimization in a function that is called at most once per second cannot be directly responsible for a CPU usage/allocation savings on the order of 10%.
In particular, _full_ blocks are allocated/parsed/propagated/etc fresh an average of once per 20 seconds.
If the 48 bytes per second accounts for 10% of allocation, then blocks would have to be less than 20 * .90 * (48 / .10) = 8640 bytes.
Even just their serialized bytes are are much greater than that, not to even consider their parsed heap footprint, etc.

The system-level benchmarks are generally quite reproducible, as evidenced by occasional variance analaysis runs (repeats of the same run showing final stats within 1 millisecond, eg).
So it seems likely that there is in fact a savings on the order of 10%.
But our working hypothesis is that that savings is only indirectly cause by this particular manual optimization.

Unfortunately, so far, we haven't come up with any possible explanations more specific than the usual suspects: the GHC garbage collector is complicated, work-stealing is complicated, instruction caches are powerful, and so on.
