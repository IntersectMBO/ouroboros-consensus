{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Ouroboros.Consensus.ChainGenerator.Honest
  ( -- * Generating
    ChainSchema (ChainSchema)
  , CheckedHonestRecipe (UnsafeCheckedHonestRecipe, chrScgDensity, chrWin)
  , HonestLbl
  , HonestRecipe (HonestRecipe)
  , NoSuchHonestChainSchema (BadKcp, BadLen)
  , SomeCheckedHonestRecipe (SomeCheckedHonestRecipe)
  , SomeHonestChainSchema (SomeHonestChainSchema)
  , checkHonestRecipe
  , countChainSchema
  , genHonestRecipe
  , uniformTheHonestChain

    -- * Testing
  , HonestChainViolation (BadCount, BadScgWindow, BadLength)
  , ScgLbl
  , ScgViolation (ScgViolation, scgvPopCount, scgvWindow)
  , checkHonestChain
  , prettyChainSchema
  , prettyWindow
  ) where

import Control.Monad (void, when)
import qualified Control.Monad.Except as Exn
import Data.Monoid (Endo (Endo, appEndo))
import Data.Proxy (Proxy (Proxy))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as V
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import Test.Ouroboros.Consensus.ChainGenerator.Params
  ( Asc
  , Delta (Delta)
  , Kcp (Kcp)
  , Len (Len)
  , Scg (Scg)
  , genKSD
  )
import Test.Ouroboros.Consensus.ChainGenerator.Slot
  ( E (ActiveSlotE, SlotE)
  , S
  )
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Extras (sized1)
import Prelude hiding (words)

-----

-- | The argument of checkHonestRecipe'
data HonestRecipe = HonestRecipe !Kcp !Scg !Delta !Len
  deriving (Eq, Read, Show)

-- | The argument of 'uniformTheHonestChain' and the output of 'checkHonestRecipe'
--
-- * @base@ the type-level name of the top-level timeline
-- * @hon@ the type-level name of the honest chain's slot interval
--
-- TODO: Rename to CheckedHonestSchemaSpec
data CheckedHonestRecipe base hon = UnsafeCheckedHonestRecipe
  { chrScgDensity :: !(BV.SomeDensityWindow S.NotInverted)
  -- ^ Desired density
  , chrWin :: !(C.Contains SlotE base hon)
  -- ^ Window in the @base@ containing sequence where the density should be
  -- ensured
  }
  deriving (Eq, Read, Show)

-- TODO: Rename to SomeCheckedHonestSpec
data SomeCheckedHonestRecipe
  = forall base hon.
    SomeCheckedHonestRecipe
      !(Proxy base)
      !(Proxy hon)
      !(CheckedHonestRecipe base hon)

instance Show SomeCheckedHonestRecipe where
  showsPrec p (SomeCheckedHonestRecipe base hon recipe) =
    Some.runShowsPrec p $
      Some.showCtor SomeCheckedHonestRecipe "SomeCheckedHonestRecipe"
        `Some.showArg` base
        `Some.showArg` hon
        `Some.showArg` recipe

instance Read SomeCheckedHonestRecipe where
  readPrec =
    Some.runReadPrec $
      Some.readCtor SomeCheckedHonestRecipe "SomeCheckedHonestRecipe"
        <*> Some.readArg
        <*> Some.readArg
        <*> Some.readArg

data NoSuchHonestChainSchema
  = -- | must have @1 <= 'Kcp' < 'Scg'@
    --
    -- Chosing @Kcp > 0@ allows adversarial schemas to have at least 1 active
    -- slot and still lose density comparisons and races.
    BadKcp
  | -- | 'Len' must be positive
    BadLen
  deriving (Eq, Read, Show)

-- Note [Minimum schema length]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We want schemas to have at least k+1 active slots after the intersection.
-- This would produce sufficiently long chains to test the long range attack and
-- rollbacks.
--
-- The minimum length is calculated to allow k+1 active slots in every schema,
-- and then allowing the intersection to be the genesis block. When the schemas
-- are longer than the minimum, chosing a later intersection is possible.
--
-- We divide the length of the schemas in the following three segments:
--
-- s + 1 | d | k
--
-- To ensure the honest schema has at least k+1 active slots, we need at least
-- the s slots, by the Extended Praos Chain Growth assumption. We draw these
-- slots from the first segment.
--
-- To ensure the alternative schema can have k+1 active slots, we reserve
-- k unstable slots at the end of the schema (this is the last of our segments),
-- and we make sure to activate one more earlier slot.
--
-- To reserve k unstable slots, we need to meet two conditions. One condition is
-- that there needs to be a gap of at lest d slots between the first unstable
-- slot and the k+1st active slot in the honest schema. This is the middle
-- segment in the schema.
--
-- The second condition for unstable slots requires that the first alternative
-- active slot after the intersection is s slots before the first unstable slot.
-- To ensure that we can activate a slot at this point or earlier, we chose k>0,
-- which we already needed to ensure that the alternative schema can have at
-- least one active slot, yet lose density and race comparisons; and we also
-- draw one more slot from the first segment to ensure there is an actual slot
-- to activate when d=0.
--
-- Therefore, to the total length is s + d + k + 1.

genHonestRecipe :: QC.Gen HonestRecipe
genHonestRecipe = sized1 $ \sz -> do
  (Kcp k, Scg s, Delta d) <- genKSD
  -- See Note [Minimum schema length].
  l <- (+ (s + d + k + 1)) <$> QC.choose (0, 5 * sz)
  pure $ HonestRecipe (Kcp k) (Scg s) (Delta d) (Len l)

-- | Checks whether the given 'HonestRecipe' determines a valid input to
-- 'uniformTheHonestChain'
checkHonestRecipe :: HonestRecipe -> Exn.Except NoSuchHonestChainSchema SomeCheckedHonestRecipe
checkHonestRecipe recipe = do
  when (l <= 0) $ Exn.throwError BadLen

  when (k < 1 || s < k) $ Exn.throwError BadKcp

  C.withTopWindow (C.Lbl @HonestLbl) l $ \base topWindow -> do
    C.SomeWindow Proxy slots <- pure topWindow

    pure $
      SomeCheckedHonestRecipe
        base
        Proxy
        UnsafeCheckedHonestRecipe
          { chrScgDensity = BV.SomeDensityWindow (C.Count (k + 1)) (C.Count s)
          , chrWin = slots
          }
 where
  HonestRecipe (Kcp k) (Scg s) (Delta _d) (Len l) = recipe

-----

-- | The leader schedule of an honest /chain/
--
-- The represented chain grows by one block per active slot. A different data
-- type would represent a leader schedule in which leaders do not necessarily
-- extend the block from the previous active slot.
--
-- INVARIANT: at least one active slot
data ChainSchema base inner
  = ChainSchema
      !(C.Contains SlotE base inner)
      !(C.Vector inner SlotE S)
  deriving (Eq, Read, Show)

countChainSchema :: ChainSchema base inner -> C.Size inner ActiveSlotE
countChainSchema sched =
  BV.countActivesInV S.notInverted v
 where
  ChainSchema _slots v = sched

prettyWindow :: C.Contains SlotE base inner -> String -> String
prettyWindow win s =
  -- for example, i=0 n=1 should be "[)"
  replicate i ' ' <> "[" <> replicate (n - theOpenBracket) ' ' <> ")" <> s
 where
  C.Count i = C.windowStart win
  C.Count n = C.windowSize win

  theOpenBracket = 1

prettyChainSchema ::
  forall base inner.
  ChainSchema base inner ->
  String ->
  [String]
prettyChainSchema sched s =
  map (replicate (C.getCount shift) ' ' <>) $
    [ prettyWindow slots s
    , V.foldMap (Endo . S.showS) (C.getVector v) `appEndo` ""
    ]
 where
  ChainSchema slots v = sched

  shift = C.windowStart slots

data SomeHonestChainSchema
  = forall base hon.
    SomeHonestChainSchema
      !(Proxy base)
      !(Proxy hon)
      !(ChainSchema base hon)

instance Show SomeHonestChainSchema where
  showsPrec p (SomeHonestChainSchema base hon sched) =
    Some.runShowsPrec p $
      Some.showCtor SomeHonestChainSchema "SomeHonestChainSchema"
        `Some.showArg` base
        `Some.showArg` hon
        `Some.showArg` sched

instance Read SomeHonestChainSchema where
  readPrec =
    Some.runReadPrec $
      Some.readCtor SomeHonestChainSchema "SomeHonestChainSchema"
        <*> Some.readArg
        <*> Some.readArg
        <*> Some.readArg

data HonestLbl

data RemainingHcgWindowsLbl

{- TODO

Our original intent was for 'uniformTheHonestChain' to generate a chain
according to the given 'Asc' and then toggle /as few as possible/ empty slots
to active such that the Extended Chain Growth Assumption is satisfied.

However, the minimality of that is very difficult to ensure, in general. It's
an integer linear programming (ILP) problem, where:

  * Each empty slot in a sparse window is a binary aka ("zero-one") variable.

  * The objective is to minimize the sum of those binary variables.

  * Each sparse window imposes a minimum bound on the number of variables in
    that window that must be toggled.

Because all of the coefficients in the objective (the minimization) and each
constraint (one per window) are positive, this is a /set covering problem/:
each variable is an empty slot in some number of sparse windows, and we want to
choose as few such slots as possible such that no sparse window remain.

Moreover, each window needs a different number of slots (because some windows
may be more sparse than others). Because some windows may require multiple
empty slots, this is a /set multi-covering problem/.

Lastly, we can only toggle an empty slot once (recall that it's a /zero-one/
variable), so this is a /set multi-cover with multiplicity constraints
problem/.

After some searching, I found this reference

  * Qiang-Sheng Hua, Yuexuan Wang, Dongxiao Yu, Francis C. M. Lau: Dynamic
    programming based algorithms for set multicover and multiset multicover
    problems. Theor. Comput. Sci. 411(26-28): 2467-2474 (2010)

which includes the claim "[...] we give the first known exact algorithm for the
MSMC or the SMC with multiplicity constraints problem".

It /may/ be the case that our problem is particularly degenerate, and so easier
to solve. In particular, our variables are binary, their coefficients in the
constraints are binary, and in fact our constraints form a banded matrix. Only
the lower bound of each constraint can be greater than 1. But I have not yet
recognized how to do it.

The closest I have come to a solution is an incremental algorithm that toggles
one empty slot at a time. I /think/ the completeness of this algorithm requires
that the next toggle is one of the empty slots that occurs in the most number
of sparse windows. However, I have not proved that. Moreover, even if it is
true, a counterexample proves that choosing an arbitrary such slot is not
sufficient:

>     A   B   C
> -----   -----
> 01110111011101110
>     -----   -----

In this example, only slots A, B, and C occur in two sparse windows, so those
are the candidates for the next toggle by my conjecture. If we suppose that
each window is only missing one active slot (eg need 4 actives per 5 slots),
then toggling B would require three toggles in total, whereas toggling A and C
would solve the problem with just two toggles.

-}

-- | A 'ChainSchema' that satisfies 'checkHonestChain'
--
-- This generator proceeds in three stages to create a random schema that
-- satisfies the requirement of at least @k+1@ blocks in every @s@ slots.
--
-- * It begins by drawing a sample of length 'Len' from the Bernoulli process
--   induced by the active slot coefficient 'Asc', just like in Praos. (IE
--   'Len' many i.i.d. samples from @Uniform(asc)@).
-- * It then visits the first window in that sampled vector. If it has less
--   than @k+1@ active slots, the generator randomly toggles empty slots to be
--   active until the window contains exactly @k+1@ active slots.
-- * It then visits the rest of the windows in oldest-to-youngest order. Each
--   window must contain at least @k@ active slots when visited, since it
--   shares all but its youngest slot with the previous window, which was
--   already visited. In particular, when the window contains only @k@ active
--   slots, that youngest slot must be empty, and so the generator will toggle
--   it, thereby re-establishing the required density.
--
-- NOTE: This process may add more active slots to the initial sampled vector
-- than strictly necessary---ensuring the minimum number of toggles would be an
-- integer programming optimization problem as far as we can tell. We have
-- settled for this relatively simple algorithm that approaches the minimal
-- number of toggled slots (TODO calculate how tight the bounds are).
--
-- NOTE: When visting windows after the first, only the youngest slot can be
-- toggled. If we activated any other slot in the sliding window, then older
-- windows, which already have at least @k+1@ active slots, would unnecessarily
-- end up with even more active slots.
--
-- NOTE: The larger 'Asc' is, the more active slots there will be when sampling
-- from the Bernoulli process. When 'Asc' is much larger than @(k+1)/s@ the
-- sample will require toggling very few additional slots.
--
-- NOTE: When no 'Asc' value is provided, we start with a vector with no active
-- slots, and the second phase causes the first window to end up with @k+1@
-- active slots in a pattern that is then repeated exactly for the rest of the
-- chain. For instance,
--
-- > k=2, s=6
-- >
-- > 000000000000000000000000 -- stage 1
-- > 1 11                     -- stage 2
-- >       1 11  1 11  1 11   -- stage 3
-- > 101100101100101100101100 -- final
--
-- > k=2, s=6
-- >
-- > 000000000000000000000000 -- stage 1
-- >    111                   -- stage 2
-- >          111   111   111 -- stage 3
-- > 000111000111000111000111 -- final
--
-- NOTE: When @'Asc'@ is close to 0, the sample will have a few active slots.
-- In the relatively common case where none of those initially active slots are
-- in the same window, the final schema will be periodic between the active
-- slots from the sample, but the patterns in those intervals may vary. For
-- instance,
--
-- > k=2, s=6
-- >
-- > 000000000000010000000000000000001000000000000000000 -- stage 1
-- > 1 1 1                                               -- stage 2
-- >       1 1 1 1   1 11  1 11  1 11    111   111   111 -- stage 3
-- > 101010101010110010110010110010111000111000111000111 -- final
--
-- TODO sample from a disjunction of generators that explore interesting
-- boundary conditions. i.e. different windows could use different strategies
-- to fill each of the windows. For instance, we could arrange for some windows
-- to start with empty slots and be dense at the end.
uniformTheHonestChain ::
  forall base hon g.
  R.RandomGen g =>
  -- | When @Nothing@, the generated schema has a minimal amount
  -- of active slots. Deactivating any of them would violate
  -- safety properties. Such a minimal schema is necessarily
  -- completely periodic.
  Maybe Asc ->
  CheckedHonestRecipe base hon ->
  g ->
  ChainSchema base hon
{-# INLINEABLE uniformTheHonestChain #-}
uniformTheHonestChain mbAsc recipe g0 = wrap $ C.createV $ do
  BV.SomeDensityWindow (C.Count (toEnum -> numerator)) (C.Count (toEnum -> denominator)) <-
    pure chrScgDensity
  let _ = numerator :: C.Var hon ActiveSlotE
      _ = denominator :: C.Var hon SlotE

  g <- R.newSTGenM g0

  -- randomly initialize the bitstring
  mv <- C.replicateMV sz $ case mbAsc of
    Nothing -> pure $ S.mkActive S.inverted
    Just asc -> S.genS asc `R.applySTGen` g

  -- /always/ ensure at least one slot is filled
  void $ BV.fillInWindow S.notInverted (C.Count 1 `BV.SomeDensityWindow` sz) g mv

  -- fill the first window up to @k+1@
  rtot <- do
    -- NB @withWindow@ truncates if it would reach past @slots@
    C.SomeWindow Proxy scg <- pure $ C.withWindow sz (C.Lbl @ScgLbl) (C.Count 0) (C.toSize denominator)
    tot <- C.fromWindowVar scg <$> BV.fillInWindow S.notInverted chrScgDensity g (C.sliceMV scg mv)

    firstSlot <- BV.testMV S.notInverted mv (C.Count 0)
    newSTRef $ (if firstSlot then subtract 1 else id) $ (tot :: C.Var hon ActiveSlotE)

  C.SomeWindow Proxy remainingFullWindows <- do
    -- "number of windows that fit" is usually "total - windowWidth + 1",
    -- but we do not add the one here because the previous init step above
    -- already handled the first window
    let numRemainingFullWindows = sz C.- C.getCount denominator
    pure $ C.withWindow sz (C.Lbl @RemainingHcgWindowsLbl) (C.Count 1) numRemainingFullWindows

  -- visit all subsequent windows that do not reach beyond @slots@
  --
  -- Visiting a window ensures it has at least k+1 active slots; thus the
  -- first window beyond @slots@ will have at least k actives in its actual
  -- slots. We assume slots beyond @slots@ are active; thus the first window
  -- beyond has at least k+1 active slots. And subsequent windows can only have
  -- more active slots than that; thus we don't need to visit windows that
  -- reach beyond @slots@.
  --
  -- LOOP INVARIANT: @rtot@ contains the count active slots in the current window excluding its youngest slot
  --
  -- LOOP INVARIANT: @numerator - 1 <= rtot@
  --
  -- This loop only alters the final slot in each window. That is key to this
  -- whole function being a /uniform/ sampler. In particular:
  --
  --     * Every excessive empty slot in the first window has an equal chance
  --       to be filled in (by the init step above).
  --
  --     * If some subsequent window is sparse, then its final slot is filled
  --       in (by this loop). It must never fill in any older slot in the
  --       window because those slots have already been sampled (either by
  --       the init step above or by previous iterations of this loop).
  --
  --     * Every slot that was not filled in was drawn from @mbAsc@.
  --
  --     * In total: the init step uniformly fills the first window up to
  --       @numerator@, and then each slot not in the first window is either
  --       forced to @1@ by its preceding @denominator - 1@ samples or is
  --       sampled from @mbAsc@.
  C.forRange_ (C.windowSize remainingFullWindows) $ \(C.fromWindow remainingFullWindows -> islot) -> do
    -- NB will not be truncated
    C.SomeWindow Proxy scgSlots <- pure $ C.withWindow sz (C.Lbl @ScgLbl) islot (C.toSize denominator)

    tot <- do
      tot <- readSTRef rtot
      end <- BV.testMV S.notInverted mv (C.windowLast scgSlots)
      pure $ (if end then (+ 1) else id) $ tot

    let sparse = tot == numerator - 1 -- see LOOP INVARIANT
    tot' <-
      if not sparse
        then pure tot
        else do
          BV.setMV S.notInverted mv (C.windowLast scgSlots)
          pure numerator

    start <- BV.testMV S.notInverted mv (C.windowStart scgSlots)
    writeSTRef rtot $! (if start then subtract 1 else id) $ tot'

  pure mv
 where
  UnsafeCheckedHonestRecipe
    { chrScgDensity
    , chrWin = slots
    } = recipe

  sz = C.windowSize slots :: C.Size hon SlotE -- ie 'Len'
  wrap v = ChainSchema slots v

-----

data ScgViolation hon
  = forall skolem.
  ScgViolation
  { scgvPopCount :: !(C.Size (C.Win ScgLbl skolem) ActiveSlotE)
  -- ^ How many active slots 'scgvWindow' has
  , scgvWindow :: !(C.Contains SlotE hon (C.Win ScgLbl skolem))
  -- ^ The ChainGrowth window that doesn't have enough active slots
  }

instance Eq (ScgViolation hon) where
  ScgViolation l1 l2 == ScgViolation r1 r2 =
    C.forgetBase l1 == C.forgetBase r1
      && C.forgetWindow l2 == C.forgetWindow r2

instance Show (ScgViolation hon) where
  showsPrec p (ScgViolation x y) =
    Some.runShowsPrec p $
      Some.showCtor ScgViolation "ScgViolation"
        `Some.showArg` x
        `Some.showArg` y

instance Read (ScgViolation hon) where
  readPrec =
    Some.runReadPrec $
      Some.readCtor ScgViolation "ScgViolation"
        <*> Some.readArg
        <*> Some.readArg

data HonestChainViolation hon
  = -- | The schema does not contain a positive number of active slots
    BadCount
  | -- | The schema has some window of 'Scg' slots that contains less than
    -- 'Kcp+1' active slots, even despite optimistically assuming that all slots
    -- beyond 'Len' are active
    BadScgWindow !(ScgViolation hon)
  | -- | The schema does not span exactly 'Len' slots
    BadLength !(C.Size hon SlotE)
  deriving (Eq, Read, Show)

-- | A stability window
data ScgLbl

-- | Check the Extended Praos Chain Growth assumption
--
-- Definition of /window/ and /anchored window/. A window is a contiguous
-- sequence of slots. A window anchored at a block starts with the slot
-- immediately after that block.
--
-- Definition of /Extended Praos Chain Growth Assumption/. We assume the honest chain
-- contains at least @k+1@ blocks in every window that contains @s@ slots.
--
-- Definition of /Stability Window/. The @s@ parameter from the Praos Chain
-- Growth property. (At the time of writing, this is @2k@ during Byron and
-- @3k/f@ after Byron on Cardano @mainnet@.)
checkHonestChain ::
  forall base hon.
  HonestRecipe ->
  ChainSchema base hon ->
  Exn.Except (HonestChainViolation hon) ()
checkHonestChain recipe sched = do
  when (C.getCount sz /= l) $ Exn.throwError $ BadLength sz

  do
    let pc = countChainSchema sched
    when (C.toVar pc <= 0) $ Exn.throwError BadCount

  -- every slot is the first slot of a unique stability window
  C.forRange_ sz $ \i -> do
    -- note that withWindow truncates if the requested slots reach past 'Len'
    C.SomeWindow Proxy scg <- pure $ C.withWindow sz (C.Lbl @ScgLbl) i (C.Count s)

    let pc = BV.countActivesInV S.notInverted (C.sliceV scg v)

    -- generously assume that the slots of this stability window that extend past 'Len' are active
    let benefitOfTheDoubt = s - C.getCount (C.windowSize scg)

    -- check the density in the stability window
    when (C.getCount pc + benefitOfTheDoubt < k + 1) $ do
      Exn.throwError $
        BadScgWindow $
          ScgViolation
            { scgvPopCount = pc
            , scgvWindow = scg
            }
 where
  HonestRecipe (Kcp k) (Scg s) (Delta _d) (Len l) = recipe

  ChainSchema hon v = sched

  sz = C.windowSize hon
