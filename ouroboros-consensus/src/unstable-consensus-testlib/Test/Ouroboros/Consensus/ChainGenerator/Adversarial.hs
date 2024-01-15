{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- TODO rename to .Alternative?
module Test.Ouroboros.Consensus.ChainGenerator.Adversarial (
    -- * Generating
    AdversarialRecipe (AdversarialRecipe, arHonest, arParams, arPrefix)
  , CheckedAdversarialRecipe (UnsafeCheckedAdversarialRecipe, carHonest, carParams, carWin)
  , NoSuchAdversarialChainSchema (NoSuchAdversarialBlock, NoSuchCompetitor, NoSuchIntersection, KcpIs1)
  , SomeCheckedAdversarialRecipe (SomeCheckedAdversarialRecipe)
  , checkAdversarialRecipe
  , uniformAdversarialChain
    -- * Testing
  , AdversarialViolation (..)
  , AnchorViolation (HonestActiveMustAnchorAdversarial)
  , ChainSchema (ChainSchema)
  , RaceViolation (AdversaryWonRace, rvAdv, rvHon)
  , checkAdversarialChain
  , genPrefixBlockCount
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (void, when)
import qualified Control.Monad.Except as Exn
import           Control.Monad.ST (ST)
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Vector.Unboxed as Vector
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (ChainSchema (ChainSchema))
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc,
                     Delta (Delta), Kcp (Kcp), Scg (Scg))
import qualified Test.Ouroboros.Consensus.ChainGenerator.RaceIterator as RI
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot
                     (E (ActiveSlotE, EmptySlotE, SlotE))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some

-----

data AnchorViolation =
    -- | An honest active slot must immediately precede the adversarial interval
    HonestActiveMustAnchorAdversarial
  |
    -- | The were not exactly 'arPrefix' many active slots preceding @adv@
    WrongNumberOfHonestPredecessors
  deriving (Eq, Read, Show)

-- | A violation of Races Before Acceleration Assumption
--
-- INVARIANT: @'C.windowLast' 'rvAdv' < 'C.windowLast' 'rvHon' + 'Delta'@
--
-- INVARIANT: @'C.windowStart' 'rvHon' <= 'C.windowStart' 'rvAdv'@
data RaceViolation hon adv = AdversaryWonRace {
    -- | The adversarial race window
    rvAdv :: !(RI.Race adv)
  ,
    -- | The honest race window
    rvHon :: !(RI.Race hon)
  }
  deriving (Eq, Read, Show)

data AdversarialViolation hon adv =
    BadAnchor !AnchorViolation
  |
    -- | The schema does not contain a positive number of active slots
    BadCount
  |
    BadRace !(RaceViolation hon adv)
  |
    -- | The density of the adversarial schema is not less than the density of
    -- the honest schema in the first stability window after the intersection.
    --
    -- In @BadDensity h a@, @h@ is the amount of active slots in the first
    -- stability window after the intersection in the honest schema and @a@ is
    -- the amount of active slots in the first stability window after the
    -- intersection in the adversarial schema.
    BadDensity Int Int
  deriving (Eq, Read, Show)

-- | Check the chain matches the given 'AdversarialRecipe'.
--
-- * It must intersect the honest chain at an active slot
-- * It must satisfy the Races Before Acceleration Assumption
--   (which implies the Length of Competing Chains Assumption)
--
-- Definition of a /Praos Race Window/ of a chain. It is an interval of slots
-- that contains at least @k+1@ blocks of the chain and exactly 'Delta' slots
-- after the @k+1@st block.
--
-- Definition of the /Length of Competing Chains Assumption/. We assume every adversarial
-- chain contains at most @k@ blocks in the Praos Race Window anchored at the
-- the intersection.
--
-- Definition of /Acceleration Lower Bound of an Adversarial Chain/. It is
-- the lowest slot at which an adversary could speed up its elections on a
-- given adversarial chain. It is defined as the 'Scg'-th slot after the first
-- adversarial block or the 'Delta'-th slot after the @k+1@st honest block after the
-- intersection, whichever is greater.
--
-- Definition of the /Races Before Acceleration Assumption/. Every adversarial
-- chain has at most @k@ blocks in every Praos Race Window of the honest chain
-- that starts after the intersection and ends before the acceleration lower
-- bound.
--
-- Definition of the /Genesis Implication Conjecture/. We conjecture that
-- assuming that the Genesis window length is no greater than the Stability
-- Window and that every possible adversarial chain satisfies the Races
-- in Stability Windows Assumption, then the honest chain strictly wins every possible density
-- comparison from the Ouroboros Genesis paper. The key intuition is that a less
-- dense chain would have to lose at least one Praos race within the Genesis window.
checkAdversarialChain ::
  forall base hon adv.
     AdversarialRecipe base hon
  -> ChainSchema base adv
  -> Exn.Except (AdversarialViolation hon adv) ()
checkAdversarialChain recipe adv = do
    checkStart
    checkCount
    checkRaces
    checkDensity
  where
    AdversarialRecipe {
        arHonest = ChainSchema winH vH
      ,
        arParams = (Kcp k, Scg s, Delta d)
      ,
        arPrefix
      } = recipe

    ChainSchema winA vA = adv

    checkStart = do
        let startA       = C.windowStart winA :: C.Index base SlotE
            intersection = startA C.- 1       :: C.Index base SlotE

        -- The intersection must be at an active slot in the honest chain.
        case C.toWindow winH intersection of
            Nothing -> do
              -- the genesis block is the only permissible anchor outside of @hon@
              when (startA   /= C.Count 0) $ Exn.throwError $ BadAnchor HonestActiveMustAnchorAdversarial
              when (arPrefix /= C.Count 0) $ Exn.throwError $ BadAnchor WrongNumberOfHonestPredecessors

            Just i -> do
                let _ = i :: C.Index hon SlotE

                when (BV.testV S.inverted vH i) $ do
                    Exn.throwError $ BadAnchor HonestActiveMustAnchorAdversarial

                C.SomeWindow Proxy precedingSlots <-
                    pure $ C.withWindowBetween (C.windowSize winH) (C.Lbl @"foo") (C.Count 0) i
                let pc = BV.countActivesInV S.notInverted (C.sliceV precedingSlots vH)

                -- arPrefix must correctly count the active slots in the part of
                -- the chain upto the intersection
                when (C.fromWindowVar precedingSlots (C.toVar pc) /= arPrefix) $ do
                    Exn.throwError $ BadAnchor WrongNumberOfHonestPredecessors

    checkCount = do
        let pc = BV.countActivesInV S.notInverted vA
        when (C.toVar pc <= 0) $ Exn.throwError BadCount

    -- the youngest slot in which the adversarial schedule cannot have accelerated
    --
    -- (IE @s@ past the first active adversarial slot, or @d@ past the @k+1@st
    --  slot after the intersection)
    youngestStableA :: C.Index base SlotE
    youngestStableA =
        let sYoungest = case BV.findIthEmptyInV S.inverted vA (C.Count 0) of
              BV.JustFound firstActiveA ->
                -- if s=0, then the slot of their block is the youngest stable slot
                C.fromWindow winA $ firstActiveA C.+ s
              BV.NothingFound           ->
                -- the rest of the function won't force this since there are no
                -- adversarial active slots
                error "dead code"
            kPlus1stYoungest = case BV.findIthEmptyInV S.inverted vH (C.toIndex arPrefix C.+ k) of
              BV.JustFound kPlus1st -> C.fromWindow winH kPlus1st
              BV.NothingFound       ->
                -- If the honest fork didn't reach @k+1@ before it ended, then
                -- the conservative assumption that all slots after 'Len' are
                -- active on the honest chain implies the @k+1@st block is in
                -- slot @C.windowLast winH + (k+1-x)@, where the honest chain
                -- has @x@ remaining blocks such that @x<k+1@.
                let honestBlocksAfterIntersection =
                      C.getCount (BV.countActivesInV S.notInverted vH) - C.getCount arPrefix
                in
                    C.windowLast winH
                  C.+
                    (k + 1 - honestBlocksAfterIntersection)
         in
            max sYoungest (kPlus1stYoungest C.+ d)

    checkRaces = do
        let iterH =
                maybe (RI.initConservative vH) id
              $ RI.init (Kcp k) vH
        case RI.init (Kcp (k - 1)) vA >>= RI.next vA of
            Nothing    -> pure ()   -- there are <= k total adversarial active slots
            Just iterA ->
                -- TODO optimization: how to skip all the honest Race Windows that
                -- don't reach beyond the intersection? Perhaps skip to i - s + d?
                go iterH iterA

    -- INVARIANT iterH spans k+1 active slots (possibly conservatively)
    --
    -- INVARIANT iterA spans k active slots (actually, not conservatively)
    go !iterH !iterA = do
        C.SomeWindow Proxy raceWinH <- pure $ let RI.Race x = iterH in x
        C.SomeWindow Proxy raceWinA <- pure $ let RI.Race x = iterA in x

        -- lift both windows to @base@ so that they're comparable
        let raceWinH' = C.joinWin winH raceWinH
            raceWinA' = C.joinWin winA raceWinA

        if

          -- any Race Window that ends /after/ the adversary can accelerate is unconstrained
          | youngestStableA < C.windowLast raceWinH' C.+ d -> pure ()

          -- advance the adversarial Race Window if its start is <= the honest Race Window's start
          | C.windowStart raceWinA' <= C.windowStart raceWinH' ->
            case RI.next vA iterA of
                Just iterA' -> go iterH iterA'
                Nothing     -> pure ()   -- there are < k remaining adversarial active slots

          -- fail if the adversary won or tied the race
          | C.windowLast raceWinA' <= C.windowLast raceWinH' C.+ d ->
                -- iterA contains exactly k active slots, but A) it's anchored
                -- in an active slot and B) iterH contains that anchor. Thus
                -- adv has k+1 in iterH.
                Exn.throwError $ BadRace AdversaryWonRace {
                    rvAdv = iterA
                  ,
                    rvHon = iterH
                  }

          -- advance the honest Race Window
          | otherwise -> case RI.next vH iterH <|> RI.nextConservative vH iterH of
                Just iterH' -> go iterH' iterA
                Nothing     -> pure ()   -- there are no remaining honest active slots
                                         --
                                         -- TODO hpc shows this never executes

    -- | Check that the density of the adversarial schema is less than the
    -- density of the honest schema in the first stability window after the
    -- intersection and in any prefix that contains the first race to the
    -- k+1st block.
    --
    -- See description of @ensureLowerDensityInWindows@
    checkDensity = do
        let
          -- window of the honest schema after the intersection
          winHAfterIntersection =
              C.UnsafeContains
                  (fromJust $ C.toWindow winH $ C.windowStart winA)
                  (C.Count $ C.getCount (C.windowSize winA))
          -- honest schema after the intersection
          vHAfterIntersection = C.sliceV winHAfterIntersection vH
          iterH :: RI.Race adv
          iterH =
              maybe (RI.initConservative vHAfterIntersection) id
            $ RI.init (Kcp k) vHAfterIntersection

        -- first race window after the intersection
        C.SomeWindow _ w0 <- let RI.Race x = iterH in pure x

        let
          w0' = C.UnsafeContains (C.windowStart w0) (min (C.Count s) (C.windowSize w0))
          vvH = C.getVector vHAfterIntersection
          vvA = C.getVector vA
          -- cumulative sums of active slots per slot after the intersection
          hSum = Vector.scanl (+) 0 (Vector.map (\x -> if S.test S.notInverted x then 1 else 0) vvH)
          aSum = Vector.scanl (+) 0 (Vector.map (\x -> if S.test S.notInverted x then 1 else 0) vvA)
          -- cumulative sums of active slots per slot until the first stability
          -- window after the intersection
          hwSum = Vector.toList $ Vector.drop (C.getCount $ C.windowSize w0') $ Vector.take (s + 1) hSum
          awSum = Vector.toList $ Vector.drop (C.getCount $ C.windowSize w0') $ Vector.take (s + 1) aSum
        case filter (\(x, y) -> x < y) (zip hwSum awSum) of
          []         -> pure ()
          ((x, y):_) -> Exn.throwError $ BadDensity x y

-----

-- | Named arguments for 'checkAdversarialRecipe'
data AdversarialRecipe base hon =
    AdversarialRecipe {
        -- | The honest chain to branch off of
        arHonest :: !(ChainSchema base hon)
      ,
        -- | protocol parameters
        arParams :: (Kcp, Scg, Delta)
      ,
        -- | Where to branch off of 'arHonest'
        --
        -- It is the amount of blocks shared by the honest and the adversarial
        -- chain. In other words, the 0-based index of their intersection
        -- in blocks, such that
        --
        -- * @0@ identifies the genesis block
        -- * @1@ identifies the first block in arHonest
        -- * @2@ identifies the second block in arHonest
        -- * etc
        arPrefix :: !(C.Var hon ActiveSlotE)
      }
  deriving (Eq, Read, Show)

-- | See 'CheckedAdversarialRecipe'
data SomeCheckedAdversarialRecipe base hon =
    forall adv.
    SomeCheckedAdversarialRecipe
        !(Proxy adv)
        !(CheckedAdversarialRecipe base hon adv)

instance Show (SomeCheckedAdversarialRecipe base hon) where
    showsPrec p (SomeCheckedAdversarialRecipe adv car) =
        Some.runShowsPrec p
      $ Some.showCtor SomeCheckedAdversarialRecipe "SomeCheckedAdversarialRecipe"
            `Some.showArg` adv
            `Some.showArg` car

instance Read (SomeCheckedAdversarialRecipe base hon) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeCheckedAdversarialRecipe "SomeCheckedAdversarialRecipe"
            <*> Some.readArg
            <*> Some.readArg

-- | Image of 'checkAdversarialRecipe' when it accepts the recipe
data CheckedAdversarialRecipe base hon adv =
    UnsafeCheckedAdversarialRecipe {
        -- | The honest chain to branch off of
        carHonest :: !(ChainSchema base hon)
      ,
        -- | protocol parameters
        carParams :: (Kcp, Scg, Delta)
      ,
        -- | The window starting at the first slot after the intersection
        -- and ending at the last slot of 'carHonest'.
        --
        -- INVARIANT: there is at least one active honest slot in @adv@
        --
        -- In other words, the adversarial leader schedule does not /extend/ the
        -- chain represented by 'carHonest', it competes with it.
        carWin    :: !(C.Contains SlotE hon adv)
      }
  deriving (Eq, Read, Show)

-- | Image of 'checkAdversarialRecipe' when it rejects the recipe
data NoSuchAdversarialChainSchema =
    -- | There is no slot the adversary can lead
    --
    -- Two possible reasons, where @X@ is the slot of 'arPrefix' and Y is the youngest slot of 'arHonest'.
    --
    --    * The interval @[X, Y)@ is empty.
    --
    --    * @k=0@
    --
    -- Suppose k=0 and slot x and slot y are two honest active slots with only
    -- honest empty slots between them.
    --
    -- Length of Competing Chains prohibits the adversary from leading in the interval (x,y].
    --
    -- In fact, by induction, the adversary can never lead: suppose the same y
    -- and a slot z are two honest active slots with only honest empty slots
    -- between them...
    NoSuchAdversarialBlock
  |
    -- | @not ('arPrefix' < C)@ where @C@ is the number of active slots in 'arHonest'
    NoSuchCompetitor
  |
    -- | @not (0 <= 'arPrefix' <= C)@ where @C@ is the number of active slots in 'arHonest'
    NoSuchIntersection
  |
    -- | @k=1@
    --
    -- This may technically be viable, but our current specification for
    -- 'uniformAdversarialChain' requires @k>1@.
    KcpIs1
  deriving (Eq, Show)

-----

-- | The suffix of the slots starting strictly /after/ the intersection
data AdvLbl

-- | The interval of slots that have most recently attained their final value
data SettledLbl

-- | Reject a bad 'AdversarialRecipe'
checkAdversarialRecipe ::
  forall base hon.
     AdversarialRecipe base hon
  -> Exn.Except
         NoSuchAdversarialChainSchema
         (SomeCheckedAdversarialRecipe base hon)
checkAdversarialRecipe recipe = do
    when (0 == k) $ Exn.throwError NoSuchAdversarialBlock

    when (1 == k) $ Exn.throwError KcpIs1

    -- validate 'arPrefix'
    firstAdvSlot <- case compare arPrefix 0 of
        LT -> Exn.throwError NoSuchIntersection
        EQ -> pure $ C.Count 0
        GT -> case BV.findIthEmptyInV S.inverted vH $ C.toIndex $ arPrefix - 1 of
            BV.NothingFound -> Exn.throwError NoSuchIntersection
            BV.JustFound x  -> do
                when (x == C.lastIndex (C.windowSize winH)) $ Exn.throwError NoSuchAdversarialBlock
                pure (x C.+ 1)

    C.SomeWindow Proxy winA <- pure $ C.withSuffixWindow (C.windowSize winH) (C.Lbl @AdvLbl) firstAdvSlot

    -- there must be at least one honest active slot in @adv@
    case BV.findIthEmptyInV S.inverted (C.sliceV winA vH) (C.Count 0) of
        BV.NothingFound -> Exn.throwError NoSuchCompetitor
        BV.JustFound{}  -> pure ()

    pure $ SomeCheckedAdversarialRecipe Proxy $ UnsafeCheckedAdversarialRecipe {
        carHonest = arHonest
      ,
        carParams = arParams
      ,
        carWin    = winA
      }
  where
    AdversarialRecipe {
        arHonest
      ,
        arParams
      ,
        arPrefix
      } = recipe

    (Kcp k, _scg, _delta) = arParams

    ChainSchema winH vH = arHonest

-----

data RaceAssumptionLbl
data UntouchableLbl
data TouchableLbl

-- | Generate an adversarial 'ChainSchema' that satifies 'checkExtendedRaceAssumption'
--
-- The distribution this function samples from is not simple to describe. It
-- begins by drawing a sample of length 'adv' from the Bernoulli process
-- induced by the active slot coefficient 'Asc'. (IE 'adv' many i.i.d. samples
-- from @Uniform(f)@). Then it visits the Extended Praos Race Windows that
-- overlap with the prefix of that leader schedule that ends one stability
-- window after the first (remaining) active adversarial slot in it. In each
-- such Window, it removes the youngest active slots from the adversarial
-- leader schedule until it loses that Race.
--
-- Finally, it ensures that the density of the alternative schema is
-- less than the density of the honest schema in the first stability window
-- after the intersection.
uniformAdversarialChain ::
  forall g base hon adv.
     R.RandomGen g
  => Maybe Asc   -- ^ 'Nothing' means @1@
  -> CheckedAdversarialRecipe base hon adv
  -> g
  -> ChainSchema base adv
{-# INLINABLE uniformAdversarialChain #-}
uniformAdversarialChain mbAsc recipe g0 = wrap $ C.createV $ do
    g <- R.newSTGenM g0

    let sz = C.windowSize carWin :: C.Size adv SlotE

    -- randomly initialize the bitstring
    mv <- C.replicateMV sz $ case mbAsc of
        Nothing  -> pure $ S.mkActive S.notInverted
        Just asc -> S.genS asc `R.applySTGen` g

    -- ensure the adversarial leader schedule is not empty
    do  void $ BV.fillInWindow
            S.notInverted
            (BV.SomeDensityWindow (C.Count 1) (C.windowSize carWin))
            g
            mv

    -- find the slot of the k+1 honest block
    let kPlus1st :: C.Index adv SlotE
        kPlus1st = case BV.findIthEmptyInV S.inverted (C.sliceV carWin vH) (C.Count k) of
          BV.NothingFound -> maybe (error "dead code") id $ C.toWindow carWin $ C.windowLast carWin
          BV.JustFound x  -> x


    -- ensure the adversarial leader schedule does not win any of the races for
    -- which the honest Race Window fits within the Stability Window anchored at
    -- the first adversarial active slot
    --
    -- TODO Why is it ok to skip early honest races, some of which overlap with
    -- @adv@? Is it because not having >k in some prefix [0, n] of adv ensures
    -- you can't have >k in the interval [0, C.frWindow adv n] either?
    let iterH :: RI.Race adv
        iterH =
            maybe (RI.initConservative vA) id
          $ RI.init kcp vA

    ensureLowerDensityInWindows iterH g mv

    unfillRaces kPlus1st (C.Count 0) UnknownYS iterH g mv

    pure mv
  where
    UnsafeCheckedAdversarialRecipe {
        carHonest
      ,
        carParams = (kcp, scg, delta)
      ,
        carWin
      } = recipe

    wrap v = ChainSchema (C.joinWin winH carWin) v

    Kcp   k = kcp
    Scg   s = scg
    Delta d = delta

    ChainSchema winH vH = carHonest

    vA = C.sliceV carWin vH

    -- ensure the adversary loses this 'RI.Race' and each subsequent race that ends before it can accelerate
    unfillRaces kPlus1st !scope !mbYS !iter !g !mv = when (withinYS delta mbYS iter) $ do
        C.SomeWindow Proxy rwin <- pure $ let RI.Race x = iter in x

        C.SomeWindow (Proxy :: Proxy skolem) win <-
            pure
          $ C.withWindowBetween
                (C.windowSize carWin)
                (C.Lbl @RaceAssumptionLbl)
                (C.windowStart rwin)
                (C.windowLast  rwin C.+ d)   -- rwin ends in a block, so if d=0
                                             -- then the slot after that block
                                             -- is unconstrained; hence no +1

        -- INVARIANT: @win@ contains @scope@
        let _ = scope :: C.Index adv SlotE

        -- remove adversarial active slots as needed
        --
        -- But only remove them from /after/ @scope@ (ie do not remove them
        -- from slots in the previous Race Window).
        do  untouchZeroCount :: C.Var adv EmptySlotE <- do
                C.SomeWindow Proxy untouch <-   -- untouchable slots
                    pure
                  $ C.withWindowBetween
                        (C.windowSize carWin)
                        (C.Lbl @UntouchableLbl)
                        (C.windowStart rwin)
                        (scope C.- 1)   -- window will be empty if scope is 0
                C.fromWindowVar untouch <$> BV.countActivesInMV S.inverted (C.sliceMV untouch mv)

            C.SomeWindow (Proxy :: Proxy skolem2) touch <-   -- touchable slots
                pure
              $ C.withWindowBetween
                    (C.windowSize carWin)
                    (C.Lbl @TouchableLbl)
                    scope
                    (C.windowLast rwin C.+ d)


            let
              maxActive :: C.Var (C.Win RaceAssumptionLbl skolem) ActiveSlotE
              maxActive = C.Count k

              -- at most k can be active in this race window, so at least size - k must be empty
              minEmpty :: C.Var (C.Win RaceAssumptionLbl skolem) EmptySlotE
              minEmpty = S.complementActive S.notInverted (C.windowSize win) maxActive

            -- Discount that basic requirement by the number of zeros already
            -- in the untouchable portion of this race window.
            let touchableEmpty =
                    max 0
                  $ C.fromWindowVar win minEmpty - untouchZeroCount
                  :: C.Var adv EmptySlotE

            void $ BV.fillInWindow
                S.inverted
                (BV.SomeDensityWindow (C.toWindowVar touch touchableEmpty) (C.windowSize touch))
                g
                (C.sliceMV touch mv)

        case RI.next vA iter <|> RI.nextConservative vA iter of
            Nothing -> pure ()   -- there are no remaining honest active slots

            Just iter' -> do
                C.SomeWindow Proxy rwin' <- pure $ let RI.Race x = iter' in x
                mbYS' <- case mbYS of
                    KnownYS{} -> pure mbYS
                    UnknownYS -> do
                        -- check whether the slots that are settled as of just
                        -- now contain the first adversarial active slot
                        C.SomeWindow Proxy settledSlots <-
                            pure
                          $ C.withWindowBetween
                                (C.windowSize carWin)
                                (C.Lbl @SettledLbl)
                                (C.windowStart rwin)
                                (C.windowStart rwin' C.- 1)
                        mbFound <- BV.findIthEmptyInMV S.inverted (C.sliceMV settledSlots mv) (C.Count 0)
                        case mbFound of
                            BV.NothingFound -> pure UnknownYS
                            BV.JustFound x  ->
                                -- x is the first settled adversarial slot, so
                                -- the adversary can accelerate its growth as
                                -- of x+s+1 (If s were 0, it could accelerate
                                -- in the very next slot, thus the plus 1.)
                                pure $! KnownYS $!
                                  max
                                    (kPlus1st C.+ d C.+ 1)
                                    (C.fromWindow settledSlots x C.+ s C.+ 1)
                unfillRaces kPlus1st (C.windowLast win C.+ 1) mbYS' iter' g mv

    -- | Ensure the density of the adversarial schema is less than the density
    -- of the honest schema in the first stability window after the intersection
    -- and in any prefix window that contains the first race to the k+1st block.
    --
    -- Ensuring lower density of the prefix windows is necessary to avoid chains like
    --
    -- > k: 3
    -- > s: 9
    -- > H: 0111100
    -- > A: 1110011
    --
    -- where the honest chain wins the race to the k+1st block, might win the
    -- density comparison if the chains are extended to include a full stability
    -- window after the intersection, but loses the density comparison if the
    -- chains aren't extended.
    --
    -- For the sake of shrinking test inputs, we also prevent the above scenario
    -- to occur in any intersection, not just the intersections near the end of
    -- the chains.
    --
    ensureLowerDensityInWindows
      :: R.StatefulGen sg (ST s) => RI.Race adv -> sg -> C.MVector adv SlotE s S.S -> ST s ()
    ensureLowerDensityInWindows (RI.Race (C.SomeWindow _ w0)) g mv = do
        let
          -- A window after the intersection as short as the shortest of the
          -- stability window or the first race to the k+1st block.
          w0' = C.UnsafeContains (C.windowStart w0) (min (C.Count s) (C.windowSize w0))
          hCount = BV.countActivesInV S.notInverted (C.sliceV w0' vA)

        aCount <- ensureLowerDensityInWindow w0' g mv hCount

        go w0' (C.toVar hCount) aCount
      where
        -- Ensure low densities in all windows containing @w@ until the first
        -- stability window after the intersection.
        --
        -- @hc@ is the number of active slots in the honest schema in @w@
        -- @ac@ is the number of active slots in the adversarial schema in @w@
        go w hc ac =
          when (C.windowSize w C.+ 1 <= C.Count s
                && C.windowLast w C.+ 1 < C.toIndex (C.toVar $ C.lengthMV mv)) $ do
              let w' = increaseSizeW w

              sA <- BV.testMV S.notInverted mv (C.windowLast w')

              let
                ac' = if sA then ac C.+ 1 else ac
                sH = BV.testV S.notInverted vA (C.windowLast w')
                hc' = if sH then hc C.+ 1 else hc

              ac'' <-
                  if ac' >= hc' then
                    ensureLowerDensityInWindow w' g mv hc'
                  else
                    pure ac'

              go w' hc' ac''

    -- | Increase the size of a window by one slot
    increaseSizeW (C.UnsafeContains start size) =
        C.UnsafeContains start (size C.+ 1)

    -- | Ensure the density of the adversarial schema is less than the density
    -- of the honest schema in the given window.
    --
    -- @hCount@ is the number of active slots in the honest schema in the
    -- given window.
    ensureLowerDensityInWindow w g mv hCount = do
        let emptyCountTarget = C.toVar $ S.complementActive S.notInverted (C.windowSize w) hCount C.+ 1

        emptyCount <- BV.fillInWindow
            S.inverted
            (BV.SomeDensityWindow emptyCountTarget (C.windowSize w))
            g
            (C.sliceMV w mv)

        pure $ C.toVar $ S.complementActive S.inverted (C.windowSize w) emptyCount


-- | The youngest stable slot
--
-- If @x@ is the first adversarial active slot, then @x+s@ (see 'Scg') is the youngest stable slot.
data MaybeYS base = UnknownYS | KnownYS !(C.Index base SlotE)
  deriving (Eq, Read, Show)

-- | Does the Race Window end in a stable slot?
withinYS :: Delta -> MaybeYS base -> RI.Race base -> Bool
withinYS (Delta d) !mbYS !(RI.Race (C.SomeWindow Proxy win)) = case mbYS of
    KnownYS ys -> C.windowLast win C.+ d < ys
    UnknownYS  -> True   -- Honest Chain Growth ensures every Race Window is at most @'Scg' - 'Delta'@ slots wide

-- | Draw a random active slot count for the prefix of a fork.
--
-- The count will be strictly smaller than the number of active slots in the given 'ChainSchema'.
--
-- The result is guaranteed to leave more than k active slots after the
-- intersection.
genPrefixBlockCount :: R.RandomGen g => Kcp -> g -> ChainSchema base hon -> C.Var hon 'ActiveSlotE
genPrefixBlockCount (Kcp k) g schedH
    | C.getCount numChoices <= 0 = error "there should be at least k+1 blocks in the honest schema"
    | otherwise =
        -- uniformIndex is going to pick a number between 0 and numChoices-1
        C.toVar $ R.runSTGen_ g $ C.uniformIndex numChoices
  where
    ChainSchema _slots v = schedH

    numChoices = actives C.- k

    -- 'H.uniformTheHonestChain' ensures k < active
    actives = BV.countActivesInV S.notInverted v
