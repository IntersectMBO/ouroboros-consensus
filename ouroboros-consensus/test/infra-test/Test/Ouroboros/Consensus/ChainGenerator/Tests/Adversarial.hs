{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE TypeApplications          #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial         #-}
#endif

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial (
    SomeTestAdversarial (..)
  , TestAdversarial (..)
  , tests
  ) where

import           Control.Applicative ((<|>))
import qualified Control.Monad.Except as Exn
import           Data.Functor ((<&>))
import           Data.Functor.Identity (runIdentity)
import           Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import           Data.Proxy (Proxy (Proxy))
import qualified System.Random as R
import qualified System.Timeout as IO (timeout)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import           Test.Ouroboros.Consensus.ChainGenerator.Adversarial
                     (genPrefixBlockCount)
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc,
                     Delta (Delta), Kcp (Kcp), Len (Len), Scg (Scg), genAsc)
import qualified Test.Ouroboros.Consensus.ChainGenerator.RaceIterator as RI
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (SlotE))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest as H
import qualified Test.QuickCheck as QC hiding (elements)
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.QuickCheck.Random (QCGen)
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT
import qualified Test.Util.QuickCheck as QC

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "k+1 blocks after the intersection" prop_kPlus1BlocksAfterIntersection
  ,
    TT.testProperty "Adversarial chains lose density and race comparisons" prop_adversarialChain
  ,
    TT.localOption (TT.QuickCheckMaxSize 6) $ TT.testProperty "Adversarial chains win if checked with relaxed parameters" prop_adversarialChainMutation
  ]

-----

data SomeTestAdversarial =
    forall base hon.
    SomeTestAdversarial
        !(Proxy base)
        !(Proxy hon)
        !(TestAdversarial base hon)

instance Show SomeTestAdversarial where
    showsPrec p (SomeTestAdversarial base hon test) =
        Some.runShowsPrec p
      $ Some.showCtor SomeTestAdversarial "SomeTestAdversarial"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` test

instance Read SomeTestAdversarial where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeTestAdversarial "SomeTestAdversarial"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

data TestAdversarial base hon = TestAdversarial {
    testAscA     :: !Asc
  ,
    testAscH     :: !Asc
  ,
    testRecipeA  :: !(A.AdversarialRecipe base hon)
  ,
    testRecipeA' :: !(A.SomeCheckedAdversarialRecipe base hon)
  ,
    testRecipeH  :: !H.HonestRecipe
  ,
    testRecipeH' :: !(H.CheckedHonestRecipe base hon)
  ,
    testSeedH    :: !QCGen
  }
  deriving (Read, Show)

instance QC.Arbitrary SomeTestAdversarial where
    arbitrary = unsafeMapSuchThatJust $ do
        H.TestHonest {
            H.testAsc     = testAscH
          ,
            H.testRecipe  = testRecipeH
          ,
            H.testRecipe' = someTestRecipeH'
          } <- QC.arbitrary

        H.SomeCheckedHonestRecipe Proxy Proxy testRecipeH' <- pure someTestRecipeH'

        testSeedH <- QC.arbitrary

        let arHonest = H.uniformTheHonestChain (Just testAscH) testRecipeH' testSeedH

        testSeedPrefix <- QC.arbitrary @QCGen

        let arPrefix = genPrefixBlockCount testRecipeH testSeedPrefix arHonest

            H.HonestRecipe kcp scg delta _len = testRecipeH

            testRecipeA = A.AdversarialRecipe {
                A.arPrefix
              ,
                A.arParams = (kcp, scg, delta)
              ,
                A.arHonest
              }

        testAscA <- genAsc

        case Exn.runExcept $ A.checkAdversarialRecipe testRecipeA of
            Left e -> case e of
                A.NoSuchAdversarialBlock -> pure Nothing
                A.NoSuchCompetitor       -> error $ "impossible! " <> show e
                A.NoSuchIntersection     -> error $ "impossible! " <> show e

            Right testRecipeA' -> do
                pure $ Just $ SomeTestAdversarial Proxy Proxy $ TestAdversarial {
                    testAscA
                  ,
                    testAscH
                  ,
                    testRecipeA
                  ,
                    testRecipeA'
                  ,
                    testRecipeH
                  ,
                    testRecipeH'
                  ,
                    testSeedH
                  }

-- | Both the honest and the alternative schema have k+1 blocks after the
-- intersection.
prop_kPlus1BlocksAfterIntersection :: SomeTestAdversarial -> QCGen -> QC.Property
prop_kPlus1BlocksAfterIntersection someTestAdversarial testSeedA = runIdentity $ do
    SomeTestAdversarial Proxy Proxy TestAdversarial {
        testAscA
      ,
        testRecipeA
      ,
        testRecipeA'
      } <- pure someTestAdversarial
    A.SomeCheckedAdversarialRecipe Proxy recipeA' <- pure testRecipeA'

    let A.AdversarialRecipe { A.arHonest = schedH } = testRecipeA
        schedA = A.uniformAdversarialChain (Just testAscA) recipeA' testSeedA
        H.ChainSchema winA vA = schedA
        H.ChainSchema _winH vH = schedH
        A.AdversarialRecipe { A.arParams = (Kcp k, scg, _delta) } = testRecipeA

    C.SomeWindow Proxy stabWin <- do
        pure $ calculateStability scg schedA

    pure $
      QC.counterexample (unlines $
                            H.prettyChainSchema schedH "H"
                            ++ H.prettyChainSchema schedA "A"
                          )
      $ QC.counterexample ("arPrefix = " <> show (A.arPrefix testRecipeA))
      $ QC.counterexample ("stabWin  = " <> show stabWin)
      $ QC.counterexample ("stabWin' = " <> show (C.joinWin winA stabWin))
      $ QC.counterexample ("The honest chain should have k+1 blocks after the intersection")
          (BV.countActivesInV S.notInverted vH
            `QC.ge` C.toSize (C.Count (k + 1) + A.arPrefix testRecipeA)
          )
        QC..&&.
          QC.counterexample ("The alternative chain should have k+1 blocks after the intersection")
            (BV.countActivesInV S.notInverted vA `QC.ge` C.Count (k + 1))

-- | No seed exists such that each 'A.checkAdversarialChain' rejects the result of 'A.uniformAdversarialChain'
prop_adversarialChain :: SomeTestAdversarial -> QCGen -> QC.Property
prop_adversarialChain someTestAdversarial testSeedA = runIdentity $ do
    SomeTestAdversarial Proxy Proxy TestAdversarial {
        testAscA
      ,
        testRecipeA
      ,
        testRecipeA'
      } <- pure someTestAdversarial
    A.SomeCheckedAdversarialRecipe Proxy recipeA' <- pure testRecipeA'

    let A.AdversarialRecipe { A.arHonest = schedH } = testRecipeA

        schedA = A.uniformAdversarialChain (Just testAscA) recipeA' testSeedA

    let H.ChainSchema winA _vA = schedA

    C.SomeWindow Proxy stabWin <- do
        let A.AdversarialRecipe { A.arParams = (_kcp, scg, _delta) } = testRecipeA
        pure $ calculateStability scg schedA

    pure $ case Exn.runExcept $ A.checkAdversarialChain testRecipeA schedA of
        Right () -> QC.property ()
        Left e   -> case e of
            A.BadAnchor{} -> QC.counterexample (show e) False
            A.BadCount{}  -> QC.counterexample (show e) False
            A.BadDensity{}  -> QC.counterexample (show e) False
            A.BadRace rv  -> case rv of
                A.AdversaryWonRace {
                    A.rvAdv = RI.Race (C.SomeWindow Proxy rAdv)
                  ,
                    A.rvHon = RI.Race (C.SomeWindow Proxy rHon)
                  } -> id
                    $ QC.counterexample (advCounterexample schedH schedA winA stabWin rv)
                    $ QC.counterexample ("arPrefix = " <> show (A.arPrefix testRecipeA))
                    $ QC.counterexample ("rvAdv    = " <> show rAdv)
                    $ QC.counterexample ("rvAdv'   = " <> show (C.joinWin winA rAdv))
                    $ QC.counterexample ("rvHon    = " <> show rHon)
                    $ QC.counterexample ("stabWin  = " <> show stabWin)
                    $ QC.counterexample ("stabWin' = " <> show (C.joinWin winA stabWin))
                    $ QC.counterexample (show e)
                    $ False

data AdvStabLbl

-- | Calculate the interval in which the adversary can not yet have accelerated
calculateStability :: Scg -> H.ChainSchema base adv -> C.SomeWindow AdvStabLbl adv SlotE
calculateStability (Scg s) schedA =
    C.withWindow (C.windowSize winA) (C.Lbl @AdvStabLbl) (C.Count 0) (C.Count $ firstActive + theBlockItself + s)
  where
    H.ChainSchema winA vA = schedA

    C.Count firstActive = case BV.findIthEmptyInV S.inverted vA (C.Count 0) of
            BV.JustFound x  -> x
            BV.NothingFound -> error $ "impossible! " <> H.unlines' (H.prettyChainSchema schedA "A")
    theBlockItself = 1

-- | A nice rendering for failures of 'prop_adversarialChain'
advCounterexample ::
     A.ChainSchema base hon
  -> A.ChainSchema base adv
  -> C.Contains 'SlotE base adv
  -> C.Contains 'SlotE adv stab
  -> A.RaceViolation hon adv
  -> String
advCounterexample schedH schedA winA stabWin rv =
    case rv of
        A.AdversaryWonRace {
            A.rvAdv = RI.Race (C.SomeWindow Proxy rAdv)
          ,
            A.rvHon = RI.Race (C.SomeWindow Proxy rHon)
          } ->
            H.unlines' $ []
             <> [H.prettyWindow rHon "rHon"]
             <> reverse (H.prettyChainSchema schedH "H")
             <> H.prettyChainSchema schedA "A"
             <> [H.prettyWindow (C.joinWin winA rAdv) "rAdv'"]
             <> [H.prettyWindow (C.joinWin winA stabWin) "stabWin'"]

-----

-- | A mutation that causes some honest Race Windows to end one slot sooner
data AdversarialMutation =
    -- | Increasing 'Delta' by the given amount may cause the adversary to win a race
    AdversarialMutateDelta Int
  |
    -- | Decreasing 'Kcp' by one may cause the adversary to win a race
    AdversarialMutateKcp
{-
  |
    -- | Decreasing 'Scg' by one may case the adversary to win a (conservative) race
    AdversarialMutateScgNeg
  |
    -- | Increasing 'Scg' by one may case the adversary to accelerate prematurely
    AdversarialMutateScgPos
-}
  deriving (Eq, Read, Show)

data TestAdversarialMutation base hon =
    TestAdversarialMutation
        !H.HonestRecipe
        !(H.CheckedHonestRecipe          base hon)
        !(A.AdversarialRecipe            base hon)
        !(A.SomeCheckedAdversarialRecipe base hon)
        !AdversarialMutation
  deriving (Read, Show)

data SomeTestAdversarialMutation =
    forall base hon.
    SomeTestAdversarialMutation
        !(Proxy base)
        !(Proxy hon)
        !(TestAdversarialMutation base hon)

instance Show SomeTestAdversarialMutation where
    showsPrec p (SomeTestAdversarialMutation base hon testMut) =
        Some.runShowsPrec p
      $ Some.showCtor SomeTestAdversarialMutation "SomeTestAdversarialMutation"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` testMut

instance Read SomeTestAdversarialMutation where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeTestAdversarialMutation "SomeTestAdversarialMutation"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

mutateAdversarial :: A.AdversarialRecipe base hon -> AdversarialMutation -> A.AdversarialRecipe base hon
mutateAdversarial recipe mut =
    A.AdversarialRecipe { A.arHonest, A.arParams = (Kcp k', Scg s', Delta d'), A.arPrefix }
  where
    A.AdversarialRecipe { A.arHonest, A.arParams = (Kcp k,  Scg s,  Delta d ), A.arPrefix } = recipe

    (k', s', d') = case mut of
        AdversarialMutateDelta dInc -> (k,     s,     d + dInc)
        AdversarialMutateKcp        -> (k - 1, s,     d       )
--        AdversarialMutateScgNeg -> (k,     s - 1, d    )
--        AdversarialMutateScgPos -> (k,     s + 1, d    )

instance QC.Arbitrary SomeTestAdversarialMutation where
    arbitrary = do
        unsafeMapSuchThatJust $ do
            recipeH@(H.HonestRecipe kcp scg delta len) <- H.genHonestRecipe

            someTestRecipeH' <- case Exn.runExcept $ H.checkHonestRecipe recipeH of
                Left e  -> error $ "impossible! " <> show (recipeH, e)
                Right x -> pure x

            H.SomeCheckedHonestRecipe Proxy Proxy recipeH' <- pure someTestRecipeH'

            seedH <- QC.arbitrary @QCGen

            let arHonest = H.uniformTheHonestChain Nothing recipeH' seedH

            testSeedPrefix <- QC.arbitrary @QCGen

            let arPrefix = genPrefixBlockCount recipeH testSeedPrefix arHonest

            let recipeA = A.AdversarialRecipe {
                    A.arPrefix
                  ,
                    A.arParams = (kcp, scg, delta)
                  ,
                    A.arHonest
                  }

            case Exn.runExcept $ A.checkAdversarialRecipe recipeA of
                Left e -> pure $ case e of
                    A.NoSuchAdversarialBlock -> Nothing
                    A.NoSuchCompetitor       -> error $ "impossible! " <> show e
                    A.NoSuchIntersection     -> error $ "impossible! " <> show e

                Right recipeA' -> do
                  let dInc = deltaIncrementFromRecipe recipeA'
                  mut <- QC.elements [AdversarialMutateKcp, AdversarialMutateDelta dInc]
                  pure $ case Exn.runExcept $ A.checkAdversarialRecipe $ mutateAdversarial recipeA mut of
                    Left{} -> Nothing

                    Right (A.SomeCheckedAdversarialRecipe Proxy mutRecipeA)
                        -- no mutation matters if the adversary doesn't even
                        -- have k+1 slots of any kind, let alone active slots
                      | let A.UnsafeCheckedAdversarialRecipe {
                                A.carParams = (Kcp k', _scg', _delta')
                              ,
                                A.carWin
                              } = mutRecipeA
                      , C.getCount (C.windowSize carWin) < k' + 1
                      -> Nothing

                      -- ASSUMPTION: the adversary has k blocks in the first race.
                      --
                      -- If there's no slot after the first race, then the
                      -- increment of delta cannot make a difference.
                      --
                      -- If there are not at least k+1 slots in the honest schema,
                      -- then decreasing k cannot make the schema check fail.
                      | let H.ChainSchema _winH vH = arHonest
                      , let Len l   = len
                      , let (Kcp k', _scg', Delta d') = A.carParams mutRecipeA
                        -- slot of the k+1 honest active
                      , endOfFirstRace <- case BV.findIthEmptyInV S.inverted vH (C.toIndex arPrefix C.+ k') of
                            BV.NothingFound{} -> l
                            BV.JustFound x    -> C.getCount x + d'
                      , l <= endOfFirstRace
                      -> Nothing

                      | otherwise ->
                        Just
                      $ SomeTestAdversarialMutation Proxy Proxy
                      $ TestAdversarialMutation
                            recipeH
                            recipeH'
                            recipeA
                            recipeA'
                            mut
      where
        -- | The increment of delta depends on what the honest schema is
        -- in the first stability window after the intersection.
        --
        -- If the stability window has more than k+1 slots. Incrementing
        -- delta by 1 should cause verification to fail for some adversarial
        -- schema. For instance if s=4, k=1, d=0, the intersection is Genesis
        -- and the honest schema is
        --
        -- > 0111 0101
        --
        -- then the following adversarial schema fails to validate with d=1.
        --
        -- > 0101 0011
        --
        -- However, if the stability window has exactly k+1 slots, then we
        -- need a higher increment for delta. Suppose the honest schema is
        --
        -- > 0110 0111
        --
        -- with the same parameters as before. In this situation, the
        -- adversarial schemas are constrained to have only one slot
        -- in the first stability window, and therefore it is impossible
        -- for the adversary to win a race to the k+1st active slot.
        --
        -- We compute the needed increment as the distance from the
        -- k+1st slot after the intersection to the first slot after the
        -- first stability window after the intersection. In our example,
        -- the increment is 2, and therefore with d=2 we can find the
        -- following alternative schema that fails validation.
        --
        -- > 0100 1000
        --
        deltaIncrementFromRecipe (A.SomeCheckedAdversarialRecipe _ r) =
          let H.ChainSchema _ v = A.carHonest r
              sv = C.sliceV (A.carWin r) v
              (Kcp k, Scg s, _) = A.carParams r
              kPlus1st = case BV.findIthEmptyInV S.inverted sv (C.Count k) of
                BV.NothingFound -> 1
                BV.JustFound i -> C.getCount i
           in
              case BV.findIthEmptyInV S.inverted sv (C.Count (k+1)) of
                BV.JustFound i | C.getCount i < s -> 1
                _ -> s - kPlus1st

-- | There exists a seed such that each 'TestAdversarialMutation' causes
-- 'A.checkAdversarialChain' to reject the result of 'A.uniformAdversarialChain'
--
-- TODO this did fail after >500,000 tests. Is that amount of flakiness acceptable?
prop_adversarialChainMutation :: SomeTestAdversarialMutation -> QCGen -> QC.Property
prop_adversarialChainMutation (SomeTestAdversarialMutation Proxy Proxy testAdversarialMut) testSeedAsSeed0 =
  QC.ioProperty $ do
    A.SomeCheckedAdversarialRecipe Proxy recipeA' <- pure someRecipeA'

    counter <- newIORef @Int 0
    catch   <- newIORef @(QCGen, [String]) (undefined, [])

    -- we're willing to wait up to 20s to find a failure for each 'TestHonestMutation'
    IO.timeout
        (20 * 10^(6::Int))
        (go catch counter recipeA' testSeedAsSeed0) >>= \case
            Just prop -> pure prop
            Nothing   ->    -- did not find a failure caused by the mutation
                ((,) <$> readIORef catch <*> readIORef counter) <&> \((seedA, schedA'), n) -> id
                  $ QC.counterexample ("n = " <> show n)
                  $ QC.counterexample
                        (advMutCounterexample testAdversarialMut mutatedRecipe schedA' seedA)
                  $ False
  where
    TestAdversarialMutation recipeH _recipeH' recipeA someRecipeA' mut = testAdversarialMut

    H.HonestRecipe _kcp scg _delta _len = recipeH

    mutatedRecipe = mutateAdversarial recipeA mut

    go catch counter recipeA' testSeedAsSeed = do
        modifyIORef' counter (+1)
        let -- TODO is this a low quality random stream? Why is there no @'R.Random' 'QCGen'@ instance?
            (testSeedA, testSeedAsSeed') = R.split testSeedAsSeed

            schedA = A.uniformAdversarialChain Nothing recipeA' (testSeedA :: QCGen)
            m      = A.checkAdversarialChain mutatedRecipe schedA
        -- We discard tests where the first race ends past the acceleration bound
        -- as these race windows are unconstrained
        case Exn.runExcept m of
            Right () -> do
                let A.UnsafeCheckedAdversarialRecipe { A.carWin } = recipeA'
                    pretty = case calculateStability scg schedA of
                        C.SomeWindow Proxy win ->
                            [H.prettyWindow (C.joinWin carWin win) "no accel"]
                         <> [show (H.countChainSchema schedA)]
                writeIORef catch (testSeedA,  H.prettyChainSchema schedA "A" <> pretty)
                go catch counter recipeA' testSeedAsSeed'
            Left e   -> case e of
                A.BadAnchor{}  -> error $ "impossible! " <> show e
                A.BadCount{}   -> error $ "impossible! " <> show e
                A.BadDensity{} -> pure $ QC.property ()
                A.BadRace{}    -> pure $ QC.property ()

-----

-- | A nice rendering for failures of 'prop_adversarialChainMutation'
advMutCounterexample ::
     TestAdversarialMutation base hon
  -> A.AdversarialRecipe base hon
  -> [String]
  -> QCGen
  -> String
advMutCounterexample testAdversarialMut mutatedRecipe schedA' seedA =
    H.unlines' $ []
     <> [show seedA]
     <> [show ch <> " - " <> show arPrefix <> " = " <> show (C.toVar ch - arPrefix) <> "   vs " <> show (kcp, mut)]
     <> schedH'
     <> schedA'
     <> go' (((,) False <$> RI.init kcp' vH) <|> Just (True, RI.initConservative vH))
  where
    TestAdversarialMutation recipeH _recipeH' recipeA someRecipeA' mut = testAdversarialMut

    H.HonestRecipe kcp _scg _delta _len = recipeH

    A.AdversarialRecipe { A.arHonest, A.arPrefix } = recipeA

    H.ChainSchema _winH vH = arHonest

    A.AdversarialRecipe { A.arParams = (kcp', _scg', _delta') } = mutatedRecipe

    schedH' = H.prettyChainSchema arHonest "H"

    ch = H.countChainSchema arHonest

    next iter =
            ((,) False <$> RI.next vH iter)
        <|>
            ((,) True <$> RI.nextConservative vH iter)

    go' = \case
        Nothing -> []
        Just (cons, iter@(RI.Race (C.SomeWindow Proxy win)))
          | A.SomeCheckedAdversarialRecipe Proxy recipeA' <- someRecipeA'
          , A.UnsafeCheckedAdversarialRecipe { A.carWin } <- recipeA'
          , C.windowStart win < C.windowStart carWin
          -> go' (next iter)

          | otherwise ->
              ""
            : head (tail schedH')
            : H.prettyWindow win ("raceH" <> if cons then " (conservative)" else "")
            : take 2 (tail schedA')
