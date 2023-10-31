{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.BitVector (tests) where

import           Data.Monoid (Endo (Endo, appEndo))
import qualified Data.Vector.Unboxed as V
import           GHC.Generics (Generic)
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot
                     (E (EmptySlotE, SlotE), POL, PreImage, S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT
import qualified Test.Util.QuickCheck as QC

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "prop_findIthZeroInV" prop_findIthZeroInV,
    TT.testProperty "prop_fillInWindow" prop_fillInWindow
  ]

-----

data SomeFindTestSetup = forall pol. TestPOL pol => SomeFindTestSetup (FindTestSetup pol)

instance Show SomeFindTestSetup where
    showsPrec p (SomeFindTestSetup testSetup) =
        showParen (p >= 11)
      $ showString "SomeFindTestSetup " . showsPrec 11 testSetup

data ProxyPol (pol :: S.Pol) = ProxyPol
  deriving (Eq)

instance TestPOL pol => Show (ProxyPol pol) where
    showsPrec p pol =
        Some.runShowsPrec p
      $ Some.showCtor pol ("ProxyPol :: ProxyPol " <> showPol pol)

instance TestPOL pol => Read (ProxyPol pol) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor pol (show pol)
      where
        pol = ProxyPol :: ProxyPol pol

data TestB

instance QC.Arbitrary (ProxyPol pol) where
    arbitrary = pure ProxyPol
    shrink    = QC.shrinkNothing

data SomePol = forall pol. TestPOL pol => SomePol (ProxyPol pol)

deriving instance Show SomePol

data FindTestSetup (pol :: S.Pol) = FindTestSetup {
    testPol   :: ProxyPol pol
  ,
    -- | INVARIANT @'testIndex' < V.sum (V.map ('countZeros' 'testPol') 'testV')@
    testIndex :: C.Index TestB (PreImage pol EmptySlotE)
  ,
    testV     :: C.Vector TestB SlotE S
  }
  deriving (Eq, Generic, Read, Show)

instance QC.Arbitrary SomePol where
    arbitrary = do
        inverted <- QC.arbitrary
        if inverted then
          pure $ SomePol (ProxyPol :: ProxyPol S.Inverted)
        else
          pure $ SomePol (ProxyPol :: ProxyPol S.NotInverted)
    shrink    = QC.shrinkNothing

instance QC.Arbitrary SomeFindTestSetup where
    arbitrary = do
        SomePol (_ :: ProxyPol pol) <- QC.arbitrary
        SomeFindTestSetup <$> (QC.arbitrary :: QC.Gen (FindTestSetup pol))

    shrink (SomeFindTestSetup x) = [ SomeFindTestSetup y | y <- QC.shrink x ]

instance TestPOL pol => QC.Arbitrary (FindTestSetup pol) where
    arbitrary = do
        let testPol = ProxyPol :: ProxyPol pol

        v <- V.fromList <$> QC.arbitrary

        let tc = targetCount testPol v
        i <- if 0 == tc then QC.discard else QC.choose (0, tc - 1)

        pure FindTestSetup {
            testPol
          ,
            testIndex = C.Count i
          ,
            testV     = C.Vector v
          }

    shrink x =
        [ y
        | y <- QC.genericShrink x
        , let FindTestSetup {testIndex, testPol, testV} = y
        , C.getCount testIndex < targetCount testPol (C.getVector testV)
        ]

targetCount :: TestPOL pol => proxy pol -> V.Vector S -> Int
targetCount pol = V.length . V.filter (not . S.test pol)

-----

class POL pol => TestPOL (pol :: S.Pol) where showPol :: proxy pol -> String
instance TestPOL S.Inverted             where showPol _pol = "Inverted"
instance TestPOL S.NotInverted          where showPol _pol = "NotInverted"

prop_findIthZeroInV :: SomeFindTestSetup -> QC.Property
prop_findIthZeroInV testSetup = case testSetup of
    SomeFindTestSetup FindTestSetup {
        testIndex
      ,
        testPol
      ,
        testV
      } -> case BV.findIthEmptyInV testPol testV testIndex of
        BV.NothingFound -> QC.counterexample "NothingFound" False
        BV.JustFound i  ->
              id
            $ QC.counterexample (showPol testPol)
            $ QC.counterexample
                (let C.Vector v = testV
                 in
                 "v =" <> V.foldMap (Endo . S.showS) v `appEndo` ""
                )
            $ QC.counterexample ("i = " <> show i)
            $
               (   id
                 $ QC.counterexample "i isn't the position of a post-polarization-0 in w!"
                 $ BV.testV testPol testV i QC.=== False
               )
              QC..&&.
                  (   let targetsInPrecedingWords =
                              targetCount testPol $ V.take (C.getCount i) $ C.getVector testV
                      in
                        id
                      $ QC.counterexample "There are not testIndex-many post-polarization-0s preceding i!"
                      $ targetsInPrecedingWords QC.=== C.getCount testIndex
                  )


data FillInWindowSetup =
    FillInWindowSetup
      SomePol
      Int -- ^ k
      Int -- ^ size of the window to fill
      QCGen -- ^ random generator
      (V.Vector S) -- ^ the vector to fill

instance Show FillInWindowSetup where
  showsPrec p (FillInWindowSetup spol k szW qcGen v) =
        showParen (p >= 11)
      $ showString "FillInWindowSetup"
      . showString "\n  pol = " . shows spol
      . showString "\n  k = " . shows k
      . showString "\n  qcGen = " . shows qcGen
      . showString "\n  szW = " . shows szW
      . showString "\n  v = " . shows (showBitVector v)

showBitVector :: V.Vector S -> String
showBitVector v =
    take bvLen [ if b then '1' else '0' | s <- V.toList v, let b = S.test S.notInverted s ]
    ++ if szV > bvLen then "..." else ""
  where
    szV = V.length v
    bvLen = 80

instance QC.Arbitrary FillInWindowSetup where
  arbitrary = do
      pol <- QC.arbitrary
      QC.NonNegative k <- QC.arbitrary
      QC.NonNegative k1 <- QC.arbitrary
      QC.NonNegative k2 <- QC.arbitrary
      qcGen <- QC.arbitrary
      let szV = k1
          szW = max k szV + k2 -- k <= szW && szV <= szW
      ss <- QC.vectorOf szV QC.arbitrary
      pure $ FillInWindowSetup pol k szW qcGen (V.fromList ss)

prop_fillInWindow :: FillInWindowSetup -> QC.Property
prop_fillInWindow
        (FillInWindowSetup
          (SomePol pol)
          k
          szW
          qcGen
          v
        ) = R.runSTGen_ qcGen $ \g -> do
    vmv <- V.thaw v
    let mv = C.MVector vmv
        szV = V.length v
    actives0 <- C.getCount <$> BV.countActivesInMV pol mv
    c <- C.getCount <$> BV.fillInWindow pol (BV.SomeDensityWindow (C.Count k) (C.Count szW)) g mv
    actives1 <- C.getCount <$> BV.countActivesInMV pol mv
    mvFrozen <- V.freeze vmv
    pure $
        QC.counterexample (showPol pol) $
        QC.counterexample ("actives0 = " ++ show actives0) $
        QC.counterexample ("actives1 = " ++ show actives1) $
        QC.counterexample ("szV = " ++ show szV) $
        QC.counterexample ("v = " ++ show (showBitVector mvFrozen)) $

        (QC.counterexample "actives0 <= actives1" $ actives0 `QC.le` actives1)
        QC..&&.
        (QC.counterexample "c == actives1" $ c QC.=== actives1)
        QC..&&.
        (QC.counterexample "k <= actives1 + szW - szV" $ k `QC.le` actives1 + szW - szV)
