{-# LANGUAGE PatternSynonyms #-}

module Test.Consensus.Peras.Context (tests) where

import Data.Either (isRight)
import Ouroboros.Consensus.Block.SupportsPeras (PerasEpochContext)
import Ouroboros.Consensus.Peras.Context
  ( BoundedPerasEpochContext (..)
  , PerasEpochContextResolver (..)
  , perasEpochContextResolverBounds
  , resolveRoundNo
  )
import Ouroboros.Consensus.Peras.Params
  ( pattern NoPerasEnabled
  , pattern PerasEnabled
  )
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras (genMockPerasEpochContext)
import Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests =
  testGroup
    "PerasEpochContextResolver"
    [ testProperty "bounds agree with round resolution" prop_boundsAgreeWithResolution
    ]

prop_boundsAgreeWithResolution :: Property
prop_boundsAgreeWithResolution =
  forAll (genMockPerasEpochContext :: Gen (PerasEpochContext TestBlock)) $ \context ->
    forAll (choose (0 :: Int, 1000)) $ \start ->
      forAll (choose (1 :: Int, 100)) $ \previousLength ->
        forAll (choose (1 :: Int, 100)) $ \currentLength ->
          let split = start + previousLength
              end = split + currentLength
              previous = BoundedPerasEpochContext (fromIntegral start) (fromIntegral split) context
              current = BoundedPerasEpochContext (fromIntegral split) (fromIntegral end) context
              resolverCases =
                [ ("error", PerasEpochContextResolverError "test")
                , ("disabled", PerasEpochContextResolver NoPerasEnabled NoPerasEnabled)
                , ("current only", PerasEpochContextResolver (PerasEnabled current) NoPerasEnabled)
                , ("previous only", PerasEpochContextResolver NoPerasEnabled (PerasEnabled previous))
                , ("both epochs", PerasEpochContextResolver (PerasEnabled current) (PerasEnabled previous))
                ]
              roundNumbers = fromIntegral <$> filter (>= 0) [0, start - 1, start, split - 1, split, end - 1, end, end + 1]
           in conjoin
                [ let (lowerBound, upperBound) = perasEpochContextResolverBounds resolver
                   in counterexample
                        (caseName <> ": round " <> show roundNo <> ", bounds " <> show (lowerBound, upperBound))
                        $ (lowerBound <= roundNo && roundNo < upperBound)
                          === isRight (resolveRoundNo resolver roundNo)
                | (caseName, resolver) <- resolverCases
                , roundNo <- roundNumbers
                ]
