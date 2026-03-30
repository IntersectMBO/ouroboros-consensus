{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Committee.WFALS.Conformance
  ( CommitteeSize
  , NumPersistent
  , NumNonPersistent
  , PoolId
  , Stake
  , StakeDistr
  , conformsToRustImplementation
  )
where

import Data.Aeson
  ( FromJSON (..)
  , eitherDecodeStrict
  , withObject
  , (.:)
  )
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.FileEmbed as FileEmbed
import Data.Map.Strict (Map)
import Data.Word (Word64)
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

type CommitteeSize = Word64
type NumPersistent = Word64
type NumNonPersistent = Word64
type PoolId = String
type Stake = Rational
type StakeDistr = Map PoolId Stake

-- | A single result of the Rust implementation
data RustResult = RustResult
  { targetCommitteeSize :: CommitteeSize
  , numPersistent :: NumPersistent
  , numNonPersistent :: NumNonPersistent
  }
  deriving (Show, Eq)

instance FromJSON RustResult where
  parseJSON = withObject "RustResult" $ \obj ->
    RustResult
      <$> obj .: "target"
      <*> obj .: "persistent"
      <*> obj .: "nonpersistent"

-- | Embedded Rust implementation results
rustResults :: Array Int RustResult
rustResults =
  either error toArray $
    eitherDecodeStrict $
      $( FileEmbed.embedFile $
           "ouroboros-consensus"
             </> "test"
             </> "consensus-test"
             </> "data"
             </> "rust_results.json"
       )
 where
  toArray pairs =
    Array.listArray (0, length pairs - 1) pairs

-- | Embedded stake distribution
exampleStakeDistr :: StakeDistr
exampleStakeDistr =
  either error id $
    eitherDecodeStrict $
      $( FileEmbed.embedFile $
           "ouroboros-consensus"
             </> "test"
             </> "consensus-test"
             </> "data"
             </> "stake_distr.json"
       )

-- | Check that a weighted fait accompli committee selection implementation
-- conforms to the Rust implementation by comparing the number persistent and
-- non-persistent committee members it selects for a given target committee size.
conformsToRustImplementation ::
  String ->
  (Map PoolId Stake -> stakeDistr) ->
  ( stakeDistr ->
    CommitteeSize ->
    ( NumPersistent
    , NumNonPersistent
    )
  ) ->
  TestTree
conformsToRustImplementation name mkStakeDistr wfa = do
  testCase name (go (Array.bounds rustResults))
 where
  stakeDistr = mkStakeDistr exampleStakeDistr

  go (currStep, lastStep)
    | currStep > lastStep =
        pure ()
    | otherwise = do
        step ((Array.!) rustResults currStep)
        go (succ currStep, lastStep)

  step RustResult{targetCommitteeSize, numPersistent, numNonPersistent} = do
    let (actualNumPersistent, actualNumNonPersistent) =
          wfa stakeDistr targetCommitteeSize
    assertEqual
      ( unlines
          [ "Target committee size: "
              <> show targetCommitteeSize
          , "Expected (persistent, non-persistent): "
              <> show (numPersistent, numNonPersistent)
          , "Actual (persistent, non-persistent): "
              <> show (actualNumPersistent, actualNumNonPersistent)
          ]
      )
      (numPersistent, numNonPersistent)
      (actualNumPersistent, actualNumNonPersistent)
