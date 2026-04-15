{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Committee.WFALS.Conformance
  ( conformsToRustImplementation
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
import System.FilePath ((</>))
import Test.QuickCheck (Property)
import Test.QuickCheck.Gen (Gen, choose)
import Test.QuickCheck.Property (counterexample, forAll, (===))

type CommitteeSize = Int
type NumPersistent = Int
type NumNonPersistent = Int
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

-- | Embedded Rust implementation results.
--
-- These are the results of the Rust implementation when applied to
-- 'exampleStakeDistr' for every valid target committee size, i.e., from 1 to
-- the maximum number of pools with strictly positive stake, which, in the case
-- of 'exampleStakeDistr', corresponds to 2052 pools.
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
stakeDistr :: StakeDistr
stakeDistr =
  either error id $
    eitherDecodeStrict $
      $( FileEmbed.embedFile $
           "ouroboros-consensus"
             </> "test"
             </> "consensus-test"
             </> "data"
             </> "stake_distr.json"
       )

-- | Sample a value from an array
sampleArray :: Array Int a -> Gen a
sampleArray array = do
  i <- choose (Array.bounds array)
  pure $ (Array.!) array i

-- | Check that a weighted fait accompli committee selection implementation
-- conforms to the Rust implementation by comparing the number persistent and
-- non-persistent committee members it selects for a given target committee size.
conformsToRustImplementation ::
  ( Map PoolId Stake ->
    CommitteeSize ->
    ( NumPersistent
    , NumNonPersistent
    )
  ) ->
  Property
conformsToRustImplementation wfals = do
  forAll (sampleArray rustResults) $
    \RustResult
       { targetCommitteeSize
       , numPersistent
       , numNonPersistent
       } -> do
        let (actualNumPersistent, actualNumNonPersistent) =
              wfals stakeDistr targetCommitteeSize
        counterexample
          ( unlines
              [ "Target committee size: "
                  <> show targetCommitteeSize
              , "Expected (persistent, non-persistent): "
                  <> show (numPersistent, numNonPersistent)
              , "Actual (persistent, non-persistent): "
                  <> show (actualNumPersistent, actualNumNonPersistent)
              ]
          )
          $ (actualNumPersistent, actualNumNonPersistent)
            === (numPersistent, numNonPersistent)
