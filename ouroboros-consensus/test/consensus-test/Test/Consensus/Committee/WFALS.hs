{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Consensus.Committee.WFALS (tests) where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import qualified Cardano.Ledger.Keys as SL
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Either (isRight, partitionEithers)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (IsString (..))
import qualified Ouroboros.Consensus.Committee.Types as WFA
import Ouroboros.Consensus.Committee.WFA (WFATiebreaker (..))
import qualified Ouroboros.Consensus.Committee.WFA as WFA
import Test.Consensus.Committee.WFALS.Conformance (conformsToRustImplementation)
import qualified Test.Consensus.Committee.WFALS.Model as Model
import qualified Test.Consensus.Committee.WFALS.Model.Test as Model
import qualified Test.Consensus.Committee.WFALS.Model.Utils as Model
import qualified Test.Consensus.Committee.WFALS.Test as Impl
import Test.Consensus.Committee.WFALS.Utils (mkPoolId)
import Test.QuickCheck
  ( Property
  , Testable (..)
  , counterexample
  , tabulate
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "weighted Fait-Accompli committee selection tests"
    [ Model.tests
    , Impl.tests
    , modelConformsToRustImplementation
    , realImplementationConformsToRustImplementation
    , modelConformsToRealImplementation
    ]

-- | Check that the model implementation matches the Rust one
modelConformsToRustImplementation :: TestTree
modelConformsToRustImplementation =
  conformsToRustImplementation
    "model conforms to Rust implementation"
    mkStakeDistr
    model
 where
  -- NOTE: this is not a good tiebreaker for real-world use, but it is
  -- sufficient here because the actual order of pools with the same stake does
  -- not matter when computing the persistent vs. non-persistent seat /counts/.
  tiebreaker =
    compare

  mkStakeDistr =
    Map.map Model.rationalToStake

  model stakeDistr targetCommitteeSize = do
    let totalSeats = fromIntegral targetCommitteeSize
    case Model.weightedFaitAccompliPersistentSeats tiebreaker totalSeats stakeDistr of
      Left err ->
        error $ "Model implementation failed with error: " <> show err
      Right (persistentSeats, numNonPersistentSeats, _) ->
        ( fromIntegral (Map.size persistentSeats)
        , fromIntegral numNonPersistentSeats
        )

-- | Check that the real implementation matches the Rust one
realImplementationConformsToRustImplementation :: TestTree
realImplementationConformsToRustImplementation =
  conformsToRustImplementation
    "real implementation conforms to Rust implementation"
    mkStakeDistr
    impl
 where
  -- NOTE: this is not a good tiebreaker for real-world use, but it is
  -- sufficient here because the actual order of pools with the same stake does
  -- not matter when computing the persistent vs. non-persistent seat /counts/.
  tiebreaker =
    WFATiebreaker compare

  mkStakeDistr =
    expectRight
      ( \err ->
          error ("could not build a strake distribution: " <> show err)
      )
      . WFA.mkExtWFAStakeDistr tiebreaker
      . Map.mapKeys
        ( \str ->
            WFA.PoolId
              . SL.hashKey
              . SL.VKey
              . SL.deriveVerKeyDSIGN
              . SL.genKeyDSIGN
              . SL.mkSeedFromBytes
              . fromString
              $ str
        )
      . Map.map
        ( \stake ->
            (WFA.LedgerStake stake, ())
        )

  impl stakeDistr targetCommitteeSize =
    let
      totalSeats =
        WFA.TargetCommitteeSize (fromIntegral targetCommitteeSize)
      (persistentSeats, nonPersistentSeats, _, _) =
        expectRight
          ( \err ->
              error ("weightedFaitAccompliSplitSeats failed: " <> show err)
          )
          $ WFA.weightedFaitAccompliSplitSeats stakeDistr totalSeats
     in
      ( fromIntegral (WFA.unPersistentCommitteeSize persistentSeats)
      , fromIntegral (WFA.unNonPersistentCommitteeSize nonPersistentSeats)
      )

  expectRight onLeft = \case
    Left err -> onLeft err
    Right a -> a

-- | Check that the model implementation conforms to the real implementation
-- using QuickCheck generators
modelConformsToRealImplementation :: TestTree
modelConformsToRealImplementation =
  adjustQuickCheckTests (* 10) $
    testProperty
      "model conforms to real implementation"
      prop_modelConformsToRealImplementation

-- | Property: the model and real implementations should produce the same
-- number of persistent and non-persistent seats
prop_modelConformsToRealImplementation :: Property
prop_modelConformsToRealImplementation =
  Model.forAllPossiblyInvalidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    -- NOTE: we use a trivial tiebreaker here because we are not interested in
    -- fairness here. However, the model's tiebreaker needs to be adapted below
    -- to produce the same ordering based on the 'PoolId' we derive from the
    -- string keys in the random stake distribution, so that both
    -- implementations select the same pools as persistent vs. non-persistent.
    let realTiebreaker :: WFA.PoolId -> WFA.PoolId -> Ordering
        realTiebreaker = compare
    -- Run the model
    let modelOutput =
          Model.weightedFaitAccompliPersistentSeats
            (realTiebreaker `on` mkPoolId)
            numSeats
            stakeDistr
    -- Run the real implementation
    let realOutput = do
          let mkEntry stake = (WFA.LedgerStake (Model.stakeToRational stake), ())
          extWFAStakeDistr <-
            WFA.mkExtWFAStakeDistr (WFATiebreaker realTiebreaker)
              . Map.mapKeys (\str -> mkPoolId str)
              . Map.map (\stake -> mkEntry stake)
              $ stakeDistr
          wfaOutput <-
            WFA.weightedFaitAccompliSplitSeats
              extWFAStakeDistr
              (WFA.TargetCommitteeSize (fromIntegral numSeats))
          return (extWFAStakeDistr, wfaOutput)
    -- Compare their results
    tabulate
      "Model vs real implementation output"
      [show (isRight modelOutput, isRight realOutput)]
      $ case (modelOutput, realOutput) of
        -- Both implementations failed (we don't care about the specific error)
        (Left _, Left _) ->
          property True
        -- Model failed but real implementation succeeded
        (Left modelErr, Right _) ->
          counterexample
            ( "Model implementation failed with error: "
                <> show modelErr
                <> " but real implementation succeeded with output: "
                <> show realOutput
            )
            $ property False
        -- Model succeeded but real implementation failed
        (Right _, Left realErr) ->
          counterexample
            ( "Real implementation failed with error: "
                <> show realErr
                <> " but model implementation succeeded with output: "
                <> show modelOutput
            )
            $ property False
        -- Both implementations succeeded, compare their outputs
        ( Right
            ( modelPersistentStakeDistr
              , modelNonPersistentSeats
              , modelNonPersistentStakeDistr
              )
          , Right
              ( extWFAStakeDistr
                , ( WFA.PersistentCommitteeSize realPersistentSeats
                    , WFA.NonPersistentCommitteeSize realNonPersistentSeats
                    , WFA.TotalPersistentStake
                        (WFA.Cumulative (WFA.LedgerStake realPersistentStake))
                    , WFA.TotalNonPersistentStake
                        (WFA.Cumulative (WFA.LedgerStake realNonPersistentStake))
                    )
                )
          ) -> do
            let modelPersistentPools =
                  Set.map mkPoolId (Map.keysSet modelPersistentStakeDistr)
            let modelNonPersistentPools =
                  Set.map mkPoolId (Map.keysSet modelNonPersistentStakeDistr)
            let Model.StakeLedgerResidual modelNonPersistentStake =
                  sum (Map.elems modelNonPersistentStakeDistr)
            let Model.StakeWeightPersistent modelPersistentStake =
                  sum (Map.elems modelPersistentStakeDistr)
            let splitPools (WFA.SeatIndex i, (poolId, _, _, _))
                  | realPersistentSeats == 0 = Right poolId
                  | i >= realPersistentSeats = Right poolId
                  | otherwise = Left poolId
            let (realPersistentPools, realNonPersistentPools) =
                  bimap Set.fromList Set.fromList
                    . partitionEithers
                    . fmap splitPools
                    . Array.assocs
                    . WFA.unExtWFAStakeDistr
                    $ extWFAStakeDistr
            counterexample
              ( unlines
                  [ "Model persistent pools: " <> show modelPersistentPools
                  , "Model non-persistent pools: " <> show modelNonPersistentPools
                  , "Model persistent stake: " <> show modelPersistentStake
                  , "Model non-persistent stake: " <> show modelNonPersistentStake
                  , "Real persistent pools: " <> show realPersistentPools
                  , "Real non-persistent pools: " <> show realNonPersistentPools
                  , "Real persistent stake: " <> show realPersistentStake
                  , "Real non-persistent stake: " <> show realNonPersistentStake
                  ]
              )
              $ (modelPersistentPools === realPersistentPools)
                .&&. (modelNonPersistentPools === realNonPersistentPools)
                .&&. (modelNonPersistentStake === realNonPersistentStake)
                .&&. (modelPersistentStake === realPersistentStake)
                .&&. (fromIntegral modelNonPersistentSeats === realNonPersistentSeats)
