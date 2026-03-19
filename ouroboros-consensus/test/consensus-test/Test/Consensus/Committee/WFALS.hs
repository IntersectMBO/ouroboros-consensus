module Test.Consensus.Committee.WFALS
  ( tests
  )
where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import qualified Cardano.Ledger.Keys as SL
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import qualified Ouroboros.Consensus.Committee.Types as WFA
import qualified Ouroboros.Consensus.Committee.WFA as WFA
import Test.Consensus.Committee.WFALS.Conformance (conformsToRustImplementation)
import qualified Test.Consensus.Committee.WFALS.Model as Model
import qualified Test.Consensus.Committee.WFALS.Model.Test as Model
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "weighted Fait-Accompli committee selection tests"
    [ Model.tests
    , modelConformsToRustImplementation
    , realImplementationConformsToRustImplementation
    ]

-- | Check that the model implementation matches the Rust one
modelConformsToRustImplementation :: TestTree
modelConformsToRustImplementation =
  conformsToRustImplementation
    "model conforms to Rust implementation"
    mkStakeDistr
    model
 where
  mkStakeDistr =
    Map.map Model.rationalToStake

  model stakeDistr targetCommitteeSize =
    let (persistentSeats, numNonPersistentSeats, _) =
          Model.weightedFaitAccompliPersistentSeats
            (fromIntegral targetCommitteeSize)
            stakeDistr
     in ( fromIntegral (Map.size persistentSeats)
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
  -- NOTE: we don't seem to have an easy way to convert the input hash into its
  -- corresponding 'KeyHash StakePool', so here we are just recreating a new one
  -- derived from the input string. This is fine for our purposes since we don't
  -- inspect the actual pool IDs in the implementation, and we only rely on them
  -- being unique, which they should be as long as the input strings are unique.
  mkStakeDistr =
    WFA.mkExtWFAStakeDistr
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

  impl stakeDistr targetCommitteeSize = do
    let totalSeats = WFA.TargetCommitteeSize (fromIntegral targetCommitteeSize)
    case WFA.weightedFaitAccompliSplitSeats stakeDistr totalSeats of
      Left err ->
        error $ "weightedFaitAccompliSplitSeats failed with error: " <> show err
      Right (persistentSeats, nonPersistentSeats, _, _) ->
        ( fromIntegral (WFA.unPersistentCommitteeSize persistentSeats)
        , fromIntegral (WFA.unNonPersistentCommitteeSize nonPersistentSeats)
        )
