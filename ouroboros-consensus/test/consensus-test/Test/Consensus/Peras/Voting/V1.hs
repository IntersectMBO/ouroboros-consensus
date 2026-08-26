{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Peras.Voting.V1 (tests) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError, knownNonZeroCoin)
import Cardano.Ledger.Hashes (StakePool)
import Cardano.Ledger.Keys (KeyHash, toVRFVerKeyHash)
import Cardano.Ledger.State (BlsKey (..), IndividualPoolStake (..), PoolDistr (..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Ouroboros.Consensus.Committee.Crypto.BLS (KeyRole (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.Types (PoolId (..))
import Ouroboros.Consensus.Peras.Voting.V1 (extractPerasStakeDistrAndPublicKeys)
import Test.QuickCheck
  ( Gen
  , Property
  , counterexample
  , cover
  , elements
  , forAll
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras.Common
  ( NonEmptyListWithUniqueIds (..)
  , genNonEmptyListWithUniqueIds
  , genPoolId
  )
import Test.Util.Peras.V1 (genPrivateKey)
import Test.Util.TestEnv (adjustQuickCheckTests)

data KeyCase = NoKey | HasKey
  deriving (Show, Eq, Bounded, Enum)

genBlsKeyFor :: KeyHash StakePool -> Gen BlsKey
genBlsKeyFor stakePoolHash = do
  sk <- genPrivateKey (Proxy @POP)
  let pk = BLS.derivePublicKey sk
      pop = BLS.createProofOfPossession sk stakePoolHash
  pure
    BlsKey
      { blsPubKey = BLS.rawPublicKey pk
      , blsPossessionProof = BLS.rawProofOfPossession pop
      }

mkStake :: StrictMaybe BlsKey -> IndividualPoolStake
mkStake blsKey =
  IndividualPoolStake
    { individualPoolStake = 1
    , individualTotalPoolStake = compactCoinOrError (Coin 1)
    , individualPoolStakeVrf = dummyVrf
    , individualPoolStakeBls = blsKey
    }
 where
  dummyVrf =
    toVRFVerKeyHash
      . Hash.castHash
      . Hash.hashWith id
      $ ("ledgerkeys-test-vrf" :: BS.ByteString)

genPoolEntry :: Gen (PoolId, KeyCase, IndividualPoolStake)
genPoolEntry = do
  poolId <- genPoolId
  keyCase <- elements [minBound .. maxBound]
  stake <- case keyCase of
    NoKey -> pure (mkStake SNothing)
    HasKey -> mkStake . SJust <$> genBlsKeyFor (unPoolId poolId)
  pure (poolId, keyCase, stake)

prop_extractPerasStakeDistrAndPublicKeys :: Property
prop_extractPerasStakeDistrAndPublicKeys =
  forAll
    (genNonEmptyListWithUniqueIds (\(poolId, _, _) -> poolId) genPoolEntry)
    $ \(NonEmptyListWithUniqueIds entries') -> do
      let entries = NonEmpty.toList entries'
      let poolDistr =
            PoolDistr
              { unPoolDistr = Map.fromList [(unPoolId poolId, stake) | (poolId, _, stake) <- entries]
              , pdTotalActiveStake = knownNonZeroCoin @1
              }
      let expectedPoolIds =
            Set.fromList [poolId | (poolId, HasKey, _) <- entries]
      let result =
            extractPerasStakeDistrAndPublicKeys poolDistr
      let hasCase keyCase =
            any (\(_, keyCase', _) -> keyCase' == keyCase) entries
      let coverageLabels =
            [ (NoKey, "contains a pool with no registered key")
            , (HasKey, "contains a pool with a registered key")
            ]
      foldr
        (\(keyCase, label) -> cover 1 (hasCase keyCase) label)
        (counterexample (show entries) $ Map.keysSet result === expectedPoolIds)
        coverageLabels

tests :: TestTree
tests =
  testGroup
    "V1"
    [ adjustQuickCheckTests (* 10) $
        testProperty
          "extractPerasStakeDistrAndPublicKeys includes exactly the pools with a registered key"
          prop_extractPerasStakeDistrAndPublicKeys
    ]
