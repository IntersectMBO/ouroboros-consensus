{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions for the voting committee tests.
module Test.Consensus.Committee.Utils
  ( -- * General utilities
    mkPoolId
  , unfairWFATiebreaker

    -- * QuickCheck generators
  , genEpochNonce
  , genPositiveStake
  , genPools

    -- * Property helpers
  , eqWithShowCmp
  , onError
  , mkBucket

    -- * Tabulation helpers
  , tabulateNumPools
  , tabulatePoolStake
  ) where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import Cardano.Ledger.BaseTypes (Nonce (..), mkNonceFromNumber)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Keys as SL
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.Committee.WFA (WFATiebreaker (..))
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , choose
  , counterexample
  , elements
  , frequency
  , tabulate
  , vectorOf
  )
import Test.Util.QuickCheck (geometric)

-- * General utilities

-- | Create a pool ID from an arbitrary string of any length.
--
-- NOTE: we are assuming that this function preserves uniqueness.
mkPoolId :: String -> PoolId
mkPoolId str =
  PoolId
    . SL.hashKey
    . SL.VKey
    . SL.deriveVerKeyDSIGN
    . SL.genKeyDSIGN
    . SL.mkSeedFromBytes
    . fromString
    $ paddedStr
 where
  paddedStr
    | length str >= neededBytes = take neededBytes str
    | otherwise = str <> replicate (neededBytes - length str) '0'

  neededBytes = 32

-- | An unfair tie-breaker that compares pool IDs lexicographically.
unfairWFATiebreaker :: WFATiebreaker
unfairWFATiebreaker =
  WFATiebreaker compare

-- * QuickCheck generators

-- | Generate a random nonce for testing purposes.
genEpochNonce :: Gen Nonce
genEpochNonce =
  frequency
    [ (1, pure NeutralNonce)
    , (9, mkNonceFromNumber <$> arbitrary)
    ]

-- | Generate a positive stake value using a geometric distribution.
genPositiveStake :: Gen LedgerStake
genPositiveStake =
  LedgerStake
    . toRational
    . (+ 1)
    <$> geometric 0.25

-- | Generate an alphanumeric string of length 8.
-- | Generate a non-empty map of pools with crypto keys and varying stakes.
--
-- NOTE: the generator ensures at least one pool has positive stake.
genPools ::
  -- | Maximum number of pools to generate
  Int ->
  -- | Keypair generator
  Gen (privateKey, publicKey) ->
  Gen (Map PoolId (privateKey, publicKey, LedgerStake))
genPools maxPools genKeyPair = do
  numPools <-
    choose (1, maxPools)
  numPoolsWithZeroStake <-
    choose (0, numPools - 1)
  poolsWithZeroStake <-
    vectorOf
      numPoolsWithZeroStake
      (genOnePool (pure (LedgerStake 0)))
  poolsWithPositiveStake <-
    vectorOf
      (numPools - numPoolsWithZeroStake)
      (genOnePool genPositiveStake)
  pure $
    Map.fromList (poolsWithZeroStake <> poolsWithPositiveStake)
 where
  genOnePool genStake = do
    poolId <- alphaNumString
    (privateKey, publicKey) <- genKeyPair
    stake <- genStake
    pure (mkPoolId poolId, (privateKey, publicKey, stake))

  alphaNumString =
    vectorOf 8 $
      elements $
        ['a' .. 'z']
          <> ['A' .. 'Z']
          <> ['0' .. '9']

-- * Property helpers

-- | Check equality using a custom comparison and show function.
eqWithShowCmp ::
  -- | Custom show function
  (a -> String) ->
  -- | Custom equality function
  (a -> a -> Bool) ->
  -- | First value
  a ->
  -- | Second value
  a ->
  Property
eqWithShowCmp showValue eqValue x y =
  counterexample (showValue x <> interpret res <> showValue y) res
 where
  res = eqValue x y
  interpret True = " == "
  interpret False = " /= "

-- | Handle 'Either' errors by converting them to values.
onError :: Either err a -> (err -> a) -> a
onError action onLeft =
  case action of
    Left err -> onLeft err
    Right val -> val

-- | Create a bucketized label for tabulation.
mkBucket ::
  -- | Bucket size
  Integer ->
  -- | Value to bucket
  Integer ->
  String
mkBucket size val
  | val <= 0 =
      "<= 0"
  | otherwise =
      "[ " <> show lo <> ", " <> show hi <> " )"
 where
  lo = (val `div` size) * size
  hi = lo + size

-- * Tabulation helpers

-- | Tabulate the number of pools in a test run.
tabulateNumPools ::
  Map
    PoolId
    ( privateKey
    , publicKey
    , LedgerStake
    ) ->
  Property ->
  Property
tabulateNumPools pools =
  tabulate
    "Number of pools"
    [mkBucket 100 (fromIntegral (Map.size pools))]

-- | Tabulate whether a pool has positive or zero stake.
tabulatePoolStake ::
  LedgerStake ->
  Property ->
  Property
tabulatePoolStake (LedgerStake stake) =
  tabulate
    "Pool stake"
    [ if stake > 0
        then "> 0"
        else "== 0"
    ]
