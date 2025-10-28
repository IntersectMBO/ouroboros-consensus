{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains benchmarks for Peras Object diffusion decision logic
-- as implemented by the by the function
-- 'Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision.makeDecision'
module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Hashable (Hashable)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision qualified as OD
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.TestUtils qualified as OD
import System.Random (uniformR)
import System.Random.SplitMix (initSMGen)
import Test.QuickCheck (Arbitrary (..), generate)
import Test.Tasty.Bench

-- TODO: We will probably want to use the actual types used in vote/cert diffusion,
-- instead of placeholders.
newtype DummyPeerAddr = DummyPeerAddr Int
  deriving (Eq, Ord, Generic, NFData)

instance Arbitrary DummyPeerAddr where
  arbitrary = DummyPeerAddr <$> arbitrary

newtype DummyObjectId = DummyObjectId Int
  deriving (Eq, Ord, Generic, Hashable, NFData)

instance Arbitrary DummyObjectId where
  arbitrary = DummyObjectId <$> arbitrary

data DummyObject = DummyObject
  { doId :: DummyObjectId
  , doPayload :: ()
  }
  deriving (Eq, Ord, Generic, Hashable, NFData)

instance Arbitrary DummyObject where
  arbitrary = DummyObject <$> arbitrary <*> arbitrary

-- TODO: We should probably use specific policies that are well suited to the
-- number of peers and objects.

const_POOL_SIZE :: Int
const_POOL_SIZE = 100_000

-- | Create a benchmark that uses a pool of environments that are all created
-- during setup time. Each benchmark iteration randomly selects one of the
-- environments from the pool to run the benchmark function with.
randomEnvFromPool :: (NFData t, NFData b) => String -> IO t -> (t -> IO b) -> Benchmark
randomEnvFromPool name mkSingleEnv benchFn =
  env
    ( do
        pool <- Vector.fromList <$> for [0 .. const_POOL_SIZE - 1] (\_ -> mkSingleEnv)
        evaluate (rnf pool)

        !rng <- initSMGen
        !rngRef <- newIORef rng

        pure $! (pool, rngRef)
    )
    ( \(pool, rngRef) ->
        bench name $
          nfAppIO
            ( \() -> do
                i <- atomicModifyIORef' rngRef $ \rng ->
                  let (i, rng') = uniformR (0, const_POOL_SIZE - 1) rng
                   in (rng', i)
                benchFn (pool Vector.! i)
            )
            ()
    )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "ouroboros-consensus:ObjectDiffusion"
        [ bgroup
            "VoteDiffusion"
            [ randomEnvFromPool
                "makeDecisions: 10"
                (generate $ OD.genDecisionContext 10 50 doId Nothing)
                (pure . makeVoteDiffusionDecision)
            , randomEnvFromPool
                "makeDecisions: 100"
                (generate $ OD.genDecisionContext 100 500 doId Nothing)
                (pure . makeVoteDiffusionDecision)
            , randomEnvFromPool
                "makeDecisions: 1_000"
                (generate $ OD.genDecisionContext 1_000 5_000 doId Nothing)
                (pure . makeVoteDiffusionDecision)
            ]
        , bgroup "CertDiffusion" []
        ]
    ]
 where
  -- TODO: We probably want to use the decision policy for vote/cert diffusion
  -- instead of an arbitrary one.
  makeVoteDiffusionDecision decisionContext =
    OD.makeDecisions @DummyPeerAddr @DummyObjectId @DummyObject decisionContext
