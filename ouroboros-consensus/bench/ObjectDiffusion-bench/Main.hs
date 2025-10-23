{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains benchmarks for Peras Object diffusion decision logic
-- as implemented by the by the function
-- 'Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision.makeDecision'
module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Hashable (Hashable)
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)
import System.Random.SplitMix qualified as SM
import Test.Tasty.Bench
import Test.QuickCheck (Arbitrary (..))

import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision qualified as OD

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
  deriving (Generic, NFData)

instance Arbitrary DummyObject where
  arbitrary = DummyObject <$> arbitrary <*> arbitrary

main :: IO ()
main =
  defaultMain
    [ bgroup "ouroboros-consensus:ObjectDiffusion"
      [ bgroup "VoteDiffusion"
          [ env
            (do let a = OD.mkDecisionContext (SM.mkSMGen 123) 10
                evaluate (rnf a)
                traceMarkerIO "evaluated decision context"
                return a
            )
            (\a -> bench "makeDecisions: 10" $
              nf makeVoteDiffusionDecision a
            )
        , env
            (do let a = OD.mkDecisionContext (SM.mkSMGen 456) 100
                evaluate (rnf a)
                traceMarkerIO "evaluated decision context"
                return a
            )
            (\a -> bench "makeDecisions: 100" $
              nf makeVoteDiffusionDecision a
            )
        , env
            (do let a = OD.mkDecisionContext (SM.mkSMGen 789) 1_000
                evaluate (rnf a)
                traceMarkerIO "evaluated decision context"
                return a
            )
            (\a -> bench "makeDecisions: 1_000" $
              nf makeVoteDiffusionDecision a
            )
        ]
      , bgroup "CertDiffusion" []
      ]
    ]
  where
    -- TODO: We probably want to use the decision policy for vote/cert diffusion
    -- instead of an arbitrary one.
    makeVoteDiffusionDecision = \OD.DecisionContext 
      { OD.dcRng
      , OD.dcHasObject
      , OD.dcDecisionPolicy
      , OD.dcGlobalState
      , OD.dcPrevDecisions
      } -> OD.makeDecisions @DummyPeerAddr @DummyObjectId @DummyObject
        dcRng
        dcHasObject
        dcDecisionPolicy
        dcGlobalState
        dcPrevDecisions
