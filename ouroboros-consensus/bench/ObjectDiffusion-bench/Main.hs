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
import Debug.Trace (traceMarkerIO)
import GHC.Generics (Generic)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision qualified as OD
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.TestUtils qualified as OD
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

main :: IO ()
main =
  defaultMain
    [ bgroup
        "ouroboros-consensus:ObjectDiffusion"
        [ bgroup
            "VoteDiffusion"
            [ env
                ( do
                    a <- generate $ OD.genDecisionContext 10 50 doId Nothing
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                ( \a ->
                    bench "makeDecisions: 10" $
                      nf makeVoteDiffusionDecision a
                )
            , env
                ( do
                    a <- generate $ OD.genDecisionContext 100 500 doId Nothing
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                ( \a ->
                    bench "makeDecisions: 100" $
                      nf makeVoteDiffusionDecision a
                )
            , env
                ( do
                    a <- generate $ OD.genDecisionContext 1_000 5_000 doId Nothing
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                ( \a ->
                    bench "makeDecisions: 1_000" $
                      nf makeVoteDiffusionDecision a
                )
            ]
        , bgroup "CertDiffusion" []
        ]
    ]
 where
  -- TODO: We probably want to use the decision policy for vote/cert diffusion
  -- instead of an arbitrary one.
  makeVoteDiffusionDecision decisionContext =
    OD.makeDecisions @DummyPeerAddr @DummyObjectId @DummyObject decisionContext
