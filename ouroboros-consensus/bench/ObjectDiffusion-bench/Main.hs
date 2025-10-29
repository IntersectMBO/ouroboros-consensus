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
import GHC.Generics (Generic)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision qualified as OD
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.TestUtils qualified as OD
import Test.QuickCheck (Arbitrary (..), generate)
import Test.QuickCheck.Gen (vectorOf)
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
                (genToNF $ vectorOf 1_000 $ OD.genDecisionContext 10 50 doId Nothing)
                ( \contexts ->
                    bench "makeDecisions: 1000 decisions with (10 pairs, 50 objects) each" $
                      nf (fmap makeVoteDiffusionDecisions) contexts
                )
            , env
                (genToNF $ vectorOf 1_000 $ OD.genDecisionContext 100 500 doId Nothing)
                ( \contexts ->
                    bench "makeDecisions: 1000 decisions with (100 pairs, 500 objects) each" $
                      nf (fmap makeVoteDiffusionDecisions) contexts
                )
            , env
                (genToNF $ vectorOf 1_000 $ OD.genDecisionContext 1_000 5_000 doId Nothing)
                ( \contexts ->
                    bench "makeDecisions: 1000 decisions with (1000 pairs, 5000 objects) each" $
                      nf (fmap makeVoteDiffusionDecisions) contexts
                )
            ]
        , bgroup "CertDiffusion" []
        ]
    ]
 where
  genToNF gen = do
    x <- generate gen
    evaluate $ rnf x
    pure $! x

  makeVoteDiffusionDecisions decisionContext =
    OD.makeDecisions @DummyPeerAddr @DummyObjectId @DummyObject decisionContext
