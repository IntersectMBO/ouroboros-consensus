{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test properties relating Peras and voting committee types.
module Test.Consensus.Peras.Voting.Committee
  ( tests
  ) where

import Cardano.Ledger.BaseTypes (SlotNo (..))
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Block.RealPoint (RealPoint (..), realPointToPoint)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.EveryoneVotes (EveryoneVotes)
import Ouroboros.Consensus.Committee.WFALS (WFALS)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Ouroboros.Consensus.Peras.Voting.Committee
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  )
import Test.Consensus.Peras.Util
  ( genPerasCert
  , genPerasVote
  , perasCertContainsOnlyPersistentVotes
  , perasVoteIsPersistent
  , tabulatePerasCert
  , tabulatePerasVote
  )
import Test.Ouroboros.Storage.TestBlock (TestBlock, TestHeaderHash (..))
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , Testable (..)
  , counterexample
  , forAll
  , frequency
  , tabulate
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- | Generate an arbitrary 'Point' for 'TestBlock'.
-- We reuse 'TestBlock' from "Test.Ouroboros.Storage.TestBlock" which already
-- provides 'ConvertRawHash'.
genTestPoint :: Gen (Point TestBlock)
genTestPoint = do
  slotNo <- SlotNo <$> arbitrary
  hash <- TestHeaderHash <$> arbitrary
  pure $ realPointToPoint $ RealPoint slotNo hash

tests :: TestTree
tests =
  testGroup
    "Roundtrip for Peras types via abstract committee types"
    [ adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasVote via WFALS" $
          prop_roundtrip_vote
            (Proxy @(V1.PerasVote TestBlock))
            (Proxy @WFALS)
            -- WFALS supports both persistent and non-persistent of votes
            (const True)
            -- Generate both persistent and non-persistent votes
            (genPerasVote genTestPoint True)
            tabulatePerasVote
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasVote via EveryoneVotes" $
          prop_roundtrip_vote
            (Proxy @(V1.PerasVote TestBlock))
            (Proxy @EveryoneVotes)
            -- EveryoneVotes only supports non-persistent votes
            perasVoteIsPersistent
            -- Generate both persistent and non-persistent votes to trigger
            -- conversion errors in a reasonable amount of tests
            (genPerasVote genTestPoint =<< frequency [(2, pure True), (1, pure False)])
            tabulatePerasVote
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasCert via WFALS" $
          prop_roundtrip_cert
            (Proxy @(V1.PerasCert TestBlock))
            (Proxy @WFALS)
            -- WFALS supports certs with both persistent and non-persistent votes
            (const True)
            -- Generate certs with both persistent and non-persistent votes
            (genPerasCert genTestPoint True)
            tabulatePerasCert
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasCert via EveryoneVotes" $
          prop_roundtrip_cert
            (Proxy @(V1.PerasCert TestBlock))
            (Proxy @EveryoneVotes)
            -- EveryoneVotes only supports certs with persistent votes
            perasCertContainsOnlyPersistentVotes
            -- Only sometimes generate certs with non-persistent votes to
            -- trigger conversion errors in a reasonable amount of tests
            (genPerasCert genTestPoint =<< frequency [(2, pure False), (1, pure True)])
            tabulatePerasCert
    ]

-- * Properties

-- | Test that converting a concrete Peras vote to an committee vote and back
-- again results in the original Peras vote.
--
-- NOTE: this takes a predicate to determine whether triggering an exception is
-- a test failure or an expected outcome for the given voting committee scheme.
prop_roundtrip_vote ::
  forall vote crypto committee.
  ( Show vote
  , Eq vote
  , PerasVoteCompatibleWithVotingCommittee vote crypto committee
  ) =>
  Proxy vote ->
  Proxy committee ->
  (vote -> Bool) ->
  Gen vote ->
  (vote -> Property -> Property) ->
  Property
prop_roundtrip_vote _ _ shouldPass gen tabulateValue =
  forAll gen $ \vote -> do
    tabulateValue vote $
      case fromPerasVote vote of
        Left err
          | shouldPass vote ->
              counterexample
                ( unlines
                    [ "fromPerasVote failed with:"
                    , show err
                    , "Original vote:"
                    , show vote
                    ]
                )
                False
          | otherwise ->
              tabulateOutcome "Fails as expected" $
                property True
        Right (committeeVote :: Committee.Vote crypto committee) ->
          case toPerasVote committeeVote of
            Left err ->
              counterexample
                ( unlines
                    [ "toPerasVote failed with:"
                    , show err
                    ]
                )
                $ property False
            Right vote' ->
              tabulateOutcome "Roundtrips successfully"
                . counterexample
                  ( unlines
                      [ "Original vote:"
                      , show vote
                      , "Roundtripped vote:"
                      , show vote'
                      ]
                  )
                $ vote === vote'

-- | Test that converting a concrete Peras cert to an committee cert and back
-- again results in the original Peras cert.
--
-- NOTE: this takes a predicate to determine whether triggering an exception is
-- a test failure or an expected outcome for the given voting committee scheme.
prop_roundtrip_cert ::
  forall cert crypto committee.
  ( Show cert
  , Eq cert
  , PerasCertCompatibleWithVotingCommittee cert crypto committee
  ) =>
  Proxy cert ->
  Proxy committee ->
  (cert -> Bool) ->
  Gen cert ->
  (cert -> Property -> Property) ->
  Property
prop_roundtrip_cert _ _ shouldPass gen tabulateValue =
  forAll gen $ \cert -> do
    tabulateValue cert $
      case fromPerasCert cert of
        Left err
          | shouldPass cert ->
              counterexample
                ( unlines
                    [ "fromPerasCert failed with:"
                    , show err
                    , "Original cert:"
                    , show cert
                    ]
                )
                $ property False
          | otherwise ->
              tabulateOutcome "Fails as expected" $
                property True
        Right (committeeCert :: Committee.Cert crypto committee) ->
          case toPerasCert committeeCert of
            Left err ->
              counterexample
                ( unlines
                    [ "toPerasCert failed with:"
                    , show err
                    ]
                )
                False
            Right cert' ->
              tabulateOutcome "Roundtrips successfully"
                . counterexample
                  ( unlines
                      [ "Original cert:"
                      , show cert
                      , "Roundtripped cert:"
                      , show cert'
                      ]
                  )
                $ cert === cert'

tabulateOutcome :: String -> Property -> Property
tabulateOutcome outcome = tabulate "Outcome" [outcome]
