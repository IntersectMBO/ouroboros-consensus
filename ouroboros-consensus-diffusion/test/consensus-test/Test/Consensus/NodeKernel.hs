-- | Properties of how the node kernel runs its components.
module Test.Consensus.NodeKernel (tests) where

import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( maxAnnouncementAgeRecv
  , maxAnnouncementAgeSend
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State (voteLateGrace)
import Ouroboros.Consensus.NodeKernel (leiosDeadlinePrunerInterval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, conjoin, counterexample, once, testProperty)

tests :: TestTree
tests =
  testGroup
    "NodeKernel"
    [ testProperty
        "the deadline pruner runs far more often than a downstream peer's grace period"
        prop_leiosDeadlinePrunerInterval
    ]

-- | The node relays a notification at most 'leiosDeadlinePrunerInterval' after it
-- is no longer worth relaying, which must leave most of a downstream peer's
-- grace period for transmission and queueing: 'voteLateGrace' for a vote, and
-- the margin between 'maxAnnouncementAgeSend' and 'maxAnnouncementAgeRecv' for
-- an announcement.
prop_leiosDeadlinePrunerInterval :: Property
prop_leiosDeadlinePrunerInterval =
  once . conjoin $
    [ counterexample "the vote grace period" $
        leiosDeadlinePrunerInterval `farBelow` voteLateGrace
    , counterexample "the announcement grace period" $
        leiosDeadlinePrunerInterval `farBelow` (maxAnnouncementAgeRecv - maxAnnouncementAgeSend)
    ]
 where
  -- Leaving at least 90% of the grace period to transmission and queueing.
  farBelow a b = 10 * a <= b
