-- | Properties of how the node kernel runs its components.
module Test.Consensus.NodeKernel (tests) where

import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( maxAnnouncementAgeRecv
  , maxAnnouncementAgeSend
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State (voteLateGrace)
import Ouroboros.Consensus.NodeKernel (leiosDeadlinePruneCadence)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, conjoin, counterexample, once, testProperty)

tests :: TestTree
tests =
  testGroup
    "NodeKernel"
    [ testProperty
        "the deadline pruner runs far more often than a downstream peer's grace"
        prop_leiosDeadlinePruneCadence
    ]

-- | The node relays a notification at most 'leiosDeadlinePruneCadence' after it
-- is no longer worth relaying, which must leave a downstream peer's grace for
-- transmission and queueing: 'voteLateGrace' for a vote, and the margin between
-- 'maxAnnouncementAgeSend' and 'maxAnnouncementAgeRecv' for an announcement.
prop_leiosDeadlinePruneCadence :: Property
prop_leiosDeadlinePruneCadence =
  once . conjoin $
    [ counterexample "the vote grace" $
        leiosDeadlinePruneCadence `farBelow` voteLateGrace
    , counterexample "the announcement grace" $
        leiosDeadlinePruneCadence `farBelow` (maxAnnouncementAgeRecv - maxAnnouncementAgeSend)
    ]
 where
  farBelow a b = 10 * a <= b
