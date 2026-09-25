-- | Ledger DB tests.
--
-- The ledger DB consists of two subcomponents: an in-memory component, which is
-- pure Haskell (no IO anywhere) and so can be tested using normal property
-- tests, and the on-disk component, which is tested with a model based test.
module Test.Ouroboros.Storage.LedgerDB (tests) where

import qualified Test.Ouroboros.Storage.LedgerDB.Serialisation as Serialisation
import qualified Test.Ouroboros.Storage.LedgerDB.SnapshotPolicySanityCheck as SnapshotPolicySanityCheck
import qualified Test.Ouroboros.Storage.LedgerDB.Snapshots as Snapshots
import qualified Test.Ouroboros.Storage.LedgerDB.StateMachine as StateMachine
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "LedgerDB"
    [ Serialisation.tests
    , Snapshots.tests
    , SnapshotPolicySanityCheck.tests
    , StateMachine.tests
    ]
