-- | Ledger DB tests.
--
-- The ledger DB consists of two subcomponents: an in-memory component, which is
-- pure Haskell (no IO anywhere) and so can be tested using normal property
-- tests, and the on-disk component, which is tested with a model based test.
--
module Test.Ouroboros.Storage.LedgerDB (tests) where

import qualified Test.Ouroboros.Storage.LedgerDB.Serialisation as Serialisation
import qualified Test.Ouroboros.Storage.LedgerDB.SnapshotPolicy as SnapshotPolicy
import qualified Test.Ouroboros.Storage.LedgerDB.Snapshots as Snapshots
import qualified Test.Ouroboros.Storage.LedgerDB.StateMachine as StateMachine
import qualified Test.Ouroboros.Storage.LedgerDB.V1.BackingStore as BackingStore
import qualified Test.Ouroboros.Storage.LedgerDB.V1.DbChangelog as DbChangelog
import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "LedgerDB" [
      testGroup "V1" [
          BackingStore.tests
        , DbChangelog.tests
    ]
      -- Independent of the LedgerDB implementation
    , SnapshotPolicy.tests
    , Serialisation.tests
    , Snapshots.tests
      -- Tests both V1 and V2
    , StateMachine.tests
    ]
