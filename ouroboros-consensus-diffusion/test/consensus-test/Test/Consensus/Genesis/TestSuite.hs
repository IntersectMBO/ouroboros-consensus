{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A 'TestSuite' data structure for quick access to 'ConformanceTest' values.
-- It encodes a hierarchical nested structure allowing it to compile into a
-- tasty 'TestTree'.
-- It's purpose is interfacing between property test execution and the
-- conformance testing harness.
module Test.Consensus.Genesis.TestSuite
  ( -- * 'SmallKey' class
    -- $deriveSmallkey
    Generic
  , Generically (..)
  , SmallKey

    -- * 'TestSuite' API
  , TestSuite
  , at
  , getTest
  , group
  , grouping
  , mkTestSuite
  , newTestSuite
  , toTestTree
  ) where

import Data.Foldable (toList)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo (..))
import GHC.Generics (Generic, Generically (..))
import Ouroboros.Consensus.Block
  ( BlockSupportsDiffusionPipelining
  , ConvertRawHash
  , Header
  )
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory)
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Ledger.Inspect (InspectLedger)
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import Ouroboros.Consensus.Storage.LedgerDB.API
  ( CanUpgradeLedgerTables
  )
import Ouroboros.Consensus.Util.Condense (Condense, CondenseList)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)
import Test.Consensus.Genesis.Setup
  ( ConformanceTest (..)
  , runConformanceTest
  )
import Test.Consensus.Genesis.TestSuite.SmallKey (SmallKey, getAllKeys)
import Test.Consensus.PeerSimulator.StateView (StateView)
import Test.Consensus.PointSchedule (HasPointScheduleTestParams)
import Test.Consensus.PointSchedule.NodeState (NodeState)
import Test.Tasty (TestTree, testGroup)
import Test.Util.TersePrinting (Terse)

data TestSuiteData blk = TestSuiteData
  { tsPrefix :: [String]
  -- ^ A prefix representing a path through the test group tree.
  -- The convention is to nest by appending, i.e. the head of the prefix
  -- corresponds to the top-level test group.
  , tsTest :: ConformanceTest blk
  -- ^ The test itself.
  }

-- | A @TestSuite blk key@ contains one 'ConformanceTest'@blk@ for each @key@.
newtype TestSuite blk key = TestSuite (Map key (TestSuiteData blk))

-- | Build a 'TestSuite' by looking 'at' 'TestSuiteData', allowing to preserve the
-- hierarchical structure of a previously constructed 'TestSuite'.
--
-- See NOTE [DeriveSmallKey]
mkTestSuite ::
  (Ord key, SmallKey key) =>
  (key -> TestSuiteData blk) ->
  TestSuite blk key
mkTestSuite toData =
  TestSuite . Map.fromList . fmap ((,) <$> id <*> toData) $ getAllKeys

-- | Build a 'TestSuite' from a function mapping a @key@ type to 'ConformanceTest'
-- making all tests top-level.
--
-- See NOTE [DeriveSmallKey]
newTestSuite ::
  (Ord key, SmallKey key) =>
  (key -> ConformanceTest blk) ->
  TestSuite blk key
newTestSuite toConformanceTest =
  let toData k =
        TestSuiteData
          { tsPrefix = []
          , tsTest = toConformanceTest k
          }
   in mkTestSuite toData

-- $deriveSmallKey
--
-- NOTE [DeriveSmallKey]
-- The 'SmallKey' constraint on 'TestSuite' @key@s is meant to be derived
-- @via Generically@ only; because of this, some its class methods are not
-- exported to prevent users of this class from instantiating it for large
-- finite data types (such as 'Int' or 'Word32'), which are likely to flood the
-- memory when constructing a 'TestSuite' because 'allKeys' are used
-- operationally to drive its exhaustive construction. As precaution, product
-- and syntactically-recursive types are forbidden from instanciating it and
-- some large types have been explicitly black-listed.
--
-- The rationale behind the enforced restrictions is that @keys@ are expected
-- to be constructed /primarily/ from user defined enumeration types (i.e.
-- coproducts of nullary constructors) corresponding to single tests
-- module wise, but this is not a hard requirement. For instance, keys can be
-- aggregated into higher order types to define hierarchical 'TestSuite's
-- by means of 'mkTestSuite' and 'at'.

at :: Ord key => TestSuite blk key -> key -> TestSuiteData blk
at (TestSuite m) k = case Map.lookup k m of
  Just t -> t
  Nothing -> error "TestSuite.at: Impossible! A TestSuite is a total map."

getTest :: TestSuiteData blk -> ConformanceTest blk
getTest = tsTest

-- | Appends the given string to the prefix of all tests in the 'TestSuite',
-- effectively grouping them on a `TestTree` of the given name when compiled.
group :: String -> TestSuite blk key -> TestSuite blk key
group name = grouping (const name)

-- | A more general version of 'group' that allows to group tests by a key
-- specific prefix.
grouping :: (key -> String) -> TestSuite blk key -> TestSuite blk key
grouping f (TestSuite m) =
  TestSuite $
    Map.mapWithKey
      (\k testData -> testData{tsPrefix = f k : tsPrefix testData})
      m

{-------------------------------------------------------------------------------
   Compile a TestSuite into a TestTree
-------------------------------------------------------------------------------}

-- | Intermediary representation for a 'TestSuite' to be compiled
-- into a 'TestTree'.
data TestTrie = TestTrie
  { _here :: ![TestTree]
  -- ^ Top level tests (whose prefix ends here).
  , _children :: !(MonoidalMap String TestTrie)
  -- ^ Grouped tests correspond to prefix maps.
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via (Generically TestTrie)

-- | Create a 'TestTrie' with a single value.
mkTestTrie :: [String] -> TestTree -> TestTrie
mkTestTrie pfs t =
  let nest :: String -> Endo TestTrie
      nest pf = Endo $ \tt -> TestTrie [] (MMap.singleton pf tt)
      leaf = TestTrie [t] mempty
   in appEndo (foldMap nest pfs) leaf

-- | Fold a 'TestTrie' into a list of 'TestTree's by recursively
-- rendering each trie node as a 'testGroup'.
render :: TestTrie -> [TestTree]
render (TestTrie here children) =
  here
    <> fmap
      (\(p, tt) -> testGroup p (render tt))
      (MMap.toList children)

-- | Compile a 'TestSuite' into a list of tasty 'TestTree'.
toTestTree ::
  ( Condense (StateView blk)
  , CondenseList (NodeState blk)
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , SerialiseDiskConstraints blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , CanUpgradeLedgerTables LedgerState blk
  , HasPointScheduleTestParams blk
  , Eq (Header blk)
  , Eq blk
  , Terse blk
  , Condense (NodeState blk)
  ) =>
  TestSuite blk key -> [TestTree]
toTestTree (TestSuite m) =
  render $ mconcat $ do
    TestSuiteData{tsPrefix, tsTest} <- toList m
    pure $ mkTestTrie tsPrefix $ runConformanceTest tsTest
