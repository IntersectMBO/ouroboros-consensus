{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of the LSM backend's ledger tables handles.
module Test.Ouroboros.Storage.LedgerDB.V2.LSM (tests) where

import Control.ResourceRegistry (runWithTempRegistry)
import Control.Tracer (nullTracer)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Word (Word64)
import Ouroboros.Consensus.Block (SlotNo (..), pattern BlockPoint)
import Ouroboros.Consensus.Ledger.Abstract (withLedgerTables)
import Ouroboros.Consensus.Ledger.Tables (KeysMK (..), LedgerTables (..), ValuesMK (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( createAndPopulateStateRefFromGenesis
  , mkResources
  , releaseResources
  )
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM (DiskCachePolicy (..), stdMkBlockIOFS)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as LedgerSeq
import System.FS.API (SomeHasFS (..), mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.IO.Temp (withSystemTempDirectory)
import Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock
  ( TValue (..)
  , TestBlock
  , Token (..)
  , genesis
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util.TestBlock (TestHash (..))

tests :: TestTree
tests =
  testGroup
    "V2.LSM"
    [ testCase "reads more keys than one lsm-tree lookup batch takes" readManyKeys
    ]

-- | lsm-tree indexes a lookup batch's keys with 16 bits, so a batch over
-- 65,536 keys that hit disk runs reads out of bounds.
readManyKeys :: IO ()
readManyKeys =
  withSystemTempDirectory "lsm" $ \fp -> do
    let fs = SomeHasFS (ioHasFS (MountPoint fp))
        args = LSM.LSMArgs (mkFsPath ["lsm"]) Nothing 0 DiskCacheAll (stdMkBlockIOFS fp)
    res <- runWithTempRegistry $ (\r -> (r, ())) <$> mkResources (Proxy @TestBlock) nullTracer args fs
    ref <-
      createAndPopulateStateRefFromGenesis nullTracer res $
        genesis `withLedgerTables` LedgerTables (ValuesMK (Map.fromSet (const (TValue ())) tokens))
    LedgerTables (ValuesMK found) <-
      LedgerSeq.read
        (LedgerSeq.tables ref)
        (forgetLedgerTables (LedgerSeq.state ref))
        (LedgerTables (KeysMK tokens))
    Map.size found @?= numTokens
    LedgerSeq.close (LedgerSeq.tables ref)
    releaseResources (Proxy @TestBlock) res
 where
  numTokens = 100_000
  tokens =
    Set.fromList
      [ Token (BlockPoint (SlotNo i) (TestHash (i :| [])))
      | i <- [1 .. fromIntegral numTokens :: Word64]
      ]
