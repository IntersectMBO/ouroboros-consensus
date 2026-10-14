{-# LANGUAGE ImportQualifiedPost #-}

-- | Benchmarks for storing certificates in the 'PerasImmutableCertDB' on the
--   real file system.
--
--   A syncing node stores the certificates boosting immutable blocks in
--   batches, as they are garbage-collected from the volatile 'PerasCertDB'.
--   We measure the time it takes to store a batch of 'batchSize' certificates
--   for fresh rounds, so that it can be compared against the rate at which a
--   syncing node collects certificates (at most one per Peras round).
--
--   Every iteration stores certificates for new rounds, so the database (and
--   its directory) grows as the benchmark runs, as it would while syncing.
--
--   The database is created in a temporary directory. As @/tmp@ is often a
--   @tmpfs@, set @TMPDIR@ to a directory on the disk of interest to obtain
--   meaningful results.
module Main (main) where

import Control.Monad (forM_, void)
import Control.Tracer (nullTracer)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set.NonEmpty qualified as NESet
import Data.Word (Word64)
import Ouroboros.Consensus.Block
  ( PerasRoundNo (..)
  , PerasWeight (..)
  , ValidatedPerasCert (..)
  , blockPoint
  )
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..), WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import Ouroboros.Consensus.Storage.PerasImmutableCertDB
  ( PerasImmutableCertDB
  , PerasImmutableCertDbArgs (..)
  , PerasImmutableCertDbIntegrityCheckPolicy (..)
  )
import Ouroboros.Consensus.Storage.PerasImmutableCertDB qualified as PerasImmutableCertDB
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.IO.Temp (withSystemTempDirectory)
import Test.Ouroboros.Storage.TestBlock (TestBlock, TestBody (..))
import Test.Ouroboros.Storage.TestBlock qualified as TestBlock
import Test.Tasty.Bench

-- | Number of certificates stored per benchmark iteration.
batchSize :: Word64
batchSize = 1000

-- | Number of voters in each certificate, determining its size on disk.
nbVoters :: Word64
nbVoters = 100

main :: IO ()
main =
  withSystemTempDirectory "PerasImmutableCertDB-bench" $ \dir -> do
    db <-
      PerasImmutableCertDB.openDB
        PerasImmutableCertDbArgs
          { picdbaCodecConfig = TestBlock.TestBlockCodecConfig
          , picdbaHasFS = SomeHasFS $ ioHasFS (MountPoint dir)
          , picdbaTracer = nullTracer
          , picdbaIntegrityCheckPolicy = CheckOnRead
          }
    nextRound <- newIORef 0
    Test.Tasty.Bench.defaultMain
      [ bench ("addCert of " <> show batchSize <> " certificates for fresh rounds") $
          whnfIO (addBatch db nextRound)
      ]
    PerasImmutableCertDB.closeDB db

addBatch :: PerasImmutableCertDB IO TestBlock -> IORef Word64 -> IO ()
addBatch db nextRound = do
  firstRound <- atomicModifyIORef' nextRound $ \r -> (r + batchSize, r)
  forM_ [firstRound .. firstRound + batchSize - 1] $ \r ->
    void $ PerasImmutableCertDB.addCert db (mkCert (PerasRoundNo r))

mkCert :: PerasRoundNo -> WithArrivalTime (ValidatedPerasCert TestBlock)
mkCert roundNo =
  WithArrivalTime
    { getArrivalTime = RelativeTime 0
    , forgetArrivalTime =
        ValidatedPerasCert
          { vpcCert =
              MockPerasCert
                { mockCertRound = roundNo
                , mockCertBlock = blockPoint boostedBlock
                , mockCertVoters =
                    NESet.fromList $
                      NonEmpty.fromList
                        [PerasSeatIndex (fromIntegral i) | i <- [1 .. nbVoters]]
                }
          , vpcCertBoost = PerasWeight 15
          }
    }
 where
  boostedBlock =
    TestBlock.firstBlock 0 TestBody{tbForkNo = 0, tbIsValid = True, tbPerasCert = Nothing}
