{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for the 'PerasImmutableCertDB' on the real file system, using
-- the production certificate type ('V1.PerasCert', with BLS signatures) of
-- the Dijkstra era.
--
-- They answer two questions:
--
-- * __Storing.__ Does the @certStoringRunner@ keep up with garbage collection
--   on a syncing node, so that the unbounded @CertStoringQueue@ stays short
--   (see its documentation in "Ouroboros.Consensus.Storage.ChainDB.Impl.Background")?
--   We measure the time to 'addCert' a batch of 'storeBatchSize' certificates
--   for fresh rounds into a database already holding a large number of them,
--   and compare the resulting throughput with the rate at which a syncing
--   node collects certificates (at most one per Peras round), see
--   'syncingCertsPerSecond'.
--
-- * __Indexing.__ How much does opening the database cost as it grows? About
--   'roundsPerYear' certificates are added per year. We index databases of
--   increasing sizes at three levels of scrutiny:
--
--     1. reading the file names only (what 'openDB' does with 'CheckOnRead');
--     2. additionally reading every file and checking its CRC32;
--     3. additionally decoding every certificate (what 'openDB' does with
--        'CheckAllOnOpen').
--
--   Decoding a certificate decompresses and subgroup-checks one BLS12-381
--   point for its aggregate signature plus one per non-persistent voter (their
--   VRF outputs). Its cost therefore depends on the composition of the
--   committee, which is why every benchmark is run for several
--   'CertProfile's, ranging from the cheapest to the most expensive
--   certificate.
--
-- At the end, the measured times are extrapolated to time per year of
-- certificates.
--
-- = Caveats
--
-- * Times are wall-clock times (forced with 'WallTime'): the default CPU time
--   of @tasty-bench@ would not account for the time spent waiting for I/O.
--
-- * The databases are created in a temporary directory. As @/tmp@ is often a
--   @tmpfs@, set @TMPDIR@ to a directory on the disk of interest to obtain
--   meaningful results. Make sure there is enough space: see the file sizes
--   reported in the benchmark names, multiplied by the largest entry of
--   'indexSizes' (plus the stored batches).
--
-- * Indexing is measured with a warm page cache, since every benchmark
--   iteration indexes the same files. After a reboot, levels 2 and 3 also pay
--   for reading every file from disk; drop the page cache between runs (e.g.
--   @echo 3 > /proc/sys/vm/drop_caches@) to observe this.
module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (catch, throwIO)
import Control.Monad (forM, forM_, unless, void)
import Control.Tracer (Tracer (..), emit)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word64)
import GHC.Clock (getMonotonicTime)
import Ouroboros.Consensus.Block
  ( PerasRoundNo (..)
  , SlotNo (..)
  , ValidatedPerasCert (..)
  , WithOrigin (..)
  )
import Ouroboros.Consensus.Block.RealPoint (Bytes32RealPoint (..))
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..), WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Crypto.BLS qualified as BLS
import Ouroboros.Consensus.Peras.Cert.V1 qualified as V1
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCryptoAggregateVoteSignature (..)
  , VRFOutput (..)
  )
import Ouroboros.Consensus.Peras.Params
  ( PerasParams (..)
  , PerasQuorumWeightThreshold (..)
  , PerasRoundLength (..)
  , TargetCommitteeSize (..)
  , defaultPerasParams
  , dijkstraPerasRoundLength
  , fromPerasEnabled
  )
import Ouroboros.Consensus.Peras.Types (PerasBoostedBlock (..), PerasSeatIndex (..))
import Ouroboros.Consensus.Peras.Vote.V1 (PerasVoteEligibilityProof (..))
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (..))
import Ouroboros.Consensus.Storage.PerasImmutableCertDB
  ( PerasImmutableCertDB
  , PerasImmutableCertDbArgs (..)
  , PerasImmutableCertDbIntegrityCheckPolicy (..)
  )
import Ouroboros.Consensus.Storage.PerasImmutableCertDB qualified as PerasImmutableCertDB
import Ouroboros.Consensus.Storage.PerasImmutableCertDB.Impl
  ( TraceEvent (..)
  , checkCertFileCRC
  , decodeCertFile
  , displayCertFileError
  , fsPathCertFile
  )
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode)
import System.FS.API (SomeHasFS (..), withFile)
import System.FS.API.Lazy (hGetAll)
import System.FS.API.Types (MountPoint (..), OpenMode (..))
import System.FS.IO (ioHasFS)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (localOption, mkTimeout)
import Test.Tasty.Bench
import Text.Printf (printf)

type Blk = StandardDijkstraBlock

{-------------------------------------------------------------------------------
  Parameters
-------------------------------------------------------------------------------}

-- | Length of a Peras round in slots in the Dijkstra era.
roundLength :: Word64
roundLength = unPerasRoundLength (fromPerasEnabled (error "Peras disabled") dijkstraPerasRoundLength)

-- | Number of Peras rounds, and hence maximum number of certificates, per
-- year (slots are one second long).
roundsPerYear :: Word64
roundsPerYear = (365 * 24 * 60 * 60) `div` roundLength

-- | Number of voters in a certificate: enough seats of the target committee
-- to reach the quorum, assuming equally weighted seats.
votersPerCert :: Word16
votersPerCert =
  ceiling (unPerasQuorumWeightThreshold (perasQuorumWeightThreshold params) * fromIntegral committeeSize)
 where
  params = defaultPerasParams :: PerasParams Blk
  committeeSize = unTargetCommitteeSize (perasTargetCommitteeSize params)

-- | Rate at which a syncing node collects certificates: at most one per
-- round, assuming it adopts 'syncingBlocksPerSecond' blocks per second, with
-- one block every 20 slots (the mainnet active slot coefficient).
syncingCertsPerSecond :: Double
syncingCertsPerSecond = syncingBlocksPerSecond * 20 / fromIntegral roundLength

syncingBlocksPerSecond :: Double
syncingBlocksPerSecond = 1000

-- | Number of certificates in the indexed databases, as fractions of a year,
-- to show how indexing scales with the size of the database. Make sure there
-- is enough disk space for the largest one.
indexSizes :: [Word64]
indexSizes = [decodeIndexSize, roundsPerYear `div` 64, roundsPerYear `div` 16, roundsPerYear `div` 4]

-- | The only size at which indexing with decoding (level 3) is measured.
--
-- Decoding a certificate costs up to tens of milliseconds, which would make
-- level 3 take hours on the larger databases. As decoding is CPU-bound and
-- independent of the size of the database, measuring it on a small one is
-- enough to extrapolate.
decodeIndexSize :: Word64
decodeIndexSize = roundsPerYear `div` 256

-- | Decoding a database with many non-persistent voters takes tens of seconds
-- per iteration, beyond the default timeout of the benchmark harness.
levelTimeout :: IndexLevel -> Benchmark -> Benchmark
levelTimeout FileNamesCRCAndDecode = localOption (mkTimeout (20 * 60 * 1000000))
levelTimeout _ = id

-- | Number of certificates stored per iteration of the storing benchmark.
storeBatchSize :: Word64
storeBatchSize = 1000

-- | The composition of the certificates of a benchmark.
data CertProfile = CertProfile
  { cpName :: String
  , cpNonPersistentVoters :: Word16
  -- ^ The number of the 'votersPerCert' voters that are non-persistent, each
  -- of which carries its VRF output (a BLS signature) in the certificate.
  }

certProfiles :: [CertProfile]
certProfiles =
  [ CertProfile "all voters persistent" 0
  , CertProfile "half the voters non-persistent" (votersPerCert `div` 2)
  , CertProfile "all voters non-persistent" votersPerCert
  ]

{-------------------------------------------------------------------------------
  Benchmarks
-------------------------------------------------------------------------------}

main :: IO ()
main =
  withSystemTempDirectory "PerasImmutableCertDB-bench" $ \tmpDir -> do
    sigs <- mkSignatures
    results <- newIORef Map.empty
    profiles <- forM (zip [0 :: Int ..] certProfiles) $ \(i, profile) -> do
      -- Store one certificate to obtain its file, to know its size and to
      -- benchmark decoding it.
      db <- newPopulatedDB (tmpDir </> ("sample-" <> show i)) sigs profile
      populate db 1
      bytes <- readCertFileBytes (pdDir db) (PerasRoundNo 0)
      pure (i, profile, bytes)
    defaultMain
      [ localOption WallTime $
          bgroup "PerasImmutableCertDB" $
            [ envWithCleanup
                (newPopulatedDB (tmpDir </> ("profile-" <> show i)) sigs profile)
                (removeDirectoryRecursive . pdDir)
                $ \db -> profileBenchmarks results sigs profile db bytes
            | (i, profile, bytes) <- profiles
            ]
      ]
      `catch` \(e :: ExitCode) -> do
        printProjections results [(profile, bytes) | (_, profile, bytes) <- profiles]
        throwIO e

profileBenchmarks ::
  Results -> Signatures -> CertProfile -> PopulatedDB -> BSL.ByteString -> Benchmark
profileBenchmarks results sigs profile db bytes =
  bgroup (profileDescription profile bytes) $
    [ bench decodeName $
        recordTime results (profile, decodeName) 1 $
          whnf decode bytes
    ]
      <> [ env (populate db n) $ \_ ->
             bgroup (show n <> " certificates") $
               [ levelTimeout lvl . bench (indexLevelName lvl) $
                   recordTime results (profile, indexLevelName lvl) n $
                     whnfIO (indexDB lvl (pdDir db) n)
               | lvl <- [minBound .. maxBound]
               , lvl /= FileNamesCRCAndDecode || n == decodeIndexSize
               ]
         | n <- indexSizes
         ]
      -- Last, as it adds certificates beyond those indexed above.
      <> [ envWithCleanup
             ( StoreEnv
                 <$> (readIORef (pdPopulated db) >>= newIORef)
                 <*> openCertDB CheckOnRead (pdDir db)
             )
             (PerasImmutableCertDB.closeDB . seDB)
             $ \storeEnv ->
               bench storeName $
                 recordTime results (profile, storeName) storeBatchSize $
                   whnfIO (storeBatch sigs profile storeEnv)
         ]
 where
  decode = either (error . displayCertFileError) (const ()) . decodeCertFile @Blk ShelleyCodecConfig

decodeName :: String
decodeName = "CRC32 check and decoding of a certificate in memory"

storeName :: String
storeName =
  "addCert of "
    <> show storeBatchSize
    <> " certificates for fresh rounds, into a database of "
    <> show (maximum indexSizes)

profileDescription :: CertProfile -> BSL.ByteString -> String
profileDescription CertProfile{cpName, cpNonPersistentVoters} bytes =
  printf
    "%s (%d voters, %d non-persistent, %d bytes per file)"
    cpName
    votersPerCert
    cpNonPersistentVoters
    (BSL.length bytes)

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

data IndexLevel = FileNames | FileNamesAndCRC | FileNamesCRCAndDecode
  deriving (Eq, Ord, Show, Enum, Bounded)

indexLevelName :: IndexLevel -> String
indexLevelName = \case
  FileNames -> "(1) index file names (openDB with CheckOnRead)"
  FileNamesAndCRC -> "(2) index file names, read files and check CRC32"
  FileNamesCRCAndDecode -> "(3) index file names, read files, check CRC32 and decode (openDB with CheckAllOnOpen)"

-- | Index a database holding the certificates of rounds @[0 .. n - 1]@.
indexDB :: IndexLevel -> FilePath -> Word64 -> IO ()
indexDB lvl dir n = case lvl of
  FileNames ->
    openCertDB CheckOnRead dir >>= PerasImmutableCertDB.closeDB
  FileNamesAndCRC -> do
    -- The same as 'CheckAllOnOpen' does, except for decoding.
    db <- openCertDB CheckOnRead dir
    forM_ [0 .. n - 1] $ \r -> do
      bytes <- readCertFileBytes dir (PerasRoundNo r)
      either (throwIO . userError . displayCertFileError) (const (pure ())) $
        checkCertFileCRC bytes
    PerasImmutableCertDB.closeDB db
  FileNamesCRCAndDecode ->
    openCertDB CheckAllOnOpen dir >>= PerasImmutableCertDB.closeDB

{-------------------------------------------------------------------------------
  Storing
-------------------------------------------------------------------------------}

data StoreEnv = StoreEnv
  { seNextRound :: IORef Word64
  , seDB :: PerasImmutableCertDB IO Blk
  }

instance NFData StoreEnv where
  rnf StoreEnv{seNextRound, seDB} = seNextRound `seq` seDB `seq` ()

storeBatch :: Signatures -> CertProfile -> StoreEnv -> IO ()
storeBatch sigs profile StoreEnv{seNextRound, seDB} = do
  firstRound <- atomicModifyIORef' seNextRound $ \r -> (r + storeBatchSize, r)
  forM_ [firstRound .. firstRound + storeBatchSize - 1] $ \r ->
    void $ PerasImmutableCertDB.addCert seDB (mkCert sigs profile (PerasRoundNo r))

{-------------------------------------------------------------------------------
  Databases
-------------------------------------------------------------------------------}

openCertDB :: PerasImmutableCertDbIntegrityCheckPolicy -> FilePath -> IO (PerasImmutableCertDB IO Blk)
openCertDB policy dir =
  PerasImmutableCertDB.openDB
    PerasImmutableCertDbArgs
      { picdbaCodecConfig = ShelleyCodecConfig
      , picdbaHasFS = SomeHasFS $ ioHasFS (MountPoint dir)
      , picdbaTracer = failOnQuarantine
      , picdbaIntegrityCheckPolicy = policy
      }
 where
  -- Quarantining would silently skip certificates, invalidating the results.
  failOnQuarantine :: Tracer IO (TraceEvent Blk)
  failOnQuarantine = Tracer $ emit $ \case
    QuarantinedCert r err -> throwIO $ userError $ "Quarantined " <> show r <> ": " <> err
    _ -> pure ()

-- | A database being populated with the certificates of rounds @[0 .. n - 1]@
-- for increasing @n@.
data PopulatedDB = PopulatedDB
  { pdDir :: FilePath
  , pdPopulated :: IORef Word64
  -- ^ The number @n@ of certificates stored so far.
  , pdCert :: PerasRoundNo -> WithArrivalTime (ValidatedPerasCert Blk)
  }

instance NFData PopulatedDB where
  rnf PopulatedDB{pdDir, pdPopulated, pdCert} = rnf pdDir `seq` pdPopulated `seq` pdCert `seq` ()

-- | Create an empty database in a new directory.
newPopulatedDB :: FilePath -> Signatures -> CertProfile -> IO PopulatedDB
newPopulatedDB dir sigs profile = do
  createDirectory dir
  populated <- newIORef 0
  pure PopulatedDB{pdDir = dir, pdPopulated = populated, pdCert = mkCert sigs profile}

-- | Make sure the database holds the certificates of rounds @[0 .. n - 1]@,
-- adding the missing ones.
populate :: PopulatedDB -> Word64 -> IO ()
populate PopulatedDB{pdDir, pdPopulated, pdCert} n = do
  populated <- readIORef pdPopulated
  db <- openCertDB CheckOnRead pdDir
  forM_ [populated .. n - 1] $ \r ->
    void $ PerasImmutableCertDB.addCert db (pdCert (PerasRoundNo r))
  PerasImmutableCertDB.closeDB db
  modifyIORef' pdPopulated (max n)

readCertFileBytes :: FilePath -> PerasRoundNo -> IO BSL.ByteString
readCertFileBytes dir r =
  withFile hasFS (fsPathCertFile r) ReadMode (hGetAll hasFS)
 where
  hasFS = ioHasFS (MountPoint dir)

{-------------------------------------------------------------------------------
  Certificates
-------------------------------------------------------------------------------}

data Signatures = Signatures
  { sigAggregate :: !(BLS.Signature BLS.SIGN)
  , sigVRFOutputs :: ![BLS.Signature BLS.VRF]
  -- ^ A pool of distinct VRF outputs, reused across voters and certificates;
  -- only their decoding cost matters here.
  }

mkSignatures :: IO Signatures
mkSignatures = do
  let sigAggregate = BLS.signWithRole (privateKey @BLS.SIGN) (BS8.pack "certificate")
      sigVRFOutputs =
        [BLS.signWithRole (privateKey @BLS.VRF) (BS8.pack ("vrf-" <> show i)) | i <- [1 :: Int .. 64]]
  pure $! forceList sigVRFOutputs `seq` Signatures{sigAggregate, sigVRFOutputs}
 where
  privateKey :: forall r. BLS.PrivateKey r
  privateKey =
    fromMaybe (error "invalid private key") $
      BLS.rawDeserialisePrivateKey (BS8.pack "PERAS-BENCH") (BS.replicate 32 1)
  forceList = foldr seq ()

-- | A certificate for the given round, as stored by the ChainDB.
--
-- The non-persistent voters occupy the highest seat indices, as is the case
-- for certificates using the WFALS voting committee.
mkCert :: Signatures -> CertProfile -> PerasRoundNo -> WithArrivalTime (ValidatedPerasCert Blk)
mkCert Signatures{sigAggregate, sigVRFOutputs} CertProfile{cpNonPersistentVoters} roundNo =
  WithArrivalTime
    { getArrivalTime = RelativeTime 0
    , forgetArrivalTime =
        ValidatedPerasCert
          { vpcCert =
              V1.PerasCert
                { V1.pcRoundNo = roundNo
                , V1.pcBoostedBlock =
                    PerasBoostedBlock $
                      NotOrigin $
                        Bytes32RealPoint
                          (SlotNo (unPerasRoundNo roundNo * roundLength))
                          (SBS.toShort (BS.replicate 32 0xab))
                , V1.pcVoters = V1.PerasCertVoters (NEMap.fromList (NonEmpty.fromList voters))
                , V1.pcSignature = PerasBLSCryptoAggregateVoteSignature sigAggregate
                }
          , vpcCertBoost = perasWeight (defaultPerasParams :: PerasParams Blk)
          }
    }
 where
  nbPersistent = votersPerCert - cpNonPersistentVoters
  voters =
    zip (PerasSeatIndex <$> [0 ..]) $
      replicate (fromIntegral nbPersistent) PersistentPerasVoteEligibilityProof
        <> take
          (fromIntegral cpNonPersistentVoters)
          (cycle (NonPersistentPerasVoteEligibilityProof . PerasBLSCryptoVRFOutput <$> sigVRFOutputs))

{-------------------------------------------------------------------------------
  Projections
-------------------------------------------------------------------------------}

-- | For each benchmark (identified by the 'cpName' of its profile and its
-- name) and number of certificates processed per iteration, the time per
-- iteration in seconds, recorded by 'recordTime'.
type Results = IORef (Map (String, String) (Map Word64 (Word64, Double)))

-- | Record the time per iteration of a benchmark processing @n@ certificates
-- per iteration, for 'printProjections'. We keep the run with the most
-- iterations, the most precise one, which is also the last one @tasty-bench@
-- uses for its estimate.
recordTime :: Results -> (CertProfile, String) -> Word64 -> Benchmarkable -> Benchmarkable
recordTime results (profile, name) n (Benchmarkable run) = Benchmarkable $ \iters -> do
  start <- getMonotonicTime
  run iters
  end <- getMonotonicTime
  let perIter = (end - start) / fromIntegral iters
  modifyIORef' results $
    Map.insertWith (Map.unionWith mostIterations) (cpName profile, name) $
      Map.singleton n (iters, perIter)
 where
  mostIterations new@(itersNew, _) old@(itersOld, _)
    | itersNew >= itersOld = new
    | otherwise = old

-- | Print the time per certificate of every benchmark, extrapolated to a year
-- of certificates, followed by a summary table.
printProjections :: Results -> [(CertProfile, BSL.ByteString)] -> IO ()
printProjections results profiles = do
  rs <- readIORef results
  unless (Map.null rs) $ do
    printf
      "\nExtrapolation to a year of certificates (%d, one per %d-slot round)\n"
      roundsPerYear
      roundLength
    forM_ profiles $ \(profile, bytes) -> do
      let measured = benchmarksOf rs profile
      unless (null measured) $ do
        printf "\n%s\n" (profileDescription profile bytes)
        printf
          "  certificate files: %.0f MiB per year (excluding file system overhead)\n"
          (fromIntegral (BSL.length bytes) * fromIntegral roundsPerYear / 2 ^ (20 :: Int) :: Double)
        forM_ measured $ \(name, perSize) -> do
          printf "  %s\n" name
          forM_ (Map.toList perSize) $ \(n, (_, t)) ->
            printf
              "    %6d certificates: %10.3f s, %10.2f us/certificate => %s per year\n"
              n
              t
              (t / fromIntegral n * 1e6)
              (showDuration (perYear n t))
          if name == storeName
            then forM_ (Map.toList perSize) $ \(n, (_, t)) ->
              printf
                "    => %.0f certificates/s, vs. at most %.0f/s collected by a node syncing %.0f blocks/s\n"
                (fromIntegral n / t)
                syncingCertsPerSecond
                syncingBlocksPerSecond
            else pure ()

    printf "\nSummary: time per year of certificates, from the largest database measured\n"
    printf "  %-40s" ("" :: String)
    forM_ profiles $ \(profile, _) ->
      printf "%24s" (show (cpNonPersistentVoters profile) <> "/" <> show votersPerCert <> " non-persistent")
    printf "\n"
    forM_ summaryRows $ \(label, name) -> do
      printf "  %-40s" label
      forM_ profiles $ \(profile, _) ->
        printf "%24s" $
          case Map.lookupMax =<< Map.lookup (cpName profile, name) rs of
            Just (n, (_, t)) -> showDuration (perYear n t)
            Nothing -> "-"
      printf "\n"
 where
  benchmarksOf rs profile =
    [(name, perSize) | ((p, name), perSize) <- Map.toList rs, p == cpName profile]
  perYear n t = t / fromIntegral n * fromIntegral roundsPerYear
  summaryRows =
    [ ("indexing: (1) file names", indexLevelName FileNames)
    , ("indexing: (2) + read and CRC32 check", indexLevelName FileNamesAndCRC)
    , ("indexing: (3) + decoding", indexLevelName FileNamesCRCAndDecode)
    , ("decoding only, in memory", decodeName)
    , ("storing (addCert)", storeName)
    ]

showDuration :: Double -> String
showDuration s
  | s < 1 = printf "%.0f ms" (s * 1e3)
  | s < 120 = printf "%.1f s" s
  | s < 7200 = printf "%.1f min" (s / 60)
  | otherwise = printf "%.1f h" (s / 3600)
