module Test.Ouroboros.Storage.PerasImmutableCertDB (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTMVar
  , newTMVar
  )
import Control.Monad (forM_, void)
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word16, Word8, Word64)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import qualified Ouroboros.Consensus.Storage.PerasImmutableCertDB as DB
import Ouroboros.Consensus.Util.Args (Complete)
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.FS.API.Lazy
import qualified System.FS.Sim.MockFS as MockFS
import System.FS.Sim.STM (simHasFS)
import Test.Ouroboros.Storage.TestBlock
  ( CodecConfig (TestBlockCodecConfig)
  , TestBlock
  )
import Test.QuickCheck
  ( Positive (..)
  , Property
  , ioProperty
  , withMaxSuccess
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "PerasImmutableCertDB"
    [ testCase "query is strict, ascending and bounded" testQuerySemantics
    , testCase "duplicate round preserves the first certificate" testDuplicateRound
    , testCase "reopening preserves numeric round order" testReopenOrder
    , testCase "reopening removes abandoned temporary files" testTempFileCleanup
    , testCase "non-certificate directory entries are not served" testForeignFilesIgnored
    , testCase "a missing file is quarantined on read" testMissingFileQuarantined
    , testCase "a corrupt file is quarantined on read" testCorruptFileQuarantined
    , testCase "eager validation quarantines corruption on open" testEagerValidation
    , testProperty "query agrees with a sorted-set model" $
        withMaxSuccess 50 propQueryMatchesModel
    , testProperty "pagination returns every round exactly once" $
        withMaxSuccess 50 propPagination
    , testProperty "reopening preserves the observable certificate set" $
        withMaxSuccess 50 propReopenPreservesQuery
    ]

testQuerySemantics :: IO ()
testQuerySemantics = withFreshDB $ \_args db -> do
  addRounds db [10, 2, 1, 100]

  roundsOf <$> DB.getCertsAfter db (PerasRoundNo 2) 2
    >>= (@?= [PerasRoundNo 10, PerasRoundNo 100])

  roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) maxBound
    >>= (@?= map PerasRoundNo [1, 2, 10, 100])

  DB.getCertsAfter db (PerasRoundNo 0) 0
    >>= (@?= [])

  DB.getCertsAfter db (PerasRoundNo 100) 10
    >>= (@?= [])

testDuplicateRound :: IO ()
testDuplicateRound = withFreshDB $ \_args db -> do
  let first = mkCertWithBoost 7 3
      second = mkCertWithBoost 7 99

  DB.addCert db first
    >>= (@?= DB.AddedCertToImmutableDB)
  DB.addCert db second
    >>= (@?= DB.CertAlreadyInImmutableDB)

  DB.getCertsAfter db (PerasRoundNo 6) 1
    >>= (@?= [first])

testReopenOrder :: IO ()
testReopenOrder = withFreshDB $ \args db -> do
  addRounds db [2, 10]

  roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
    >>= (@?= [PerasRoundNo 2, PerasRoundNo 10])

  reopened <- DB.createDB args
  roundsOf <$> DB.getCertsAfter reopened (PerasRoundNo 0) 10
    >>= (@?= [PerasRoundNo 2, PerasRoundNo 10])

testTempFileCleanup :: IO ()
testTempFileCleanup =
  withFreshArgs $ \fs args -> do
    writeRawFile fs (mkFsPath ["17.cert.tmp"]) (BSL.pack [0, 1, 2])
    db <- DB.createDB args

    listDirectory (simHasFS fs) (mkFsPath [])
      >>= (@?= Set.empty)
    DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [])

testForeignFilesIgnored :: IO ()
testForeignFilesIgnored =
  withFreshArgs $ \fs args -> do
    forM_
      [ "README"
      , "foo.cert"
      , "1.cert.bak"
      , "-1.cert"
      , "001.cert"
      ]
      $ \name ->
        writeRawFile fs (mkFsPath [name]) (BSL.pack [0])

    db <- DB.createDB args
    DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [])

testMissingFileQuarantined :: IO ()
testMissingFileQuarantined =
  withFreshDBFS $ \fs _args db -> do
    addRounds db [1, 2]
    removeFile (simHasFS fs) (certPath 1)

    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])

testCorruptFileQuarantined :: IO ()
testCorruptFileQuarantined =
  withFreshDBFS $ \fs _args db -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])

testEagerValidation :: IO ()
testEagerValidation =
  withFreshDBFS $ \fs args db -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    reopened <-
      DB.createDB
        args
          { DB.picdbaValidationPolicy = DB.ValidateAllOnOpen
          }
    roundsOf <$> DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])

propQueryMatchesModel :: [Positive Word16] -> Property
propQueryMatchesModel generated =
  ioProperty $
    withFreshDB $ \_args db -> do
      let rounds = generatedRounds generated
          expected = sort (nub rounds)
      addRoundNos db rounds
      actual <- roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) maxBound
      pure (actual == expected)

propPagination :: [Positive Word16] -> Positive Word8 -> Property
propPagination generated (Positive pageSize) =
  ioProperty $
    withFreshDB $ \_args db -> do
      let rounds = generatedRounds generated
          expected = sort (nub rounds)
      addRoundNos db rounds
      actual <-
        paginate db (PerasRoundNo 0) (fromIntegral pageSize)
      pure (roundsOf actual == expected)

propReopenPreservesQuery :: [Positive Word16] -> Property
propReopenPreservesQuery generated =
  ioProperty $
    withFreshDB $ \args db -> do
      let rounds = generatedRounds generated
      addRoundNos db rounds
      before <- DB.getCertsAfter db (PerasRoundNo 0) maxBound
      reopened <- DB.createDB args
      after <- DB.getCertsAfter reopened (PerasRoundNo 0) maxBound
      pure (before == after)

withFreshDB ::
  ( Complete DB.PerasImmutableCertDbArgs IO TestBlock ->
    DB.PerasImmutableCertDB IO TestBlock ->
    IO a
  ) ->
  IO a
withFreshDB action =
  withFreshDBFS $ \_fs args db ->
    action args db

withFreshDBFS ::
  ( StrictTMVar IO MockFS.MockFS ->
    Complete DB.PerasImmutableCertDbArgs IO TestBlock ->
    DB.PerasImmutableCertDB IO TestBlock ->
    IO a
  ) ->
  IO a
withFreshDBFS action =
  withFreshArgs $ \fs args -> do
    db <- DB.createDB args
    action fs args db

withFreshArgs ::
  ( StrictTMVar IO MockFS.MockFS ->
    Complete DB.PerasImmutableCertDbArgs IO TestBlock ->
    IO a
  ) ->
  IO a
withFreshArgs action = do
  fs <- atomically $ newTMVar MockFS.empty
  let args :: Complete DB.PerasImmutableCertDbArgs IO TestBlock
      args =
        DB.defaultArgs
          { DB.picdbaCodecConfig = TestBlockCodecConfig
          , DB.picdbaHasFS = SomeHasFS (simHasFS fs)
          }
  action fs args

certPath :: Word64 -> FsPath
certPath roundNo =
  mkFsPath [show roundNo <> ".cert"]

writeRawFile ::
  StrictTMVar IO MockFS.MockFS ->
  FsPath ->
  BSL.ByteString ->
  IO ()
writeRawFile fs path bytes = do
  let hasFS = simHasFS fs
  createDirectoryIfMissing hasFS True (mkFsPath [])
  withFile hasFS path (WriteMode MustBeNew) $ \h ->
    void $ hPutAll hasFS h bytes

replaceRawFile ::
  StrictTMVar IO MockFS.MockFS ->
  FsPath ->
  BSL.ByteString ->
  IO ()
replaceRawFile fs path bytes = do
  removeFile (simHasFS fs) path
  writeRawFile fs path bytes

addRounds ::
  DB.PerasImmutableCertDB IO TestBlock ->
  [Word64] ->
  IO ()
addRounds db =
  addRoundNos db . map PerasRoundNo

addRoundNos ::
  DB.PerasImmutableCertDB IO TestBlock ->
  [PerasRoundNo] ->
  IO ()
addRoundNos db =
  mapM_ (void . DB.addCert db . mkCert)

paginate ::
  DB.PerasImmutableCertDB IO TestBlock ->
  PerasRoundNo ->
  Word64 ->
  IO [ValidatedPerasCert TestBlock]
paginate db cursor pageSize = do
  page <- DB.getCertsAfter db cursor pageSize
  case page of
    [] -> pure []
    _ ->
      (page <>)
        <$> paginate db (getPerasCertRound (last page)) pageSize

generatedRounds :: [Positive Word16] -> [PerasRoundNo]
generatedRounds =
  map $ \(Positive n) -> PerasRoundNo (fromIntegral n)

roundsOf :: [ValidatedPerasCert TestBlock] -> [PerasRoundNo]
roundsOf = map getPerasCertRound

mkCert :: PerasRoundNo -> ValidatedPerasCert TestBlock
mkCert roundNo =
  mkCertWithBoost
    (unPerasRoundNo roundNo)
    1

mkCertWithBoost :: Word64 -> Word64 -> ValidatedPerasCert TestBlock
mkCertWithBoost roundNo boost =
  ValidatedPerasCert
    { vpcCert =
        MockPerasCert
          { mockCertRound = PerasRoundNo roundNo
          , mockCertBlock = GenesisPoint
          , mockCertVoters =
              NESet.fromList (PerasSeatIndex 0 :| [])
          }
    , vpcCertBoost = PerasWeight boost
    }
