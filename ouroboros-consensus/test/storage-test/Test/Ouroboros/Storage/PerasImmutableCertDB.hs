{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Storage.PerasImmutableCertDB (tests) where

import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTMVar
  , StrictTVar
  , newTMVar
  , newTVarIO
  )
import qualified Control.Exception as Exception
import Control.Monad (forM_, replicateM, void)
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word16, Word8, Word64)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import qualified Ouroboros.Consensus.Storage.PerasImmutableCertDB as DB
import Ouroboros.Consensus.Util.Args (Complete)
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.FS.API.Lazy
import System.FS.Sim.Error
  ( Errors (..)
  , emptyErrors
  , simErrorHasFS
  , withErrors
  )
import qualified System.FS.Sim.MockFS as MockFS
import qualified System.FS.Sim.Stream as Stream
import System.FS.Sim.STM (simHasFS)
import Test.Ouroboros.Storage.TestBlock
  ( CodecConfig (TestBlockCodecConfig)
  , TestBlock
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Positive (..)
  , Property
  , counterexample
  , frequency
  , ioProperty
  , listOf
  , property
  , resize
  , shrinkList
  , withMaxSuccess
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Tracer (recordingTracerIORef)

tests :: TestTree
tests =
  testGroup
    "PerasImmutableCertDB"
    [ testCase "query is strict, ascending and bounded" testQuerySemantics
    , testCase "duplicate round preserves the first certificate" testDuplicateRound
    , testCase "reopening preserves numeric round order" testReopenOrder
    , testCase "reopening quarantines an abandoned temporary write" testTempFileCleanup
    , testCase "non-certificate directory entries are not served" testForeignFilesIgnored
    , testCase "a missing file is quarantined on read" testMissingFileQuarantined
    , testCase "a corrupt file is quarantined on read" testCorruptFileQuarantined
    , testCase "eager validation quarantines corruption on open" testEagerValidation
    , testCase "lazy validation traces quarantine exactly once" testLazyQuarantineTrace
    , testCase "eager validation traces quarantine during open" testEagerQuarantineTrace
    , testCase "concurrent duplicate adds commit exactly once" testConcurrentDuplicateAdds
    , testGroup
        "failed adds are atomic across reopen"
        [ testCase (show fault) (testFailedAddAtomic fault)
        | fault <- [minBound .. maxBound]
        ]
    , testProperty "query agrees with a sorted-set model" $
        withMaxSuccess 50 propQueryMatchesModel
    , testProperty "pagination returns every round exactly once" $
        withMaxSuccess 50 propPagination
    , testProperty "reopening preserves the observable certificate set" $
        withMaxSuccess 50 propReopenPreservesQuery
    , testProperty "arbitrary cursor and limit agree with the model" $
        withMaxSuccess 100 propQuerySliceMatchesModel
    , testProperty "insertion order does not affect observable contents" $
        withMaxSuccess 50 propInsertionOrderIndependent
    , testProperty "validation policies agree on valid files" $
        withMaxSuccess 50 propValidationPoliciesAgree
    , testProperty "generated add/query/reopen sequences agree with the model" $
        withMaxSuccess 100 propCommandSequence
    , testProperty "validation policies agree after generated file damage" $
        withMaxSuccess 50 propValidationPoliciesAgreeAfterDamage
    , testProperty "generated abandoned temporary files are swept" $
        withMaxSuccess 50 propGeneratedTempFilesAreSwept
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

  reopened <- DB.openDB args
  roundsOf <$> DB.getCertsAfter reopened (PerasRoundNo 0) 10
    >>= (@?= [PerasRoundNo 2, PerasRoundNo 10])

testTempFileCleanup :: IO ()
testTempFileCleanup =
  withFreshArgs $ \fs args -> do
    writeRawFile fs (mkFsPath ["17.cert.tmp"]) (BSL.pack [0, 1, 2])
    db <- DB.openDB args

    listDirectory (simHasFS fs) (mkFsPath [])
      >>= (@?= Set.singleton "17.cert.quarantined")
    DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [])
    atomically (DB.getQuarantinedRounds db)
      >>= (@?= Set.singleton (PerasRoundNo 17))

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

    db <- DB.openDB args
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
    atomically (DB.getQuarantinedRounds db)
      >>= (@?= Set.singleton (PerasRoundNo 1))

testCorruptFileQuarantined :: IO ()
testCorruptFileQuarantined =
  withFreshDBFS $ \fs _args db -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    atomically (DB.getQuarantinedRounds db)
      >>= (@?= Set.singleton (PerasRoundNo 1))

testEagerValidation :: IO ()
testEagerValidation =
  withFreshDBFS $ \fs args db -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    reopened <-
      DB.openDB
        args
          { DB.picdbaValidationPolicy = DB.ValidateAllOnOpen
          }
    roundsOf <$> DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    atomically (DB.getQuarantinedRounds reopened)
      >>= (@?= Set.singleton (PerasRoundNo 1))

testLazyQuarantineTrace :: IO ()
testLazyQuarantineTrace =
  withFreshTracedDB $ \fs _args db getTrace -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    quarantineEvents <$> getTrace
      >>= (@?= [])

    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    firstEvents <- quarantineEvents <$> getTrace
    map fst firstEvents
      @?= [PerasRoundNo 1]

    roundsOf <$> DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    quarantineEvents <$> getTrace
      >>= (@?= firstEvents)

-- This distinguishes eager validation from lazy validation: the quarantine
-- event must already have been emitted when 'createDB' returns, before any
-- query is made against the reopened database.
testEagerQuarantineTrace :: IO ()
testEagerQuarantineTrace =
  withFreshDBFS $ \fs args db -> do
    addRounds db [1, 2]
    replaceRawFile fs (certPath 1) (BSL.pack [0xde, 0xad, 0xbe, 0xef])

    (tracer, getTrace) <- recordingTracerIORef
    reopened <-
      DB.createDB
        args
          { DB.picdbaTracer = tracer
          , DB.picdbaValidationPolicy = DB.ValidateAllOnOpen
          }

    events <- getTrace
    map eventShape events
      @?= [QuarantinedShape (PerasRoundNo 1), OpenedShape 1]

    roundsOf <$> DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [PerasRoundNo 2])
    map eventShape <$> getTrace
      >>= (@?= [QuarantinedShape (PerasRoundNo 1), OpenedShape 1])

testConcurrentDuplicateAdds :: IO ()
testConcurrentDuplicateAdds =
  withFreshDB $ \args db -> do
    let cert = mkCertWithBoost 7 3
        workerCount = 16
    done <- replicateM workerCount newEmptyMVar
    forM_ done $ \resultVar ->
      void $ forkIO $ do
        result <-
          Exception.try (DB.addCert db cert) ::
            IO (Either Exception.SomeException DB.AddPerasImmutableCertResult)
        putMVar resultVar result

    outcomes <- mapM takeMVar done
    results <- case sequence outcomes of
      Left err -> Exception.throwIO err
      Right rs -> pure rs

    length (filter (== DB.AddedCertToImmutableDB) results)
      @?= 1
    length (filter (== DB.CertAlreadyInImmutableDB) results)
      @?= workerCount - 1
    DB.getCertsAfter db (PerasRoundNo 0) 10
      >>= (@?= [cert])

    reopened <- DB.createDB args
    DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [cert])

data AddFault
  = FailOpen
  | FailWrite
  | FailRename
  deriving (Bounded, Enum, Show)

testFailedAddAtomic :: AddFault -> IO ()
testFailedAddAtomic fault =
  withFreshErrorDB $ \fs errorsVar args db -> do
    let cert = mkCertWithBoost 1 1
    result <-
      withErrors errorsVar (errorsFor fault) $
        ( Exception.try (DB.addCert db cert) ::
            IO (Either FsError DB.AddPerasImmutableCertResult)
        )
    case result of
      Left _ -> pure ()
      Right addResult ->
        assertFailure $
          "expected an injected filesystem failure, but addCert returned "
            <> show addResult

    -- Reopening must clean up any abandoned temporary file and must never
    -- expose a certificate whose add did not commit.
    reopened <- DB.createDB args
    DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [])
    listDirectory (simErrorHasFS fs errorsVar) (mkFsPath [])
      >>= (@?= Set.empty)

    DB.addCert reopened cert
      >>= (@?= DB.AddedCertToImmutableDB)
    DB.getCertsAfter reopened (PerasRoundNo 0) 10
      >>= (@?= [cert])

errorsFor :: AddFault -> Errors
errorsFor = \case
  FailOpen ->
    emptyErrors
      { hOpenE = oneError FsDeviceFull
      }
  FailWrite ->
    emptyErrors
      { hPutSomeE =
          Stream.unsafeMkFinite
            [Just (Left (FsDeviceFull, Nothing))]
      }
  FailRename ->
    emptyErrors
      { renameFileE = oneError FsDeviceFull
      }
 where
  oneError err = Stream.unsafeMkFinite [Just err]

data EventShape
  = OpenedShape Int
  | QuarantinedShape PerasRoundNo
  | OtherShape
  deriving (Eq, Show)

eventShape :: DB.TraceEvent TestBlock -> EventShape
eventShape = \case
  DB.OpenedDB count -> OpenedShape count
  DB.QuarantinedCert roundNo _ -> QuarantinedShape roundNo
  _ -> OtherShape

quarantineEvents :: [DB.TraceEvent TestBlock] -> [(PerasRoundNo, String)]
quarantineEvents events =
  [ (roundNo, reason)
  | DB.QuarantinedCert roundNo reason <- events
  ]

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
      reopened <- DB.openDB args
      after <- DB.getCertsAfter reopened (PerasRoundNo 0) maxBound
      pure (before == after)

propQuerySliceMatchesModel ::
  [Positive Word16] ->
  Word16 ->
  Word64 ->
  Property
propQuerySliceMatchesModel generated cursorWord limit =
  ioProperty $
    withFreshDB $ \_args db -> do
      let rounds = generatedRounds generated
          cursor = PerasRoundNo (fromIntegral cursorWord)
          candidates = filter (> cursor) (sort (nub rounds))
          expected = takeWord64 limit candidates
      addRoundNos db rounds
      actual <- roundsOf <$> DB.getCertsAfter db cursor limit
      pure $
        counterexample
          ( "cursor: " <> show cursor
              <> "\nlimit: " <> show limit
              <> "\nexpected: " <> show expected
              <> "\nactual: " <> show actual
          )
          (actual == expected)

propInsertionOrderIndependent :: [Positive Word16] -> Property
propInsertionOrderIndependent generated =
  ioProperty $ do
    let rounds = generatedRounds generated
    forward <-
      withFreshDB $ \_args db -> do
        addRoundNos db rounds
        DB.getCertsAfter db (PerasRoundNo 0) maxBound
    backward <-
      withFreshDB $ \_args db -> do
        addRoundNos db (reverse rounds)
        DB.getCertsAfter db (PerasRoundNo 0) maxBound
    pure (forward == backward)

propValidationPoliciesAgree :: [Positive Word16] -> Property
propValidationPoliciesAgree generated =
  ioProperty $
    withFreshDBFS $ \_fs args db -> do
      addRoundNos db (generatedRounds generated)
      lazy <-
        DB.createDB
          args
            { DB.picdbaValidationPolicy = DB.ValidateOnRead
            }
      eager <-
        DB.createDB
          args
            { DB.picdbaValidationPolicy = DB.ValidateAllOnOpen
            }
      lazyCerts <- DB.getCertsAfter lazy (PerasRoundNo 0) maxBound
      eagerCerts <- DB.getCertsAfter eager (PerasRoundNo 0) maxBound
      pure (lazyCerts == eagerCerts)

propValidationPoliciesAgreeAfterDamage ::
  [Positive Word8] ->
  [Positive Word8] ->
  [Positive Word8] ->
  Property
propValidationPoliciesAgreeAfterDamage stored missing corrupt =
  ioProperty $
    withFreshDBFS $ \fs args db -> do
      let storedRounds = Set.fromList (smallGeneratedRounds stored)
          missingRounds =
            Set.intersection
              storedRounds
              (Set.fromList $ smallGeneratedRounds missing)
          corruptRounds =
            Set.difference
              ( Set.intersection
                  storedRounds
                  (Set.fromList $ smallGeneratedRounds corrupt)
              )
              missingRounds
          expected =
            Set.toAscList $
              storedRounds
                `Set.difference` missingRounds
                `Set.difference` corruptRounds

      addRoundNos db (Set.toList storedRounds)
      forM_ missingRounds $ \roundNo ->
        removeFile (simHasFS fs) (certPath $ unPerasRoundNo roundNo)
      forM_ corruptRounds $ \roundNo ->
        replaceRawFile
          fs
          (certPath $ unPerasRoundNo roundNo)
          (BSL.pack [0xde, 0xad, 0xbe, 0xef])

      lazy <-
        DB.createDB
          args
            { DB.picdbaValidationPolicy = DB.ValidateOnRead
            }
      eager <-
        DB.createDB
          args
            { DB.picdbaValidationPolicy = DB.ValidateAllOnOpen
            }
      lazyRounds <- roundsOf <$> DB.getCertsAfter lazy (PerasRoundNo 0) maxBound
      eagerRounds <- roundsOf <$> DB.getCertsAfter eager (PerasRoundNo 0) maxBound
      pure $
        counterexample
          ( "stored: " <> show storedRounds
              <> "\nmissing: " <> show missingRounds
              <> "\ncorrupt: " <> show corruptRounds
              <> "\nexpected: " <> show expected
              <> "\nlazy: " <> show lazyRounds
              <> "\neager: " <> show eagerRounds
          )
          (lazyRounds == expected && eagerRounds == expected)

propGeneratedTempFilesAreSwept :: [Positive Word8] -> Property
propGeneratedTempFilesAreSwept generated =
  ioProperty $
    withFreshArgs $ \fs args -> do
      let rounds = Set.fromList (smallGeneratedRounds generated)
      forM_ rounds $ \roundNo ->
        writeRawFile
          fs
          (certTempPath $ unPerasRoundNo roundNo)
          (BSL.pack [0, 1, 2])

      db <- DB.createDB args
      names <- listDirectory (simHasFS fs) (mkFsPath [])
      certs <- DB.getCertsAfter db (PerasRoundNo 0) maxBound
      pure $
        counterexample
          ( "rounds: " <> show rounds
              <> "\nremaining entries: " <> show names
              <> "\ncertificates: " <> show certs
          )
          (Set.null names && null certs)

data Command
  = Add Word16 Word16
  | Query Word16 Word8
  | Reopen
  deriving Show

instance Arbitrary Command where
  arbitrary =
    frequency
      [ (5, Add <$> arbitrary <*> arbitrary)
      , (4, Query <$> arbitrary <*> arbitrary)
      , (1, pure Reopen)
      ]
  shrink command = case command of
    Add roundNo boost ->
      [Add roundNo' boost | roundNo' <- shrink roundNo]
        <> [Add roundNo boost' | boost' <- shrink boost]
    Query cursor limit ->
      [Query cursor' limit | cursor' <- shrink cursor]
        <> [Query cursor limit' | limit' <- shrink limit]
    Reopen -> []

newtype CommandSequence = CommandSequence [Command]
  deriving Show

instance Arbitrary CommandSequence where
  arbitrary = CommandSequence <$> resize 30 (listOf arbitrary)
  shrink (CommandSequence commands) =
    CommandSequence <$> shrinkList shrink commands

propCommandSequence :: CommandSequence -> Property
propCommandSequence (CommandSequence commands) =
  ioProperty $
    withFreshDB $ \args db ->
      go 0 args db Map.empty commands
 where
  go
    :: Int
    -> Complete DB.PerasImmutableCertDbArgs IO TestBlock
    -> DB.PerasImmutableCertDB IO TestBlock
    -> Map.Map PerasRoundNo (ValidatedPerasCert TestBlock)
    -> [Command]
    -> IO Property
  go _ _ _ _ [] = pure (property True)
  go step args db model (command : rest) =
    case command of
      Add roundWord boostWord -> do
        let roundNo = PerasRoundNo (fromIntegral roundWord)
            cert =
              mkCertWithBoost
                (fromIntegral roundWord)
                (fromIntegral boostWord)
            expectedResult
              | Map.member roundNo model = DB.CertAlreadyInImmutableDB
              | otherwise = DB.AddedCertToImmutableDB
            model' = Map.insertWith (\_ old -> old) roundNo cert model
        actualResult <- DB.addCert db cert
        if actualResult == expectedResult
          then go (step + 1) args db model' rest
          else
            mismatch
              step
              command
              expectedResult
              actualResult
      Query cursorWord limitWord -> do
        let cursor = PerasRoundNo (fromIntegral cursorWord)
            limit = fromIntegral limitWord
            expected =
              take (fromIntegral limitWord)
                [ cert
                | (roundNo, cert) <- Map.toAscList model
                , roundNo > cursor
                ]
        actual <- DB.getCertsAfter db cursor limit
        if actual == expected
          then go (step + 1) args db model rest
          else mismatch step command expected actual
      Reopen -> do
        reopened <- DB.createDB args
        go (step + 1) args reopened model rest

  mismatch :: (Show expected, Show actual) => Int -> Command -> expected -> actual -> IO Property
  mismatch step command expected actual =
    pure $
      counterexample
        ( "step: " <> show step
            <> "\ncommand: " <> show command
            <> "\nexpected: " <> show expected
            <> "\nactual: " <> show actual
        )
        (property False)

takeWord64 :: Word64 -> [a] -> [a]
takeWord64 limit xs =
  take
    (fromIntegral $ min limit (fromIntegral $ length xs))
    xs

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
    db <- DB.openDB args
    action fs args db

withFreshTracedDB ::
  ( StrictTMVar IO MockFS.MockFS ->
    Complete DB.PerasImmutableCertDbArgs IO TestBlock ->
    DB.PerasImmutableCertDB IO TestBlock ->
    IO [DB.TraceEvent TestBlock] ->
    IO a
  ) ->
  IO a
withFreshTracedDB action = do
  fs <- atomically $ newTMVar MockFS.empty
  (tracer, getTrace) <- recordingTracerIORef
  let args :: Complete DB.PerasImmutableCertDbArgs IO TestBlock
      args =
        (DB.defaultArgs @IO)
          { DB.picdbaCodecConfig = TestBlockCodecConfig
          , DB.picdbaHasFS = SomeHasFS (simHasFS fs)
          , DB.picdbaTracer = tracer
          }
  db <- DB.createDB args
  action fs args db getTrace

withFreshErrorDB ::
  ( StrictTMVar IO MockFS.MockFS ->
    StrictTVar IO Errors ->
    Complete DB.PerasImmutableCertDbArgs IO TestBlock ->
    DB.PerasImmutableCertDB IO TestBlock ->
    IO a
  ) ->
  IO a
withFreshErrorDB action = do
  fs <- atomically $ newTMVar MockFS.empty
  errorsVar <- newTVarIO emptyErrors
  let args :: Complete DB.PerasImmutableCertDbArgs IO TestBlock
      args =
        DB.defaultArgs
          { DB.picdbaCodecConfig = TestBlockCodecConfig
          , DB.picdbaHasFS = SomeHasFS (simErrorHasFS fs errorsVar)
          }
  db <- DB.createDB args
  action fs errorsVar args db

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

certTempPath :: Word64 -> FsPath
certTempPath roundNo =
  mkFsPath [show roundNo <> ".cert.tmp"]

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

smallGeneratedRounds :: [Positive Word8] -> [PerasRoundNo]
smallGeneratedRounds =
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
