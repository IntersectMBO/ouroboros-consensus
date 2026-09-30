{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Low-level SQLite primitives that turn every SQLite error into a
-- 'LeiosDbException'.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
  ( useStmt
  , dbBindBlob
  , dbBindUtf8
  , dbBindInt64
  , dbExec
  , dbFinalize
  , dbPrepare
  , withStmt
  , orCloseOnError
  , dbWithTransaction
  , dbWithWriteTransactionRaw
  , dbWithTransactionAs
  , dbStep
  , dbStep1
  , dbStepSafe
  , dbStep1Safe
  , readSingleInt64
  , readReturningInt64
  , queryInt64
  , collectBlobs
  , execJson
  , dbStepInsert
  , dbStepInsertOrTrace
  , jsonHexArray
  , jsonIntArray
  , withDie
  , withDieStmt
  , withDieJust
  , withDieDoneStmt
  , throwDbException
  ) where

import Control.Exception (SomeException, throwIO)
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow (bracket, catch, finally, generalBracket)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , LeiosDbFailure (..)
  , throwLeiosDbException
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (ExitCase (..))

-- | Run an action on a pre-prepared statement and always @sqlite3_reset@
-- it afterwards, regardless of outcome. Reset uses raw 'DB.reset' (no
-- error re-throw) because SQLite reports the /previous/ step's error via
-- reset; we let the original exception propagate instead.
useStmt :: DB.Statement -> IO a -> IO a
useStmt stmt action =
  action `finally` (void $ DB.reset stmt)

dbBindBlob :: HasCallStack => DB.Statement -> DB.ParamIndex -> ByteString -> IO ()
dbBindBlob q p v = withDieStmt q $ DB.bindBlob q p v

-- | Bind as TEXT. Needed for JSON1 payloads: 'json_each' interprets BLOB
-- arguments as JSONB (SQLite ≥ 3.45), our payload is ASCII JSON.
dbBindUtf8 :: HasCallStack => DB.Statement -> DB.ParamIndex -> ByteString -> IO ()
dbBindUtf8 q p v = withDieStmt q $ DB.bindText q p (DB.Utf8 v)

dbBindInt64 :: HasCallStack => DB.Statement -> DB.ParamIndex -> Int64 -> IO ()
dbBindInt64 q p v = withDieStmt q $ DB.bindInt64 q p v

dbExec :: HasCallStack => DB.Database -> DB.Utf8 -> IO ()
dbExec db q = withDie db $ fmap (first fst) $ DB.exec db q

-- | Finalize a statement, exactly once, ignoring the return code.
--
-- @sqlite3_finalize@ always frees the statement; its return code merely
-- replays the most recent evaluation's error (sticky, like 'DB.reset' -- see
-- 'useStmt'). Neither retrying nor throwing is ever right here: a busy-retry
-- would call @sqlite3_finalize@ on freed memory (the use-after-free behind
-- the devnet segfaults of 2026-08-31), and a throw would propagate from
-- bracket cleanup.
dbFinalize :: DB.Statement -> IO ()
dbFinalize q = void $ DB.finalize q

dbPrepare :: HasCallStack => DB.Database -> DB.Utf8 -> IO DB.Statement
dbPrepare db q = withDieJust db $ DB.prepare db q

-- | Prepare a statement, run the action, finalize.
withStmt :: HasCallStack => DB.Database -> String -> (DB.Statement -> IO a) -> IO a
withStmt db sql = bracket (dbPrepare db (fromString sql)) dbFinalize

-- | Close the connection if the action throws, then rethrow. For whatever
-- is acquired on a fresh connection -- an attach, a schema, a statement --
-- after the open succeeded and before anything owns the close.
orCloseOnError :: DB.Database -> IO a -> IO a
orCloseOnError db act =
  act `catch` \(e :: SomeException) -> do
    _ <- DB.close db
    throwIO e

-- TODO: alternative: bind and use https://www.sqlite.org/c3ref/busy_handler.html

-- | A read-only transaction: @BEGIN DEFERRED@, so readers do not exclude each
-- other. Any transaction that writes must use @dbWithWriteTransaction@.
dbWithTransaction :: HasCallStack => DB.Database -> IO a -> IO a
dbWithTransaction = dbWithTransactionAs "BEGIN"

-- | @dbWithWriteTransaction@ for the maintenance paths (promotion to immutable, GC), which hold
-- a raw 'DB.Database' rather than a 'Conn'.
dbWithWriteTransactionRaw :: HasCallStack => DB.Database -> IO a -> IO a
dbWithWriteTransactionRaw = dbWithTransactionAs "BEGIN IMMEDIATE"

dbWithTransactionAs :: HasCallStack => String -> DB.Database -> IO a -> IO a
dbWithTransactionAs begin db k =
  do
    fmap fst
    $ generalBracket
      (dbExec db (fromString begin))
      ( \() -> \case
          ExitCaseSuccess _ -> dbExec db (fromString "COMMIT")
          ExitCaseException _ -> dbExec db (fromString "ROLLBACK")
          ExitCaseAbort -> dbExec db (fromString "ROLLBACK")
      )
      (\() -> k)

dbStep :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStep stmt = withDieStmt stmt $ DB.stepNoCB stmt

dbStep1 :: HasCallStack => DB.Statement -> IO ()
dbStep1 stmt = withDieDoneStmt stmt $ DB.stepNoCB stmt

-- | 'dbStep' through the safe FFI call ('DB.step' rather than
-- 'DB.stepNoCB'): a safe call does not block its RTS capability, so the
-- potentially long-running maintenance statements (copy, GC) must use it.
dbStepSafe :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStepSafe stmt = withDieStmt stmt $ DB.step stmt

-- | 'dbStep1' through the safe FFI call; see 'dbStepSafe'.
dbStep1Safe :: HasCallStack => DB.Statement -> IO ()
dbStep1Safe stmt = withDieDoneStmt stmt $ DB.step stmt

-- | Read a single-row, single-column integer result, stepping (safe FFI)
-- through to completion -- which also suits @RETURNING@ statements, whose
-- write only certainly happened once they report 'DB.Done'.
readSingleInt64 :: HasCallStack => DB.Statement -> IO Int64
readSingleInt64 stmt =
  dbStepSafe stmt >>= \case
    DB.Done -> throwLeiosDbException "readSingleInt64: expected a row"
    DB.Row -> do
      n <- DB.columnInt64 stmt 0
      dbStepSafe stmt >>= \case
        DB.Done -> pure n
        DB.Row -> throwLeiosDbException "readSingleInt64: expected exactly one row"

-- | Read a single-column @Int64@ from a statement that uses a
-- @RETURNING@ clause on a PK-scoped @UPDATE@ (i.e. produces exactly one
-- row followed by 'DB.Done'). Any other shape is a programmer error.
readReturningInt64 :: DB.Statement -> IO Int64
readReturningInt64 stmt =
  dbStep stmt >>= \case
    DB.Done ->
      throwLeiosDbException "readReturningInt64: expected one row from RETURNING, got Done"
    DB.Row -> do
      n <- DB.columnInt64 stmt 0
      dbStep stmt >>= \case
        DB.Done -> pure n
        DB.Row -> throwLeiosDbException "readReturningInt64: expected exactly one row from RETURNING"

-- | Run a query that yields exactly one integer column.
queryInt64 :: HasCallStack => DB.Database -> String -> IO Int64
queryInt64 db sql =
  bracket (dbPrepare db (fromString sql)) dbFinalize $ \stmt ->
    dbStep stmt >>= \case
      DB.Row -> DB.columnInt64 stmt 0
      DB.Done -> error ("queryInt64: expected a row: " <> sql)

-- | Step a statement (safe FFI) to completion, collecting blob column 0.
collectBlobs :: HasCallStack => DB.Statement -> IO [ByteString]
collectBlobs stmt = loop []
 where
  loop acc =
    dbStepSafe stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        b <- DB.columnBlob stmt 0
        loop (b : acc)

-- | Bind a JSON payload ('jsonHexArray') to parameter 1 and execute the statement
--   via non-blocking Safe FFI.
execJson :: HasCallStack => DB.Statement -> ByteString -> IO ()
execJson stmt json =
  useStmt stmt $ do
    dbBindUtf8 stmt 1 json
    dbStep1Safe stmt

-- | Like 'dbStep1' but returns 'True' on success and 'False' on constraint
-- violation (duplicate key). Other errors are thrown as usual.
dbStepInsert :: HasCallStack => DB.Statement -> IO Bool
dbStepInsert stmt =
  DB.stepNoCB stmt >>= \case
    Left DB.ErrorConstraint -> pure False
    Left e -> DB.getStatementDatabase stmt >>= \db -> throwDbException db e
    Right DB.Done -> pure True
    Right DB.Row -> throwLeiosDbException "dbStepInsert: unexpected Row result"

-- | Step an INSERT statement, absorbing UNIQUE/PRIMARY KEY violations and
-- emitting a 'TraceLeiosDbInsertCollision' for each one. The caller supplies a
-- table label and a key description for the trace.
--
-- After a constraint error, sqlite3_reset reports the same error code; the
-- normal 'dbReset' would re-throw it, so we use raw 'DB.reset' and discard the
-- return value. This also leaves the statement in a clean state for the
-- subsequent bracket-time 'dbFinalize' to succeed.
dbStepInsertOrTrace ::
  HasCallStack =>
  Tracer IO TraceLeiosDb ->
  String ->
  String ->
  DB.Statement ->
  IO ()
dbStepInsertOrTrace tracer table key stmt = do
  novel <- dbStepInsert stmt
  _ <- DB.reset stmt
  unless novel $
    traceWith tracer (TraceLeiosDbInsertCollision table key)

-- ** JSON payloads

-- | Build a JSON array of hex-encoded blobs: @["aabb...","1234...",...]@.
-- Consumed on the SQL side via @json_each(?)@ + @unhex(je.value)@.
jsonHexArray :: [ByteString] -> ByteString
jsonHexArray xs =
  BSL.toStrict . BB.toLazyByteString $
    BB.char7 '[' <> commaSep (map hexElem xs) <> BB.char7 ']'
 where
  hexElem b = BB.char7 '"' <> BB.byteStringHex b <> BB.char7 '"'
  commaSep = mconcat . intersperseB (BB.char7 ',')
  intersperseB _ [] = []
  intersperseB _ [x] = [x]
  intersperseB s (x : rest) = x : s : intersperseB s rest

-- | Build a JSON array of integers: @[1,2,3,...]@. Same consumer pattern
-- as 'jsonHexArray' (values are already ints, so no decoding step).
jsonIntArray :: [Int] -> ByteString
jsonIntArray xs =
  BSL.toStrict . BB.toLazyByteString $
    BB.char7 '[' <> commaSep (map BB.intDec xs) <> BB.char7 ']'
 where
  commaSep = mconcat . intersperseB (BB.char7 ',')
  intersperseB _ [] = []
  intersperseB _ [x] = [x]
  intersperseB s (x : rest) = x : s : intersperseB s rest

-- ** Error "handling"

-- | Run a database action that may return an error, and throw a
-- 'LeiosDbException' if it does.
--
-- Including 'DB.ErrorBusy': the connections set a 'busy_timeout', so SQLite
-- has already waited that long in C before reporting it. Waiting again on
-- top of that only converts a lock nobody is going to release into an
-- unbounded stall -- and with one writer per partition, a lock nobody is
-- going to release means another process has the file.
withDie :: HasCallStack => DB.Database -> IO (Either DB.Error a) -> IO a
withDie db io =
  io >>= \case
    Left e -> throwDbException db e
    Right x -> pure x

withDieStmt :: HasCallStack => DB.Statement -> IO (Either DB.Error a) -> IO a
withDieStmt stmt io = do
  db <- DB.getStatementDatabase stmt
  withDie db io

withDieJust :: HasCallStack => DB.Database -> IO (Either DB.Error (Maybe a)) -> IO a
withDieJust db io =
  withDie db io >>= \case
    Nothing ->
      throwIO $
        LeiosDbException
          LeiosDbFailure
            { ldfErrorMessage = "unexpected Nothing"
            , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
            }
    Just x -> pure x

withDieDoneStmt :: HasCallStack => DB.Statement -> IO (Either DB.Error DB.StepResult) -> IO ()
withDieDoneStmt stmt io = do
  db <- DB.getStatementDatabase stmt
  withDie db io >>= \case
    DB.Row ->
      throwIO $
        LeiosDbException
          LeiosDbFailure
            { ldfErrorMessage = "unexpected Row"
            , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
            }
    DB.Done -> pure ()

throwDbException :: HasCallStack => DB.Database -> DB.Error -> IO a
throwDbException db e = do
  reason <- DB.errmsg db
  throwIO $
    LeiosDbException
      LeiosDbFailure
        { ldfErrorMessage = show e <> ": " <> show reason
        , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
        }
