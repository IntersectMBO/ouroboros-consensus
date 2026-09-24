{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Two shapes of the same failure, so the fields of each are partial.
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ouroboros.Consensus.Storage.LeiosDB.Exception (module Ouroboros.Consensus.Storage.LeiosDB.Exception) where

import Control.Monad.Class.MonadThrow (Exception (..), MonadThrow, SomeException, throwIO)
import Data.Aeson (KeyValue, ToJSON (..), Value (..), pairs, (.=))
import GHC.Stack (HasCallStack)
import qualified GHC.Stack

-- | Every way the Leios DB fails.
data LeiosDbException
  = -- | A database operation failed where it ran.
    LeiosDbException
      { errorMessage :: String
      , callStack :: String
      }
  | -- | A submitted write failed, as whoever awaits it sees it. A write runs
    -- on the writer rather than at its call site, so it knows what a read
    -- never can: which write it was, and where it was submitted from.
    LeiosDbWriteException
      { writeJob :: String
      , submittedFrom :: String
      , writeFailure :: SomeException
      }
  deriving Show

instance Exception LeiosDbException where
  displayException = \case
    LeiosDbException{errorMessage, callStack} ->
      errorMessage <> "\n" <> callStack
    LeiosDbWriteException{writeJob, submittedFrom, writeFailure} ->
      writeJob
        <> " failed: "
        <> displayException writeFailure
        <> "\nsubmitted from:\n"
        <> submittedFrom

-- | Fail with a 'LeiosDbException' carrying the call stack. Preferred over
-- 'error': an invariant the database broke is still a database failure, and
-- callers catch it as one.
throwLeiosDbException :: (HasCallStack, MonadThrow m) => String -> m a
throwLeiosDbException msg =
  throwIO
    LeiosDbException
      { errorMessage = msg
      , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
      }

instance ToJSON LeiosDbException where
  toJSON = Object . jsonLeiosDbException
  toEncoding = pairs . jsonLeiosDbException

jsonLeiosDbException :: (KeyValue a kv, Monoid kv) => LeiosDbException -> kv
jsonLeiosDbException = \case
  LeiosDbException{errorMessage, callStack} ->
    mconcat
      [ "kind" .= String "LeiosDbException"
      , "errorMessage" .= errorMessage
      , "callStack" .= callStack
      ]
  LeiosDbWriteException{writeJob, submittedFrom, writeFailure} ->
    mconcat
      [ "kind" .= String "LeiosDbWriteException"
      , "writeJob" .= writeJob
      , "errorMessage" .= displayException writeFailure
      , "callStack" .= submittedFrom
      ]
