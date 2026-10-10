{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , LeiosDbFailure (..)
  , LeiosDbWriteFailure (..)
  , throwLeiosDbException
  ) where

import Control.Monad.Class.MonadThrow (Exception (..), MonadThrow, SomeException, throwIO)
import Data.Aeson (KeyValue, ToJSON (..), Value (..), pairs, (.=))
import GHC.Stack (HasCallStack)
import qualified GHC.Stack

-- | Every way the Leios DB fails.
data LeiosDbException
  = LeiosDbException LeiosDbFailure
  | LeiosDbWriteException LeiosDbWriteFailure
  deriving Show

-- | A database operation failed where it ran.
data LeiosDbFailure = LeiosDbFailure
  { ldfErrorMessage :: String
  , ldfCallStack :: String
  }
  deriving Show

-- | A submitted write failed, as whoever awaits it sees it. A write runs
-- on the writer rather than at its call site, so it knows what a read
-- never can: which write it was, and where it was submitted from.
data LeiosDbWriteFailure = LeiosDbWriteFailure
  { ldwfWriteJob :: String
  , ldwfSubmittedFrom :: String
  , ldwfWriteFailure :: SomeException
  }
  deriving Show

instance Exception LeiosDbException where
  displayException = \case
    LeiosDbException LeiosDbFailure{ldfErrorMessage, ldfCallStack} ->
      ldfErrorMessage <> "\n" <> ldfCallStack
    LeiosDbWriteException LeiosDbWriteFailure{ldwfWriteJob, ldwfSubmittedFrom, ldwfWriteFailure} ->
      ldwfWriteJob
        <> " failed: "
        <> displayException ldwfWriteFailure
        <> "\nsubmitted from:\n"
        <> ldwfSubmittedFrom

-- | Fail with a 'LeiosDbException' carrying the call stack. Preferred over
-- 'error': an invariant the database broke is still a database failure, and
-- callers catch it as one.
throwLeiosDbException :: (HasCallStack, MonadThrow m) => String -> m a
throwLeiosDbException msg =
  throwIO $
    LeiosDbException
      LeiosDbFailure
        { ldfErrorMessage = msg
        , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
        }

instance ToJSON LeiosDbException where
  toJSON = Object . jsonLeiosDbException
  toEncoding = pairs . jsonLeiosDbException

jsonLeiosDbException :: (KeyValue a kv, Monoid kv) => LeiosDbException -> kv
jsonLeiosDbException = \case
  LeiosDbException LeiosDbFailure{ldfErrorMessage, ldfCallStack} ->
    mconcat
      [ "kind" .= String "LeiosDbException"
      , "errorMessage" .= ldfErrorMessage
      , "callStack" .= ldfCallStack
      ]
  LeiosDbWriteException LeiosDbWriteFailure{ldwfWriteJob, ldwfSubmittedFrom, ldwfWriteFailure} ->
    mconcat
      [ "kind" .= String "LeiosDbWriteException"
      , "writeJob" .= ldwfWriteJob
      , "errorMessage" .= displayException ldwfWriteFailure
      , "callStack" .= ldwfSubmittedFrom
      ]
