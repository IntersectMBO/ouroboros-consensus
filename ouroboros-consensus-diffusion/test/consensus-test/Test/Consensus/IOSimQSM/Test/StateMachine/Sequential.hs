{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
--
--
-- /___This is a superficial variation of the quickcheck-state-machine___/
-- /___source file that uses @io-classes@ instead of @MonadIO@, @UnliftIO@,___/
-- /___etc.___/ Perhaps
-- <https://github.com/input-output-hk/quickcheck-dynamic/tree/main/quickcheck-dynamic-iosim>
-- will supplant this.
--
--
--
-- Module      :  Test.Consensus.IOSimQSM.Test.StateMachine.Sequential
-- Copyright   :  (C) 2017, ATS Advanced Telematic Systems GmbH
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- This module contains helpers for generating, shrinking, and checking
-- sequential programs.
module Test.Consensus.IOSimQSM.Test.StateMachine.Sequential (runCommands') where

import Control.Concurrent.Class.MonadSTM.TChan
  ( TChan
  , newTChanIO
  , tryReadTChan
  , writeTChan
  )
import Control.Exception
  ( SomeAsyncException (..)
  , SomeException
  , displayException
  , fromException
  )
import Control.Monad (when)
import Control.Monad.Class.MonadSay
import Control.Monad.State.Strict (StateT, get, lift, put, runStateT)
import Data.Dynamic (Dynamic, toDyn)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Ouroboros.Consensus.Util.IOLike
  ( ExitCase (..)
  , IOLike
  , MonadCatch (..)
  , atomically
  , catch
  , throwIO
  )
import Test.StateMachine.Logic
import Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.StateMachine.Utils
import Text.Show.Pretty (ppShow)

------------------------------------------------------------------------

runCommands' ::
  (Show (cmd Concrete), Show (resp Concrete)) =>
  (Rank2.Traversable cmd, Rank2.Foldable resp) =>
  (IOLike m, MonadSay m) =>
  m (StateMachine model cmd m resp) ->
  Commands cmd resp ->
  m (History cmd resp, model Concrete, Reason)
runCommands' msm cmds = do
  hchan <- newTChanIO
  (reason, (_, _, _, model)) <-
    fst
      <$> generalBracket
        msm
        ( \sm' ec -> case ec of
            ExitCaseSuccess (_, (_, _, _, model)) -> cleanup sm' model
            _ -> getChanContents hchan >>= cleanup sm' . mkModel sm' . History
        )
        ( \sm'@StateMachine{initModel} ->
            runStateT
              (executeCommands sm' hchan (Pid 0) CheckEverything cmds)
              (emptyEnvironment, initModel, newCounter, initModel)
        )
  hist <- getChanContents hchan
  return (History hist, model, reason)

-- We should try our best to not let this function fail,
-- since it is used to cleanup resources, in parallel programs.
getChanContents :: IOLike m => TChan m a -> m [a]
getChanContents chan = reverse <$> atomically (go' [])
 where
  go' acc = do
    mx <- tryReadTChan chan
    case mx of
      Just x -> go' (x : acc)
      Nothing -> return acc

data Check
  = CheckPrecondition
  | CheckEverything

executeCommands ::
  (Show (cmd Concrete), Show (resp Concrete)) =>
  (Rank2.Traversable cmd, Rank2.Foldable resp) =>
  (IOLike m, MonadSay m) =>
  StateMachine model cmd m resp ->
  TChan m (Pid, HistoryEvent cmd resp) ->
  Pid ->
  Check ->
  Commands cmd resp ->
  StateT (Environment, model Symbolic, Counter, model Concrete) m Reason
executeCommands StateMachine{..} hchan pid check =
  go . unCommands
 where
  go [] = return Ok
  go (Command scmd _ vars : cmds) = do
    (env, smodel, counter, cmodel) <- get
    case (check, logic (precondition smodel scmd)) of
      (CheckPrecondition, VFalse ce) -> return (PreconditionFailed (show ce))
      (CheckEverything, VFalse ce) -> return (PreconditionFailed (show ce))
      _otherwise -> do
        let ccmd = fromRight (error "executeCommands: impossible") (reify env scmd)
        lift $ atomically (writeTChan hchan (pid, Invocation ccmd (S.fromList vars)))
        !ecresp <-
          lift $
            fmap Right (semantics ccmd)
              `catch` \(err :: SomeException) -> do
                when (isSomeAsyncException err) (say (displayException err) >> throwIO err)
                return (Left (displayException err))
        case ecresp of
          Left err -> do
            lift $ atomically (writeTChan hchan (pid, Exception err))
            return $ ExceptionThrown err
          Right cresp -> do
            let cvars = getUsedConcrete cresp
            if length vars /= length cvars
              then do
                let err = mockSemanticsMismatchError (ppShow ccmd) (ppShow vars) (ppShow cresp) (ppShow cvars)
                lift $ atomically (writeTChan hchan (pid, Response cresp))
                return $ MockSemanticsMismatch err
              else do
                lift $ atomically (writeTChan hchan (pid, Response cresp))
                case (check, logic (postcondition cmodel ccmd cresp)) of
                  (CheckEverything, VFalse ce) -> return (PostconditionFailed (show ce))
                  _otherwise ->
                    case (check, logic (fromMaybe (const Top) invariant cmodel)) of
                      (CheckEverything, VFalse ce') -> return (InvariantBroken (show ce'))
                      _otherwise -> do
                        let (sresp, counter') = runGenSym (mock smodel scmd) counter
                        put
                          ( insertConcretes vars cvars env
                          , transition smodel scmd sresp
                          , counter'
                          , transition cmodel ccmd cresp
                          )
                        go cmds

  isSomeAsyncException :: SomeException -> Bool
  isSomeAsyncException se = case fromException se of
    Just (SomeAsyncException _) -> True
    _ -> False

  mockSemanticsMismatchError :: String -> String -> String -> String -> String
  mockSemanticsMismatchError cmd svars cresp cvars =
    unlines
      [ ""
      , "Mismatch between `mock` and `semantics`."
      , ""
      , "The definition of `mock` for the command:"
      , ""
      , "    "
      , cmd
      , ""
      , "returns the following references:"
      , ""
      , "    "
      , svars
      , ""
      , "while the response from `semantics`:"
      , ""
      , "    "
      , cresp
      , ""
      , "returns the following references:"
      , ""
      , "    "
      , cvars
      , ""
      , "Continuing to execute commands at this point could result in scope"
      , "errors, because we might have commands that use references (returned"
      , "by `mock`) that are not available (returned by `semantics`)."
      , ""
      ]

getUsedConcrete :: Rank2.Foldable f => f Concrete -> [Dynamic]
getUsedConcrete = Rank2.foldMap (\(Concrete x) -> [toDyn x])
