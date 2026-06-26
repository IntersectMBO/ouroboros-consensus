{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
  ( ForkerEnv (..)
  , implForkerCommit
  , implForkerGetLedgerState
  , implForkerPush
  , implForkerReadStatistics
  , implForkerReadTables

    -- * The API
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ) where

import Control.Exception
import Control.Monad (when)
import Control.RAWLock (RAWLock, withWriteAccess)
import Control.Tracer
import Data.Maybe (fromMaybe)
import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import Prelude hiding (read)

-- | The state inside a forker.
data ForkerEnv m l blk = ForkerEnv
  { foeLedgerSeq :: !(StrictTVar m (LedgerSeq m l blk))
  -- ^ Local version of the LedgerSeq
  , foeSwitchVar :: !(StrictTVar m (LedgerSeq m l blk))
  -- ^ This TVar is the same as the LedgerDB one
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeLedgerDbLock :: !(RAWLock m ())
  -- ^ 'ldbOpenHandlesLock'.
  , foeWasCommitted :: !(StrictTVar m Bool)
  }
  deriving Generic

deriving instance
  ( IOLike m
  , NoThunks (l blk)
  ) =>
  NoThunks (ForkerEnv m l blk)

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

implForkerReadTables ::
  (IOLike m, GetTip (l blk)) =>
  ForkerEnv m l blk ->
  Keys blk ->
  m (Values blk)
implForkerReadTables env ks =
  encloseTimedWith (ForkerReadTables >$< foeTracer env) $ do
    stateRef <- currentHandle <$> readTVarIO (foeLedgerSeq env)
    read (tables stateRef) (state stateRef) ks

implForkerGetLedgerState ::
  (MonadSTM m, GetTip (l blk)) =>
  ForkerEnv m l blk ->
  STM m (l blk)
implForkerGetLedgerState = fmap current . readTVar . foeLedgerSeq

implForkerReadStatistics ::
  (MonadSTM m, GetTip (l blk)) =>
  ForkerEnv m l blk ->
  m Statistics
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  Statistics . tablesSize . tables . currentHandle <$> readTVarIO (foeLedgerSeq env)

implForkerPush ::
  (IOLike m, GetTip (l blk), HasCallStack) =>
  ForkerEnv m l blk ->
  l blk ->
  Diff blk ->
  m ()
implForkerPush env newState diff = do
  encloseTimedWith (ForkerPush >$< foeTracer env) $ do
    lseq <- readTVarIO (foeLedgerSeq env)

    -- We don't need to track this resource anywhere because if an exception
    -- comes here, the exception will abort ChainSel and therefore the node is
    -- shutting down so the resources (the Session in LSM) will be closed. See
    -- "Resource management in the LedgerDB" in
    -- "Ouroboros.Consensus.Storage.LedgerDB.API".
    tbs <- duplicateWithDiffs (tables $ currentHandle lseq) diff
    atomically $ writeTVar (foeLedgerSeq env) (extend (StateRef newState tbs) lseq)

implForkerCommit ::
  (IOLike m, GetTip (l blk), StandardHash (l blk)) =>
  ForkerEnv m l blk ->
  STM m (m ())
implForkerCommit env = do
  wasCommitted <- readTVar (foeWasCommitted env)
  when wasCommitted $
    throw $
      CriticalInvariantViolation "Critical invariant violation: forker has been committed twice"
  LedgerSeq lseq <- readTVar (foeLedgerSeq env)
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  (toCloseForker, toCloseLdb) <-
    stateTVar
      (foeSwitchVar env)
      ( \(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (toKeepBase, toCloseLdb) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) toKeepBase lseq
          -- Do /not/ close the anchor of @toClose@, as that is also the
          -- tip of @olddb'@ which will be used in @newdb@.
          let ldbToClose = case toCloseLdb of
                AS.Empty _ -> Nothing
                _ AS.:< closeOld' -> Just (LedgerSeq closeOld')
          pure ((AS.anchor lseq, ldbToClose), LedgerSeq newdb)
      )
  writeTVar (foeWasCommitted env) True
  -- We put 'toCloseForker' in the LedgerSeq to then close it when closing the
  -- forker.
  writeTVar (foeLedgerSeq env) (LedgerSeq (AS.Empty toCloseForker))
  pure
    ( whenJust toCloseLdb $ \seqToClose ->
        withWriteAccess (foeLedgerDbLock env) $ \() -> do
          closeLedgerSeq seqToClose
          pure ((), ())
    )
 where
  theImpossible =
    throw $
      CriticalInvariantViolation $
        unwords
          [ "Critical invariant violation:"
          , "Forker chain does no longer intersect with selected chain."
          ]

newtype CriticalInvariantViolation = CriticalInvariantViolation {message :: String}
  deriving Show
  deriving anyclass Exception
