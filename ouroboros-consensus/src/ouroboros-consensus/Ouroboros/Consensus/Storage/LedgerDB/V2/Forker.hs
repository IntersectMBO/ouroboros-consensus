{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
  ( ForkerEnv (..)
  , implForkerCommit
  , implForkerGetLedgerState
  , implForkerPush
  , implForkerRangeReadTables
  , implForkerReadStatistics
  , implForkerReadTables

    -- * The API
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ) where

import Control.RAWLock (RAWLock, withWriteAccess)
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (fromMaybe)
import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
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
data ForkerEnv m l = ForkerEnv
  { foeLedgerSeq :: !(StrictTVar m (LedgerSeq m l))
  -- ^ Local version of the LedgerSeq
  , foeSwitchVar :: !(StrictTVar m (LedgerSeq m l))
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
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  ) =>
  NoThunks (ForkerEnv m l)

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

implForkerReadTables ::
  (IOLike m, GetTip l) =>
  ForkerEnv m l ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks =
  encloseTimedWith (ForkerReadTables >$< foeTracer env) $ do
    stateRef <- currentHandle <$> readTVarIO (foeLedgerSeq env)
    read (tables stateRef) (state stateRef) ks

implForkerRangeReadTables ::
  (IOLike m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implForkerRangeReadTables qbs env rq0 =
  encloseTimedWith (ForkerRangeReadTables >$< foeTracer env) $ do
    let n = fromIntegral $ defaultQueryBatchSize qbs
    stateRef <- currentHandle <$> readTVarIO (foeLedgerSeq env)
    case rq0 of
      NoPreviousQuery -> readRange (tables stateRef) (state stateRef) (Nothing, n)
      PreviousQueryWasFinal -> pure (LedgerTables emptyMK, Nothing)
      PreviousQueryWasUpTo k ->
        readRange (tables stateRef) (state stateRef) (Just k, n)

implForkerGetLedgerState ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l ->
  STM m (l EmptyMK)
implForkerGetLedgerState = fmap current . readTVar . foeLedgerSeq

implForkerReadStatistics ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l ->
  m Statistics
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  Statistics . tablesSize . tables . currentHandle <$> readTVarIO (foeLedgerSeq env)

implForkerPush ::
  (IOLike m, GetTip l, HasLedgerTables l, HasCallStack) =>
  ForkerEnv m l ->
  l DiffMK ->
  m ()
implForkerPush env newState = do
  encloseTimedWith (ForkerPush >$< foeTracer env) $ do
    lseq <- readTVarIO (foeLedgerSeq env)
    let st0 = current lseq
        st = forgetLedgerTables newState

    tbs <- duplicateWithDiffs (tables $ currentHandle lseq) st0 newState
    atomically $ writeTVar (foeLedgerSeq env) (extend (StateRef st tbs) lseq)

implForkerCommit ::
  (IOLike m, GetTip l, StandardHash l) =>
  ForkerEnv m l ->
  STM m (m ())
implForkerCommit env = do
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
  -- Note it doesn't really matter what we put here as the forker will not be
  -- used anymore. We put the 'toCloseForker' handle which will be closed by the
  -- cleanup function below.
  writeTVar (foeLedgerSeq env) (LedgerSeq (AS.Empty toCloseForker))
  pure
    ( do
        whenJust toCloseLdb $ \seqToClose ->
          withWriteAccess (foeLedgerDbLock env) $ \() -> do
            closeLedgerSeq seqToClose
            pure ((), ())
        close $ tables toCloseForker
    )
 where
  theImpossible =
    error $
      unwords
        [ "Critical invariant violation:"
        , "Forker chain does no longer intersect with selected chain."
        ]
