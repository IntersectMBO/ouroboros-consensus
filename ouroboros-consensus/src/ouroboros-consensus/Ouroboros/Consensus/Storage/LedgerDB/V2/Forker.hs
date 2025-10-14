{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.RAWLock (RAWLock)
import Control.ResourceRegistry
import Control.Tracer
import Data.Maybe (fromMaybe)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

data ForkerEnv m l blk = ForkerEnv
  { foeLedgerSeq :: !(StrictTVar m (LedgerSeq m l))
  -- ^ Local version of the LedgerSeq
  , foeSwitchVar :: !(StrictTVar m (LedgerSeq m l))
  -- ^ This TVar is the same as the LedgerDB one
  , foeLedgerDbRegistry :: !(ResourceRegistry m)
  -- ^ The registry in the LedgerDB to move handles to in case we commit the
  -- forker.
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeResourceRegistry :: !(ResourceRegistry m)
  -- ^ The registry local to the forker
  , foeInitialHandleKey :: !(ResourceKey m)
  -- ^ Resource key for the initial handle to ensure it is released. See
  -- comments in 'implForkerCommit'.
  , foeCleanup :: !(StrictTVar m (m ()))
  -- ^ An action to run on cleanup. If the forker was not committed this will be
  -- the trivial action. Otherwise it will move the required handles to the
  -- LedgerDB and release the discarded ones.
  , foeLedgerDbLock :: !(RAWLock m ())
  -- ^ 'ldbOpenHandlesLock'.
  }
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  ) =>
  NoThunks (ForkerEnv m l blk)

implForkerReadTables ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
  traceWith (foeTracer env) ForkerReadTablesStart
  lseq <- readTVarIO (foeLedgerSeq env)
  let stateRef = currentHandle lseq
  tbs <- read (tables stateRef) (state stateRef) ks
  traceWith (foeTracer env) ForkerReadTablesEnd
  pure tbs

implForkerRangeReadTables ::
  (MonadSTM m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l blk ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implForkerRangeReadTables qbs env rq0 = do
  traceWith (foeTracer env) ForkerRangeReadTablesStart
  ldb <- readTVarIO $ foeLedgerSeq env
  let n = fromIntegral $ defaultQueryBatchSize qbs
      stateRef = currentHandle ldb
  case rq0 of
    NoPreviousQuery -> readRange (tables stateRef) (state stateRef) (Nothing, n)
    PreviousQueryWasFinal -> pure (LedgerTables emptyMK, Nothing)
    PreviousQueryWasUpTo k -> do
      tbs <- readRange (tables stateRef) (state stateRef) (Just k, n)
      traceWith (foeTracer env) ForkerRangeReadTablesEnd
      pure tbs

implForkerGetLedgerState ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeLedgerSeq env)

implForkerReadStatistics ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  m (Maybe Statistics)
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  fmap (fmap Statistics) . tablesSize . tables . currentHandle =<< readTVarIO (foeLedgerSeq env)

implForkerPush ::
  (IOLike m, GetTip l, HasLedgerTables l, HasCallStack) =>
  ForkerEnv m l blk ->
  l DiffMK ->
  m ()
implForkerPush env newState = do
  traceWith (foeTracer env) ForkerPushStart
  lseq <- readTVarIO (foeLedgerSeq env)

  let st0 = current lseq
      st = forgetLedgerTables newState

  bracketOnError
    (duplicate (tables $ currentHandle lseq) (foeResourceRegistry env))
    (release . fst)
    ( \(_, newtbs) -> do
        pushDiffs newtbs st0 newState

        let lseq' = extend (StateRef st newtbs) lseq

        traceWith (foeTracer env) ForkerPushEnd
        atomically $ writeTVar (foeLedgerSeq env) lseq'
    )

implForkerCommit ::
  (IOLike m, GetTip l, StandardHash l) =>
  ForkerEnv m l blk ->
  STM m ()
implForkerCommit env = do
  LedgerSeq lseq <- readTVar foeLedgerSeq
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  transfer <-
    stateTVar
      foeSwitchVar
      ( \(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (toKeepBase, toCloseLdb) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          (toCloseForker, toKeepTip) <-
            AS.splitAfterMeasure intersectionSlot (either predicate predicate) lseq
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) toKeepBase toKeepTip
          let transferCommitted = do
                -- Do /not/ close the anchor of @toClose@, as that is also the
                -- tip of @olddb'@ which will be used in @newdb@.
                case toCloseLdb of
                  AS.Empty _ -> pure ()
                  _ AS.:< closeOld' -> closeLedgerSeq (LedgerSeq closeOld')
                closeLedgerSeq (LedgerSeq toCloseForker)

                -- All the other remaining handles are transferred to the LedgerDB registry
                keys <- ingestRegistry foeResourceRegistry foeLedgerDbRegistry
                mapM_ (\(k, v) -> transfer (tables v) k) $ zip keys (AS.toOldestFirst toKeepTip)

          pure (transferCommitted, LedgerSeq newdb)
      )
  writeTVar foeCleanup transfer
 where
  ForkerEnv
    { foeLedgerSeq
    , foeSwitchVar
    , foeResourceRegistry
    , foeLedgerDbRegistry
    , foeCleanup
    } = env

  theImpossible =
    error $
      unwords
        [ "Critical invariant violation:"
        , "Forker chain does no longer intersect with selected chain."
        ]
