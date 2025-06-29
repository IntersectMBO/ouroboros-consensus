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

import Control.RAWLock hiding (read)
import Control.ResourceRegistry
import Control.Tracer
import Data.Maybe (fromMaybe)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
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
  , foeSecurityParam :: !SecurityParam
  -- ^ Config
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeResourcesToRelease :: !(RAWLock m (), ResourceKey m, StrictTVar m (m ()))
  -- ^ Release the resources
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
  tbs <- read (tables $ currentHandle lseq) ks
  traceWith (foeTracer env) ForkerReadTablesEnd
  pure tbs

implForkerRangeReadTables ::
  (MonadSTM m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l blk ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK)
implForkerRangeReadTables qbs env rq0 = do
  traceWith (foeTracer env) ForkerRangeReadTablesStart
  ldb <- readTVarIO $ foeLedgerSeq env
  let n = fromIntegral $ defaultQueryBatchSize qbs
  case rq0 of
    NoPreviousQuery -> readRange (tables $ currentHandle ldb) (Nothing, n)
    PreviousQueryWasFinal -> pure $ LedgerTables emptyMK
    PreviousQueryWasUpTo k -> do
      tbs <- readRange (tables $ currentHandle ldb) (Just k, n)
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
    (duplicate (tables $ currentHandle lseq))
    close
    ( \newtbs -> do
        pushDiffs newtbs st0 newState

        let lseq' = extend (StateRef st newtbs) lseq

        traceWith (foeTracer env) ForkerPushEnd
        atomically $ do
          writeTVar (foeLedgerSeq env) lseq'
          modifyTVar ((\(_, _, r) -> r) $ foeResourcesToRelease env) (>> close newtbs)
    )

implForkerCommit ::
  (IOLike m, GetTip l, StandardHash l) =>
  ForkerEnv m l blk ->
  STM m ()
implForkerCommit env = do
  LedgerSeq lseq <- readTVar foeLedgerSeq
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  closeDiscarded <- do
    stateTVar
      foeSwitchVar
      ( \(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (olddb', toClose) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) olddb' lseq
          -- Prune the resulting sequence to keep @k@ states
          let (closePruned, s) = prune (LedgerDbPruneKeeping (foeSecurityParam env)) (LedgerSeq newdb)
              closeDiscarded = do
                closePruned
                -- Do /not/ close the anchor of @toClose@, as that is also the
                -- tip of @olddb'@ which will be used in @newdb@.
                case toClose of
                  AS.Empty _ -> pure ()
                  _ AS.:< closeOld' -> closeLedgerSeq (LedgerSeq closeOld')
                -- Finally, close the anchor of @lseq@ (which is a duplicate of
                -- the head of @olddb'@).
                close $ tables $ AS.anchor lseq
          pure (closeDiscarded, s)
      )

  -- We are discarding the previous value in the TVar because we had accumulated
  -- actions for closing the states pushed to the forker. As we are committing
  -- those we have to close the ones discarded in this function and forget about
  -- those releasing actions.
  writeTVar ((\(_, _, r) -> r) $ foeResourcesToRelease) closeDiscarded
 where
  ForkerEnv
    { foeLedgerSeq
    , foeSwitchVar
    , foeResourcesToRelease
    } = env

  theImpossible =
    error $
      unwords
        [ "Critical invariant violation:"
        , "Forker chain does no longer intersect with selected chain."
        ]
