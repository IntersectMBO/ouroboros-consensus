{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- |

module Ouroboros.Consensus.Storage.LedgerDB.V2.Forker (
    ForkerEnv (..)
  , implForkerCommit
  , implForkerGetLedgerState
  , implForkerPush
  , implForkerRangeReadTables
  , implForkerReadStatistics
  , implForkerReadTables
    -- * The API
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ) where

import           Control.Tracer
import           Data.Maybe (fromMaybe)
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Prelude hiding (read)
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

data ForkerEnv m l blk = ForkerEnv {
    -- | Local version of the LedgerSeq
    foeLedgerSeq          :: !(StrictTVar m (LedgerSeq m l))
    -- | This TVar is the same as the LedgerDB one
  , foeSwitchVar          :: !(StrictTVar m (LedgerSeq m l))
    -- | Config
  , foeSecurityParam      :: !SecurityParam
    -- | Config
  , foeTracer             :: !(Tracer m TraceForkerEvent)
    -- | Release the resources
  , foeResourcesToRelease :: !(StrictTVar m (m ()))
  }
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  ) => NoThunks (ForkerEnv m l blk)

implForkerReadTables ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> LedgerTables l KeysMK
  -> m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
    traceWith (foeTracer env) ForkerReadTablesStart
    lseq <- readTVarIO (foeLedgerSeq env)
    tbs <- read (tables $ currentHandle lseq) ks
    traceWith (foeTracer env) ForkerReadTablesEnd
    pure tbs

implForkerRangeReadTables ::
     (MonadSTM m, GetTip l, LedgerTablesOp l)
  => QueryBatchSize
  -> ForkerEnv m l blk
  -> RangeQueryPrevious l
  -> m (LedgerTables l ValuesMK)
implForkerRangeReadTables qbs env rq0 = do
    traceWith (foeTracer env) ForkerRangeReadTablesStart
    ldb <- readTVarIO $ foeLedgerSeq env
    let n = fromIntegral $ defaultQueryBatchSize qbs
    case rq0 of
      NoPreviousQuery -> readRange (tables $ currentHandle ldb) (ltpure (Comp2 Nothing), n)
      PreviousQueryWasFinal -> pure $ ltpure emptyMK
      PreviousQueryWasUpTo k -> do
        tbs <- readRange (tables $ currentHandle ldb) (ltpure $ Comp2 $ Just (KeysMK $ Set.singleton k), n)
        traceWith (foeTracer env) ForkerRangeReadTablesEnd
        pure tbs

implForkerGetLedgerState ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeLedgerSeq env)

implForkerReadStatistics ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> m (Maybe Statistics)
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  fmap (fmap Statistics) . tablesSize . tables . currentHandle =<< readTVarIO (foeLedgerSeq env)

implForkerPush ::
     (IOLike m, GetTip l, HasLedgerTables l, HasCallStack, LedgerTablesOp l)
  => ForkerEnv m l blk
  -> l DiffMK
  -> m ()
implForkerPush env newState = do
  traceWith (foeTracer env) ForkerPushStart
  lseq <- readTVarIO (foeLedgerSeq env)
  let (st, tbs) = (forgetLedgerTables newState, ltprj newState)

  bracketOnError
    (duplicate (tables $ currentHandle lseq))
    close
    (\newtbs -> do
        pushDiffs newtbs tbs

        let lseq' = extend (StateRef st newtbs) lseq

        traceWith (foeTracer env) ForkerPushEnd
        atomically $ do
               writeTVar (foeLedgerSeq env) lseq'
               modifyTVar (foeResourcesToRelease env) (>> close newtbs)
     )

implForkerCommit ::
     (IOLike m, GetTip l, StandardHash l)
  => ForkerEnv m l blk
  -> STM m ()
implForkerCommit env = do
  LedgerSeq lseq <- readTVar foeLedgerSeq
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  (discardedBySelection, LedgerSeq discardedByPruning) <- do
    stateTVar
      foeSwitchVar
      (\(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (olddb', toClose) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) olddb' lseq
          -- Prune the resulting sequence to keep @k@ states
          let (l, s) = prune (foeSecurityParam env) (LedgerSeq newdb)
          pure ((toClose, l), s)
      )

  -- We are discarding the previous value in the TVar because we had accumulated
  -- actions for closing the states pushed to the forker. As we are committing
  -- those we have to close the ones discarded in this function and forget about
  -- those releasing actions.
  writeTVar foeResourcesToRelease $
       mapM_ (close . tables) $ AS.toOldestFirst discardedBySelection ++ AS.toOldestFirst discardedByPruning

  where
    ForkerEnv {
        foeLedgerSeq
      , foeSwitchVar
      , foeResourcesToRelease
      } = env

    theImpossible =
      error $ unwords [ "Critical invariant violation:"
                      , "Forker chain does no longer intersect with selected chain."
                      ]
