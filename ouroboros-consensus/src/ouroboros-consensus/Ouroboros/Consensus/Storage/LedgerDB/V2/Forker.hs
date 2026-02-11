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
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
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
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.EscapableResources
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
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeCleanup :: !(StrictTVar m (m ()))
  -- ^ An action to run on cleanup. If the forker was not committed this will be
  -- the trivial action. Otherwise it will move the required handles to the
  -- LedgerDB and release the discarded ones.
  , foeLedgerDbLock :: !(RAWLock m ())
  -- ^ 'ldbOpenHandlesLock'.
  , foeWasCommitted :: !(StrictTVar m Bool)
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
  (IOLike m, GetTip l) =>
  ForkerEnv m l blk ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks =
  encloseTimedWith (ForkerReadTables >$< foeTracer env) $ do
    lseq <- readTVarIO (foeLedgerSeq env)
    let stateRef = currentHandle lseq
    read (tables stateRef) (state stateRef) ks

implForkerRangeReadTables ::
  (IOLike m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l blk ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implForkerRangeReadTables qbs env rq0 =
  encloseTimedWith (ForkerRangeReadTables >$< foeTracer env) $ do
    ldb <- readTVarIO $ foeLedgerSeq env
    let n = fromIntegral $ defaultQueryBatchSize qbs
        stateRef = currentHandle ldb
    case rq0 of
      NoPreviousQuery -> readRange (tables stateRef) (state stateRef) (Nothing, n)
      PreviousQueryWasFinal -> pure (LedgerTables emptyMK, Nothing)
      PreviousQueryWasUpTo k ->
        readRange (tables stateRef) (state stateRef) (Just k, n)

implForkerGetLedgerState ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeLedgerSeq env)

implForkerReadStatistics ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  m Statistics
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  fmap Statistics . tablesSize . tables . currentHandle =<< readTVarIO (foeLedgerSeq env)

implForkerPush ::
  (IOLike m, GetTip l, HasLedgerTables l, HasCallStack) =>
  ForkerEnv m l blk ->
  l DiffMK ->
  ContT r m ()
implForkerPush env newState =
  encloseTimedWithContT (ForkerPush >$< foeTracer env) $ do
    lseq <- lift $ readTVarIO (foeLedgerSeq env)

    let st0 = current lseq
        st = forgetLedgerTables newState

    h <- duplicate (tables $ currentHandle lseq)
    lift $ pushDiffs h st0 newState

    let lseq' = extend (StateRef st h) lseq

    lift $ atomically $ writeTVar (foeLedgerSeq env) lseq'

implForkerCommit ::
  (IOLike m, GetTip l, StandardHash l) =>
  ForkerEnv m l blk ->
  STM m ()
implForkerCommit env = do
  LedgerSeq lseq <- readTVar foeLedgerSeq
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  (toUntrack, toCloseHandles) <-
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
          -- Do /not/ close the anchor of @toClose@, as that is also the
          -- tip of @olddb'@ which will be used in @newdb@.
          let toCloseHandles = do
                closeLedgerSeq (LedgerSeq toCloseForker)
                case toCloseLdb of
                  AS.Empty _ -> pure ()
                  _ AS.:< closeOld' -> closeLedgerSeq (LedgerSeq closeOld')
          pure ((toKeepTip, toCloseHandles), LedgerSeq newdb)
      )
  mapM_ (untrack . tables) (AS.toOldestFirst toUntrack)
  writeTVar foeCleanup toCloseHandles
  writeTVar foeWasCommitted True
 where
  ForkerEnv
    { foeLedgerSeq
    , foeSwitchVar
    , foeCleanup
    , foeWasCommitted
    } = env

  theImpossible =
    error $
      unwords
        [ "Critical invariant violation:"
        , "Forker chain does no longer intersect with selected chain."
        ]
