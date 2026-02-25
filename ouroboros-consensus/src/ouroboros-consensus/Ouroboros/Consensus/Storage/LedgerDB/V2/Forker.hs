{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , ForkerState (..)
  , withForkerEnv
  , withForkerEnvSTM
  , modifyForkerEnv
  , modifyForkerEnvSTM

    -- * The API
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ) where

import Control.Exception
import Control.RAWLock (RAWLock, withWriteAccess)
import Control.ResourceRegistry
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

-- | The state inside a forker. It is mandatory this is a pure data structure so
-- that we can check the resulting `foeLedgerSeq` to see if a handle was
-- properly transferred when duplicating handles
data ForkerState m l = ForkerState
  { foeLedgerSeq :: !(LedgerSeq m l)
  -- ^ Local version of the LedgerSeq
  , foeSwitchVar :: !(StrictTVar m (LedgerSeq m l))
  -- ^ This TVar is the same as the LedgerDB one
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeLedgerDbLock :: !(RAWLock m ())
  -- ^ 'ldbOpenHandlesLock'.
  , foeWasCommitted :: !Bool
  }
  deriving Generic

deriving instance
  ( IOLike m
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  ) =>
  NoThunks (ForkerState m l)

newtype ForkerEnv m l
  = ForkerEnv (StrictTVar m (ForkerState m l))
  deriving Generic

deriving newtype instance
  ( IOLike m
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  ) =>
  NoThunks (ForkerEnv m l)

withForkerEnv :: MonadSTM m => ForkerEnv m l -> (ForkerState m l -> m r) -> m r
withForkerEnv (ForkerEnv f) k = readTVarIO f >>= k

withForkerEnvSTM :: MonadSTM m => ForkerEnv m l -> (ForkerState m l -> STM m r) -> STM m r
withForkerEnvSTM (ForkerEnv f) k = readTVar f >>= k

modifyForkerEnv ::
  MonadSTM m => ForkerEnv m l -> (ForkerState m l -> m (ForkerState m l)) -> m ()
modifyForkerEnv (ForkerEnv f) k = readTVarIO f >>= k >>= atomically . writeTVar f

modifyForkerEnvSTM ::
  MonadSTM m => ForkerEnv m l -> (ForkerState m l -> STM m (ForkerState m l, r)) -> STM m r
modifyForkerEnvSTM (ForkerEnv f) k = readTVar f >>= k >>= \(s, r) -> writeTVar f s >> pure r

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

implForkerReadTables ::
  (IOLike m, GetTip l) =>
  ForkerEnv m l ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks =
  withForkerEnv env $ \fState ->
    encloseTimedWith (ForkerReadTables >$< foeTracer fState) $ do
      let stateRef = currentHandle (foeLedgerSeq fState)
      read (tables stateRef) (state stateRef) ks

implForkerRangeReadTables ::
  (IOLike m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implForkerRangeReadTables qbs env rq0 = withForkerEnv env $ \fState ->
  encloseTimedWith (ForkerRangeReadTables >$< foeTracer fState) $ do
    let n = fromIntegral $ defaultQueryBatchSize qbs
        stateRef = currentHandle $ foeLedgerSeq fState
    case rq0 of
      NoPreviousQuery -> readRange (tables stateRef) (state stateRef) (Nothing, n)
      PreviousQueryWasFinal -> pure (LedgerTables emptyMK, Nothing)
      PreviousQueryWasUpTo k ->
        readRange (tables stateRef) (state stateRef) (Just k, n)

implForkerGetLedgerState ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l ->
  STM m (l EmptyMK)
implForkerGetLedgerState env =
  withForkerEnvSTM env $ pure . current . foeLedgerSeq

implForkerReadStatistics ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l ->
  m Statistics
implForkerReadStatistics env = withForkerEnv env $ \fState -> do
  traceWith (foeTracer fState) ForkerReadStatistics
  pure . Statistics . tablesSize . tables . currentHandle . foeLedgerSeq $ fState

implForkerPush ::
  (IOLike m, GetTip l, HasLedgerTables l, HasCallStack) =>
  ForkerEnv m l ->
  l DiffMK ->
  m ()
implForkerPush env newState = modifyForkerEnv env $ \fState -> do
  encloseTimedWith (ForkerPush >$< foeTracer fState) $ do
    let lseq = foeLedgerSeq fState
        st0 = current lseq
        st = forgetLedgerTables newState

    runWithTempRegistry $
      (\x -> (x, foeLedgerSeq x)) <$> do
        tbs <- duplicateWithDiffs (tables $ currentHandle lseq) st0 newState
        pure fState{foeLedgerSeq = extend (StateRef st tbs) lseq}

implForkerCommit ::
  (IOLike m, GetTip l, StandardHash l) =>
  ForkerEnv m l ->
  STM m (m ())
implForkerCommit env = modifyForkerEnvSTM env $ \fState -> assert (foeWasCommitted fState == False) $ do
  let ForkerState
        { foeLedgerSeq = LedgerSeq lseq
        , foeSwitchVar
        } = fState
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  (toCloseForker, toCloseLdb) <-
    stateTVar
      foeSwitchVar
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
  pure
    ( fState{foeLedgerSeq = LedgerSeq (AS.Empty toCloseForker), foeWasCommitted = True}
    , withWriteAccess (foeLedgerDbLock fState) $ \() -> do
        whenJust toCloseLdb closeLedgerSeq
        close $ tables toCloseForker
        pure ((), ())
    )
 where
  theImpossible =
    error $
      unwords
        [ "Critical invariant violation:"
        , "Forker chain does no longer intersect with selected chain."
        ]
