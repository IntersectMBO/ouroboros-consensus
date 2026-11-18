{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  , WithBoostedBlockStatus (..)
  , forgetBoostedBlockStatus
  , AddPerasCertResult (..)
  , PerasRoundNo

    -- * Invariants
  , prop_addCertThenGetCertIds
  , prop_getCertsAfterZero
  , prop_getCertsAfterMonotonic
  , prop_garbageCollectRemovesOldCerts
  , prop_addCertLatestCertSeenMonotonic
  , prop_garbageCollectPreservesLatestCertSeen

    -- * Errors
  , PerasCertDbError (..)
  ) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot)
import Ouroboros.Consensus.Util.CallStack (HasCallStack, PrettyCallStack)
import Ouroboros.Consensus.Util.IOLike (Exception (displayException), MonadSTM (STM, atomically))
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

-- | API for the 'PerasCertDB', which stores Peras certificates.
--
-- The database has two parts:
--
-- * The /volatile/ certificates, whose boosted blocks are not (yet) older than
--   the last garbage collection. They are kept in memory, and used for chain
--   selection and voting. Every volatile certificate is also eagerly persisted
--   to disk, so that it survives restarts.
--
-- * The /historical/ certificates, whose boosted blocks are part of the
--   immutable chain. They live on disk only, and are served to syncing nodes
--   (see 'getHistoricalCertsAfter').
--
-- Volatile certificates are promoted to historical ones by 'garbageCollect'
-- once the immutable tip moves past their boosted blocks.
--
-- = Concurrency
--
-- Like the block ImmutableDB, the same database must not be opened more than
-- once at a time: each open handle keeps its own in-memory state, which would
-- otherwise diverge from the others. This is not checked by the database
-- itself:
--
-- * Across processes, it is ensured by the node's database lock file (see
--   @Ouroboros.Consensus.Node.DbLock@). The lock file lives at the root of the
--   immutable database path, under which this database's directory
--   (@perascert/@) is mounted too, also when the immutable and volatile
--   databases are stored on separate paths.
--
-- * Within a process, it is ensured by the ChainDB opening it exactly once and
--   closing it with 'closeDB' before any reopen.
--
-- A single handle may be used concurrently.
data PerasCertDB m blk = PerasCertDB
  { addCert ::
      WithArrivalTime (ValidatedPerasCert blk) ->
      STM m (m AddPerasCertResult)
  -- ^ Add a Peras certificate to the database. The STM transaction adds the
  -- certificate to the in-memory volatile state, and the resulting 'm' action
  -- performs tracing and persists the certificate to disk as a volatile
  -- certificate file. The 'AddPerasCertResult' indicates whether the
  -- certificate was actually added, and why not otherwise.
  --
  -- NOTE: Use the @join . atomically@ pattern to run both the transaction
  -- and the side-effects in sequence.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getVolatileCertIds ::
      STM m (Set PerasRoundNo)
  -- ^ Get the set of the round numbers of all the /volatile/ certificates
  -- currently in the database. Historical certificates are not included.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getVolatileCertsAfter ::
      PerasRoundNo ->
      STM m (Map PerasRoundNo (m (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ Get all /volatile/ certs with a round number strictly greater than the
  -- given one, in ascending order.
  -- Unlike 'getVotesAfter' in 'PerasVoteDB', the resulting map contains 'm' actions to
  -- retrieve the certificates, instead of the certificates directly. This is to
  -- allow implementation with on-disk storage to retrieve certificates from disk.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ Return the Peras weights in order compare the current selection against
  -- potential candidate chains, namely the weights for blocks not older than
  -- the current immutable tip. It might contain weights for even older blocks
  -- if they have not yet been garbage-collected.
  --
  -- The 'Fingerprint' is updated every time a new certificate is added, but it
  -- stays the same when certificates are garbage-collected.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getLatestCertSeen ::
      STM m (Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ This field impacts voting directly because having seen a certificate is a
  -- precondition for voting in any round except for the very first one
  -- (at origin).
  --
  -- This is only about the certificates added since the database was opened:
  -- volatile certificates loaded from disk when opening do not count.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , garbageCollect ::
      (Point blk -> m Bool) ->
      SlotNo ->
      STM m (m ())
  -- ^ @'garbageCollect' isImmutable slotNo@ garbage-collects the volatile
  -- certificates whose boosted block has a slot number strictly smaller than
  -- @slotNo@.
  --
  -- The STM transaction removes them from the in-memory volatile state, and the
  -- resulting 'm' action promotes on disk those whose boosted block satisfies
  -- @isImmutable@ (i.e. it is part of the immutable chain) to historical
  -- certificates, and deletes the others (whose boosted blocks are on forks).
  --
  -- Afterwards, certificates boosting blocks older than @slotNo@ are rejected
  -- by 'addCert' as 'PerasCertTooOld'.
  --
  -- NOTE: Use the `join . atomically` pattern to consume its output.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getHistoricalCertsAfter ::
      HasCallStack =>
      PerasRoundNo ->
      Word64 ->
      m [ValidatedPerasCert blk]
  -- ^ @'getHistoricalCertsAfter' roundNo maxCerts@ gets at most @maxCerts@
  -- historical certificates with a round number strictly greater than
  -- @roundNo@, in ascending round number order. The certificates are read from
  -- disk; those whose file turns out to be unreadable or corrupt are
  -- quarantined (see 'getMissingRounds') rather than returned.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getMissingRounds :: STM m (Set PerasRoundNo)
  -- ^ Get the round numbers of the historical certificates that were found to
  -- be unreadable or corrupt on disk (i.e. quarantined). These are not served
  -- by 'getHistoricalCertsAfter'. Note that adding a certificate for such a
  -- round with 'addCert' does /not/ repair it ('PerasCertAlreadyInDB').
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , closeDB :: HasCallStack => m ()
  -- ^ Close the database. Idempotent.
  --
  -- Calls of the other operations made after this returns throw
  -- 'ClosedDBError'. Calls already in progress are not interrupted: this waits
  -- for an operation that is already modifying the database on disk to
  -- complete, while a 'getHistoricalCertsAfter' in progress may still read
  -- certificate files and return them after the database is closed.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)

-- | Indicate whether the block being boosted by a certificate is expected to
-- still be part of the volatile chain suffix or not.
--
-- This is relevant for Peras voting because we need to know if a candidate
-- block extends the chain that contains the block being boosted by the latest
-- certificate seen. For this purpose, we want to limit the voting rules to only
-- look within the volatile chain suffix, and assume that, if the boosted block
-- is no longer in the volatile suffix, then it trivially extends the candidate
-- block (because it must have been copied into the immutable chain prefix). For
-- this reason, we need to manually keep track of whether the boosted block of
-- this certificate has already been garbage collected from the volatile suffix
-- or not.
data WithBoostedBlockStatus cert
  = -- | Certificate boosting a block within the volatile chain suffix
    CertBoostingBlockInVolatileDB cert
  | -- | Certificate boosting a block that no longer belongs to the volatile
    -- chain suffix. This typically means that the block has been copied to the
    -- immutable prefix, and then garbage collected from the volatile suffix.
    CertBoostingBlockNoLongerInVolatileDB cert
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Deconstruct a certificate from its provenance wrapper
forgetBoostedBlockStatus :: WithBoostedBlockStatus cert -> cert
forgetBoostedBlockStatus = \case
  CertBoostingBlockInVolatileDB cert -> cert
  CertBoostingBlockNoLongerInVolatileDB cert -> cert

data AddPerasCertResult
  = AddedPerasCertToDB
  | -- | A certificate for the same round is already in the database, either
    -- as a volatile, historical or quarantined certificate.
    PerasCertAlreadyInDB
  | -- | The certificate boosts a block older than the last garbage collection
    -- slot (see 'garbageCollect'), so it can no longer be added.
    PerasCertTooOld
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | Errors thrown by the 'PerasCertDB'.
--
-- Failures of individual certificate files are not thrown but quarantined
-- instead, so these only indicate incorrect usage of the database.
data PerasCertDbError
  = -- | An operation other than 'closeDB' was performed on a closed database.
    ClosedDBError PrettyCallStack
  deriving stock Show

instance Exception PerasCertDbError where
  displayException ClosedDBError{} =
    "PerasCertDB used after being closed"

-- * Invariants

-- (currently not enforced, see https://github.com/tweag/cardano-peras/issues/217)

-- | After adding a cert, its round number should be present in 'getVolatileCertIds'.
prop_addCertThenGetCertIds ::
  ( MonadSTM m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  m Bool
prop_addCertThenGetCertIds db cert =
  atomically $ do
    let certRound = getPerasCertRound cert
    _ <- addCert db cert
    certIds <- getVolatileCertIds db
    pure $
      certRound `elem` certIds

-- | 'getVolatileCertsAfter' with ticket 0 should return all certs in the database.
-- NOTE: this property is not purely STM.
prop_getCertsAfterZero ::
  ( MonadSTM m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  m Bool
prop_getCertsAfterZero db = do
  (allCertActions, certIds) <- atomically $ do
    allCerts <- getVolatileCertsAfter db zeroPerasRoundNo
    certIds <- getVolatileCertIds db
    pure (allCerts, certIds)
  allCertValues <- sequence (Map.elems allCertActions)
  let allCertIds =
        Set.fromList $
          fmap
            (getPerasCertRound . forgetArrivalTime)
            allCertValues
  pure $
    length allCertActions == Set.size certIds
      && allCertIds == certIds

-- | 'getVolatileCertsAfter' returns strictly increasing ticket numbers.
prop_getCertsAfterMonotonic ::
  MonadSTM m =>
  PerasCertDB m blk ->
  PerasRoundNo ->
  m Bool
prop_getCertsAfterMonotonic db roundNo =
  atomically $ do
    certs <- getVolatileCertsAfter db roundNo
    let tickets = Map.keys certs
    pure $
      tickets == List.sort tickets
        && all (> roundNo) tickets

-- | After garbage collection for slot S, no certs with target slot < S should remain.
-- NOTE: this property is not purely STM.
prop_garbageCollectRemovesOldCerts ::
  ( MonadSTM m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  SlotNo ->
  m Bool
prop_garbageCollectRemovesOldCerts db slotNo = do
  allCertActions <- atomically $ do
    _ <- garbageCollect db (const (pure True)) slotNo
    getVolatileCertsAfter db zeroPerasRoundNo
  allCertValues <- sequence (Map.elems allCertActions)
  let targetSlots = pointSlot . getPerasCertPoint <$> allCertValues
  pure $
    all (>= NotOrigin slotNo) targetSlots

-- | After adding a cert, the round number reported by 'getLatestCertSeen'
-- should be greater than or equal to its previous value.
prop_addCertLatestCertSeenMonotonic ::
  ( MonadSTM m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  m Bool
prop_addCertLatestCertSeenMonotonic db cert =
  atomically $ do
    prevLatest <- getLatestCertSeen db
    _ <- addCert db cert
    newLatest <- getLatestCertSeen db
    let getRound = getPerasCertRound . forgetBoostedBlockStatus
    pure $ case (prevLatest, newLatest) of
      (_, Nothing) -> False -- after adding a cert, the latest cert seen should not go back to 'Nothing'
      (Nothing, Just _) -> True -- if there was no cert seen before, any new cert should be greater than or equal to it
      (Just prev, Just new) -> getRound new >= getRound prev

-- | 'getLatestCertSeen' is not affected by garbage collection.
prop_garbageCollectPreservesLatestCertSeen ::
  ( MonadSTM m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  SlotNo ->
  m Bool
prop_garbageCollectPreservesLatestCertSeen db slotNo =
  atomically $ do
    prevLatest <- getLatestCertSeen db
    _ <- garbageCollect db (const (pure True)) slotNo
    newLatest <- getLatestCertSeen db
    let getRound = getPerasCertRound . forgetBoostedBlockStatus
    pure $ fmap getRound prevLatest == fmap getRound newLatest
