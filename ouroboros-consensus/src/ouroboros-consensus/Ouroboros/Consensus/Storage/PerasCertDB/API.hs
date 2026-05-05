{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  , AddPerasCertResult (..)
  , PerasCertTicketNo
  , zeroPerasCertTicketNo

    -- * Invariants
  , prop_addCertThenGetCertIds
  , prop_getCertsAfterZero
  , prop_getCertsAfterMonotonic
  , prop_garbageCollectRemovesOldCerts
  , prop_addCertLatestCertSeenMonotonic
  , prop_garbageCollectPreservesLatestCertSeen
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
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot)
import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM (..))
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

data PerasCertDB m blk = PerasCertDB
  { addCert ::
      WithArrivalTime (ValidatedPerasCert blk) ->
      STM m (m AddPerasCertResult)
  -- ^ Add a Peras certificate to the database. The STM transaction adds the
  -- certificate to the in-memory index, and the resulting 'm' action performs
  -- tracing and might perform side-effects in implementations with on-disk
  -- storage.
  -- The 'AddPerasCertResult' indicates whether the certificate was actually
  -- added, or if it was already present.
  --
  -- NOTE: Use the @join . atomically@ pattern to run both the transaction
  -- and the side-effects in sequence.
  , getCertIds ::
      STM m (Set PerasRoundNo)
  -- ^ Get the set of all cert IDs currently in the database.
  , getCertsAfter ::
      PerasCertTicketNo ->
      STM m (Map PerasCertTicketNo (m (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ Get all certs with a ticket number strictly greater than the given one,
  -- in ascending order.
  -- Unlike 'getVotesAfter' in 'PerasVoteDB', the resulting map contains 'm' actions to
  -- retrieve the certificates, instead of the certificates directly. This is to
  -- allow implementation with on-disk storage to retrieve certificates from disk.
  , getWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ Return the Peras weights in order compare the current selection against
  -- potential candidate chains, namely the weights for blocks not older than
  -- the current immutable tip. It might contain weights for even older blocks
  -- if they have not yet been garbage-collected.
  --
  -- The 'Fingerprint' is updated every time a new certificate is added, but it
  -- stays the same when certificates are garbage-collected.
  , getLatestCertSeen ::
      STM m (Maybe (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ This field impacts voting directly because having seen a certificate is a
  -- precondition for voting in any round except for the very first one
  -- (at origin).
  , garbageCollect ::
      SlotNo ->
      STM m (m ())
  -- ^ Garbage-collect certificates whose target slot is strictly smaller
  -- than the given slot number.
  -- The STM transaction clears the relevant state from the in-memory index, and
  -- the resulting 'm' action performs tracing and might perform side-effects in
  -- implementations with on-disk storage.
  --
  -- NOTE: Use the `join . atomically` pattern to consume its output.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)

-- | A sequence number, incremented every time we receive a new certificate.
--
-- Note that we will /usually/ receive certificates monotonically by round
-- number, so round numbers could /almost/ fulfill the role of ticket numbers.
-- However, in certain edge cases (while catching up, or during cooldowns), this
-- might not be true, such as during syncing or during cooldown periods.
-- Therefore, for robustness, we choose to maintain dedicated ticket numbers
-- separately.
newtype PerasCertTicketNo = PerasCertTicketNo Word64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasCertTicketNo :: PerasCertTicketNo
zeroPerasCertTicketNo = PerasCertTicketNo 0

data AddPerasCertResult
  = AddedPerasCertToDB
  | PerasCertAlreadyInDB
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- * Invariants

-- (currently not enforced, see https://github.com/tweag/cardano-peras/issues/217)

-- | After adding a cert, its round number should be present in 'getCertIds'.
prop_addCertThenGetCertIds ::
  MonadSTM m =>
  PerasCertDB m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  m Bool
prop_addCertThenGetCertIds db cert =
  atomically $ do
    let certRound = getPerasCertRound cert
    _ <- addCert db cert
    certIds <- getCertIds db
    pure $
      certRound `elem` certIds

-- | 'getCertsAfter' with ticket 0 should return all certs in the database.
-- NOTE: this property is not purely STM.
prop_getCertsAfterZero ::
  MonadSTM m =>
  PerasCertDB m blk ->
  m Bool
prop_getCertsAfterZero db = do
  (allCertActions, certIds) <- atomically $ do
    allCerts <- getCertsAfter db zeroPerasCertTicketNo
    certIds <- getCertIds db
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

-- | 'getCertsAfter' returns strictly increasing ticket numbers.
prop_getCertsAfterMonotonic ::
  MonadSTM m =>
  PerasCertDB m blk ->
  PerasCertTicketNo ->
  m Bool
prop_getCertsAfterMonotonic db ticketNo =
  atomically $ do
    certs <- getCertsAfter db ticketNo
    let tickets = Map.keys certs
    pure $
      tickets == List.sort tickets
        && all (> ticketNo) tickets

-- | After garbage collection for slot S, no certs with target slot < S should remain.
-- NOTE: this property is not purely STM.
prop_garbageCollectRemovesOldCerts ::
  MonadSTM m =>
  PerasCertDB m blk ->
  SlotNo ->
  m Bool
prop_garbageCollectRemovesOldCerts db slotNo = do
  allCertActions <- atomically $ do
    _ <- garbageCollect db slotNo
    getCertsAfter db zeroPerasCertTicketNo
  allCertValues <- sequence (Map.elems allCertActions)
  let targetSlots = pointSlot . getPerasCertBoostedBlock . forgetArrivalTime <$> allCertValues
  pure $
    all (>= NotOrigin slotNo) targetSlots

-- | After adding a cert, the round number reported by 'getLatestCertSeen'
-- should be greater than or equal to its previous value.
prop_addCertLatestCertSeenMonotonic ::
  MonadSTM m =>
  PerasCertDB m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  m Bool
prop_addCertLatestCertSeenMonotonic db cert =
  atomically $ do
    prevLatest <- getLatestCertSeen db
    _ <- addCert db cert
    newLatest <- getLatestCertSeen db
    let getRound = getPerasCertRound . forgetArrivalTime
    pure $ case (prevLatest, newLatest) of
      (_, Nothing) -> False -- after adding a cert, the latest cert seen should not go back to 'Nothing'
      (Nothing, Just _) -> True -- if there was no cert seen before, any new cert should be greater than or equal to it
      (Just prev, Just new) -> getRound new >= getRound prev

-- | 'getLatestCertSeen' is not affected by garbage collection.
prop_garbageCollectPreservesLatestCertSeen ::
  (MonadSTM m, StandardHash blk) =>
  PerasCertDB m blk ->
  SlotNo ->
  m Bool
prop_garbageCollectPreservesLatestCertSeen db slotNo =
  atomically $ do
    prevLatest <- getLatestCertSeen db
    _ <- garbageCollect db slotNo
    newLatest <- getLatestCertSeen db
    pure $ prevLatest == newLatest
