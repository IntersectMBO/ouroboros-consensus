{-# LANGUAGE NamedFieldPuns #-}

-- | Immutable transaction checkpoints and ordinary assignment ownership over
-- the node-owned storage handoff. Acquisition is shared across wire versions;
-- assignment release neither settles a wire exchange nor releases stored data.
--
-- Rationale and code-to-contract map (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#2-separate-policy-obligations-from-wire-attempts@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition
  ( CheckpointLimits
  , mkCheckpointLimits
  , defaultCheckpointLimits
  , AcquisitionError (..)
  , buildCheckpoint
  , prepareLegacyBatch
  , OrdinaryAssignments
  , newOrdinaryAssignments
  , assignCheckpoint
  , readAssignedOffsets
  , releaseCheckpoint
  , releasePeerAssignments
  , releaseAcquisitionAssignments
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (..)
  , STM
  , StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Monad (unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32, Word64)
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff
  ( AcquisitionSnapshot (..)
  , StorageError
  , StorageHandoff
  , StorageRequest
  , StorageSession
  , admitStorageRequest
  , readAcquisition
  , storageRequestKey
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
  ( txResponseBytes
  , txResponseElementBytes
  , txResponseHeaderBytes
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosBlockTxsRequest (..)
  , LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (..)
  , isFocusedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)

-- | Validated local batch geometry, independent of publication chunk size.
-- Hard limits can produce a batch smaller than the service target.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#1-make-each-transaction-batch-a-checkpoint@.
data CheckpointLimits = CheckpointLimits
  { checkpointTargetBytes :: !Word32
  -- ^ Stop after reaching/crossing this payload target with a whole TX.
  , checkpointPayloadLimit :: !Word32
  -- ^ Hard payload cap, including any target overhang.
  , checkpointEntryLimit :: !Word32
  -- ^ Hard whole-transaction count cap.
  , checkpointEncodedLimit :: !Word32
  -- ^ Hard encoded-response cap, including all CBOR framing.
  }
  deriving (Eq, Show)

-- | Invalid construction/admission leaves ordinary ownership unchanged.
data AcquisitionError
  = InvalidCheckpointLimits
  | UnknownAcquisition
  | StaleAcquisition
  | BodyUnknown
  | InvalidSelection
  | TransactionExceedsLimits
  | InvalidPreparation !PreparationError
  | InvalidLegacyCommitment
  | PointNotFocused
  | ClosureNotOffered
  | StorageAdmissionError !StorageError
  deriving (Eq, Show)

mkCheckpointLimits ::
  Word32 -> Word32 -> Word32 -> Word32 -> Either AcquisitionError CheckpointLimits
mkCheckpointLimits target payload entries encoded
  | any (== 0) [target, payload, entries, encoded] = Left InvalidCheckpointLimits
  | target > payload || payload > maxLeiosTxsRequestBytesSize = Left InvalidCheckpointLimits
  | fromIntegral encoded > (11 * fromIntegral maxLeiosTxsRequestBytesSize `div` 10 :: Word64) =
      Left InvalidCheckpointLimits
  | otherwise = Right (CheckpointLimits target payload entries encoded)

-- | Initial 20,000 B service target with independent protocol hard limits.
defaultCheckpointLimits :: CheckpointLimits
defaultCheckpointLimits = CheckpointLimits 20000 maxLeiosTxsRequestBytesSize 20000 550000

-- | Construct at most one immutable request from an eligible subset of the
-- current missing set. Iterate body offsets once; widen count/byte arithmetic
-- before checking caps. A hard cap stops before the next TX; an indivisible TX
-- that cannot fit an empty batch is an error, never silently skipped.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#1-make-each-transaction-batch-a-checkpoint@.
buildCheckpoint ::
  CheckpointLimits ->
  LeiosPoint ->
  AcquisitionSnapshot ->
  IntSet.IntSet ->
  Either AcquisitionError (Maybe PreparedRequest)
buildCheckpoint limits point snapshot eligible = do
  refs <- maybe (Left BodyUnknown) Right (snapshotReferences snapshot)
  unless (eligible `IntSet.isSubsetOf` snapshotMissing snapshot) (Left InvalidSelection)
  select refs 0 0 0 [] (IntSet.toAscList eligible)
 where
  select refs payload count elements picked rest
    | payload >= fromIntegral (checkpointTargetBytes limits) = finish refs count elements picked
    | otherwise = case rest of
        [] -> finish refs count elements picked
        off : more -> do
          (_, size) <- maybe (Left InvalidSelection) Right (IntMap.lookup off refs)
          when
            (off < 0 || off `div` 64 > fromIntegral (maxBound :: Word16) || size == 0)
            (Left InvalidSelection)
          let nextPayload = payload + fromIntegral size :: Word64
              nextCount = count + 1 :: Word64
              nextElements = elements + txResponseElementBytes size
              fits =
                nextPayload <= fromIntegral (checkpointPayloadLimit limits)
                  && nextCount <= fromIntegral (checkpointEntryLimit limits)
                  && txResponseHeaderBytes nextCount + nextElements <= fromIntegral (checkpointEncodedLimit limits)
          if fits
            then select refs nextPayload nextCount nextElements (off : picked) more
            else case picked of
              [] -> Left TransactionExceedsLimits
              _ -> finish refs count elements picked
  finish refs count elements picked = case NE.nonEmpty (reverse picked) of
    Nothing -> Right Nothing
    Just offsets ->
      Just
        <$> either
          (Left . InvalidPreparation)
          Right
          ( prepareBatchFromReferences
              point
              (snapshotGeneration snapshot)
              refs
              offsets
              (EncodedBytes (fromIntegral (txResponseHeaderBytes count + elements)))
          )

-- | Pin a legacy request's entire original membership, including offsets now
-- pending or durable. Check against its original job commitments, never the
-- mutable projected pool. Preparation itself allocates no identity or quota;
-- the legacy path retains its own admission/whole-vector validation boundaries.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
prepareLegacyBatch ::
  AcquisitionSnapshot -> LeiosBlockTxsRequest -> Either AcquisitionError PreparedRequest
prepareLegacyBatch snapshot (MkLeiosBlockTxsRequest point jobs) = do
  refs <- maybe (Left BodyUnknown) Right (snapshotReferences snapshot)
  let original = foldMap (\(Jobs.MkLeiosJob offsets _ _) -> offsets) jobs
      count = sum [IntSet.size offsets | Jobs.MkLeiosJob offsets _ _ <- foldr (:) [] jobs]
  unless (IntSet.size original == count) (Left InvalidLegacyCommitment)
  mapM_ (checkJob refs) jobs
  offsets <- maybe (Left InvalidLegacyCommitment) Right (NE.nonEmpty (IntSet.toAscList original))
  sizes <- traverse (fmap snd . lookupRef refs) offsets
  either (Left . InvalidPreparation) Right $
    prepareBatchFromReferences
      point
      (snapshotGeneration snapshot)
      refs
      offsets
      (EncodedBytes (fromIntegral (txResponseBytes (NE.toList sizes))))
 where
  lookupRef refs off = maybe (Left InvalidLegacyCommitment) Right (IntMap.lookup off refs)
  checkJob refs (Jobs.MkLeiosJob offsets bytes root) = do
    expected <- traverse (lookupRef refs) (IntSet.toAscList offsets)
    unless
      ( not (IntSet.null offsets)
          && sum (map (fromIntegral . snd) expected) == (fromIntegral bytes :: Word64)
          && Jobs.jobRootHashOfTxHashes (map fst expected) == root
      )
      (Left InvalidLegacyCommitment)

-- | Only current ordinary scheduling claims, keyed by complete original wire
-- identity. The handoff owns missing/pending/durable state independently. Live
-- entries are bounded by already admitted data records, not a growing history.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#2-separate-policy-obligations-from-wire-attempts@.
newtype OrdinaryAssignments m peer
  = OrdinaryAssignments (StrictTVar m (Map.Map (RequestKey peer) BatchDescriptor))

newOrdinaryAssignments :: MonadSTM m => m (OrdinaryAssignments m peer)
newOrdinaryAssignments = OrdinaryAssignments <$> newTVarIO Map.empty

-- | Check focus, generation, full-closure eligibility and ordinary ownership,
-- then atomically reserve handoff/wire capacity and record the exact claim.
-- The kernel must hold its outstanding-state lock while passing this view, just
-- as for legacy assignment. No DB wait or payload read belongs in this action.
assignCheckpoint ::
  (MonadSTM m, Ord owner, Ord peer) =>
  OrdinaryAssignments m peer ->
  StorageHandoff m owner peer ->
  CheckpointLimits ->
  StorageSession peer ->
  LeiosPoint ->
  AcquisitionGeneration ->
  PeerOffer ->
  LeiosOutstanding p ->
  STM m (Either AcquisitionError (Maybe (StorageRequest peer)))
assignCheckpoint assignments@(OrdinaryAssignments state) handoff limits session point generation offer outstanding
  | not (Map.member point (ebState outstanding) && isFocusedPoint point outstanding) =
      pure (Left PointNotFocused)
  | OfferedBody{} <- poOffered offer = pure (Left ClosureNotOffered)
  | otherwise = do
      current <- readAcquisition handoff point
      case current of
        Nothing -> pure (Left UnknownAcquisition)
        Just snapshot
          | snapshotGeneration snapshot /= generation -> pure (Left StaleAcquisition)
          | otherwise -> do
              assigned <- readAssignedOffsets assignments point generation
              let eligible = IntSet.difference (snapshotMissing snapshot) assigned
              case buildCheckpoint limits point snapshot eligible of
                Left err -> pure (Left err)
                Right Nothing -> pure (Right Nothing)
                Right (Just prepared) -> case preparedDescriptor prepared of
                  BodyRequest{} -> pure (Left InvalidSelection) -- construction only produces TX batches
                  BatchRequest descriptor -> do
                    admitted <- admitStorageRequest handoff session prepared
                    case admitted of
                      Left err -> pure (Left (StorageAdmissionError err))
                      Right request -> do
                        entries <- readTVar state
                        writeTVar state (Map.insert (storageRequestKey request) descriptor entries)
                        pure (Right (Just request))

-- | Offsets held only by current ordinary scheduling claims for this generation.
readAssignedOffsets ::
  MonadSTM m =>
  OrdinaryAssignments m peer -> LeiosPoint -> AcquisitionGeneration -> STM m IntSet.IntSet
readAssignedOffsets (OrdinaryAssignments state) point generation = do
  entries <- readTVar state
  pure $ foldMap offsets (Map.elems entries)
 where
  offsets batch
    | batchPoint batch == point && batchGeneration batch == generation =
        IntSet.fromList (map expectedOffset (NE.toList (batchExpected batch)))
    | otherwise = IntSet.empty

-- | Release this exact scheduling claim after abandonment or terminal handling.
-- The old descriptor still validates late data; this releases no wire or storage
-- reservation and cannot remove a replacement request's overlapping claim.
releaseCheckpoint ::
  (MonadSTM m, Ord peer) => OrdinaryAssignments m peer -> RequestKey peer -> STM m ()
releaseCheckpoint (OrdinaryAssignments state) key = modifyTVar state (Map.delete key)

-- | Remove a disconnected registration's ordinary claims, preserving other
-- lifetimes of the same peer and node-owned pending storage.
releasePeerAssignments ::
  (MonadSTM m, Ord peer) => OrdinaryAssignments m peer -> LeiosPeerLifetimeId peer -> STM m ()
releasePeerAssignments (OrdinaryAssignments state) lifetime =
  modifyTVar state (Map.filterWithKey (\(RequestKey original _ _) _ -> original /= lifetime))

-- | Retire one relevance generation's scheduling claims. Acquisition retirement
-- and wire drainage have their own owners and must also be performed by callers.
releaseAcquisitionAssignments ::
  MonadSTM m => OrdinaryAssignments m peer -> LeiosPoint -> AcquisitionGeneration -> STM m ()
releaseAcquisitionAssignments (OrdinaryAssignments state) point generation =
  modifyTVar
    state
    (Map.filter (\batch -> batchPoint batch /= point || batchGeneration batch /= generation))
