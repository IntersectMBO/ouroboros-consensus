{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | One node-owned, bounded local resolver, independent of the DB-awaiting
-- handoff worker. No callback here performs database IO. A cache hint becomes
-- held only through the existing worker's actual storage receipt.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.LocalReuse
  ( LocalSource (..)
  , mempoolLocalSource
  , LocalResolverConfig (..)
  , defaultLocalResolverConfig
  , resolveLocalSlice
  , runLocalResolver
  ) where

import Control.Monad (foldM, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Leios.TxCache
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Util.IOLike

-- | A coherent immutable source index and a pure, exact byte projection.
-- No bytes are retained through a capacity wait: relookup and reserve together.
data LocalSource m tx = LocalSource
  { localSourceIndex :: STM m (Natural, Map TxHash (BytesSize, tx))
  -- ^ Can temporarily wait for its state owner; shutdown must remain observable.
  , localSourceBytes :: tx -> m (Maybe LeiosTx)
  -- ^ Materialize only a bounded selected TX after local reservation succeeds.
  -- Production mempool projection is pure memory work; the monadic boundary
  -- makes reservation transfer/exception cancellation explicit and testable.
  }

mempoolLocalSource ::
  forall m blk.
  (Applicative m, LedgerSupportsMempool blk) =>
  Mempool m blk ->
  LocalSource m (Validated (GenTx blk))
mempoolLocalSource mempool =
  LocalSource
    (getLeiosTxIndex mempool)
    (pure . fmap MkLeiosTx . txBytesForLeios (Proxy @blk) . txForgetValidated @blk)

-- | Bound one memory turn by bytes and entries; a final whole TX can overhang.
data LocalResolverConfig = LocalResolverConfig
  { resolverSliceBytes :: !Word32
  , resolverSliceEntries :: !Word32
  }
  deriving (Eq, Show)

defaultLocalResolverConfig :: LocalResolverConfig
defaultLocalResolverConfig = LocalResolverConfig 20000 128

-- | Resolve one already-bounded immutable slice. Returned negative positions
-- are unusable mempool projections of the same immutable wire hash. Cache
-- discovery remains possible for them; no payload or complete snapshot is kept.
resolveLocalSlice ::
  IOLike m =>
  H.StorageHandoff m owner peer ->
  LeiosTxCache m ->
  LocalSource m tx ->
  STM m Bool ->
  STM m () ->
  LeiosPoint ->
  AcquisitionGeneration ->
  Bool ->
  IntSet.IntSet ->
  IntSet.IntSet ->
  [ExpectedTx] ->
  m IntSet.IntSet
resolveLocalSlice handoff cache source closing notify point gen bodyDurable rejected negative expected = do
  -- Return scalar metadata only, not a lazy map of values pinning the mempool.
  snapshot <- atomically $ do
    stopped <- closing
    if stopped
      then pure Nothing
      else do
        (epoch, index) <- localSourceIndex source
        let hits =
              foldl
                ( \acc e -> case Map.lookup (expectedHash e) index of
                    Just (size, _)
                      | size == expectedSize e && not (IntSet.member (expectedOffset e) negative) ->
                          IntSet.insert (expectedOffset e) acc
                    _ -> acc
                )
                IntSet.empty
                expected
        hits `seq` pure (Just (epoch, hits))
  case snapshot of
    Nothing -> pure negative
    Just (_, mempoolHits) -> do
      (_, locations) <-
        lookupLocations
          cache
          [ (expectedOffset e, expectedHash e)
          | e <- expected
          , not (IntSet.member (expectedOffset e) mempoolHits)
          , not (IntSet.member (expectedOffset e) rejected)
          ]
      let cached = IntMap.fromList [(off, loc) | (off, Just loc) <- locations]
          inspected = IntSet.fromList (map expectedOffset expected)
          candidates = mempoolHits `IntSet.union` IntMap.keysSet cached
          misses =
            (inspected `IntSet.difference` candidates) `IntSet.union` (inspected `IntSet.intersection` rejected)
      atomically $ voidResult =<< H.classifyLocalOffsets handoff point gen inspected misses
      negatives <-
        if bodyDurable
          then foldM (importOne mempoolHits cached) negative expected
          else pure negative
      atomically notify
      pure negatives
 where
  voidResult _ = pure ()
  markMiss off = atomically $ do
    voidResult
      =<< H.classifyLocalOffsets handoff point gen (IntSet.singleton off) (IntSet.singleton off)
    notify
  harmless err =
    err
      `elem` [H.LocalAlreadyOwned, H.UnknownAcquisition, H.StaleAcquisition, H.StorageClosed, H.BodyUnknown]
  capacity err = err `elem` [H.LocalByteCapacity, H.LocalEntryCapacity]
  importOne hits cached bad e
    | IntSet.member (expectedOffset e) rejected = pure bad
    | IntSet.member (expectedOffset e) hits = do
        -- Closing is checked before the possibly unavailable mempool cell in
        -- the same STM transaction. It can escape without a committed reserve.
        result <- mask $ \restore -> do
          chosen <- atomically $ do
            -- Check shutdown before a source STM read which may retry.
            -- Recheck possession and reserve together; materialize TX bytes
            -- only after that reservation succeeds, outside shared STM.
            stopped <- closing
            if stopped
              then pure (Left H.StorageClosed)
              else do
                (_, index) <- localSourceIndex source
                case Map.lookup (expectedHash e) index of
                  Just (size, tx) | size == expectedSize e -> do
                    reserved <- H.reserveLocalTransaction handoff point gen e
                    pure (fmap (\r -> (r, tx)) reserved)
                  _ -> pure (Left H.InvalidTransaction)
          case chosen of
            Left err -> pure (Left err)
            Right (reservation, tx) ->
              restore
                ( do
                    projected <- localSourceBytes source tx
                    case projected of
                      Nothing -> pure (Right False)
                      Just bytes -> do
                        committed <- H.commitLocalTransaction handoff reservation bytes
                        pure $ case committed of
                          Left H.InvalidTransaction -> Right False
                          Left err -> Left err
                          Right () -> Right True
                )
                `finally` atomically (H.dropLocalReservation handoff reservation)
        case result of
          Right True -> pure bad
          Right False -> do
            (_, fresh) <- lookupLocations cache [(expectedOffset e, expectedHash e)]
            importCopy e (lookup (expectedOffset e) fresh >>= id)
            pure (IntSet.insert (expectedOffset e) bad)
          Left err | capacity err || harmless err -> pure bad
          Left H.LocalTransactionTooLarge -> markMiss (expectedOffset e) >> pure bad
          Left H.InvalidTransaction -> do
            -- A vanished/unsupported source can still have a durable alternative.
            (_, fresh) <- lookupLocations cache [(expectedOffset e, expectedHash e)]
            importCopy e (lookup (expectedOffset e) fresh >>= id)
            pure bad
          Left err -> throwIO (H.StorageException err)
    | otherwise = importCopy e (IntMap.lookup (expectedOffset e) cached) >> pure bad
  importCopy e Nothing = markMiss (expectedOffset e)
  importCopy e (Just location) = do
    result <- atomically $ do
      stopped <- closing
      if stopped
        then pure (Left H.StorageClosed)
        else do
          reserved <- H.reserveLocalTransaction handoff point gen e
          case reserved of
            Left err -> pure (Left err)
            Right reservation -> do
              committed <- H.commitLocalCopy handoff reservation location
              H.dropLocalReservation handoff reservation
              pure committed
    case result of
      Right () -> pure ()
      Left err | capacity err || harmless err -> pure ()
      Left H.LocalTransactionTooLarge -> markMiss (expectedOffset e)
      Left err -> throwIO (H.StorageException err)

-- | Fair bounded passes over relevant generations. Epochs are captured at the
-- start of a pass, never blindly acknowledged after the last lookup: a change
-- during a pass leaves another pass due. Unrelated gossip cannot restart a pass
-- or revoke already classified network misses. A full unchanged pool parks.
runLocalResolver ::
  IOLike m =>
  LocalResolverConfig ->
  H.StorageHandoff m owner peer ->
  LeiosTxCache m ->
  LocalSource m tx ->
  W.Readiness m ->
  STM m Bool ->
  STM m () ->
  m ()
runLocalResolver config handoff cache source ready closing notify
  | resolverSliceBytes config == 0 || resolverSliceEntries config == 0 =
      throwIO (H.StorageException H.InvalidStorageLimits)
  | otherwise = idle Nothing Map.empty
 where
  epochs = do
    mempoolEpoch <- fst <$> localSourceIndex source
    cacheEpoch <- cacheSourceEpoch cache
    localEpoch <- H.readLocalRevision handoff
    pure (mempoolEpoch, cacheEpoch, localEpoch)
  hasFresh = do
    current <- H.readLocalAcquisitions handoff
    pure $
      any
        ( \(_, s) ->
            not
              ( IntSet.null
                  ( (H.snapshotMissing s `IntSet.difference` H.snapshotLocalWork s)
                      `IntSet.difference` H.snapshotLocalClassified s
                  )
              )
        )
        current
  idle seen negative = do
    stopped <-
      atomically $
        (closing >>= check >> pure True)
          `orElse` (W.awaitReadiness ready >> pure False)
          `orElse` (do now <- epochs; fresh <- hasFresh; check (Just now /= seen || fresh); pure False)
    unless stopped $ do
      -- Remember pass-start epochs, not an end-of-pass resample. A source
      -- change during this scan must still make the next scan due.
      start <- atomically $ do
        stoppedNow <- closing
        if stoppedNow
          then pure Nothing
          else do
            observed <- epochs
            current <- H.readLocalAcquisitions handoff
            pure (Just (observed, [(p, H.snapshotGeneration s, 0) | (p, s) <- current]))
      case start of
        Nothing -> pure ()
        Just (observed, points) -> do
          let keys = Map.fromList [((p, g), ()) | (p, g, _) <- points]
          negative' <- pass (Seq.fromList points) (Map.intersection negative keys)
          idle (Just observed) negative'
  pass queue negative = case Seq.viewl queue of
    Seq.EmptyL -> pure negative
    (point, gen, cursor) Seq.:< rest -> do
      current <- atomically $ do
        stopped <- closing
        if stopped then pure Nothing else H.readAcquisition handoff point
      case current of
        Just snapshot
          | H.snapshotGeneration snapshot == gen
          , Just refs <- H.snapshotReferences snapshot -> do
              let missing = H.snapshotMissing snapshot `IntSet.difference` H.snapshotLocalWork snapshot
                  offsets = IntSet.toAscList (snd (IntSet.split (cursor - 1) missing))
                  selected = takeSlice refs 0 0 offsets
                  lastOffset = maybe cursor ((+ 1) . expectedOffset) (safeLast selected)
                  remaining = IntSet.lookupGE lastOffset missing
                  nextQueue = case remaining of
                    Just _ | not (null selected) -> rest Seq.|> (point, gen, lastOffset)
                    _ -> rest
                  oldBad = Map.findWithDefault IntSet.empty (point, gen) negative
              newBad <-
                resolveLocalSlice
                  handoff
                  cache
                  source
                  closing
                  notify
                  point
                  gen
                  (H.snapshotBodyDurable snapshot)
                  (H.snapshotLocalRejected snapshot)
                  oldBad
                  selected
              yield
              pass
                nextQueue
                ( if IntSet.null newBad
                    then Map.delete (point, gen) negative
                    else Map.insert (point, gen) newBad negative
                )
        _ -> pass rest negative
  takeSlice refs bytes entries offsets
    | bytes >= fromIntegral (resolverSliceBytes config) || entries >= resolverSliceEntries config = []
    | otherwise = case offsets of
        [] -> []
        off : more -> case IntMap.lookup off refs of
          Nothing -> []
          Just (h, n) -> ExpectedTx off h n : takeSlice refs (bytes + fromIntegral n :: Word64) (entries + 1) more
  safeLast [] = Nothing
  safeLast xs = Just (last xs)
