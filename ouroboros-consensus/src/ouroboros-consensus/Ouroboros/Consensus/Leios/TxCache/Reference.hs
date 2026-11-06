-- | Independent pure Map model of the selected donor cache semantics, with a
-- small monadic handle for equivalence tests. It shares no announcement,
-- refcount, ring or packed-location implementation with the optimized policy.
module Ouroboros.Consensus.Leios.TxCache.Reference (newReferenceCache) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar

-- Pure, unpacked ownership. A table entry counts distinct retained bodies,
-- while each body's first component counts its retained announcements.
data Model = Model
  { announcements :: !(Map (SlotNo, RankingBlockHash) EndorserBlockHash)
  , bodies :: !(Map EndorserBlockHash (Int, Maybe PackedReferences))
  , transactions :: !(Map TxHash (Int, Maybe (Int, TxOffset)))
  , ring :: !(Map Int LeiosPoint)
  , nextRing :: !Int
  , frontier :: !SlotNo
  }
  deriving Eq

emptyModel :: Model
emptyModel = Model Map.empty Map.empty Map.empty Map.empty 0 (SlotNo 0)

referenceHashes :: PackedReferences -> Set.Set TxHash
referenceHashes = foldReferences (\seen _ h _ -> Set.insert h seen) Set.empty

location :: Model -> TxHash -> Maybe TxLocation
location state h = do
  (_, hint) <- Map.lookup h (transactions state)
  (slot, off) <- hint
  point <- Map.lookup slot (ring state)
  if pointSlotNo point < frontier state then Nothing else Just (MkTxLocation point off)

-- The independent model evicts one announcement, releasing its body's TX
-- references only when its final announcement leaves the retained window.
evictOne :: Model -> Model
evictOne state =
  let ((_, h), remaining) = Map.deleteFindMin (announcements state)
      without = state{announcements = remaining}
   in case Map.lookup h (bodies state) of
        Nothing -> error "TxCache reference: missing announced body"
        Just (count, refs)
          | count > 1 -> without{bodies = Map.insert h (count - 1, refs) (bodies state)}
          | otherwise ->
              without
                { bodies = Map.delete h (bodies state)
                , transactions = foldl' decrement (transactions state) (maybe [] (Set.toList . referenceHashes) refs)
                , ring = Map.filter ((/= h) . pointEbHash) (ring state)
                }
 where
  decrement txs h = case Map.lookup h txs of
    Just (n, hint) | n > 1 -> Map.insert h (n - 1, hint) txs
    _ -> Map.delete h txs

trim :: (Model -> Bool) -> Model -> Model
trim predicate state
  | predicate state = trim predicate (evictOne state)
  | otherwise = state

newReferenceCache :: IOLike m => CacheConfig -> m (LeiosTxCache m)
newReferenceCache config = do
  either (error . ("TxCache reference: " ++)) pure (validateCacheConfig config)
  state <- MVar.uncheckedNewMVar emptyModel
  epoch <- uncheckedNewTVarM 0
  let update f = MVar.modifyMVarMasked state $ \old -> do
        let (new, changed) = f old
        when changed $ atomically $ modifyTVar epoch (+ 1)
        pure (new, ())
  pure
    LeiosTxCache
      { recordAnnouncement = \slot rbh h -> update $ \old ->
          if slot < frontier old || Map.member (slot, rbh) (announcements old)
            then (old, False)
            else
              let added =
                    old
                      { announcements = Map.insert (slot, rbh) h (announcements old)
                      , bodies = Map.alter (Just . maybe (1, Nothing) (\(n, b) -> (n + 1, b))) h (bodies old)
                      }
               in (trim ((> cacheAnnouncements config) . Map.size . announcements) added, True)
      , trackDurableReferences = \h refs -> update $ \old -> case Map.lookup h (bodies old) of
          Just (n, Nothing)
            | foldReferences (\count _ _ _ -> count + 1 :: Int) 0 refs > maxTxsPerEndorserBlock ->
                error "TxCache reference: too many references"
            | otherwise ->
                let bumped =
                      foldl'
                        (\txs key -> Map.alter (Just . maybe (1, Nothing) (\(count, hint) -> (count + 1, hint))) key txs)
                        (transactions old)
                        (Set.toList (referenceHashes refs))
                 in (old{bodies = Map.insert h (n, Just refs) (bodies old), transactions = bumped}, True)
          _ -> (old, False)
      , lookupLocations = \requested -> MVar.withMVar state $ \old -> do
          observed <- readTVarIO epoch
          pure (observed, [(off, location old h) | (off, h) <- requested])
      , publishConfirmed = \point confirmed -> update $ \old ->
          if pointSlotNo point < frontier old
            || not (Map.member (pointEbHash point) (bodies old))
            || null confirmed
            then (old, False)
            else
              let prior = fmap fst (find ((== point) . snd) (Map.toList (ring old)))
                  slot = maybe (nextRing old) id prior
                  txs =
                    foldl'
                      ( \current (off, h) ->
                          if off < 0 || off >= 0x1000000
                            then error "TxCache reference: invalid offset"
                            else Map.adjust (\(n, _) -> (n, Just (slot, off))) h current
                      )
                      (transactions old)
                      confirmed
                  new =
                    old
                      { transactions = txs
                      , ring = Map.insert slot point (ring old)
                      , nextRing = maybe ((slot + 1) `mod` cacheAnnouncements config) (const (nextRing old)) prior
                      }
               in (new, new /= old)
      , invalidateLocationIfMatches = \h expected -> update $ \old ->
          if location old h /= Just expected
            then (old, False)
            else (old{transactions = Map.adjust (\(n, _) -> (n, Nothing)) h (transactions old)}, True)
      , pruneTo = \slot -> update $ \old ->
          let boundary = max slot (frontier old)
              base = old{frontier = boundary, ring = Map.filter ((>= boundary) . pointSlotNo) (ring old)}
              stale st = maybe False ((< boundary) . fst . fst) (Map.lookupMin (announcements st))
              new = trim stale base
           in (new, boundary /= frontier old || Map.size (announcements new) /= Map.size (announcements old))
      , cacheSourceEpoch = readTVar epoch
      , readCacheStats = MVar.withMVar state $ \st ->
          pure $
            CacheStats
              (Map.size (announcements st))
              (Map.size (Map.filter (maybe False (const True) . snd) (bodies st)))
              (Map.size (transactions st))
              0
      }
