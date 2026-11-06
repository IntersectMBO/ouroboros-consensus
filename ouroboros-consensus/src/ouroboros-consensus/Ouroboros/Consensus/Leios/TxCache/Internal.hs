{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Optimized announcement/reference policy with a table seam for controlled
-- interruption tests. Public operations never run arbitrary IO
-- callbacks while holding the cache lock.
module Ouroboros.Consensus.Leios.TxCache.Internal
  ( TxTable (..)
  , newCache
  , encodeLocation
  , decodeLocation
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, forM_, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Word (Word64)
import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar

-- | All accesses are serialized by the enclosing cache lock. Low 32 bits hold
-- a reference count; high bits hold the checked ring/offset location.
data TxTable m = TxTable
  { tableLookup :: TxHash -> m (Maybe Word64)
  , tableInsert :: TxHash -> Word64 -> m ()
  , tableDelete :: TxHash -> m ()
  , tableSize :: m Int
  , tableArrayBytes :: !Word64
  }

-- | Small resident state, independent of transaction-table representation.
data State = State
  { announcements :: !(Map (SlotNo, RankingBlockHash) EndorserBlockHash)
  , bodies :: !(Map EndorserBlockHash (Int, Maybe PackedReferences))
  , frontier :: !SlotNo
  , ring :: !(Map Int LeiosPoint)
  , nextRing :: !Int
  }

encodeLocation :: Int -> TxOffset -> Word64
encodeLocation ix off
  | ix < 0 || ix >= 128 || off < 0 || off >= 0x1000000 = error "TxCache: invalid packed location"
  | otherwise = (0x80000000 .|. (fromIntegral ix `shiftL` 24) .|. fromIntegral off) `shiftL` 32

decodeLocation :: Word64 -> Maybe (Int, TxOffset)
decodeLocation w
  | hi .&. 0x80000000 == 0 = Nothing
  | otherwise = Just (fromIntegral ((hi `shiftR` 24) .&. 0x7f), fromIntegral (hi .&. 0xffffff))
 where
  hi = w `shiftR` 32

newCache :: IOLike m => CacheConfig -> TxTable m -> m (LeiosTxCache m)
newCache cfg table = do
  either (error . ("TxCache: " ++)) pure (validateCacheConfig cfg)
  state <- MVar.uncheckedNewMVar (State Map.empty Map.empty (SlotNo 0) Map.empty 0)
  epoch <- uncheckedNewTVarM 0
  let changed = atomically $ modifyTVar epoch (+ 1)
      update action = MVar.modifyMVarMasked state $ \st -> do
        (st', didChange) <- action st
        when didChange changed
        pure (st', ())
      location st w = do
        (ix, off) <- decodeLocation w
        point <- Map.lookup ix (ring st)
        if pointSlotNo point < frontier st then Nothing else Just (MkTxLocation point off)
      trim predicate st
        | predicate st = do
            let ((key, h), rest) = Map.deleteFindMin (announcements st)
            case Map.lookup h (bodies st) of
              Nothing -> error "TxCache: missing announced body"
              Just (count, refs)
                | count > 1 ->
                    trim predicate st{announcements = rest, bodies = Map.insert h (count - 1, refs) (bodies st)}
                | otherwise -> do
                    forM_ refs $ \r ->
                      foldDistinctReferences
                        (\act _ txh _ -> act >> decrement txh)
                        (pure ())
                        r
                    trim
                      predicate
                      st
                        { announcements = Map.delete key (announcements st)
                        , bodies = Map.delete h (bodies st)
                        , ring = Map.filter ((/= h) . pointEbHash) (ring st)
                        }
        | otherwise = pure st
      decrement h =
        tableLookup table h >>= \case
          Nothing -> error "TxCache: missing referenced transaction"
          Just w
            | w .&. 0xffffffff <= 1 -> tableDelete table h
            | otherwise -> tableInsert table h (w - 1)
  pure
    LeiosTxCache
      { recordAnnouncement = \slot rbh h -> update $ \st ->
          if slot < frontier st || Map.member (slot, rbh) (announcements st)
            then pure (st, False)
            else do
              let st' =
                    st
                      { announcements = Map.insert (slot, rbh) h (announcements st)
                      , bodies = Map.alter (Just . maybe (1, Nothing) (\(n, b) -> (n + 1, b))) h (bodies st)
                      }
              trimmed <- trim ((> cacheAnnouncements cfg) . Map.size . announcements) st'
              pure (trimmed, True)
      , trackDurableReferences = \h refs -> update $ \st -> case Map.lookup h (bodies st) of
          Just (n, Nothing) -> do
            when (foldReferences (\count _ _ _ -> count + 1 :: Int) 0 refs > maxTxsPerEndorserBlock) $
              error "TxCache: too many references"
            foldDistinctReferences
              ( \act _ txh _ ->
                  act >> do
                    old <- tableLookup table txh
                    let w = maybe 0 id old
                    when (w .&. 0xffffffff >= fromIntegral (cacheAnnouncements cfg)) $
                      error "TxCache: reference-count overflow"
                    tableInsert table txh (w + 1)
              )
              (pure ())
              refs
            pure (st{bodies = Map.insert h (n, Just refs) (bodies st)}, True)
          _ -> pure (st, False)
      , lookupLocations = \requested -> MVar.withMVar state $ \st -> do
          observed <- readTVarIO epoch
          found <- forM requested $ \(off, h) -> do
            w <- tableLookup table h
            pure (off, w >>= location st)
          pure (observed, found)
      , publishConfirmed = \point confirmed -> update $ \st ->
          if pointSlotNo point < frontier st
            || not (Map.member (pointEbHash point) (bodies st))
            || null confirmed
            then pure (st, False)
            else do
              let existing = listToMaybe [i | (i, p) <- Map.toList (ring st), p == point]
                  ix = maybe (nextRing st) id existing
                  st' =
                    st
                      { ring = Map.insert ix point (ring st)
                      , nextRing = maybe ((ix + 1) `mod` cacheAnnouncements cfg) (const (nextRing st)) existing
                      }
              updates <- forM confirmed $ \(off, h) -> do
                old <- tableLookup table h
                case old of
                  Nothing -> pure False
                  Just w -> do
                    let new = (w .&. 0xffffffff) .|. encodeLocation ix off
                    when (w /= new) $ tableInsert table h new
                    pure (w /= new)
              pure (st', ring st' /= ring st || or updates)
      , invalidateLocationIfMatches = \h failed -> update $ \st -> do
          old <- tableLookup table h
          case old of
            Just w | location st w == Just failed -> do
              tableInsert table h (w .&. 0xffffffff)
              pure (st, True)
            _ -> pure (st, False)
      , pruneTo = \slot -> update $ \st -> do
          let boundary = max slot (frontier st)
              stale v = maybe False ((< boundary) . fst . fst . fst) (Map.minViewWithKey (announcements v))
          trimmed <-
            trim stale st{frontier = boundary, ring = Map.filter ((>= boundary) . pointSlotNo) (ring st)}
          pure
            ( trimmed
            , boundary /= frontier st || Map.size (announcements trimmed) /= Map.size (announcements st)
            )
      , cacheSourceEpoch = readTVar epoch
      , readCacheStats = MVar.withMVar state $ \st ->
          CacheStats
            (Map.size (announcements st))
            (Map.size (Map.filter (maybe False (const True) . snd) (bodies st)))
            <$> tableSize table
            <*> pure (tableArrayBytes table)
      }

-- A body contributes one reference per hash even if its wire list repeats a TX.
-- Do not let an untrusted repeated reference overflow counts or leave orphaned
-- increments. Offsets/size remain in the compact body for exact DB guards.
foldDistinctReferences :: (a -> TxOffset -> TxHash -> BytesSize -> a) -> a -> PackedReferences -> a
foldDistinctReferences f z refs =
  snd $
    foldReferences
      ( \(seen, acc) off h n -> if Set.member h seen then (seen, acc) else (Set.insert h seen, f acc off h n)
      )
      (Set.empty, z)
      refs
