{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -O2 #-}

-- | A 'LeiosTxCache' handle backed by the mutable 'HT.MutableHashTable': the
-- counterpart to 'LeiosTxCache.newPureLeiosTxCache'. The ~2M-entry tx map lives
-- in the hash table (value = the tx's refcount and 2-bit state tag packed into
-- the 'Word64'); the small announcement and body state stays in 'Map's behind an
-- 'MVar' that also serializes every hash-table access (the \"Locked\" ops hold it
-- for writes; 'withLookupTx' holds it for the read batch). The refcount
-- maintenance and eviction cascade mirror 'LeiosTxCacheIndex' exactly — this is
-- the mutable re-implementation validated against the pure one.
--
-- Only @a = v = ()@ is supported (the node's instantiation), since the value is
-- a bare 'Word64'.
module LeiosTxCache.Mutable
  ( newHashTableLeiosTxCache
  ) where

import Cardano.Slotting.Slot (SlotNo)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString.Unsafe as BSU
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import LeiosDemoTypes (EbHash, RbHash, TxHash (..))
import LeiosTxCache (LeiosTxCache (..))
import qualified LeiosTxCache.MutableHashTable as HT
import LeiosTxCacheIndex
  ( BodyState (..)
  , RefCount (..)
  , ReferencesTxsByHash (..)
  , maxAnnouncementCount
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | The small, map-resident state (the tx map is the hash table, not here).
data HtState b = HtState
  { hsAnnouncements :: !(Map SlotNo (NEMap RbHash EbHash))
  , hsCount :: !Int
  , hsBodies :: !(Map EbHash (BodyState b))
  }

emptyHtState :: HtState b
emptyHtState = HtState Map.empty 0 Map.empty

-- | A hash-table-backed handle. @nshift@ sizes the table (@2 ^ nshift@ slots; use
-- 22 for the ~1.9M worst case) and @k0@\/@k1@ are the SipHash salt (feed a
-- securely-random pair).
newHashTableLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  Int ->
  Word64 ->
  Word64 ->
  m (LeiosTxCache m () () b)
{-# SPECIALISE
  newHashTableLeiosTxCache ::
    ReferencesTxsByHash b => Int -> Word64 -> Word64 -> IO (LeiosTxCache IO () () b)
  #-}
newHashTableLeiosTxCache nshift k0 k1 = do
  ht <- HT.new nshift k0 k1
  stateVar <- MVar.newMVar emptyHtState
  pure
    LeiosTxCache
      { insertAnnouncement = \slot rbh ebh ->
          MVar.modifyMVar stateVar $ \st ->
            if announcementPresent slot rbh st
              then pure (st, (Set.empty, Set.empty))
              else evictLoop ht (addAnnouncement slot rbh ebh st) Set.empty Set.empty
      , insertBody = \ebh b ->
          MVar.modifyMVar_ stateVar $ \st ->
            case Map.lookup ebh (hsBodies st) of
              Nothing -> pure st
              Just BodyAlreadyInserted{} -> pure st
              Just (BodyNotYetInserted rc) -> do
                foldTxReferences (\act txh -> act >> bumpTx ht txh) (pure ()) b
                pure (st{hsBodies = Map.insert ebh (BodyAlreadyInserted rc b) (hsBodies st)})
      , withLockedInsertUnappliedTx = \k ->
          MVar.modifyMVar_ stateVar $ \st -> do
            _ <- k () (\_ txh _ -> setTag ht tagAlreadyInserted txh)
            pure st
      , withLockedInsertAppliedTx = \k ->
          MVar.modifyMVar_ stateVar $ \st -> do
            _ <- k () (\_ txh _ -> setTag ht tagAlreadyValidated txh)
            pure st
      , withLookupTx = \k ->
          MVar.withMVar stateVar $ \_ -> k (lookupOne ht)
      }

{-------------------------------------------------------------------------------
  Announcement \/ body state (mirrors LeiosTxCacheIndex, txs excepted)
-------------------------------------------------------------------------------}

announcementPresent :: SlotNo -> RbHash -> HtState b -> Bool
announcementPresent slot rbh st =
  maybe False (NEMap.member rbh) (Map.lookup slot (hsAnnouncements st))

addAnnouncement :: SlotNo -> RbHash -> EbHash -> HtState b -> HtState b
addAnnouncement slot rbh ebh st =
  HtState
    { hsAnnouncements =
        Map.alter
          (Just . maybe (NEMap.singleton rbh ebh) (NEMap.insert rbh ebh))
          slot
          (hsAnnouncements st)
    , hsCount = hsCount st + 1
    , hsBodies =
        Map.alter
          (Just . maybe (BodyNotYetInserted (MkRefCount 1)) incBodyRc)
          ebh
          (hsBodies st)
    }

evictLoop ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  Set EbHash ->
  Set TxHash ->
  m (HtState b, (Set EbHash, Set TxHash))
{-# SPECIALISE
  evictLoop ::
    ReferencesTxsByHash b =>
    HT.MutableHashTable (PrimState IO) ->
    HtState b ->
    Set EbHash ->
    Set TxHash ->
    IO (HtState b, (Set EbHash, Set TxHash))
  #-}
evictLoop ht st !evEbs !evTxs
  | hsCount st <= maxAnnouncementCount = pure (st, (evEbs, evTxs))
  | otherwise = do
      (st', ebs', txs') <- evictOldest ht st
      evictLoop ht st' (evEbs <> ebs') (evTxs <> txs')

evictOldest ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  m (HtState b, Set EbHash, Set TxHash)
{-# SPECIALISE
  evictOldest ::
    ReferencesTxsByHash b =>
    HT.MutableHashTable (PrimState IO) ->
    HtState b ->
    IO (HtState b, Set EbHash, Set TxHash)
  #-}
evictOldest ht st = do
  let (slotMin, nem) = Map.findMin (hsAnnouncements st)
      (rbhMin, ebhEvicted) = NEMap.findMin nem
      announcements' = case NEMap.nonEmptyMap (NEMap.delete rbhMin nem) of
        Nothing -> Map.delete slotMin (hsAnnouncements st)
        Just nem' -> Map.insert slotMin nem' (hsAnnouncements st)
  (bodies', evEbs, evTxs) <- decBody ht ebhEvicted (hsBodies st)
  pure
    ( HtState{hsAnnouncements = announcements', hsCount = hsCount st - 1, hsBodies = bodies'}
    , evEbs
    , evTxs
    )

decBody ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  EbHash ->
  Map EbHash (BodyState b) ->
  m (Map EbHash (BodyState b), Set EbHash, Set TxHash)
{-# SPECIALISE
  decBody ::
    ReferencesTxsByHash b =>
    HT.MutableHashTable (PrimState IO) ->
    EbHash ->
    Map EbHash (BodyState b) ->
    IO (Map EbHash (BodyState b), Set EbHash, Set TxHash)
  #-}
decBody ht ebh bodies = case Map.lookup ebh bodies of
  Nothing -> pure (bodies, Set.empty, Set.empty)
  Just bs -> case decRefCount (bodyRefCount bs) of
    Just rc' -> pure (Map.insert ebh (setBodyRefCount rc' bs) bodies, Set.empty, Set.empty)
    Nothing -> do
      evTxs <- case bs of
        BodyNotYetInserted _ -> pure Set.empty
        BodyAlreadyInserted _ b -> decBodyTxs ht b
      pure (Map.delete ebh bodies, Set.singleton ebh, evTxs)

decBodyTxs ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  b ->
  m (Set TxHash)
{-# SPECIALISE
  decBodyTxs ::
    ReferencesTxsByHash b => HT.MutableHashTable (PrimState IO) -> b -> IO (Set TxHash)
  #-}
decBodyTxs ht =
  foldTxReferences
    ( \act txh -> do
        s <- act
        evicted <- decTx ht txh
        pure (if evicted then Set.insert txh s else s)
    )
    (pure Set.empty)

{-------------------------------------------------------------------------------
  Refcount helpers (RefCount's own inc\/dec are internal to LeiosTxCacheIndex)
-------------------------------------------------------------------------------}

incBodyRc :: BodyState b -> BodyState b
incBodyRc (BodyNotYetInserted (MkRefCount n)) = BodyNotYetInserted (MkRefCount (n + 1))
incBodyRc (BodyAlreadyInserted (MkRefCount n) b) = BodyAlreadyInserted (MkRefCount (n + 1)) b

bodyRefCount :: BodyState b -> RefCount
bodyRefCount (BodyNotYetInserted rc) = rc
bodyRefCount (BodyAlreadyInserted rc _) = rc

setBodyRefCount :: RefCount -> BodyState b -> BodyState b
setBodyRefCount rc (BodyNotYetInserted _) = BodyNotYetInserted rc
setBodyRefCount rc (BodyAlreadyInserted _ b) = BodyAlreadyInserted rc b

decRefCount :: RefCount -> Maybe RefCount
decRefCount (MkRefCount n)
  | n <= 1 = Nothing
  | otherwise = Just (MkRefCount (n - 1))

{-------------------------------------------------------------------------------
  Tx map operations, over the hash table
-------------------------------------------------------------------------------}

tagNotYetInserted, tagAlreadyInserted, tagAlreadyValidated :: Word64
tagNotYetInserted = 0
tagAlreadyInserted = 1
tagAlreadyValidated = 2

-- value = (refcount << 2) | tag
mkVal :: Word64 -> Word64 -> Word64
mkVal rc tag = (rc `unsafeShiftL` 2) .|. tag

valRefcount :: Word64 -> Word64
valRefcount w = w `unsafeShiftR` 2

valTag :: Word64 -> Word64
valTag w = w .&. 3

-- | A body now refers to this tx: create at refcount 1 (NotYetInserted) or bump.
bumpTx :: PrimMonad m => HT.MutableHashTable (PrimState m) -> TxHash -> m ()
{-# SPECIALISE bumpTx :: HT.MutableHashTable (PrimState IO) -> TxHash -> IO () #-}
bumpTx ht txh = do
  let key = toKey txh
  mv <- HT.lookup ht key
  case mv of
    Nothing -> HT.insert ht key (mkVal 1 tagNotYetInserted)
    Just w -> HT.insert ht key (mkVal (valRefcount w + 1) (valTag w))

-- | An evicted body no longer refers to this tx: decrement, deleting (and
-- reporting) it at zero.
decTx :: PrimMonad m => HT.MutableHashTable (PrimState m) -> TxHash -> m Bool
{-# SPECIALISE decTx :: HT.MutableHashTable (PrimState IO) -> TxHash -> IO Bool #-}
decTx ht txh = do
  let key = toKey txh
  mv <- HT.lookup ht key
  case mv of
    Nothing -> pure False
    Just w
      | valRefcount w <= 1 -> HT.delete ht key >> pure True
      | otherwise -> HT.insert ht key (mkVal (valRefcount w - 1) (valTag w)) >> pure False

-- | Set a present tx's state tag, preserving its refcount; no-op if absent.
setTag :: PrimMonad m => HT.MutableHashTable (PrimState m) -> Word64 -> TxHash -> m ()
{-# SPECIALISE setTag :: HT.MutableHashTable (PrimState IO) -> Word64 -> TxHash -> IO () #-}
setTag ht tag txh = do
  let key = toKey txh
  mv <- HT.lookup ht key
  case mv of
    Nothing -> pure ()
    Just w -> HT.insert ht key (mkVal (valRefcount w) tag)

lookupOne :: PrimMonad m => HT.MutableHashTable (PrimState m) -> TxHash -> m (Maybe (Either () ()))
{-# SPECIALISE lookupOne :: HT.MutableHashTable (PrimState IO) -> TxHash -> IO (Maybe (Either () ())) #-}
lookupOne ht txh = do
  mv <- HT.lookup ht (toKey txh)
  pure $ case mv of
    Nothing -> Nothing
    Just w
      | valTag w == tagAlreadyInserted -> Just (Left ())
      | valTag w == tagAlreadyValidated -> Just (Right ())
      | otherwise -> Nothing -- NotYetInserted: referenced but not held

-- | The 32-byte hash as the table's four-word 'HT.Key' (big-endian words).
-- TODO: direct once 'TxHash' is @PackedBytes 32@ rather than a 'ByteString'.
toKey :: TxHash -> HT.Key
toKey (MkTxHash bs) = HT.Key (rd 0) (rd 8) (rd 16) (rd 24)
 where
  rd off = go 0 off (off + 8)
  go !acc o end
    | o >= end = acc
    | otherwise =
        go ((acc `unsafeShiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs o)) (o + 1) end
