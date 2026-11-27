{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -O2 #-}

-- | A 'LeiosTxCache' handle backed by the mutable 'HT.MutableHashTable': the
-- counterpart to 'Ouroboros.Consensus.Leios.TxCache.newPureLeiosTxCache'. The
-- txs live in the hash table, each one's state, youngest reference and location
-- packed into a 'Word64' (see 'tagOf', 'youngestOf' and 'locOf'); the small
-- point state stays in maps behind an 'MVar' that also serializes every
-- hash-table access, since each operation holds it while it runs. It behaves as
-- "Ouroboros.Consensus.Leios.TxCache.Reference" does, and is tested against it.
--
-- Where the reference implementation names a point by its 'LeiosPoint', a
-- packed word instead names it by its 'PointId'.
--
-- Only @a = ()@ and @v = 'VersionWord16'@ are supported (the node's
-- instantiation), since the hash table only maps each tx to a bare 'Word64'.
module Ouroboros.Consensus.Leios.TxCache.Optimized
  ( newHashTableLeiosTxCache
  ) where

import Cardano.Crypto.Hash.Class (PackedBytes (PackedBytes32), hashToPackedBytes)
import Cardano.Crypto.PackedBytes (unpackBytes)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM, void, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (bit, complement, testBit, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString.Short as SBS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word64)
import Ouroboros.Consensus.Ledger.SupportsMempool (VersionWord16 (..))
import Ouroboros.Consensus.Leios.TxCache.API
  ( LandedTx (..)
  , LeiosTxCache (..)
  , LeiosTxCacheInsertBodySummary (..)
  , ReferencesTxsByHash (..)
  , TxArrivalPrior (..)
  , TxState (..)
  , bucketTxArrival
  , maxLeiosPointCount
  )
import qualified Ouroboros.Consensus.Leios.TxCache.Optimized.MutableHashTable as HT
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosPoint (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (FetchArrivalBytes)
import Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar

-- | The 'LeiosPoint' state; the txs are in the hash table.
data HtState b = HtState
  { hsPrunedSlot :: !SlotNo
  -- ^ Greatest slot 'evictOlderThanFoldl'' has pruned to;
  -- 'insertBodyAndFoldl'' retains no point strictly older. Mirrors 'Ouroboros.Consensus.Leios.TxCache.Reference.prunedSlot'.
  , hsPoints :: !(Map LeiosPoint (RetainedPoint b))
  , hsPointsById :: !(IntMap LeiosPoint)
  -- ^ The retained points by 'PointId'.
  --
  -- INVARIANT: the inverse of the ids in 'hsPoints'.
  }

-- | A retained point's id and body.
data RetainedPoint b = MkRetainedPoint !PointId !b

-- | The id of a retained point, in @[0, 'maxLeiosPointCount')@, so that it fits
-- a packed word's 7-bit fields. A point holds it while it is retained.
newtype PointId = MkPointId Int

unPointId :: PointId -> Int
unPointId (MkPointId i) = i

emptyHtState :: HtState b
emptyHtState = HtState (SlotNo 0) Map.empty IntMap.empty

-- | A hash-table-backed handle. @nshift@ sizes the table (@2 ^ nshift@ slots;
-- the node uses 22, 'Ouroboros.Consensus.Leios.TxCache.API.defaultLeiosTxCacheShift') and
-- @k0@\/@k1@ are the SipHash salt (feed a securely-random pair).
--
-- Every update masks asynchronous exceptions: it mutates the table in place,
-- which restoring the 'HtState' cannot undo, so a peer thread killed halfway
-- through would otherwise leave entries that nothing will ever evict.
newHashTableLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  Int ->
  Word64 ->
  Word64 ->
  m (LeiosTxCache m () VersionWord16 b)
{-# SPECIALIZE newHashTableLeiosTxCache ::
  ReferencesTxsByHash b => Int -> Word64 -> Word64 -> IO (LeiosTxCache IO () VersionWord16 b)
  #-}
newHashTableLeiosTxCache nshift k0 k1 = do
  ht <- HT.new nshift k0 k1
  stateVar <- MVar.uncheckedNewMVar emptyHtState
  pure
    LeiosTxCache
      { evictOlderThanFoldl' = \boundary nil snoc ->
          MVar.modifyMVarMasked stateVar $ \st -> do
            let pruned = max (hsPrunedSlot st) boundary
                (old, _kept) = Map.spanAntitone ((< pruned) . pointSlotNo) (hsPoints st)
                !evPoints = Map.keysSet old
            (st', !w) <-
              foldM
                (\(s, !acc) (point, rp) -> releasePoint ht s point rp acc snoc)
                (st{hsPrunedSlot = pruned}, nil)
                (Map.toAscList old)
            pure (st', (evPoints, w))
      , insertBodyAndFoldl' = \point b nil snoc ->
          MVar.modifyMVarMasked stateVar $ \st -> insertBodyLocked ht st point b nil snoc
      , insertLandedTxs = \point landed ->
          MVar.modifyMVarMasked stateVar $ \st -> case Map.lookup point (hsPoints st) of
            Nothing -> do
              let !arrival = foldMap (bucketTxArrival TxEvicted . ltSize) landed
              pure (st, arrival)
            Just (MkRetainedPoint pid _) ->
              (,) st <$> foldM (insertLandedTx ht st point pid) mempty landed
      , withLookupTx = \k ->
          MVar.withMVar stateVar $ \_ -> k (lookupOne ht)
      }

{-------------------------------------------------------------------------------
  Points
-------------------------------------------------------------------------------}

-- | 'insertBodyAndFoldl'', holding the lock. See
-- 'Ouroboros.Consensus.Leios.TxCache.Reference.insertBodyAndFoldl''.
insertBodyLocked ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  LeiosPoint ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  m (HtState b, (Maybe LeiosTxCacheInsertBodySummary, w))
{-# SPECIALIZE insertBodyLocked ::
  ReferencesTxsByHash b =>
  HT.MutableHashTable (PrimState IO) ->
  HtState b ->
  LeiosPoint ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  IO (HtState b, (Maybe LeiosTxCacheInsertBodySummary, w))
  #-}
insertBodyLocked ht st point b nil snoc
  | pointSlotNo point < hsPrunedSlot st = ignore st
  | Map.member point (hsPoints st) = ignore st
  | Map.size (hsPoints st) >= maxLeiosPointCount
  , Just (oldest, _) <- Map.lookupMin (hsPoints st)
  , point < oldest {- hash breaks slot ties -} =
      ignore st
  | otherwise = do
      st1 <- evictOldestIfFull ht st
      -- Small scan of 128 is fine
      case find (`IntMap.notMember` hsPointsById st1) [0 .. maxLeiosPointCount - 1] of
        -- Unreachable: after 'evictOldestIfFull', fewer than
        -- 'maxLeiosPointCount' points hold ids.
        --
        -- The test for parity with @Reference.hs@ should fail if this is
        -- actually reachable.  And eve nif if that test some how didn't fail,
        -- ignoring a body incorrectly here would only lose out on reuse,
        -- without threatening soundless/safety.
        Nothing -> ignore st1
        Just i -> insertNewPoint ht st1 point (MkPointId i) b nil snoc
 where
  ignore s = (,) s . (,) Nothing <$> locateTxs ht s b nil snoc

-- | Retain this point as the given 'PointId' with the given body
--
-- PREREQ this point is not already retained
--
-- PREREQ this 'PointId' is free
--
-- Make this point the youngest reference of each tx the table holds, if it is
-- younger, while summarising those txs and folding over all of them with their
-- locations.
insertNewPoint ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  LeiosPoint ->
  PointId ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  m (HtState b, (Maybe LeiosTxCacheInsertBodySummary, w))
insertNewPoint ht st point pid b nil snoc = do
  (n, acquired, validated, !w) <-
    foldTxReferences
      ( \acc txh sz -> do
          (!nn, !aa, !vv, !w) <- acc
          let key = toKey txh
          mv <- HT.lookup ht key
          case mv of
            Nothing -> pure (nn + 1, aa, vv, snoc w nn txh sz Nothing)
            Just word -> do
              when (younger st point (youngestOf word)) $ HT.insert ht key (withYoungest pid word)
              let dv
                    | isValidated word = 1
                    | otherwise = 0
              pure (nn + 1, aa + 1, vv + dv, snoc w nn txh sz (locationOf st word))
      )
      (pure (0, 0, 0, nil))
      b
  cacheTxCount <- HT.size ht
  let !summary =
        MkLeiosTxCacheInsertBodySummary
          { ibsTxsInEb = n
          , ibsAcquired = acquired
          , ibsValidated = validated
          , ibsCacheTxCount = cacheTxCount
          , ibsCacheCapacity = HT.capacity ht
          }
  pure
    ( st
        { hsPoints = Map.insert point (MkRetainedPoint pid b) (hsPoints st)
        , hsPointsById = IntMap.insert (unPointId pid) point (hsPointsById st)
        }
    , (Just summary, w)
    )

-- | Evict the oldest retained point if 'maxLeiosPointCount' are retained.
evictOldestIfFull ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  m (HtState b)
evictOldestIfFull ht st = case Map.minViewWithKey (hsPoints st) of
  Just ((oldest, rp), _rest)
    | Map.size (hsPoints st) >= maxLeiosPointCount ->
        fst <$> releasePoint ht st oldest rp () (\() _txh -> ())
  _ -> pure st

-- | Evict the oldest retained point, freeing that 'PointId'
--
-- Also visits every tx the body references. If this point is the tx's youngest
-- reference, evicts the tx. Otherwise, if this point is the tx's recorded
-- location, forget's that the tx is located there (because the stored 'PointId'
-- is now free!).
--
-- Updates the given @w@ accumulator with each evicted tx, in body order.
releasePoint ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  LeiosPoint ->
  RetainedPoint b ->
  w ->
  (w -> TxHash -> w) ->
  m (HtState b, w)
{-# SPECIALIZE releasePoint ::
  ReferencesTxsByHash b =>
  HT.MutableHashTable (PrimState IO) ->
  HtState b ->
  LeiosPoint ->
  RetainedPoint b ->
  w ->
  (w -> TxHash -> w) ->
  IO (HtState b, w)
  #-}
releasePoint ht st point (MkRetainedPoint pid b) nil snoc = do
  !w <-
    foldTxReferences
      ( \act txh _sz -> do
          !acc <- act
          let key = toKey txh
          mv <- HT.lookup ht key
          case mv of
            Nothing -> pure acc
            Just word
              | youngestOf word == unPointId pid -> do
                  void $ HT.delete ht key
                  pure (snoc acc txh)
              | Just (i, _) <- locOf word
              , unPointId pid == i -> do
                  HT.insert ht key (withoutLoc word)
                  pure acc
              | otherwise -> pure acc
      )
      (pure nil)
      b
  pure
    ( st
        { hsPoints = Map.delete point (hsPoints st)
        , hsPointsById = IntMap.delete (unPointId pid) (hsPointsById st)
        }
    , w
    )

-- | Whether this point is younger than the retained point with this id.
younger :: HtState b -> LeiosPoint -> Int -> Bool
younger st point i = maybe True (point >) (IntMap.lookup i (hsPointsById st))

{-------------------------------------------------------------------------------
  Txs
-------------------------------------------------------------------------------}

-- | Record that this 'PointId' now holds this tx, at this offset of its
-- body. See 'Ouroboros.Consensus.Leios.TxCache.Reference.insertLandedTx'.
insertLandedTx ::
  PrimMonad m =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  LeiosPoint ->
  PointId ->
  FetchArrivalBytes ->
  LandedTx () VersionWord16 ->
  m FetchArrivalBytes
{-# SPECIALIZE insertLandedTx ::
  HT.MutableHashTable (PrimState IO) ->
  HtState b ->
  LeiosPoint ->
  PointId ->
  FetchArrivalBytes ->
  LandedTx () VersionWord16 ->
  IO FetchArrivalBytes
  #-}
insertLandedTx ht st point pid fab (MkLandedTx off txh sz txst) = do
  let key = toKey txh
  mv <- HT.lookup ht key
  case mv of
    Nothing -> do
      HT.insert
        ht
        key
        (withLoc pid off (withYoungest pid (tagBits txst)))
      pure $! fab <> bucketTxArrival TxWasAbsent sz
    Just word -> do
      let updTag = case txst of
            TxValidated _ -> withTag txst
            TxInserted () -> id
          updLocation = case locationOf st word of
            Just (MkTxLocation src _) | point < src -> id
            _ -> withLoc pid off
          updYoungest
            | younger st point (youngestOf word) = withYoungest pid
            | otherwise = id
      HT.insert ht key $ updTag . updLocation . updYoungest $ word
      pure $! fab <> bucketTxArrival TxWasHeld sz

lookupOne ::
  PrimMonad m =>
  HT.MutableHashTable (PrimState m) -> TxHash -> m (StrictMaybe (TxState () VersionWord16))
{-# SPECIALIZE lookupOne ::
  HT.MutableHashTable (PrimState IO) -> TxHash -> IO (StrictMaybe (TxState () VersionWord16))
  #-}
lookupOne ht txh =
  HT.lookup ht (toKey txh) >>= \case
    Nothing -> pure SNothing
    Just word -> pure $ SJust $ tagOf word

-- | Fold over the txs this body references, each with its location, if the
-- table knows one.
locateTxs ::
  (PrimMonad m, ReferencesTxsByHash b) =>
  HT.MutableHashTable (PrimState m) ->
  HtState b ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  m w
{-# SPECIALIZE locateTxs ::
  ReferencesTxsByHash b =>
  HT.MutableHashTable (PrimState IO) ->
  HtState b ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  IO w
  #-}
locateTxs ht st b nil snoc = do
  (_, !w) <-
    foldTxReferences
      ( \acc txh sz -> do
          (!nn, !w) <- acc
          mv <- HT.lookup ht (toKey txh)
          pure (nn + 1 :: Int, snoc w nn txh sz (locationOf st =<< mv))
      )
      (pure (0, nil))
      b
  pure w

{-------------------------------------------------------------------------------
  The packed word

  > bit  63 whether located
  > bit  62 whether validated
  > bits 61..55 location's point id, when located
  > bits 54..31 location's offset, when located
  > bits 30..24 unused
  > bits 23..8 validated version, when validated
  > bits 7..1 youngest reference's point id
  > bit 0 unused

  15 bits cover all offsets the design's limit on EB body size might require. 16
  bits hold all of the 'VersionWord16' (so decoding can't fail) this tx was
  (most recently) validated under.
-------------------------------------------------------------------------------}

tagBits :: TxState () VersionWord16 -> Word64
tagBits = \case
  TxInserted () -> 0
  TxValidated (MkVersionWord16 v) -> bit 62 .|. (fromIntegral v `unsafeShiftL` 8)

-- | Whether bit 62 says the tx was validated.
isValidated :: Word64 -> Bool
isValidated w = testBit w 62

-- | Bit 62 says whether the tx was validated, and bits 23..8 under which major
-- protocol version.
tagOf :: Word64 -> TxState () VersionWord16
tagOf w
  | not (isValidated w) = TxInserted ()
  | otherwise = TxValidated $ MkVersionWord16 $ fromIntegral $ w `unsafeShiftR` 8

-- | Replace both the validation flag and the version.
withTag :: TxState () VersionWord16 -> Word64 -> Word64
withTag tag w = (w .&. complement tagMask) .|. tagBits tag
 where
  tagMask = bit 62 .|. (0xFFFF `unsafeShiftL` 8)

-- | The id of the tx's youngest reference.
youngestOf :: Word64 -> Int
youngestOf w = fromIntegral ((w `unsafeShiftR` 1) .&. 0x7F)

withYoungest :: PointId -> Word64 -> Word64
withYoungest pid w =
  (w .&. complement (0x7F `unsafeShiftL` 1))
    .|. (fromIntegral (unPointId pid) `unsafeShiftL` 1)

-- | The id of the tx's location's point, and its offset there, if it has one.
locOf :: Word64 -> Maybe (Int, TxOffset)
locOf w
  | testBit w 63 =
      Just
        (fromIntegral ((w `unsafeShiftR` 55) .&. 0x7F), fromIntegral ((w `unsafeShiftR` 31) .&. 0xFFFFFF))
  | otherwise = Nothing

withLoc :: PointId -> TxOffset -> Word64 -> Word64
withLoc pid off w =
  withoutLoc w
    .|. bit 63
    .|. (fromIntegral (unPointId pid) `unsafeShiftL` 55)
    .|. ((fromIntegral off .&. 0xFFFFFF) `unsafeShiftL` 31)

-- | Clear the location, preserving the validation flag and lower fields.
withoutLoc :: Word64 -> Word64
withoutLoc w = w .&. (bit 62 .|. 0x7FFFFFFF)

-- | The tx's location, if it has one.
locationOf :: HtState b -> Word64 -> Maybe TxLocation
locationOf st w = do
  (i, off) <- locOf w
  src <- IntMap.lookup i (hsPointsById st)
  Just (MkTxLocation src off)

-- | The 32-byte hash as the table's four-word 'HT.Key' (big-endian words).
toKey :: TxHash -> HT.Key
toKey txh = case hashToPackedBytes txh of
  PackedBytes32 w0 w1 w2 w3 -> HT.Key w0 w1 w2 w3
  -- A 32-byte hash is always 'PackedBytes32'; this arm is unreachable and pays
  -- a copy only if it ever runs.
  pb -> HT.Key (wordAt 0) (wordAt 8) (wordAt 16) (wordAt 24)
   where
    bytes = unpackBytes pb
    wordAt i = foldl (\acc j -> acc `unsafeShiftL` 8 .|. fromIntegral (SBS.index bytes (i + j))) 0 [0 .. 7]
