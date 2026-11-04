{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LeiosDB.Impl.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  ) where

import Cardano.Prelude (Generic, fromMaybe, unless, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockEnvelope (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  , hashLeiosTx
  , mkEndorserBlockEnvelope
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LocalFill
  , Promise (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (LeiosDbWriteException)
  , LeiosDbWriteFailure (..)
  , throwLeiosDbException
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks (..)
  , atomically
  , throwIO
  , toException
  , try
  )
import Ouroboros.Network.Block (Serialised (..))

-- | In-memory database state.
data InMemoryLeiosDb = InMemoryLeiosDb
  { imEbTxBytes :: !(Map EndorserBlockHash (IntMap {- txOffset -} ByteString))
  -- ^ Tx bytes owned by the referencing EB, keyed by offset -- the SQLite
  -- backend's @ebClosures@ table, one tx at a time. A tx shared by two EBs is
  -- stored twice, deliberately.
  , imEbPoints :: !(Map LeiosPoint BytesSize)
  -- ^ Inserted EB points with their expected sizes. The same EB
  -- content can be inserted at multiple slots; each
  -- @(slot, hash)@ point has its own entry.
  , imEbBodies :: !(Map EndorserBlockHash StoredBody)
  -- ^ EB bodies, keyed by EB hash. Shared across every point that references
  -- the same EB.
  , imImmutableEbBodies :: !(Map EndorserBlockHash StoredBody)
  -- ^ The bodies of the EBs 'leiosDbPromoteToImmutable' promoted. The garbage
  -- collection evicts such an EB from the other fields like any other, and the
  -- reads fall back to this and 'imImmutableEbTxBytes'.
  , imImmutableEbTxBytes :: !(Map EndorserBlockHash (IntMap {- txOffset -} ByteString))
  -- ^ The tx bytes of the bodies in 'imImmutableEbBodies', by offset.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyInMemoryLeiosDb :: InMemoryLeiosDb
emptyInMemoryLeiosDb = InMemoryLeiosDb mempty mempty mempty mempty mempty

-- | An EB body: the version and bytes of the envelope it arrived in, as the
-- SQLite backend's @ebBodies@ row, and the txs it lists by offset.
data StoredBody = StoredBody
  { sbVersion :: !Word
  , sbBytes :: !ByteString
  , sbTxs :: !(IntMap {- txOffset -} EbTxEntry)
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | EB transaction entry (no bytes stored here, see 'imEbTxBytes')
data EbTxEntry = EbTxEntry
  { eteTxHash :: !TxHash
  , eteTxBytesSize :: !BytesSize
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | Create a new in-memory Leios database handle.
-- This is suitable for testing in IOSim.
newLeiosDBInMemory :: IOLike m => m (LeiosDbHandle m)
newLeiosDBInMemory = do
  stateVar <- newTVarIO emptyInMemoryLeiosDb
  newLeiosDBInMemoryWith stateVar

newLeiosDBInMemoryWith :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbHandle m)
newLeiosDBInMemoryWith stateVar =
  pure $
    LeiosDbHandle
      { -- Nothing to close: the state is a 'StrictTVar'.
        closeLeiosDbHandle = pure ()
      , garbageCollect = imGarbageCollect stateVar
      , leiosDbPromoteToImmutable = imPromoteToImmutable stateVar
      , -- The in-memory implementation does not track stats.
        leiosDbSampleStats = pure (LeiosDbStats 0 0 0)
      , openReader = openInMemoryReader stateVar
      , openWriter = openInMemoryWriter stateVar
      }

-- | Copy the closures of the given EBs to 'imImmutableEbBodies' and
-- 'imImmutableEbTxBytes', unless they are there already. The node pins only what
-- the immutable chain certifies, and only once it holds its closure, so a
-- closure that is incomplete is a caller's bug: this then fails, changing
-- nothing.
imPromoteToImmutable :: IOLike m => StrictTVar m InMemoryLeiosDb -> [LeiosPoint] -> m ()
imPromoteToImmutable stateVar points = do
  incomplete <- atomically $ do
    s <- readTVar stateVar
    let closures =
          Map.fromSet
            ( \ebHash -> do
                body <- Map.lookup ebHash (imEbBodies s)
                let txBytes = volatileTxBytes s ebHash
                if hashesMatch txBytes body then Just (body, txBytes) else Nothing
            )
            ( Set.fromList (map pointEbHash points)
                `Set.difference` Map.keysSet (imImmutableEbBodies s)
            )
        copied = Map.mapMaybe id closures
        incomplete = Map.keys (Map.filter isNothing closures)
    when (null incomplete) $
      writeTVar
        stateVar
        s
          { imImmutableEbBodies = imImmutableEbBodies s <> fmap fst copied
          , imImmutableEbTxBytes = imImmutableEbTxBytes s <> fmap snd copied
          }
    pure incomplete
  unless (null incomplete) $
    throwLeiosDbException $
      "leiosDbPromoteToImmutable: incomplete closure (programmer error): " <> show incomplete

-- | Evict every EB all of whose points are older than the slot, with its body
-- and tx bytes, and drop the older points of every other EB. A promoted EB
-- keeps its copy in 'imImmutableEbBodies' and 'imImmutableEbTxBytes'.
imGarbageCollect :: IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m ()
imGarbageCollect stateVar slot = atomically $ modifyTVar stateVar $ \s ->
  let newestPointSlots =
        Map.fromListWith max [(pointEbHash p, pointSlotNo p) | p <- Map.keys (imEbPoints s)]
      evicted = Map.keysSet (Map.filter (< slot) newestPointSlots)
   in s
        { imEbPoints = Map.filterWithKey (\p _ -> pointSlotNo p >= slot) (imEbPoints s)
        , imEbBodies = Map.withoutKeys (imEbBodies s) evicted
        , imEbTxBytes = Map.withoutKeys (imEbTxBytes s) evicted
        }

-- | Reads off the 'StrictTVar'. Nothing to open or close.
openInMemoryReader :: IOLike m => StrictTVar m InMemoryLeiosDb -> m (LeiosDbReader m)
openInMemoryReader stateVar =
  pure
    LeiosDbReader
      { closeReader = pure ()
      , lookupEbBody = imLookupEbBody stateVar
      , lookupEbClosure = imLookupEbClosure stateVar
      , batchRetrieveTxs = imBatchRetrieveTxs stateVar
      , testScanEbPoints = imScanEbPoints stateVar
      , -- ThreadNet persists 'stateVar' across simulated restarts, so on
        -- restart this seeds the restored acquired-EB-closures set.
        scanCompleteEbClosuresNotOlderThanSlot = imScanCompleteEbClosuresSince stateVar
      }

-- | No worker and no queue: the state is a 'StrictTVar', so each write runs
-- at submission and its 'Promise' comes back already resolved. A failure is
-- captured rather than thrown here, so it surfaces at 'await' exactly like
-- the SQLite backend's.
openInMemoryWriter ::
  forall m.
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  m (LeiosDbWriter m)
openInMemoryWriter stateVar =
  pure
    LeiosDbWriter
      { closeWriter = pure ()
      , writeEbPoint = \point ebBytesSize ->
          resolved ("WriteEbPoint " <> show point) (imInsertEbPoint stateVar point ebBytesSize)
      , writeEbBody = \point eb fills ->
          resolved ("WriteEbBody " <> show point) (imInsertEbBody stateVar point eb fills)
      , writeTxs = \point txs ->
          resolved
            ("WriteTxs " <> show point <> " (" <> show (length txs) <> " txs)")
            (imInsertTxs stateVar point txs)
      }
 where
  resolved :: HasCallStack => String -> m a -> m (Promise m a)
  resolved job action = do
    result <- try action
    pure $ Promise $ case result of
      Right x -> pure x
      Left cause ->
        throwIO $
          LeiosDbWriteException
            LeiosDbWriteFailure
              { ldwfWriteJob = job
              , ldwfSubmittedFrom = GHC.prettyCallStack GHC.callStack
              , ldwfWriteFailure = toException (cause :: LeiosDbException)
              }

-- * Top-level implementations

imScanEbPoints :: IOLike m => StrictTVar m InMemoryLeiosDb -> m [(SlotNo, EndorserBlockHash)]
imScanEbPoints stateVar = atomically $ do
  state <- readTVar stateVar
  pure
    [ (pointSlotNo p, pointEbHash p)
    | p <- Map.keys (imEbPoints state)
    ]

-- | Insert an announced EB point. Idempotent: a second insert at the
-- same point keeps the first-seen size.
imInsertEbPoint :: IOLike m => StrictTVar m InMemoryLeiosDb -> LeiosPoint -> BytesSize -> m ()
imInsertEbPoint stateVar point ebBytesSize = atomically $
  modifyTVar stateVar $ \s ->
    s{imEbPoints = Map.insertWith (\_ old -> old) point ebBytesSize (imEbPoints s)}

imLookupEbBody ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EndorserBlockHash -> m (Maybe EndorserBlockEnvelope)
imLookupEbBody stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $
    (\body -> MkEndorserBlockEnvelope (sbVersion body) (Serialised (BSL.fromStrict (sbBytes body))))
      <$> (Map.lookup ebHash (imEbBodies state) <|> Map.lookup ebHash (imImmutableEbBodies state))

-- | Mirrors the SQLite backend: the body, then the local fills, the first of
-- which wins an offset. A fill copies the source's bytes only if they hash to
-- the tx this body lists at the fill's offset.
imInsertEbBody ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  m [TxOffset]
imInsertEbBody stateVar point eb fills = do
  when (null refs) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  known <- Map.member point . imEbPoints <$> atomically (readTVar stateVar)
  unless known $
    throwLeiosDbException "writeEbBody: point not registered (programmer error)"
  atomically $ do
    modifyTVar stateVar $ \s ->
      s
        { -- The body is fully determined by the EB content hash, so a second
          -- insertion at the same hash is a no-op.
          imEbBodies =
            Map.insertWith (\_ old -> old) ebHash body (imEbBodies s)
        }
    s <- readTVar stateVar
    let copied =
          IntMap.fromListWith
            (\_new old -> old)
            [ (off, bytes)
            | (off, MkTxLocation srcHash srcOff) <- fills
            , Just e <- [IntMap.lookup off refs]
            , Just bytes <- [IntMap.lookup srcOff (volatileTxBytes s srcHash)]
            , hashLeiosTx (MkLeiosTx bytes) == eteTxHash e
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            ]
    unless (IntMap.null copied) $
      modifyTVar stateVar $ \s' ->
        s'{imEbTxBytes = Map.insertWith IntMap.union ebHash copied (imEbTxBytes s')}
    pure (IntMap.keys copied)
 where
  ebHash = pointEbHash point
  refs =
    IntMap.fromList
      [ (offset, EbTxEntry{eteTxHash = txHash, eteTxBytesSize = size})
      | (offset, (txHash, size)) <- zip [0 ..] (endorserBlockReferences eb)
      ]
  MkEndorserBlockEnvelope version (Serialised ebBytes) = mkEndorserBlockEnvelope eb
  body = StoredBody{sbVersion = version, sbBytes = BSL.toStrict ebBytes, sbTxs = refs}

imInsertTxs ::
  IOLike m =>
  StrictTVar m InMemoryLeiosDb ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  m ()
imInsertTxs stateVar point offBytes = atomically $ do
  let ebHash = pointEbHash point
  -- Mirrors the SQLite backend's in-place fill: a tx only lands at an offset
  -- the body declared, at the declared size, the first of a write winning.
  modifyTVar stateVar $ \s ->
    let declared = maybe IntMap.empty sbTxs (Map.lookup ebHash (imEbBodies s))
        accepted =
          IntMap.fromListWith
            (\_new old -> old)
            [ (off, bytes)
            | (off, bytes) <- offBytes
            , Just e <- [IntMap.lookup off declared]
            , fromIntegral (BS.length bytes) == eteTxBytesSize e
            ]
     in if IntMap.null accepted
          then s
          else s{imEbTxBytes = Map.insertWith IntMap.union ebHash accepted (imEbTxBytes s)}

-- | Implements 'scanCompleteEbClosuresNotOlderThanSlot': each EB whose closure
-- is complete, at its youngest point no older than the given slot, as in the
-- SQLite backend. Like there, a closure is complete if each of its txs hashes
-- to what the body lists. A promoted EB is complete too, since only complete
-- EBs are promoted.
imScanCompleteEbClosuresSince ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> SlotNo -> m [LeiosPoint]
imScanCompleteEbClosuresSince stateVar sinceSlot = atomically $ do
  s <- readTVar stateVar
  let isComplete ebHash =
        maybe False (hashesMatch (volatileTxBytes s ebHash)) (Map.lookup ebHash (imEbBodies s))
          || Map.member ebHash (imImmutableEbBodies s)
      youngest =
        Map.fromListWith
          max
          [ (pointEbHash p, pointSlotNo p)
          | p <- Map.keys (imEbPoints s)
          , pointSlotNo p >= sinceSlot
          ]
  pure
    [ MkLeiosPoint slot ebHash
    | (ebHash, slot) <- Map.toList youngest
    , isComplete ebHash
    ]

-- | Each requested offset the body declares, with its bytes, zeroes if not
-- written. A tx the volatile copy lacks is read from the immutable copy, if
-- any.
imBatchRetrieveTxs ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EndorserBlockHash -> [Int] -> m [(Int, ByteString)]
imBatchRetrieveTxs stateVar ebHash offsets = atomically $ do
  state <- readTVar stateVar
  pure
    [ (offset, txBytesOrZeroes state ebHash offset e)
    | Just body <-
        [Map.lookup ebHash (imEbBodies state) <|> Map.lookup ebHash (imImmutableEbBodies state)]
    , offset <- IntSet.toAscList (IntSet.fromList offsets)
    , Just e <- [IntMap.lookup offset (sbTxs body)]
    ]

imLookupEbClosure ::
  IOLike m => StrictTVar m InMemoryLeiosDb -> EndorserBlockHash -> m (Maybe [ByteString])
imLookupEbClosure stateVar ebHash = atomically $ do
  state <- readTVar stateVar
  pure $
    fmap
      (\body -> [txBytesOrZeroes state ebHash off e | (off, e) <- IntMap.toAscList (sbTxs body)])
      (Map.lookup ebHash (imEbBodies state) <|> Map.lookup ebHash (imImmutableEbBodies state))

-- | A tx's bytes, from the volatile copy or else the immutable one, or zeroes
-- of its size if not written, as the SQLite backend's closure is allocated.
txBytesOrZeroes :: InMemoryLeiosDb -> EndorserBlockHash -> Int -> EbTxEntry -> ByteString
txBytesOrZeroes state ebHash offset e =
  fromMaybe
    (BS.replicate (fromIntegral (eteTxBytesSize e)) 0)
    ( IntMap.lookup offset (volatileTxBytes state ebHash)
        <|> IntMap.lookup offset (Map.findWithDefault IntMap.empty ebHash (imImmutableEbTxBytes state))
    )

-- | The EB's tx bytes by offset, from 'imEbTxBytes'.
volatileTxBytes :: InMemoryLeiosDb -> EndorserBlockHash -> IntMap ByteString
volatileTxBytes s ebHash = Map.findWithDefault IntMap.empty ebHash (imEbTxBytes s)

-- | Whether each tx of the body, zeroes if not written as the reads return it,
-- hashes to what the body lists.
hashesMatch :: IntMap ByteString -> StoredBody -> Bool
hashesMatch txBytesByOffset body =
  and
    [ hashLeiosTx (MkLeiosTx tx) == eteTxHash e
    | (off, e) <- IntMap.toList (sbTxs body)
    , let tx =
            fromMaybe
              (BS.replicate (fromIntegral (eteTxBytesSize e)) 0)
              (IntMap.lookup off txBytesByOffset)
    ]
