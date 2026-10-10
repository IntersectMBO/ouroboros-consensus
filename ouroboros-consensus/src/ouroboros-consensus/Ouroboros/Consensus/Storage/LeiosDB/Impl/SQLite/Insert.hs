{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbPoint
  , sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosEb
  , LeiosPoint (..)
  , TxLocation (..)
  , TxOffset
  , ebHashBytes
  , encodeLeiosEbSize
  , leiosEbBodyItems
  , txHashBytes
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( CompletedEbs
  , LeiosEbNotification (..)
  , LocalFill
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , dbWithWriteTransaction
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (bumpVolatileStats)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))

sqlInsertEbPoint ::
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  BytesSize ->
  IO CompletedEbs
sqlInsertEbPoint conn notify point ebBytesSize = do
  (inserted, mbAcquired) <- dbWithWriteTransaction conn $ do
    inserted <- useStmt stInsertEbPoint $ do
      dbBindInt64 stInsertEbPoint 1 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindBlob stInsertEbPoint 2 (ebHashBytes (pointEbHash point))
      dbBindInt64 stInsertEbPoint 3 (fromIntegral ebBytesSize)
      dbStep1 stInsertEbPoint
      DB.changes db
    if inserted == 0
      then pure (inserted, Nothing)
      else do
        -- Cannot match the row just inserted, whose @missingTxCount@ is NULL.
        mbStoredSize <- useStmt stLookupStoredEbSize $ do
          dbBindBlob stLookupStoredEbSize 1 (ebHashBytes (pointEbHash point))
          dbStep stLookupStoredEbSize >>= \case
            DB.Done -> pure Nothing
            DB.Row ->
              Just . fromIntegral <$> DB.columnInt64 stLookupStoredEbSize 0
        case mbStoredSize of
          Nothing -> pure (inserted, Nothing)
          Just storedSize -> do
            -- The @ebs.ebHashBytes@ field might have been incorrect, since
            -- announcements aren't necessarily
            -- correct. 'sql_lookup_stored_eb_size' needs it to be correct, so
            -- overwrite it now.
            useStmt stSetEbPointSize $ do
              dbBindInt64 stSetEbPointSize 1 (fromIntegral storedSize)
              dbBindInt64 stSetEbPointSize 2 (fromIntegral $ unSlotNo (pointSlotNo point))
              dbBindBlob stSetEbPointSize 3 (ebHashBytes (pointEbHash point))
              dbStep1 stSetEbPointSize
            missingCount <- useStmt stInitMissingCount $ do
              dbBindBlob stInitMissingCount 1 (ebHashBytes (pointEbHash point))
              dbBindBlob stInitMissingCount 2 (ebHashBytes (pointEbHash point))
              dbBindInt64 stInitMissingCount 3 (fromIntegral $ unSlotNo (pointSlotNo point))
              readReturningInt64 stInitMissingCount
            completed <-
              if missingCount /= 0
                then pure []
                else do
                  useStmt stMarkPointNotified $ do
                    dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo (pointSlotNo point))
                    dbBindBlob stMarkPointNotified 2 (ebHashBytes (pointEbHash point))
                    dbStep1 stMarkPointNotified
                  pure [point]
            pure (inserted, Just (storedSize, completed))
  bumpVolatileStats conn inserted
  forM_ mbAcquired $ \(storedSize, completed) -> do
    notify (AcquiredEb point storedSize)
    forM_ completed $ notify . AcquiredEbTxs
  pure $ maybe [] snd mbAcquired
 where
  Conn
    { conVolDb = db
    , connVolStmts =
      VolStmts
        { stInsertEbPoint
        , stLookupStoredEbSize
        , stSetEbPointSize
        , stInitMissingCount
        , stMarkPointNotified
        }
    } = conn

-- | Persist an EB body. The point MUST already be present (inserted
-- via 'sqlInsertEbPoint' on the announcement path).
--
-- TODO Nothing here says an endorser block was caught misstating a
-- transaction's size, though this is where it is first detectable: the size
-- predicate in 'sql_insert_missing_txs' leaves such a body waiting on a
-- transaction we already hold, so its @missingTxCount@ stays positive for
-- good. A trace would want to fire once per offending endorser block rather
-- than once per transaction, which needs state neither this function nor
-- 'sqlInsertTxs' keeps, and there is no action to take on it either way ---
-- the issuer is not a peer we are connected to, and the peer that relayed the
-- body may be perfectly honest. Until that is worth building, an old endorser
-- block stuck at a positive @missingTxCount@ is the signal, and this names
-- them:
--
-- > SELECT e.ebHashBytes, COUNT(*) FROM ebTxs e
-- > JOIN txs t ON e.txHashBytes = t.txHashBytes
-- > WHERE t.txBytesSize <> e.txBytesSize
-- > GROUP BY e.ebHashBytes
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  LeiosEb ->
  [LocalFill] ->
  IO (CompletedEbs, [TxOffset])
sqlInsertEbBody tracer conn notify point eb fills = do
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  (completedNow, filledOffs) <- dbWithWriteTransaction conn $ do
    forM_ items $ \(txOffset, txHash, txBytesSize) -> useStmt stInsertEbTxsRow $ do
      dbBindBlob stInsertEbTxsRow 1 ebHashRaw
      dbBindInt64 stInsertEbTxsRow 2 (fromIntegral txOffset)
      dbBindBlob stInsertEbTxsRow 3 (txHashBytes txHash)
      dbBindInt64 stInsertEbTxsRow 4 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show (pointEbHash point) <> "@" <> show txOffset)
        stInsertEbTxsRow
    -- The body is the authority on its own size, whatever size the
    -- announcement's point was inserted at; see 'sql_lookup_stored_eb_size'.
    useStmt stSetEbPointSize $ do
      dbBindInt64 stSetEbPointSize 1 (fromIntegral ebBytesSize)
      dbBindInt64 stSetEbPointSize 2 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindBlob stSetEbPointSize 3 ebHashRaw
      dbStep1 stSetEbPointSize
    -- Allocate the closure's rows in one offset-ordered pass; see
    -- 'sql_prealloc_ebTxBytes'.
    useStmt stPreallocEbTxBytes $ do
      dbBindBlob stPreallocEbTxBytes 1 ebHashRaw
      dbStep1 stPreallocEbTxBytes
    -- Cross-EB fills: copy locally-held bytes into the freshly-allocated
    -- rows, in this transaction, so the count below already sees them. A
    -- vanished source changes nothing and the offset stays missing.
    filledOffs <-
      foldM
        ( \acc (dstOff, MkTxLocation srcEbHash srcOff) -> do
            useStmt stFillFromLocal $ do
              dbBindBlob stFillFromLocal 1 ebHashRaw
              dbBindInt64 stFillFromLocal 2 (fromIntegral dstOff)
              dbBindBlob stFillFromLocal 3 (ebHashBytes srcEbHash)
              dbBindInt64 stFillFromLocal 4 (fromIntegral srcOff)
              dbStep1Safe stFillFromLocal
            changed <- DB.changes (conVolDb conn)
            pure (if changed > 0 then dstOff : acc else acc)
        )
        []
        fills
    -- Initialize missingTxCount from the unfilled rows and read the result
    -- via @RETURNING missingTxCount@. In this transaction, so an arrival can
    -- never see the rows without the count.
    missingCount <- useStmt stInitMissingCount $ do
      dbBindBlob stInitMissingCount 1 ebHashRaw
      dbBindBlob stInitMissingCount 2 ebHashRaw
      dbBindInt64 stInitMissingCount 3 (fromIntegral $ unSlotNo (pointSlotNo point))
      readReturningInt64 stInitMissingCount
    completed <-
      if missingCount == 0
        then do
          useStmt stMarkPointNotified $ do
            dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo (pointSlotNo point))
            dbBindBlob stMarkPointNotified 2 ebHashRaw
            dbStep1 stMarkPointNotified
          pure [point]
        else pure []
    pure (completed, reverse filledOffs)
  notify $ AcquiredEb point ebBytesSize
  forM_ completedNow $ notify . AcquiredEbTxs
  pure (completedNow, filledOffs)
 where
  ebHashRaw = ebHashBytes (pointEbHash point)
  items = leiosEbBodyItems eb
  ebBytesSize = encodeLeiosEbSize eb
  Conn{connVolStmts} = conn
  VolStmts
    { stInsertEbTxsRow
    , stPreallocEbTxBytes
    , stFillFromLocal
    , stInitMissingCount
    , stMarkPointNotified
    , stSetEbPointSize
    } = connVolStmts

-- | Persist arriving transactions, completing the closure of every endorser
-- block that was waiting on them.
--
-- TODO As 'sqlInsertEbBody': an endorser block left waiting here because it
-- misstated this transaction's size goes unremarked, and for the same reasons.
sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  [(TxOffset, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs _tracer conn notify point offBytes = do
  completed <- dbWithWriteTransaction conn $ do
    -- In-place fills of the pre-allocated rows; the guards in
    -- 'sql_fill_ebTxBytes' make one change exactly one previously-missing
    -- valid tx, so counting by @changes()@ is sound: a bogus offset, a
    -- duplicate delivery or a wrong-sized payload changes nothing.
    n <-
      foldM
        ( \acc (txOffset, txBytes) -> do
            useStmt stFillEbTxBytes $ do
              dbBindBlob stFillEbTxBytes 1 (ebHashBytes (pointEbHash point))
              dbBindInt64 stFillEbTxBytes 2 (fromIntegral txOffset)
              dbBindBlob stFillEbTxBytes 3 txBytes
              dbStep1 stFillEbTxBytes
            changed <- DB.changes (conVolDb conn)
            pure (acc + fromIntegral changed)
        )
        (0 :: Int64)
        offBytes
    if n == 0
      then pure []
      else do
        -- Decrement every announcement of this content hash and collect the
        -- ones this batch completed.
        completedSlots <- useStmt stDecrMissingCount $ do
          dbBindBlob stDecrMissingCount 1 (ebHashBytes (pointEbHash point))
          dbBindInt64 stDecrMissingCount 2 n
          let loop acc =
                dbStep stDecrMissingCount >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    slot <- SlotNo . fromIntegral <$> DB.columnInt64 stDecrMissingCount 0
                    left <- DB.columnInt64 stDecrMissingCount 1
                    loop (if left == 0 then slot : acc else acc)
          loop []
        -- Mark them notified so they are not completed twice.
        forM_ completedSlots $ \slot -> useStmt stMarkPointNotified $ do
          dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo slot)
          dbBindBlob stMarkPointNotified 2 (ebHashBytes (pointEbHash point))
          dbStep1 stMarkPointNotified
        pure [MkLeiosPoint slot (pointEbHash point) | slot <- completedSlots]
  -- Emit a closure-completion notification for each completed EB
  forM_ completed $ notify . AcquiredEbTxs
  pure completed
 where
  Conn{connVolStmts} = conn
  VolStmts
    { stFillEbTxBytes
    , stDecrMissingCount
    , stMarkPointNotified
    } = connVolStmts
