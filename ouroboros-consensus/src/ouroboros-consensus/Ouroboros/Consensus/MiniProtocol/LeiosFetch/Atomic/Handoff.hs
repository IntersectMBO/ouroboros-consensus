{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Node-owned, bounded handoff of independently verified whole objects to
-- LeiosDB. Receive callbacks validate outside STM, then atomically claim and
-- enqueue into capacity reserved before dispatch. Only the single storage
-- worker waits for database submission/durability. A session's accepted data
-- remains charged after disconnect, including across reconnects to its owner.
--
-- Rationale and contract map (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff
  ( StorageLimits (..)
  , StorageHandoff
  , StorageSession
  , StorageRequest
  , storageRequestKey
  , storageRequestPrepared
  , StorageError (..)
  , StorageException (..)
  , Acceptance (..)
  , BodyAcceptance (..)
  , AcquisitionSnapshot (..)
  , StorageSnapshot (..)
  , StorageEvent (..)
  , newStorageHandoff
  , runStorageHandoff
  , withStorageHandoff
  , closeStorageHandoff
  , openStorageSession
  , closeStorageSession
  , beginAcquisition
  , retireAcquisition
  , registerDurableBody
  , admitStorageRequest
  , dispatchStorageRequest
  , cancelStorageRequest
  , finishStorageRequest
  , acceptTransaction
  , acceptBody
  , acceptLegacyBatch
  , readAcquisition
  , readStorageSnapshot
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (..)
  , STM
  , StrictTVar
  , newTVarIO
  , readTVar
  , throwSTM
  , writeTVar
  )
import Control.Monad (foldM, unless, when)
import Control.Monad.Class.MonadAsync (link, wait, withAsync)
import Control.Monad.Class.MonadThrow (Exception)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Word (Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosEbBounds (..)
  , LeiosEbSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , TxOffset
  , endorserBlockBytes
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , leiosEbSizes
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | Local resource limits, separate from negotiated wire limits. The eventual
-- node integration supplies the actual maximum session/EB counts. Counts and
-- sizes are bounded; wider snapshot arithmetic avoids intermediate overflow.
data StorageLimits = StorageLimits
  { storageBytesPerOwner :: !Word32
  -- ^ Reserved, queued and awaiting-settlement payload per stable upstream.
  , storageEntriesPerOwner :: !Word32
  -- ^ Corresponding whole-object slots, including a body as one object.
  , storageMaxSessions :: !Word32
  -- ^ Total live plus draining ledgers, across all registrations/owners.
  , storageMaxAcquisitions :: !Word32
  -- ^ Bound on retained EB acquisition metadata.
  , storageCheckpointBytes :: !Word32
  -- ^ Coalesce already-ready TXs until reaching this target; final TX may cross it.
  }
  deriving (Eq, Show)

-- | Handle owned by the node, rather than any individual fetch connection.
data StorageHandoff m owner peer = StorageHandoff
  { handoffLimits :: !StorageLimits
  -- ^ Validated local limits.
  , handoffState :: !(StrictTVar m (HandoffState owner peer))
  -- ^ Admission ledgers, acquisition claims and bounded payload ownership.
  }

-- | Unforgeable local handle for one registered fetch session.
data StorageSession peer = StorageSession !(LeiosPeerLifetimeId peer) !FetchSessionToken
  deriving (Eq, Ord, Show)

-- | Pinned immutable context returned only after reserving handoff capacity.
data StorageRequest peer = StorageRequest
  { storageRequestKey :: !(RequestKey peer)
  -- ^ Original lifetime/session/ordinal, never a mutable current-request pointer.
  , requestPrepared :: !PreparedRequest
  -- ^ Original expected positions, hashes/sizes and EB generation.
  }
  deriving (Eq, Show)

-- | Read the immutable descriptor and bounds reserved at admission. Receive
-- scopes pin this value before decoding; reassignment cannot replace it.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#validation-descriptors-and-progress-events@.
storageRequestPrepared :: StorageRequest peer -> PreparedRequest
storageRequestPrepared = requestPrepared

data Session owner peer = Session
  { sessionOwner :: !owner
  , sessionLedger :: !(A.AdmissionState peer)
  , sessionClosed :: !Bool
  }

-- | Acquisition state for one live EB point and relevance generation, shared
-- across peers and wire attempts. Pending claims suppress duplicate fetching;
-- only storage confirmation establishes durable possession.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
data Acquisition = Acquisition
  { acquisitionGeneration :: !AcquisitionGeneration
  -- ^ This point's current relevance generation. A receipt from a retired
  -- generation must not complete work for a later incarnation of the point.
  , acquisitionBody :: !(Maybe EndorserBlock)
  -- ^ Verified whole body, once known. Its metadata can be used while its
  -- storage write is still pending; this field alone does not imply durability.
  , acquisitionReferences :: !(IntMap.IntMap (TxHash, Word32))
  -- ^ Expected transaction hash and declared byte size by EB-body offset.
  -- Empty until the verified body is known.
  , acquisitionBodyDurable :: !Bool
  -- ^ Whether body storage has completed, directly or through the durable-body
  -- bridge. Queued transaction writes wait for this dependency.
  , acquisitionHeld :: !IntSet.IntSet
  -- ^ Offsets confirmed durable for this point, by a transaction receipt or
  -- the held-offset result of a committed body write.
  , acquisitionCredited :: !IntSet.IntSet
  -- ^ Offsets already reported as newly useful in this generation. Retained
  -- after an unconfirmed write so a replacement copy cannot earn credit again.
  , acquisitionPending :: !(IntMap.IntMap Natural)
  -- ^ Transaction offset to the claim ID of its queued or writing copy.
  -- This suppresses another storage claim without implying durable possession.
  , acquisitionPendingBody :: !(Maybe Natural)
  -- ^ Claim ID of the queued or writing whole body, if any. Its original
  -- session ticket remains charged until that claim is released.
  , acquisitionClosureNotified :: !Bool
  -- ^ Whether 'ClosureDurable' has been emitted for this generation, preventing
  -- repeated notification as other copies settle.
  }

-- | One independently verified whole object owned by a storage claim.
data ClaimPayload
  = -- | A complete EB body. Its durable write supplies the metadata rows needed
    -- before dependent transaction writes can be submitted.
    BodyPayload !EndorserBlock
  | -- | An EB-body transaction offset and an owned copy of that whole TX's
    -- bytes. Copying at acceptance avoids retaining a larger decoder buffer.
    TxPayload !TxOffset !BS.ByteString

-- | Node-owned storage responsibility for one accepted whole object, identified
-- by its key in 'storageClaims'. It retains the original session's charge until
-- receipt reconciliation or discard, even after that session disconnects.
-- Writing claims also survive retirement of their acquisition generation.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
data Claim peer = Claim
  { claimTicket :: !(A.HandoffKey peer)
  -- ^ Original session's charged handoff ticket. Releasing this exact ticket
  -- returns its payload/entry capacity without touching another request.
  , claimPoint :: !LeiosPoint
  -- ^ Exact EB point used for storage and acquisition lookup.
  , claimGeneration :: !AcquisitionGeneration
  -- ^ Acquisition generation captured at acceptance. An obsolete receipt may
  -- release its ticket but must not update a replacement generation.
  , claimPayload :: !ClaimPayload
  -- ^ Verified whole object retained while queued or awaiting write settlement.
  , claimWriting :: !Bool
  -- ^ Selected by the storage worker, including time blocked on submission.
  -- Pruning may discard queued claims, but keeps these charged until settlement.
  }

-- | Node-wide mutable state of one storage handoff manager. Session ledgers
-- account for reserved/unsettled capacity, claims own accepted payloads, and
-- acquisitions track progress for relevant EB generations. Their ownership
-- transitions are committed together in the handoff's STM state.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
data HandoffState owner peer = HandoffState
  { storageSessions :: !(Map.Map (StorageSession peer) (Session owner peer))
  -- ^ Live and closed-but-draining sessions. A closed entry remains while it
  -- owns pending tickets, so reconnecting cannot reset those charges.
  , storageAcquisitions :: !(Map.Map LeiosPoint Acquisition)
  -- ^ Relevant points and their current generations. Retirement removes an
  -- entry without waiting for older writes to finish.
  , storageClaims :: !(Map.Map Natural (Claim peer))
  -- ^ Local claim ID to queued or writing ownership, including writing copies
  -- for retired EBs. Acquisition pending maps refer to these same IDs.
  , storageNextSession :: !Natural
  -- ^ Next never-reused fetch-session token within this manager, across all
  -- registrations and upstream owners.
  , storageNextGeneration :: !Natural
  -- ^ Next never-reused acquisition generation, fencing late callbacks after
  -- a point is retired and refocused.
  , storageNextClaim :: !Natural
  -- ^ Next never-reused local storage claim ID, distinguishing replacement
  -- copies from the claims captured by earlier receipts.
  , storageClosing :: !Bool
  -- ^ Admission is closed while the storage worker drains accepted data.
  -- New sessions, acquisitions and data requests are rejected.
  , storageWorkerStarted :: !Bool
  -- ^ Whether the storage worker has ever started. Remains set after exit or
  -- failure so a second worker cannot start on the same handle.
  }

-- | Rejected transitions leave all reservations/claims unchanged.
data StorageError
  = InvalidStorageLimits
  | StorageClosed
  | SessionCapacity
  | AcquisitionCapacity
  | UnknownStorageSession
  | UnknownAcquisition
  | StaleAcquisition
  | BodyUnknown
  | OwnerByteCapacity
  | OwnerEntryCapacity
  | InvalidPosition
  | InvalidTransaction
  | InvalidBody
  | InvalidDescriptor
  | WrongRequestKind
  | WorkerAlreadyStarted
  | AdmissionFailure !A.AdmissionError
  deriving (Eq, Show)

-- | A violated worker/receipt invariant is fatal, like a real writer failure.
newtype StorageException = StorageException StorageError deriving Show

instance Exception StorageException

-- | Only 'Useful' earns newly useful network service. 'Reclaimed' obtains
-- storage ownership again after an unconfirmed write without crediting the
-- same offset twice in this generation. Pending copies are not durable yet.
data Acceptance = Useful | Reclaimed | PendingDuplicate | DurableDuplicate | Irrelevant
  deriving (Eq, Show)

-- | Report an announcement-size fault after handing off the valid whole body.
data BodyAcceptance = BodyAcceptance
  { acceptedBody :: !Acceptance
  -- ^ Whether this copy obtained node-owned storage responsibility.
  , bodySizeMisstated :: !Bool
  -- ^ The peer's announced size disagrees with the verified actual size.
  }
  deriving (Eq, Show)

-- | A bounded observation of one live acquisition generation.
data AcquisitionSnapshot = AcquisitionSnapshot
  { snapshotGeneration :: !AcquisitionGeneration
  -- ^ Generation to match before acting on an asynchronous observation.
  , snapshotBodyDurable :: !Bool
  -- ^ A body promise/legacy durable-body bridge has completed.
  , snapshotHeld :: !IntSet.IntSet
  -- ^ Durably confirmed transaction offsets.
  , snapshotPending :: !IntSet.IntSet
  -- ^ Unique offsets owned by storage, including queued and writing copies.
  , snapshotMissing :: !IntSet.IntSet
  -- ^ Known offsets still needing service; excludes held and storage-owned data.
  }
  deriving (Eq, Show)

-- | Payload accounting includes unreceived reservations and retired sessions.
-- It is a resource bound, not a measurement of ByteString/descriptor heap use.
data StorageSnapshot = StorageSnapshot
  { snapshotSessions :: !Word32
  -- ^ All live and draining ledgers.
  , snapshotDrainingSessions :: !Word32
  -- ^ Closed ledgers retained solely for pending storage tickets.
  , snapshotReservedBytes :: !Word64
  -- ^ Total unreceived/queued/writing payload charge.
  , snapshotReservedEntries :: !Word64
  -- ^ Total unreceived/queued/writing whole-object slots.
  , snapshotClaims :: !Word64
  -- ^ Unique accepted copies still owned by storage.
  }
  deriving (Eq, Show)

-- | Synchronous bounded coordinator notifications; consumers must not enqueue
-- an unbounded event history or wait on storage from these STM callbacks.
data StorageEvent
  = BodyDurable !LeiosPoint !AcquisitionGeneration !EndorserBlock !IntSet.IntSet
  | TransactionsDurable !LeiosPoint !AcquisitionGeneration !IntSet.IntSet
  | TransactionsUnconfirmed !LeiosPoint !AcquisitionGeneration !IntSet.IntSet
  | ClosureDurable !LeiosPoint !AcquisitionGeneration
  | StorageCapacityAvailable
  deriving (Eq, Show)

-- | Allocate a dormant arena with validated limits. Start its worker separately.
newStorageHandoff ::
  MonadSTM m => StorageLimits -> m (Either StorageError (StorageHandoff m owner peer))
newStorageHandoff limits
  | any
      (== 0)
      [ storageBytesPerOwner limits
      , storageEntriesPerOwner limits
      , storageMaxSessions limits
      , storageMaxAcquisitions limits
      , storageCheckpointBytes limits
      ] =
      pure (Left InvalidStorageLimits)
  | otherwise =
      Right . StorageHandoff limits
        <$> newTVarIO
          (HandoffState Map.empty Map.empty Map.empty 0 0 0 False False)

-- | Open a session under a caller-supplied stable upstream identity. Reconnects
-- use the same owner even if their connection/lifetime identifiers differ.
openStorageSession ::
  (MonadSTM m, Ord peer) =>
  StorageHandoff m owner peer ->
  owner ->
  LeiosPeerLifetimeId peer ->
  Word16 ->
  Word32 ->
  STM m (Either StorageError (StorageSession peer))
openStorageSession h owner lifetime d responseBytes = transition h $ \s -> do
  when (storageClosing s) (Left StorageClosed)
  when
    (fromIntegral (Map.size (storageSessions s)) >= storageMaxSessions (handoffLimits h))
    (Left SessionCapacity)
  limits <-
    admission $
      A.mkAdmissionLimits
        d
        (EncodedBytes (fromIntegral responseBytes))
        (PayloadBytes (fromIntegral (storageBytesPerOwner (handoffLimits h))))
        (EntryCount (fromIntegral (storageEntriesPerOwner (handoffLimits h))))
        maxBound
  let token = FetchSessionToken (storageNextSession s)
      session = StorageSession lifetime token
  pure
    ( session
    , s
        { storageNextSession = storageNextSession s + 1
        , storageSessions =
            Map.insert
              session
              (Session owner (A.newAdmissionState limits lifetime token) False)
              (storageSessions s)
        }
    )

-- | Close the wire side; accepted data and its budget survive until settlement.
closeStorageSession ::
  (MonadSTM m, Ord peer) => StorageHandoff m owner peer -> StorageSession peer -> STM m ()
closeStorageSession h key = do
  s <- readTVar (handoffState h)
  writeTVar (handoffState h) $
    cleanupSessions
      s
        { storageSessions =
            Map.adjust
              (\session -> session{sessionClosed = True, sessionLedger = A.retireSession (sessionLedger session)})
              key
              (storageSessions s)
        }

-- | Find the live generation or allocate a fresh one. Refocusing after
-- retirement never reuses the point's old generation.
beginAcquisition ::
  MonadSTM m =>
  StorageHandoff m owner peer -> LeiosPoint -> STM m (Either StorageError AcquisitionGeneration)
beginAcquisition h point = transition h $ \s -> do
  when (storageClosing s) (Left StorageClosed)
  case Map.lookup point (storageAcquisitions s) of
    Just a -> Right (acquisitionGeneration a, s)
    Nothing -> do
      when
        (fromIntegral (Map.size (storageAcquisitions s)) >= storageMaxAcquisitions (handoffLimits h))
        (Left AcquisitionCapacity)
      let gen = AcquisitionGeneration (storageNextGeneration s)
          a = Acquisition gen Nothing IntMap.empty False IntSet.empty IntSet.empty IntMap.empty Nothing False
      Right
        ( gen
        , s
            { storageNextGeneration = storageNextGeneration s + 1
            , storageAcquisitions = Map.insert point a (storageAcquisitions s)
            }
        )

-- | Drop queued irrelevant copies promptly; writing copies remain charged
-- until their receipt. Old wire descriptors still validate/drain normally.
retireAcquisition ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer -> LeiosPoint -> AcquisitionGeneration -> STM m ()
retireAcquisition h point gen = do
  s <- readTVar (handoffState h)
  case Map.lookup point (storageAcquisitions s) of
    Just a | acquisitionGeneration a == gen -> do
      let obsolete =
            Map.filter
              (\c -> claimPoint c == point && claimGeneration c == gen && not (claimWriting c))
              (storageClaims s)
          without = s{storageAcquisitions = Map.delete point (storageAcquisitions s)}
      next <-
        either (throwSTM . StorageException) pure $
          foldM (\st (cid, c) -> releaseClaim cid c st) without (Map.toList obsolete)
      writeTVar (handoffState h) (cleanupSessions next)
    _ -> pure ()

-- | Bridge an already committed whole-body write (including the legacy path).
-- Caller awaits storage first. No database read/wait belongs in this callback.
registerDurableBody ::
  IOLike m =>
  StorageHandoff m owner peer ->
  (StorageEvent -> STM m ()) ->
  LeiosPoint ->
  AcquisitionGeneration ->
  EndorserBlock ->
  IntSet.IntSet ->
  m (Either StorageError ())
registerDurableBody h notify point gen body held
  | not (validBody point body) = pure (Left InvalidBody)
  | otherwise = atomically $ transitionNotify h notify $ \s -> do
      a <- current point gen s
      let refs = references body
      unless (held `IntSet.isSubsetOf` IntMap.keysSet refs) (Left InvalidDescriptor)
      let next =
            a
              { acquisitionBody = Just body
              , acquisitionReferences = refs
              , acquisitionBodyDurable = True
              , acquisitionHeld = IntSet.union held (acquisitionHeld a)
              }
          (done, events) = completeClosure point next
          newlyHeld = IntSet.difference held (acquisitionHeld a)
          bodyEvents
            | not (acquisitionBodyDurable a) = [BodyDurable point gen body held]
            | not (IntSet.null newlyHeld) = [TransactionsDurable point gen newlyHeld]
            | otherwise = []
      pure ((), putAcquisition point done s, bodyEvents ++ events)

-- | Reserve the complete expected handoff before wire admission. A rejected
-- allocation consumes neither capacity nor a request ordinal.
admitStorageRequest ::
  (MonadSTM m, Ord owner, Ord peer) =>
  StorageHandoff m owner peer ->
  StorageSession peer ->
  PreparedRequest ->
  STM m (Either StorageError (StorageRequest peer))
admitStorageRequest h session prepared = transition h $ \s -> do
  when (storageClosing s) (Left StorageClosed)
  entry <- lookupSession session s
  let (point, gen) = descriptorIdentity (preparedDescriptor prepared)
  a <- current point gen s
  case preparedDescriptor prepared of
    BodyRequest{} -> pure ()
    BatchRequest batch -> do
      when (acquisitionBody a == Nothing) (Left BodyUnknown)
      unless (all (matchesExpected a) (batchExpected batch)) (Left InvalidDescriptor)
  (key, ledger) <- admission (A.admitData prepared (sessionLedger entry))
  let next = s{storageSessions = Map.insert session (entry{sessionLedger = ledger}) (storageSessions s)}
      usages =
        [ A.admissionUsage (sessionLedger v)
        | v <- Map.elems (storageSessions next)
        , sessionOwner v == sessionOwner entry
        ]
  when
    (sum (map A.usedHandoffBytes usages) > fromIntegral (storageBytesPerOwner (handoffLimits h)))
    (Left OwnerByteCapacity)
  when
    (sum (map A.usedHandoffEntries usages) > fromIntegral (storageEntriesPerOwner (handoffLimits h)))
    (Left OwnerEntryCapacity)
  Right (StorageRequest key prepared, next)

-- | Hand the next admitted command to the driver, before its send returns.
dispatchStorageRequest ::
  (MonadSTM m, Ord peer) =>
  StorageHandoff m owner peer -> RequestKey peer -> STM m (Either StorageError ())
dispatchStorageRequest h key = updateLedger h key $ \ledger -> ((),) <$> admission (A.dispatchNext key ledger)

-- | Admit a cancel's own exchange, preserving pending storage ownership.
cancelStorageRequest ::
  (MonadSTM m, Ord peer) =>
  StorageHandoff m owner peer -> StorageRequest peer -> STM m (Either StorageError (RequestKey peer))
cancelStorageRequest h req = updateLedger h (storageRequestKey req) $ admission . A.admitCancel (storageRequestKey req)

-- | Retire a FIFO wire result without releasing data that awaits storage.
finishStorageRequest ::
  (MonadSTM m, Ord peer) =>
  StorageHandoff m owner peer ->
  RequestKey peer ->
  A.OriginalTerminal ->
  STM m (Either StorageError ())
finishStorageRequest h key terminal = updateLedger h key $ \ledger -> ((),) <$> admission (A.finishHead key terminal ledger)

-- | Validate a complete TX before the short STM handoff. Position is local to
-- the pinned original descriptor, even if the EB is now obsolete or duplicate.
acceptTransaction ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  StorageRequest peer ->
  Word32 ->
  LeiosTx ->
  m (Either StorageError Acceptance)
acceptTransaction h req position tx = case validateTransaction req position tx of
  Left err -> pure (Left err)
  Right expected -> atomically $ transition h $ acceptTx req position expected tx

-- | Whole-vector legacy integration point: validate every element first, then
-- claim through the same acquisition state. Ordinary legacy admission/scheduling
-- is bridged separately by the checkpoint integration, not activated here.
acceptLegacyBatch ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  StorageRequest peer ->
  [LeiosTx] ->
  m (Either StorageError [Acceptance])
acceptLegacyBatch h req txs = case preparedDescriptor (requestPrepared req) of
  BodyRequest{} -> pure (Left WrongRequestKind)
  BatchRequest batch
    | length txs /= NE.length (batchExpected batch) -> pure (Left InvalidPosition)
    | otherwise -> case traverse (\(i, tx) -> (i,tx,) <$> validateTransaction req i tx) (zip [0 ..] txs) of
        Left err -> pure (Left err)
        Right validated -> atomically $ transition h $ \s -> do
          (rev, next) <-
            foldM
              ( \(acc, st) (i, tx, expected) -> do
                  (result, st') <- acceptTx req i expected tx st
                  pure (result : acc, st')
              )
              ([], s)
              validated
          pure (reverse rev, next)

acceptBody ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  StorageRequest peer ->
  EndorserBlock ->
  m (Either StorageError BodyAcceptance)
acceptBody h req body = case preparedDescriptor (requestPrepared req) of
  BatchRequest{} -> pure (Left WrongRequestKind)
  BodyRequest descriptor
    | not (validBody (bodyPoint descriptor) body) -> pure (Left InvalidBody)
    | (fromIntegral (BS.length (endorserBlockBytes body)) :: Word64)
        > fromIntegral (lebMaxBodySize bounds) ->
        pure (Left InvalidBody)
    | lesBodySize sizes > lebMaxBodySize bounds || lesTxsSize sizes > fromIntegral (lebMaxTxsSize bounds) ->
        pure (Left InvalidBody)
    | otherwise -> atomically $ transition h $ \s -> do
        let point = bodyPoint descriptor
            gen = bodyGeneration descriptor
            result = case current point gen s of
              Left _ -> Irrelevant
              Right a
                | acquisitionBodyDurable a -> DurableDuplicate
                | acquisitionPendingBody a /= Nothing -> PendingDuplicate
                | otherwise -> Useful
        (ticket, next) <- observe req 0 (lesBodySize sizes) (result == Useful) s
        let cid = storageNextClaim s
            claimed = case ticket of
              Nothing -> next
              Just t ->
                putAcquisition
                  point
                  ( (getCurrent point gen next)
                      { acquisitionBody = Just body
                      , acquisitionReferences = references body
                      , acquisitionPendingBody = Just cid
                      }
                  )
                  next
                    { storageNextClaim = cid + 1
                    , storageClaims = Map.insert cid (Claim t point gen (BodyPayload body) False) (storageClaims next)
                    }
        pure (BodyAcceptance result (lesBodySize sizes /= bodyAnnouncedBytes descriptor), claimed)
   where
    bounds = bodyTrustedBounds descriptor
    sizes = leiosEbSizes body

-- | A single node-owned worker. Spawn/link it once in the node registry, or use
-- 'withStorageHandoff'. Submission and promise waits occur only in this worker;
-- an exception is fatal and is never converted into an empty success receipt.
runStorageHandoff ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer -> LeiosDbWriter m -> (StorageEvent -> STM m ()) -> m ()
runStorageHandoff h writer notify = do
  atomically $ do
    s <- readTVar (handoffState h)
    when (storageWorkerStarted s) (throwSTM (StorageException WorkerAlreadyStarted))
    writeTVar (handoffState h) s{storageWorkerStarted = True}
  loop
 where
  loop = do
    work <- atomically $ takeWork h notify
    case work of
      StorageStopped -> pure ()
      StorageDiscarded -> loop
      StorageWrite claims@((_, first) NE.:| _) -> do
        case claimPayload first of
          BodyPayload body -> do
            held <- await =<< writeEbBody writer (claimPoint first) body
            atomically $ settleBody h notify claims body (IntSet.fromList held)
          TxPayload{} -> do
            let txs = [(off, bytes) | (_, c) <- NE.toList claims, TxPayload off bytes <- [claimPayload c]]
            receipt <- await =<< writeTxs writer (claimPoint first) txs
            atomically $ settleTransactions h notify claims receipt
        loop

-- | Scoped worker ownership. Normal exit retires wire reservations and drains
-- accepted data; exceptional node shutdown cancels/joins the worker.
withStorageHandoff ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  LeiosDbWriter m ->
  (StorageEvent -> STM m ()) ->
  (StorageHandoff m owner peer -> m a) ->
  m a
withStorageHandoff h writer notify action = withAsync (runStorageHandoff h writer notify) $ \worker -> do
  link worker
  result <- action h
  atomically $ closeStorageHandoff h
  wait worker
  pure result

closeStorageHandoff ::
  MonadSTM m => StorageHandoff m owner peer -> STM m ()
closeStorageHandoff h = do
  s <- readTVar (handoffState h)
  writeTVar (handoffState h) $
    cleanupSessions
      s
        { storageClosing = True
        , storageSessions =
            fmap
              (\v -> v{sessionClosed = True, sessionLedger = A.retireSession (sessionLedger v)})
              (storageSessions s)
        }

-- | Observe current missing/pending/held state without creating a point.
readAcquisition ::
  MonadSTM m => StorageHandoff m owner peer -> LeiosPoint -> STM m (Maybe AcquisitionSnapshot)
readAcquisition h point = do
  s <- readTVar (handoffState h)
  pure $ do
    a <- Map.lookup point (storageAcquisitions s)
    let held = acquisitionHeld a; pending = IntMap.keysSet (acquisitionPending a)
    pure
      ( AcquisitionSnapshot
          (acquisitionGeneration a)
          (acquisitionBodyDurable a)
          held
          pending
          (IntSet.difference (IntMap.keysSet (acquisitionReferences a)) (IntSet.union held pending))
      )

-- | Inspect total pressure, including retired sessions with unsettled data.
readStorageSnapshot :: MonadSTM m => StorageHandoff m owner peer -> STM m StorageSnapshot
readStorageSnapshot h = do
  s <- readTVar (handoffState h)
  let entries = Map.elems (storageSessions s)
      usage = map (A.admissionUsage . sessionLedger) entries
  pure
    ( StorageSnapshot
        (fromIntegral (length entries))
        (fromIntegral (length (filter sessionClosed entries)))
        (fromIntegral (sum (map A.usedHandoffBytes usage)))
        (fromIntegral (sum (map A.usedHandoffEntries usage)))
        (fromIntegral (Map.size (storageClaims s)))
    )

-- Internal transitions never wait on external resources.
transition ::
  MonadSTM m =>
  StorageHandoff m owner peer ->
  (HandoffState owner peer -> Either StorageError (a, HandoffState owner peer)) ->
  STM m (Either StorageError a)
transition h f = do
  s <- readTVar (handoffState h)
  case f s of
    Left err -> pure (Left err)
    Right (result, next) -> writeTVar (handoffState h) next >> pure (Right result)

transitionNotify ::
  MonadSTM m =>
  StorageHandoff m owner peer ->
  (StorageEvent -> STM m ()) ->
  (HandoffState owner peer -> Either StorageError (a, HandoffState owner peer, [StorageEvent])) ->
  STM m (Either StorageError a)
transitionNotify h notify f = do
  s <- readTVar (handoffState h)
  case f s of
    Left err -> pure (Left err)
    Right (result, next, events) -> writeTVar (handoffState h) next >> mapM_ notify events >> pure (Right result)

admission :: Either A.AdmissionError a -> Either StorageError a
admission = either (Left . AdmissionFailure) Right

requestSession :: RequestKey peer -> StorageSession peer
requestSession (RequestKey lifetime session _) = StorageSession lifetime session

lookupSession ::
  Ord peer =>
  StorageSession peer -> HandoffState owner peer -> Either StorageError (Session owner peer)
lookupSession key s = maybe (Left UnknownStorageSession) Right (Map.lookup key (storageSessions s))

updateLedger ::
  (MonadSTM m, Ord peer) =>
  StorageHandoff m owner peer ->
  RequestKey peer ->
  (A.AdmissionState peer -> Either StorageError (a, A.AdmissionState peer)) ->
  STM m (Either StorageError a)
updateLedger h key f = transition h $ \s -> do
  entry <- lookupSession (requestSession key) s
  (result, ledger) <- f (sessionLedger entry)
  pure
    ( result
    , s
        { storageSessions =
            Map.insert (requestSession key) (entry{sessionLedger = ledger}) (storageSessions s)
        }
    )

descriptorIdentity :: RequestDescriptor -> (LeiosPoint, AcquisitionGeneration)
descriptorIdentity (BodyRequest body) = (bodyPoint body, bodyGeneration body)
descriptorIdentity (BatchRequest batch) = (batchPoint batch, batchGeneration batch)

current ::
  LeiosPoint -> AcquisitionGeneration -> HandoffState owner peer -> Either StorageError Acquisition
current point gen s = do
  a <- maybe (Left UnknownAcquisition) Right (Map.lookup point (storageAcquisitions s))
  unless (acquisitionGeneration a == gen) (Left StaleAcquisition)
  Right a

getCurrent :: LeiosPoint -> AcquisitionGeneration -> HandoffState owner peer -> Acquisition
getCurrent point gen = either (error . show) id . current point gen

putAcquisition :: LeiosPoint -> Acquisition -> HandoffState owner peer -> HandoffState owner peer
putAcquisition point a s = s{storageAcquisitions = Map.insert point a (storageAcquisitions s)}

references :: EndorserBlock -> IntMap.IntMap (TxHash, Word32)
references = IntMap.fromDistinctAscList . zip [0 ..] . endorserBlockReferences

validBody :: LeiosPoint -> EndorserBlock -> Bool
validBody point body = endorserBlockHash body == pointEbHash point && not (null (endorserBlockReferences body))

matchesExpected :: Acquisition -> ExpectedTx -> Bool
matchesExpected a expected =
  IntMap.lookup (expectedOffset expected) (acquisitionReferences a)
    == Just (expectedHash expected, expectedSize expected)

validateTransaction :: StorageRequest peer -> Word32 -> LeiosTx -> Either StorageError ExpectedTx
validateTransaction req position tx = case preparedDescriptor (requestPrepared req) of
  BodyRequest{} -> Left WrongRequestKind
  BatchRequest batch -> do
    when
      ((fromIntegral position :: Word64) >= fromIntegral (NE.length (batchExpected batch)))
      (Left InvalidPosition)
    let expected = NE.toList (batchExpected batch) !! fromIntegral position
    unless
      ( (fromIntegral (BS.length (leiosTxBytes tx)) :: Word64) == fromIntegral (expectedSize expected)
          && hashLeiosTx tx == expectedHash expected
      )
      (Left InvalidTransaction)
    Right expected

observe ::
  Ord peer =>
  StorageRequest peer ->
  Word32 ->
  Word32 ->
  Bool ->
  HandoffState owner peer ->
  Either StorageError (Maybe (A.HandoffKey peer), HandoffState owner peer)
observe req position bytes useful s = do
  let key = storageRequestKey req; session = requestSession key
  entry <- lookupSession session s
  observed <- admission (A.observedElements key (sessionLedger entry))
  unless (observed == EntryCount (fromIntegral position)) (Left InvalidPosition)
  (ticket, ledger) <-
    admission (A.observeElement key (PayloadBytes (fromIntegral bytes)) useful (sessionLedger entry))
  pure
    ( ticket
    , s{storageSessions = Map.insert session (entry{sessionLedger = ledger}) (storageSessions s)}
    )

acceptTx ::
  Ord peer =>
  StorageRequest peer ->
  Word32 ->
  ExpectedTx ->
  LeiosTx ->
  HandoffState owner peer ->
  Either StorageError (Acceptance, HandoffState owner peer)
acceptTx req position expected tx s = do
  let (point, gen) = descriptorIdentity (preparedDescriptor (requestPrepared req))
      off = expectedOffset expected
      result = case current point gen s of
        Left _ -> Irrelevant
        Right a
          | IntSet.member off (acquisitionHeld a) -> DurableDuplicate
          | IntMap.member off (acquisitionPending a) -> PendingDuplicate
          | IntSet.member off (acquisitionCredited a) -> Reclaimed
          | otherwise -> Useful
  (ticket, next) <-
    observe req position (expectedSize expected) (result == Useful || result == Reclaimed) s
  let cid = storageNextClaim s
  pure
    ( result
    , case ticket of
        Nothing -> next
        Just t ->
          putAcquisition
            point
            ( (getCurrent point gen next)
                { acquisitionPending = IntMap.insert off cid (acquisitionPending (getCurrent point gen next))
                , acquisitionCredited = IntSet.insert off (acquisitionCredited (getCurrent point gen next))
                }
            )
            next
              { storageNextClaim = cid + 1
              , storageClaims =
                  Map.insert
                    cid
                    (Claim t point gen (TxPayload off (BS.copy (leiosTxBytes tx))) False)
                    (storageClaims next)
              }
    )

cleanupSessions :: HandoffState owner peer -> HandoffState owner peer
cleanupSessions s =
  s
    { storageSessions =
        Map.filter
          (\v -> not (sessionClosed v) || not (null (A.pendingHandoffs (sessionLedger v))))
          (storageSessions s)
    }

releaseClaim ::
  Ord peer =>
  Natural -> Claim peer -> HandoffState owner peer -> Either StorageError (HandoffState owner peer)
releaseClaim cid claim s = do
  let A.HandoffKey key _ = claimTicket claim; session = requestSession key
  entry <- lookupSession session s
  ledger <- admission (A.releaseHandoff (claimTicket claim) (sessionLedger entry))
  let released =
        s
          { storageClaims = Map.delete cid (storageClaims s)
          , storageSessions = Map.insert session (entry{sessionLedger = ledger}) (storageSessions s)
          }
  pure $ case current (claimPoint claim) (claimGeneration claim) released of
    Left _ -> released
    Right a ->
      putAcquisition
        (claimPoint claim)
        ( case claimPayload claim of
            BodyPayload{} | acquisitionPendingBody a == Just cid -> a{acquisitionPendingBody = Nothing}
            TxPayload off _
              | IntMap.lookup off (acquisitionPending a) == Just cid ->
                  a{acquisitionPending = IntMap.delete off (acquisitionPending a)}
            _ -> a
        )
        released

-- Discard is a separate committed worker turn: retrying STM after freeing a
-- redundant claim would roll back that release and its capacity notification.
data StorageWork peer
  = StorageStopped
  | StorageDiscarded
  | StorageWrite !(NE.NonEmpty (Natural, Claim peer))

takeWork ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer -> (StorageEvent -> STM m ()) -> STM m (StorageWork peer)
takeWork h notify = do
  s <- readTVar (handoffState h)
  case listToMaybe
    [pair | pair@(_, c) <- Map.toAscList (storageClaims s), not (claimWriting c), redundant c s] of
    Just (cid, c) -> do
      next <- either (throwSTM . StorageException) pure (releaseClaim cid c s)
      writeTVar (handoffState h) (cleanupSessions next)
      notify StorageCapacityAvailable
      pure StorageDiscarded
    Nothing -> selectWork s
 where
  redundant c s = case current (claimPoint c) (claimGeneration c) s of
    Left _ -> True
    Right a -> case claimPayload c of
      BodyPayload{} -> acquisitionBodyDurable a
      TxPayload off _ -> IntSet.member off (acquisitionHeld a)
  selectWork s = do
    let ready (_, c) =
          not (claimWriting c) && case claimPayload c of
            BodyPayload{} -> True
            TxPayload{} -> either (const False) acquisitionBodyDurable (current (claimPoint c) (claimGeneration c) s)
    case listToMaybe (filter ready (Map.toAscList (storageClaims s))) of
      Nothing
        | storageClosing s && Map.null (storageClaims s) -> pure StorageStopped
        | otherwise -> retry
      Just first@(_, c) -> do
        let selected = case claimPayload c of
              BodyPayload{} -> [first]
              TxPayload{} ->
                coalesce
                  (fromIntegral (storageCheckpointBytes (handoffLimits h)))
                  [ pair
                  | pair@(_, other) <- Map.toAscList (storageClaims s)
                  , ready pair
                  , claimPoint other == claimPoint c
                  , claimGeneration other == claimGeneration c
                  , TxPayload{} <- [claimPayload other]
                  ]
            next =
              s
                { storageClaims =
                    foldl'
                      (\claims (cid, _) -> Map.adjust (\v -> v{claimWriting = True}) cid claims)
                      (storageClaims s)
                      selected
                }
        case NE.nonEmpty selected of
          Nothing -> throwSTM (StorageException InvalidDescriptor)
          Just group -> writeTVar (handoffState h) next >> pure (StorageWrite group)

coalesce :: Word64 -> [(Natural, Claim peer)] -> [(Natural, Claim peer)]
coalesce _ [] = []
coalesce remaining (item@(_, c) : rest) =
  item : case claimPayload c of
    TxPayload _ bytes
      | fromIntegral (BS.length bytes) < remaining ->
          coalesce (remaining - fromIntegral (BS.length bytes)) rest
    _ -> []

completeClosure :: LeiosPoint -> Acquisition -> (Acquisition, [StorageEvent])
completeClosure point a
  | acquisitionBodyDurable a
      && not (acquisitionClosureNotified a)
      && IntMap.keysSet (acquisitionReferences a) `IntSet.isSubsetOf` acquisitionHeld a =
      (a{acquisitionClosureNotified = True}, [ClosureDurable point (acquisitionGeneration a)])
  | otherwise = (a, [])

settleBody ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  (StorageEvent -> STM m ()) ->
  NE.NonEmpty (Natural, Claim peer) ->
  EndorserBlock ->
  IntSet.IntSet ->
  STM m ()
settleBody h notify claims body held = do
  unless
    (held `IntSet.isSubsetOf` IntMap.keysSet (references body))
    (throwSTM (StorageException InvalidDescriptor))
  s <- readTVar (handoffState h)
  let (cid, c) = NE.head claims
      point = claimPoint c
      gen = claimGeneration c
      (updated, events) = case current point gen s of
        Right a
          | acquisitionPendingBody a == Just cid ->
              let next =
                    a
                      { acquisitionBodyDurable = True
                      , acquisitionPendingBody = Nothing
                      , acquisitionHeld = IntSet.union held (acquisitionHeld a)
                      }
                  (done, closure) = completeClosure point next
                  newlyHeld = IntSet.difference held (acquisitionHeld a)
                  notes
                    | not (acquisitionBodyDurable a) = [BodyDurable point gen body held]
                    | not (IntSet.null newlyHeld) = [TransactionsDurable point gen newlyHeld]
                    | otherwise = []
               in (putAcquisition point done s, notes ++ closure)
        _ -> (s, [])
  next <- either (throwSTM . StorageException) pure (releaseClaim cid c updated)
  writeTVar (handoffState h) (cleanupSessions next)
  mapM_ notify events
  notify StorageCapacityAvailable

settleTransactions ::
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  (StorageEvent -> STM m ()) ->
  NE.NonEmpty (Natural, Claim peer) ->
  TxWriteReceipt ->
  STM m ()
settleTransactions h notify claims receipt = do
  unless (null (txWriteWrongSize receipt)) (throwSTM (StorageException InvalidTransaction))
  let submitted = IntSet.fromList [off | (_, c) <- NE.toList claims, TxPayload off _ <- [claimPayload c]]
      yes = txWriteConfirmed receipt
      no = txWriteUnconfirmed receipt
  unless
    (IntSet.null (IntSet.intersection yes no) && IntSet.union yes no == submitted)
    (throwSTM (StorageException InvalidDescriptor))
  s <- readTVar (handoffState h)
  let first = snd (NE.head claims)
      point = claimPoint first
      gen = claimGeneration first
      (updated, events) = case current point gen s of
        Right a ->
          let matching =
                IntSet.fromList
                  [ off
                  | (cid, c) <- NE.toList claims
                  , TxPayload off _ <- [claimPayload c]
                  , IntMap.lookup off (acquisitionPending a) == Just cid
                  ]
              confirmed = IntSet.intersection matching yes
              missing = IntSet.intersection matching no
              next =
                a
                  { acquisitionPending = IntMap.withoutKeys (acquisitionPending a) matching
                  , acquisitionHeld = IntSet.union confirmed (acquisitionHeld a)
                  }
              (done, closure) = completeClosure point next
              notes =
                [TransactionsDurable point gen confirmed | not (IntSet.null confirmed)]
                  ++ [TransactionsUnconfirmed point gen missing | not (IntSet.null missing)]
                  ++ closure
           in (putAcquisition point done s, notes)
        Left _ -> (s, [])
  next <-
    either (throwSTM . StorageException) pure $
      foldM (\st (cid, c) -> releaseClaim cid c st) updated (NE.toList claims)
  writeTVar (handoffState h) (cleanupSessions next)
  mapM_ notify events
  notify StorageCapacityAvailable
