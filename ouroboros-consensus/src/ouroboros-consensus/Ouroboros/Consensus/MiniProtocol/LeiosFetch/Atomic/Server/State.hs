{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Bounded server-side request, preparation and reply-commit accounting.
-- Payloads stay outside this state, owned by the single FIFO sender.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#7-define-too-old-by-a-bounded-request-window@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State
  ( ServerLimits (..)
  , defaultServerLimits
  , ServerState
  , ServerError (..)
  , ReplyChoice (..)
  , ServerSnapshot (..)
  , newServerState
  , startServer
  , acceptServerMessage
  , cancellationRequested
  , beginPreparation
  , finishPreparation
  , commitDataReply
  , commitCancelAck
  , replySent
  , finishServer
  , stopServer
  , serverSnapshot
  , serverInvariant
  ) where

import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow (Exception)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Word (Word16, Word32, Word64)
import Ouroboros.Consensus.Leios.Types (maxEndorserBlockBytesSize)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Inbound
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)

-- | Negotiated count and local payload ceilings. One head preparation at a time
-- reserves its kind's ceiling, then shrinks to the actual encoded response.
data ServerLimits = ServerLimits
  { serverDataLimit :: !Word16
  -- ^ Negotiated positive D. Controls have their own D slots; history is 2D.
  , serverPreparedLimit :: !Word32
  -- ^ Local prepared/sending byte budget; must fit either permitted response.
  , serverBodyReplyLimit :: !Word32
  -- ^ Encoded body-response ceiling, including its framing.
  , serverTxReplyLimit :: !Word32
  -- ^ Encoded TX-response ceiling, including per-element framing.
  , serverLastOrdinal :: !Word64
  -- ^ Production uses maxBound; small ceilings exercise exhaustion in fixtures.
  }
  deriving (Eq, Show)

-- | Initial protocol-sized reservations; validate the supplied negotiated D
-- when creating the server state.
defaultServerLimits :: Word16 -> ServerLimits
defaultServerLimits d =
  ServerLimits
    d
    (1024 * 1024)
    (maxEndorserBlockBytesSize + 3)
    (maxLeiosTxsRequestBytesSize + maxLeiosTxsRequestBytesSize `div` 10)
    maxBound

data RequestKind = BodyKind | TxsKind deriving (Eq, Show)

-- | A data record remains paired with any already-accepted cancel until both
-- replies commit. Recent history recognizes a cancel crossing an older commit.
data DataRecord = DataRecord
  { recordKind :: !RequestKind
  -- ^ Select the response reservation without reading storage on intake.
  , recordCancel :: !(Maybe WireOrdinal)
  -- ^ At most one accepted cancellation, with its own ACK exchange.
  , recordCommitted :: !Bool
  -- ^ Original terminal selected; cancellation can no longer replace it.
  }
  deriving (Eq, Show)

data Preparation = Preparation
  { preparationOrdinal :: !WireOrdinal
  -- ^ The sole head request whose payload the sender/preparation task owns.
  , preparationBytes :: !Word32
  -- ^ Kind ceiling during a read; actual bytes after it has joined successfully.
  , preparationReady :: !Bool
  -- ^ The bounded preparation task has ended and no longer owns the reader.
  }
  deriving (Eq, Show)

-- | Small metadata only. The interpreter separately owns at most one committed
-- output payload; committing releases protocol slots before send-return so a
-- fast client can legally request replacement work in the meantime.
data ServerState = ServerState
  { stateLimits :: !ServerLimits
  -- ^ Immutable session limits.
  , stateInbound :: !InboundState
  -- ^ Consecutive ordinals and bounded recent cancellation-target history.
  , stateRecords :: !(Map.Map WireOrdinal DataRecord)
  -- ^ At most D original/paired data records.
  , stateControls :: !(Map.Map WireOrdinal WireOrdinal)
  -- ^ At most D ACK ordinals mapped to their original targets.
  , statePreparation :: !(Maybe Preparation)
  -- ^ The sole uncommitted prepared/preparing payload charge.
  , stateSending :: !(Maybe (WireOrdinal, Word32))
  -- ^ The sole committed response remains charged until send returns.
  , stateStarted :: !Bool
  -- ^ Prevent a second interpreter from reusing this handle.
  , stateIntakeDone :: !Bool
  -- ^ Done received; accepted replies still drain in FIFO order.
  , stateStopped :: !Bool
  -- ^ Interpreter lifetime ended; no state transition may resurrect it.
  }
  deriving (Eq, Show)

-- | Invalid peer admissions and violated local configuration/lifecycle bounds.
data ServerError
  = InvalidServerLimits
  | ServerAlreadyStarted
  | ServerNotRunning
  | ServerIncoming !InboundError
  | ServerDataCapacity
  | ServerControlCapacity
  | ServerExchangeCapacity
  | ServerReplyOrder
  | ServerInvalidPreparation
  | ServerPreparedLimit !Word32 !Word32
  | ServerInvalidReply
  | ServerNotDrained
  deriving (Eq, Show)

instance Exception ServerError

-- | The irrevocable original terminal chosen atomically by the FIFO sender.
data ReplyChoice = NormalReply | CancelledReply deriving (Eq, Show)

-- | Bounded resource observation, not a measurement of complete heap use.
data ServerSnapshot = ServerSnapshot
  { snapshotDataRecords :: !Word32
  -- ^ Live originals, including pairs awaiting their already-accepted ACK.
  , snapshotControls :: !Word32
  -- ^ Uncommitted cancel acknowledgments.
  , snapshotExchanges :: !Word32
  -- ^ Uncommitted original and ACK exchanges, bounded by 2D.
  , snapshotHistory :: !Word32
  -- ^ Recent classifications, independently bounded by 2D.
  , snapshotPreparationBytes :: !Word32
  -- ^ Current reservation or actual prepared payload; zero when idle.
  , snapshotSendingBytes :: !Word32
  -- ^ Committed output charge, retained until channel send returns.
  , snapshotSending :: !Bool
  -- ^ At most one extra committed output, outside the uncommitted exchange set.
  , snapshotIntakeDone :: !Bool
  -- ^ Whether the client has requested a graceful drain.
  , snapshotStopped :: !Bool
  -- ^ Whether interpreter cleanup has retired this handle.
  }
  deriving (Eq, Show)

newServerState :: ServerLimits -> Either ServerError ServerState
newServerState
  limits@ServerLimits
    { serverDataLimit
    , serverPreparedLimit
    , serverBodyReplyLimit
    , serverTxReplyLimit
    , serverLastOrdinal
    } = do
    unless
      ( serverDataLimit > 0
          && serverBodyReplyLimit >= 2
          && serverTxReplyLimit >= 2
          && max serverBodyReplyLimit serverTxReplyLimit <= serverPreparedLimit
      )
      $ Left InvalidServerLimits
    inbound <- either (Left . ServerIncoming) Right (newInboundState serverDataLimit serverLastOrdinal)
    pure (ServerState limits inbound Map.empty Map.empty Nothing Nothing False False False)

startServer :: ServerState -> Either ServerError ServerState
startServer s
  | stateStarted s || stateStopped s = Left ServerAlreadyStarted
  | otherwise = Right s{stateStarted = True}

acceptServerMessage ::
  Message (LeiosFetchAtomic point eb tx) StIdle to -> ServerState -> Either ServerError ServerState
acceptServerMessage message s = do
  running s
  inbound <- either (Left . ServerIncoming) Right (checkIncomingMessage message (stateInbound s))
  let next = s{stateInbound = inbound}
  case message of
    MsgDone -> Right next{stateIntakeDone = True}
    MsgLeiosBlockRequest ordinal _ -> admit ordinal BodyKind next
    MsgLeiosBlockTxsRequest ordinal _ _ -> admit ordinal TxsKind next
    MsgTryCancel ordinal target -> do
      when (Map.size (stateControls s) >= fromIntegral (serverDataLimit (stateLimits s))) $
        Left ServerControlCapacity
      exchangeCapacity s
      pure
        next
          { stateControls = Map.insert ordinal target (stateControls s)
          , stateRecords = Map.adjust (\r -> r{recordCancel = Just ordinal}) target (stateRecords s)
          }
 where
  admit ordinal kind next = do
    when (Map.size (stateRecords s) >= fromIntegral (serverDataLimit (stateLimits s))) $
      Left ServerDataCapacity
    exchangeCapacity s
    pure next{stateRecords = Map.insert ordinal (DataRecord kind Nothing False) (stateRecords s)}

cancellationRequested :: WireOrdinal -> ServerState -> Either ServerError Bool
cancellationRequested ordinal s = do
  running s
  record <- pendingRecord ordinal s
  pure (not (isNothing (recordCancel record)))

-- | True starts a read under its byte reservation. False skips work already
-- canceled on intake. The single sender serializes access to its DB reader.
beginPreparation :: WireOrdinal -> ServerState -> Either ServerError (Bool, ServerState)
beginPreparation ordinal s = do
  running s
  requireHead ordinal s
  unless (isNothing (statePreparation s) && isNothing (stateSending s)) $
    Left ServerInvalidPreparation
  record <- pendingRecord ordinal s
  if isNothing (recordCancel record)
    then
      let bytes = case recordKind record of
            BodyKind -> serverBodyReplyLimit (stateLimits s)
            TxsKind -> serverTxReplyLimit (stateLimits s)
       in Right (True, s{statePreparation = Just (Preparation ordinal bytes False)})
    else Right (False, s)

-- | Publish only after the read task and its cancellation monitor have joined.
-- Nothing discards a canceled preparation; a normal result must fit its charge.
finishPreparation :: WireOrdinal -> Maybe Word32 -> ServerState -> Either ServerError ServerState
finishPreparation ordinal result s = do
  running s
  record <- pendingRecord ordinal s
  reservation <- case statePreparation s of
    Just Preparation{preparationOrdinal, preparationBytes, preparationReady = False}
      | preparationOrdinal == ordinal -> Right preparationBytes
    _ -> Left ServerInvalidPreparation
  bytes <- case result of
    Nothing | not (isNothing (recordCancel record)) -> Right 0
    Just n | n > 0 && n <= reservation -> Right n
    Just n -> Left (ServerPreparedLimit n reservation)
    _ -> Left ServerInvalidPreparation
  pure s{statePreparation = Just (Preparation ordinal bytes True)}

-- | Commit immediately before handing the selected message to ordered output.
-- Slot retirement happens here, while payload memory remains in stateSending.
commitDataReply :: WireOrdinal -> ServerState -> Either ServerError (ReplyChoice, ServerState)
commitDataReply ordinal s = do
  running s
  requireHead ordinal s
  unless (isNothing (stateSending s)) $ Left ServerInvalidReply
  record <- pendingRecord ordinal s
  prepared <- case statePreparation s of
    Nothing -> Right Nothing
    Just Preparation{preparationOrdinal, preparationBytes, preparationReady = True}
      | preparationOrdinal == ordinal -> Right (Just preparationBytes)
    _ -> Left ServerInvalidPreparation
  (choice, bytes) <-
    if isNothing (recordCancel record)
      then case prepared of
        Just n | n > 0 -> Right (NormalReply, n)
        _ -> Left ServerInvalidPreparation
      else Right (CancelledReply, 2)
  pure
    ( choice
    , s
        { statePreparation = Nothing
        , stateSending = Just (ordinal, bytes)
        , stateRecords =
            if isNothing (recordCancel record)
              then Map.delete ordinal (stateRecords s)
              else Map.insert ordinal record{recordCommitted = True} (stateRecords s)
        }
    )

commitCancelAck :: WireOrdinal -> ServerState -> Either ServerError ServerState
commitCancelAck ordinal s = do
  running s
  requireHead ordinal s
  unless (isNothing (stateSending s) && isNothing (statePreparation s)) $ Left ServerInvalidReply
  target <- maybe (Left ServerInvalidReply) Right (Map.lookup ordinal (stateControls s))
  case Map.lookup target (stateRecords s) of
    Nothing -> pure () -- valid cancel crossing an already committed original
    Just r -> unless (recordCommitted r && recordCancel r == Just ordinal) $ Left ServerInvalidReply
  pure
    s
      { stateRecords = Map.delete target (stateRecords s)
      , stateControls = Map.delete ordinal (stateControls s)
      , stateSending = Just (ordinal, 2)
      }

replySent :: WireOrdinal -> ServerState -> Either ServerError ServerState
replySent ordinal s = do
  running s
  case stateSending s of
    Just (key, _) | key == ordinal -> Right s{stateSending = Nothing}
    _ -> Left ServerInvalidReply

finishServer :: ServerState -> Either ServerError ServerState
finishServer s = do
  running s
  unless
    ( stateIntakeDone s
        && Map.null (stateRecords s)
        && Map.null (stateControls s)
        && isNothing (statePreparation s)
        && isNothing (stateSending s)
    )
    $ Left ServerNotDrained
  pure (stopServer s)

-- | Retire metadata only after the interpreter and its preparation tasks joined.
stopServer :: ServerState -> ServerState
stopServer s =
  s
    { stateRecords = Map.empty
    , stateControls = Map.empty
    , statePreparation = Nothing
    , stateSending = Nothing
    , stateStopped = True
    }

serverSnapshot :: ServerState -> ServerSnapshot
serverSnapshot s =
  ServerSnapshot
    (fromIntegral (Map.size (stateRecords s)))
    (fromIntegral (Map.size (stateControls s)))
    (fromIntegral (length (pendingOrdinals s)))
    (incomingHistorySize (stateInbound s))
    (maybe 0 preparationBytes (statePreparation s))
    (maybe 0 snd (stateSending s))
    (not (isNothing (stateSending s)))
    (stateIntakeDone s)
    (stateStopped s)

serverInvariant :: ServerState -> Bool
serverInvariant s =
  let ServerSnapshot
        { snapshotDataRecords
        , snapshotControls
        , snapshotExchanges
        , snapshotHistory
        , snapshotPreparationBytes
        , snapshotSendingBytes
        } = serverSnapshot s
      d = fromIntegral (serverDataLimit (stateLimits s)) :: Word32
      budget = serverPreparedLimit (stateLimits s)
   in snapshotDataRecords <= d
        && snapshotControls <= d
        && snapshotExchanges <= 2 * d
        && snapshotHistory <= 2 * d
        && snapshotPreparationBytes <= budget
        && snapshotSendingBytes <= budget
        && (isNothing (statePreparation s) || isNothing (stateSending s))
        && all
          ( \(key, r) ->
              maybe
                (not (recordCommitted r))
                (\c -> Map.lookup c (stateControls s) == Just key)
                (recordCancel r)
          )
          (Map.toList (stateRecords s))

running :: ServerState -> Either ServerError ()
running s = unless (stateStarted s && not (stateStopped s)) (Left ServerNotRunning)

pendingRecord :: WireOrdinal -> ServerState -> Either ServerError DataRecord
pendingRecord ordinal s = case Map.lookup ordinal (stateRecords s) of
  Just r | not (recordCommitted r) -> Right r
  _ -> Left ServerInvalidReply

pendingOrdinals :: ServerState -> [WireOrdinal]
pendingOrdinals s = Map.keys (Map.filter (not . recordCommitted) (stateRecords s)) ++ Map.keys (stateControls s)

requireHead :: WireOrdinal -> ServerState -> Either ServerError ()
requireHead ordinal s = case pendingOrdinals s of
  [] -> Left ServerReplyOrder
  xs -> unless (minimum xs == ordinal) (Left ServerReplyOrder)

exchangeCapacity :: ServerState -> Either ServerError ()
exchangeCapacity s =
  when (toInteger (length (pendingOrdinals s)) >= 2 * toInteger (serverDataLimit (stateLimits s))) $
    Left ServerExchangeCapacity
