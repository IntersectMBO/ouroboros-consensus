{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Consecutive incoming ordinals and bounded recent cancel targets. This is
-- independent of the server's eventual preparation/data-slot accounting.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#7-define-too-old-by-a-bounded-request-window@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Inbound
  ( InboundState
  , InboundError (..)
  , newInboundState
  , checkIncomingMessage
  , incomingNextOrdinal
  , incomingHistorySize
  ) where

import Control.Monad (unless, when)
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal (..))

-- | Classify recent exchanges independently of whether their reply has already
-- committed: canceling a recent completed data request still has its own ACK.
data RecentRequest = RecentData !Bool | RecentControl
  deriving (Eq, Show)

-- | A session's incoming sequence and at most W recent exchange classifications.
-- Local Natural arithmetic distinguishes exhausted Word64 space from ordinal 0.
data InboundState = InboundState
  { inboundWindow :: !Word32
  -- ^ W = 2D, widened before multiplication.
  , inboundLastOrdinal :: !Word64
  -- ^ Production uses maxBound; reduced ceilings exercise exhaustion arithmetic.
  , incomingNextOrdinal :: !Natural
  -- ^ Exact expected next ordinal, possibly one beyond the permitted wire range.
  , inboundRecent :: !(Map.Map WireOrdinal RecentRequest)
  -- ^ Original data/control kind and whether a data target was already canceled.
  , inboundClosed :: !Bool
  -- ^ Done was received; subsequent commands are invalid.
  }
  deriving (Eq, Show)

-- | Invalid commands never change the sequence, target history or cancel flag.
data InboundError
  = InvalidIncomingLimit
  | IncomingSessionClosed
  | IncomingOrdinalExhausted
  | UnexpectedIncomingOrdinal
  | IncomingCancelOutsideWindow
  | IncomingUnknownTarget
  | IncomingControlTarget
  | IncomingRepeatedCancel
  deriving (Eq, Show)

newInboundState :: Word16 -> Word64 -> Either InboundError InboundState
newInboundState 0 _ = Left InvalidIncomingLimit
newInboundState d ceilingOrdinal =
  Right (InboundState (2 * fromIntegral d) ceilingOrdinal 0 Map.empty False)

incomingHistorySize :: InboundState -> Word32
incomingHistorySize = fromIntegral . Map.size . inboundRecent

-- | Validate a complete decoded client message before allocating server work.
-- The codec checks field widths; this enforces session-wide invariants:
--
-- * Each reply-bearing command uses the exact next ordinal, starting at zero.
--   Duplicates, gaps and attempts to continue after exhaustion are rejected.
-- * A cancel with ordinal @n@ targets an earlier data request @t@ satisfying
--   @t < n@ and @n - t < W@, where @W = 2D@. A control request or a data request
--   already canceled cannot be targeted.
-- * At most @W@ recent target classifications are retained, independently of
--   ordinal values. The local counter can represent exhaustion without wrapping.
--
-- Rejection leaves the input state unchanged. Reply commitment, FIFO sending
-- and active request/control resource limits belong to the server; sequence
-- validity alone does not bound the rate of otherwise valid commands.
checkIncomingMessage ::
  Message (LeiosFetchAtomic point eb tx) StIdle to -> InboundState -> Either InboundError InboundState
checkIncomingMessage message s = do
  when (inboundClosed s) (Left IncomingSessionClosed)
  case message of
    MsgDone -> Right s{inboundClosed = True}
    MsgLeiosBlockRequest ordinal _ -> advance ordinal (RecentData False) s
    MsgLeiosBlockTxsRequest ordinal _ _ -> advance ordinal (RecentData False) s
    MsgTryCancel ordinal@(WireOrdinal n) target@(WireOrdinal t) -> do
      checkOrdinal ordinal s
      unless
        (t < n && fromIntegral n - fromIntegral t < (fromIntegral (inboundWindow s) :: Natural))
        (Left IncomingCancelOutsideWindow)
      kind <- maybe (Left IncomingUnknownTarget) Right (Map.lookup target (inboundRecent s))
      case kind of
        RecentControl -> Left IncomingControlTarget
        RecentData True -> Left IncomingRepeatedCancel
        RecentData False ->
          advance
            ordinal
            RecentControl
            s
              { inboundRecent = Map.insert target (RecentData True) (inboundRecent s)
              }

checkOrdinal :: WireOrdinal -> InboundState -> Either InboundError ()
checkOrdinal (WireOrdinal n) s = do
  when (incomingNextOrdinal s > fromIntegral (inboundLastOrdinal s)) (Left IncomingOrdinalExhausted)
  unless (fromIntegral n == incomingNextOrdinal s) (Left UnexpectedIncomingOrdinal)

advance :: WireOrdinal -> RecentRequest -> InboundState -> Either InboundError InboundState
advance ordinal kind s = do
  checkOrdinal ordinal s
  let next = incomingNextOrdinal s + 1
      floorOrdinal = if next > fromIntegral (inboundWindow s) then next - fromIntegral (inboundWindow s) else 0
      recent =
        Map.dropWhileAntitone
          (\(WireOrdinal n) -> fromIntegral n < floorOrdinal)
          (Map.insert ordinal kind (inboundRecent s))
  Right s{incomingNextOrdinal = next, inboundRecent = recent}
