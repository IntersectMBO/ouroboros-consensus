{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The legacy upstream side of LeiosFetch, plus shared request errors. The
-- cancellation-capable server lives in 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server'.
-- Rationale for the new size errors: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( ExnLeiosInvalidRequest (..)
  , LeiosFetchRequestHandler (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeer
  ) where

import Control.Monad.Class.MonadThrow (Exception)
import Data.Word (Word32, Word64)
import Network.TypedProtocol.Peer.Server
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
  )

type LeiosFetchServerPeer point eb tx m a =
  Server (LeiosFetch point eb tx) NonPipelined StIdle m a

-- | How a server answers each request.
newtype LeiosFetchRequestHandler point eb tx m
  = MkLeiosFetchRequestHandler
      ( forall st.
        Message (LeiosFetch point eb tx) StIdle (StBusy st) ->
        m (Message (LeiosFetch point eb tx) (StBusy st) StIdle)
      )

-- | A server that answers each request with the handler, and ends when the
-- client sends 'MsgDone'.
leiosFetchServerPeer ::
  forall point eb tx m.
  Functor m =>
  LeiosFetchRequestHandler point eb tx m ->
  LeiosFetchServerPeer point eb tx m ()
leiosFetchServerPeer (MkLeiosFetchRequestHandler answer) = go
 where
  go :: LeiosFetchServerPeer point eb tx m ()
  go = Await $ \case
    MsgDone -> Done ()
    request@MsgLeiosBlockRequest{} ->
      Effect $ (\response -> Yield response go) <$> answer request
    request@MsgLeiosBlockTxsRequest{} ->
      Effect $ (\response -> Yield response go) <$> answer request

-- | Thrown by the node's LeiosFetch server, which disconnects a client that
-- requests what the node does not hold.
data ExnLeiosInvalidRequest
  = -- | An endorser block whose body the node does not hold.
    ExnLeiosUnknownBlockRequested !LeiosPoint
  | -- | Offsets into an endorser block that it does not have, or whose
    -- transactions the node does not hold.
    ExnLeiosUnknownTxsRequested !LeiosPoint ![Int]
  | -- | The requested transactions exceed the permitted payload-byte sum.
    ExnLeiosTxsRequestTooLarge !LeiosPoint !Word64 !Word32
  | -- | A requested body's or batch's encoded response exceeds the local
    -- server ceiling. Check this before reading the transaction payloads.
    ExnLeiosResponseTooLarge !LeiosPoint !Word64 !Word32
  deriving Show

instance Exception ExnLeiosInvalidRequest
