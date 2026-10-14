{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The upstream side of LeiosFetch.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( LeiosFetchRequestHandler (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeer
  ) where

import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core
  ( IsPipelined (..)
  , ReflRelativeAgency (..)
  )
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
  )

type LeiosFetchServerPeer point eb tx m a =
  Peer (LeiosFetch point eb tx) AsServer NonPipelined StIdle m a

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
  go = Await ReflClientAgency $ \case
    MsgDone -> Done ReflNobodyAgency ()
    request@MsgLeiosBlockRequest{} ->
      Effect $ (\response -> Yield ReflServerAgency response go) <$> answer request
    request@MsgLeiosBlockTxsRequest{} ->
      Effect $ (\response -> Yield ReflServerAgency response go) <$> answer request
