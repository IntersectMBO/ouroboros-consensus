{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | The upstream side of LeiosFetch.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( ExnLeiosFetchServerUnimplemented (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeerRefusing
  ) where

import Control.Monad.Class.MonadThrow (Exception, MonadThrow (..))
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core
  ( IsPipelined (..)
  , ReflRelativeAgency (..)
  )
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (LeiosFetch (..), Message (..))

type LeiosFetchServerPeer point eb tx m a =
  Peer (LeiosFetch point eb tx) AsServer NonPipelined StIdle m a

-- | Thrown by 'leiosFetchServerPeerRefusing' on any request.
data ExnLeiosFetchServerUnimplemented = MkExnLeiosFetchServerUnimplemented
  deriving Show

instance Exception ExnLeiosFetchServerUnimplemented

-- | A server that serves nothing: it disconnects a client that sends any
-- request, and ends when the client sends 'MsgDone'.
--
-- TODO Replace it with a server that serves endorser blocks and their
-- transactions from the LeiosDB.
leiosFetchServerPeerRefusing ::
  MonadThrow m =>
  LeiosFetchServerPeer point eb tx m ()
leiosFetchServerPeerRefusing =
  Await ReflClientAgency $ \case
    MsgDone -> Done ReflNobodyAgency ()
    MsgLeiosBlockRequest{} -> Effect $ throwIO MkExnLeiosFetchServerUnimplemented
    MsgLeiosBlockTxsRequest{} -> Effect $ throwIO MkExnLeiosFetchServerUnimplemented
