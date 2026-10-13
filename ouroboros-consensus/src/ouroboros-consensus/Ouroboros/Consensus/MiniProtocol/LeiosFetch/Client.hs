{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The downstream side of LeiosFetch.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeer
  , leiosFetchClientPeerIdle
  ) where

import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Data.Functor ((<&>))
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core
  ( IsPipelined (..)
  , ReflRelativeAgency (..)
  )
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (LeiosFetch (..), Message (..))

type LeiosFetchClientPeer point eb tx m a =
  Peer (LeiosFetch point eb tx) AsClient NonPipelined StIdle m a

-- | A client that requests nothing: it waits until told to stop, then sends
-- 'MsgDone'.
--
-- TODO Replace it with a client that requests the endorser blocks and
-- transactions its peer offers over LeiosNotify.
leiosFetchClientPeerIdle ::
  MonadSTM m =>
  -- | blocks until the client should stop
  STM m a ->
  LeiosFetchClientPeer point eb tx m a
leiosFetchClientPeerIdle awaitStop =
  Effect $
    atomically awaitStop <&> \x ->
      Yield ReflClientAgency MsgDone $
        Done ReflNobodyAgency x
