{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LeiosDemoOnlyTestNotify
  ( LeiosNotify (..)
  , Message (..)
  , SingLeiosNotify (..)
  , leiosNotifyMiniProtocolNum
  , byteLimitsLeiosNotify
  , codecLeiosNotify
  , codecLeiosNotifyId
  , timeLimitsLeiosNotify
  , LeiosNotifyClientPeerPipelined
  , LeiosNotifyServerPeer
  , LeiosNotifyServerPeerAntiPipelined
  , leiosNotifyClientPeer
  , leiosNotifyClientPeerPipelined
  , leiosNotifyServerPeer
  , leiosNotifyServerPeerAntiPipelined
  , toLeiosNotifyClientPeerPipelined

  , runAntiPipelinedPeerWithLimits
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import qualified Network.Mux.Types as Mux
import Network.TypedProtocol.Codec.CBOR
  ( ActiveState
  , AnyMessage (..)
  , Codec
  , CodecF (..)
  , CodecFailure (..)
  , DecodeStep (..)
  , PeerRole (..)
  , SomeMessage (..)
  , StateTokenI (..)
  , mkCodecCborLazyBS
  , notActiveState
  )
import Network.TypedProtocol.Core
  ( Agency (..)
  , IsPipelined (..)
  , Message
  , N (..)
  , Nat (..)
  , Protocol (..)
  , ReflRelativeAgency (..)
  , StateAgency
  , natToInt
  )
import Network.TypedProtocol.Peer
  ( Peer (..)
  , PeerAntiPipelined (..)
  , PeerPipelined (..)
  , Receiver (..)
  , Sender (..)
  )
import Ouroboros.Network.Protocol.Limits
  ( BearerBytes
  , ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , smallByteLimit
  , waitForever
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Text.Printf (printf)

-- for runAntiPipelinedPeerWithLimits
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..))
import Network.Mux.Timeout (withTimeoutSerial)
import Network.TypedProtocol.Driver (runAntiPipelinedPeerWithDriver)
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits (TraceSendRecv, driverWithLimits)

-----

leiosNotifyMiniProtocolNum :: Mux.MiniProtocolNum
leiosNotifyMiniProtocolNum = Mux.MiniProtocolNum 18

type LeiosNotify :: Type -> Type -> Type -> Type
data LeiosNotify point announcement vote where
  StIdle :: LeiosNotify point announcement vote
  StBusy :: LeiosNotify point announcement vote
  StDone :: LeiosNotify point announcement vote

instance
  ( ShowProxy point
  , ShowProxy announcement
  , ShowProxy vote
  ) =>
  ShowProxy (LeiosNotify point announcement vote)
  where
  showProxy _ =
    concat
      [ "LeiosNotify "
      , showProxy (Proxy :: Proxy point)
      , " "
      , showProxy (Proxy :: Proxy announcement)
      , " "
      , showProxy (Proxy :: Proxy vote)
      ]

instance ShowProxy (StIdle :: LeiosNotify point announcement vote) where
  showProxy _ = "StIdle"
instance ShowProxy (StBusy :: LeiosNotify point announcement vote) where
  showProxy _ = "StBusy"
instance ShowProxy (StDone :: LeiosNotify point announcement vote) where
  showProxy _ = "StDone"

type SingLeiosNotify ::
  LeiosNotify point announcement vote ->
  Type
data SingLeiosNotify st where
  SingIdle :: SingLeiosNotify StIdle
  SingBusy :: SingLeiosNotify StBusy
  SingDone :: SingLeiosNotify StDone

deriving instance Show (SingLeiosNotify st)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI StBusy where stateToken = SingBusy
instance StateTokenI StDone where stateToken = SingDone

-----

instance Protocol (LeiosNotify point announcement vote) where
  data Message (LeiosNotify point announcement vote) from to where
    MsgLeiosNotificationRequestNext ::
      Message (LeiosNotify point announcement vote) StIdle StBusy
    MsgLeiosBlockAnnouncement ::
      !announcement ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosBlockOffer ::
      !point ->
      !Word32 -> -- TODO this size should be redundant, determined by the announcement
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosBlockTxsOffer ::
      !point ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosVotes ::
      -- TODO: non-empty
      [vote] ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgDone ::
      Message (LeiosNotify point announcement vote) StIdle StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingLeiosNotify

instance NFData (Message (LeiosNotify point announcement vote) from to) where
  rnf = \case
    MsgLeiosNotificationRequestNext -> ()
    MsgLeiosBlockAnnouncement{} -> ()
    MsgLeiosBlockOffer{} -> ()
    MsgLeiosBlockTxsOffer{} -> ()
    MsgLeiosVotes{} -> ()
    MsgDone -> ()

deriving instance
  (Eq point, Eq announcement, Eq vote) =>
  Eq (Message (LeiosNotify point announcement vote) from to)

deriving instance
  (Show point, Show announcement, Show vote) =>
  Show (Message (LeiosNotify point announcement vote) from to)

-----

byteLimitsLeiosNotify ::
  ProtocolSizeLimits (LeiosNotify point announcement vote) bytes
byteLimitsLeiosNotify = ProtocolSizeLimits $ \case
  SingIdle -> smallByteLimit
  SingBusy -> smallByteLimit
  st@SingDone -> notActiveState st

timeLimitsLeiosNotify ::
  ProtocolTimeLimits (LeiosNotify point announcement vote)
timeLimitsLeiosNotify = ProtocolTimeLimits $ \case
  SingIdle -> waitForever
  SingBusy -> waitForever
  st@SingDone -> notActiveState st

-----

codecLeiosNotify ::
  forall point announcement vote m.
  MonadST m =>
  (point -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s point) ->
  (announcement -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s announcement) ->
  (vote -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s vote) ->
  Codec (LeiosNotify point announcement vote) CBOR.DeserialiseFailure m ByteString
codecLeiosNotify encodeP decodeP encodeA decodeA encodeV decodeV =
  mkCodecCborLazyBS
    (encodeLeiosNotify encodeP encodeA encodeV)
    decode
 where
  decode ::
    forall (st :: LeiosNotify point announcement vote).
    ActiveState st =>
    StateToken st ->
    forall s.
    CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    decodeLeiosNotify decodeP decodeA decodeV stok len key

encodeLeiosNotify ::
  forall
    point
    announcement
    vote
    (st :: LeiosNotify point announcement vote)
    (st' :: LeiosNotify point announcement vote).
  (point -> CBOR.Encoding) ->
  (announcement -> CBOR.Encoding) ->
  (vote -> CBOR.Encoding) ->
  Message (LeiosNotify point announcement vote) st st' ->
  CBOR.Encoding
encodeLeiosNotify encodeP encodeA encodeV = encode
 where
  encode ::
    forall st0 st1.
    Message (LeiosNotify point announcement vote) st0 st1 ->
    CBOR.Encoding
  encode = \case
    MsgLeiosNotificationRequestNext ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 0
    MsgLeiosBlockAnnouncement x ->
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 1
        <> encodeA x
    MsgLeiosBlockOffer p sz ->
      CBOR.encodeListLen 3
        <> CBOR.encodeWord 2
        <> encodeP p
        <> CBOR.encodeWord32 sz
    MsgLeiosBlockTxsOffer p ->
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 3
        <> encodeP p
    MsgLeiosVotes vs ->
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 4
        <> encodeVotes
     where
      encodeVotes =
        CBOR.encodeListLen (fromIntegral $ length vs)
          <> foldMap encodeV vs
    MsgDone ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 5

decodeLeiosNotify ::
  forall
    point
    announcement
    vote
    (st :: LeiosNotify point announcement vote)
    s.
  ActiveState st =>
  (forall s'. CBOR.Decoder s' point) ->
  (forall s'. CBOR.Decoder s' announcement) ->
  (forall s'. CBOR.Decoder s' vote) ->
  StateToken st ->
  Int ->
  Word ->
  CBOR.Decoder s (SomeMessage st)
decodeLeiosNotify decodeP decodeA decodeV = decode
 where
  decode ::
    forall (st' :: LeiosNotify point announcement vote).
    ActiveState st' =>
    StateToken st' ->
    Int ->
    Word ->
    CBOR.Decoder s (SomeMessage st')
  decode stok len key = do
    case (stok, len, key) of
      (SingIdle, 1, 0) ->
        return $ SomeMessage MsgLeiosNotificationRequestNext
      (SingBusy, 2, 1) -> do
        x <- decodeA
        return $ SomeMessage $ MsgLeiosBlockAnnouncement x
      (SingBusy, 3, 2) -> do
        p <- decodeP
        sz <- CBOR.decodeWord32
        return $ SomeMessage $ MsgLeiosBlockOffer p sz
      (SingBusy, 2, 3) -> do
        p <- decodeP
        return $ SomeMessage $ MsgLeiosBlockTxsOffer p
      (SingBusy, 2, 4) -> do
        vs <- decodeVotes
        return $ SomeMessage $ MsgLeiosVotes vs
       where
        decodeVotes = do
          n <- CBOR.decodeListLen
          replicateM n decodeV
      (SingIdle, 1, 5) ->
        return $ SomeMessage MsgDone
      (SingDone, _, _) -> notActiveState stok
      -- failures per protocol state
      (SingIdle, _, _) ->
        fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len
      (SingBusy, _, _) ->
        fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len

codecLeiosNotifyId ::
  forall point announcement vote m.
  Monad m =>
  Codec
    (LeiosNotify point announcement vote)
    CodecFailure
    m
    (AnyMessage (LeiosNotify point announcement vote))
codecLeiosNotifyId = Codec{encode, decode}
 where
  encode ::
    forall st st'.
    ( ActiveState st
    , StateTokenI st
    ) =>
    Message (LeiosNotify point announcement vote) st st' ->
    AnyMessage (LeiosNotify point announcement vote)
  encode = AnyMessage

  decode ::
    forall (st :: LeiosNotify point announcement vote).
    ActiveState st =>
    StateToken st ->
    m
      ( DecodeStep
          (AnyMessage (LeiosNotify point announcement vote))
          CodecFailure
          m
          (SomeMessage st)
      )
  decode stok = return $ DecodePartial $ \bytes ->
    return $ case (stok, bytes) of
      (SingIdle, Just (AnyMessage msg@MsgLeiosNotificationRequestNext)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBusy, Just (AnyMessage msg@MsgLeiosBlockAnnouncement{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBusy, Just (AnyMessage msg@MsgLeiosBlockOffer{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBusy, Just (AnyMessage msg@MsgLeiosBlockTxsOffer{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBusy, Just (AnyMessage msg@MsgLeiosVotes{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@MsgDone)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingDone, _) ->
        notActiveState stok
      (_, _) ->
        DecodeFail $ CodecFailure "codecLeiosNotifyId: no matching message"

-----

leiosNotifyClientPeer ::
  forall m announcement point vote a.
  Monad m =>
  m (Either a (Message (LeiosNotify point announcement vote) StBusy StIdle -> m ())) ->
  Peer (LeiosNotify point announcement vote) AsClient NonPipelined StIdle m a
leiosNotifyClientPeer checkDone =
  go
 where
  go :: Peer (LeiosNotify point announcement vote) AsClient NonPipelined StIdle m a
  go =
    Effect $
      checkDone <&> \case
        Left x ->
          Yield ReflClientAgency MsgDone $
            Done ReflNobodyAgency x
        Right k ->
          Yield ReflClientAgency MsgLeiosNotificationRequestNext $
            Await ReflServerAgency $ \msg -> case msg of
              MsgLeiosBlockAnnouncement{} -> react $ k msg
              MsgLeiosBlockOffer{} -> react $ k msg
              MsgLeiosBlockTxsOffer{} -> react $ k msg
              MsgLeiosVotes{} -> react $ k msg

  react action = Effect $ fmap (\() -> go) action

-----

type LeiosNotifyServerPeer point announcement vote m a =
  Peer (LeiosNotify point announcement vote) AsServer NonPipelined StIdle m ()

type LeiosNotifyServerPeerAntiPipelined point announcement vote m a =
  PeerAntiPipelined (LeiosNotify point announcement vote) AsServer StIdle m ()

leiosNotifyServerPeer ::
  forall m point announcement vote.
  Monad m =>
  m (Message (LeiosNotify point announcement vote) StBusy StIdle) ->
  Peer (LeiosNotify point announcement vote) AsServer NonPipelined StIdle m ()
leiosNotifyServerPeer handler =
  go
 where
  go :: Peer (LeiosNotify point announcement vote) AsServer NonPipelined StIdle m ()
  go = Await ReflClientAgency $ \case
    MsgDone -> Done ReflNobodyAgency ()
    MsgLeiosNotificationRequestNext -> Effect $ do
      msg <- handler
      pure $
        Yield ReflServerAgency msg $
          go

-----

-- | Merely an abbreviation local to this module
type X point announcement vote m a n =
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined n C) StIdle m a

type LeiosNotifyClientPeerPipelined point announcement vote m a =
  PeerPipelined (LeiosNotify point announcement vote) AsClient StIdle m a

toLeiosNotifyClientPeerPipelined ::
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a ->
  LeiosNotifyClientPeerPipelined point announcement vote m a
toLeiosNotifyClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining

leiosNotifyClientPeerPipelined ::
  forall m point announcement vote a.
  PrimMonad m =>
  -- | either the return value or else the current max pipelining depth
  m (Either a Int) ->
  m (Message (LeiosNotify point announcement vote) StBusy StIdle -> m ()) ->
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a
leiosNotifyClientPeerPipelined checkDone k0 =
  Effect $ do
    stop <- Prim.newMutVar NotYetDraining
    pure $ go stop Zero
 where
  go :: MutVar (PrimState m) WhetherDraining -> Nat n -> X point announcement vote m a n
  go stop !n =
    Effect $
      checkDone <&> \case
        Left x -> Effect $ do
          Prim.writeMutVar stop AlreadyDraining
          pure $ drainThePipe x n
        Right maxDepth ->
          case n of
            Zero -> sendAnother stop n
            Succ m ->
              Collect
                (if natToInt n >= maxDepth then Nothing else Just $ sendAnother stop n)
                (\MkC -> go stop m)

  sendAnother :: MutVar (PrimState m) WhetherDraining -> Nat n -> X point announcement vote m a n
  sendAnother stop !n =
    YieldPipelined
      ReflClientAgency
      MsgLeiosNotificationRequestNext
      (receiver stop)
      (go stop $ Succ n)

  receiver ::
    MutVar (PrimState m) WhetherDraining ->
    Receiver (LeiosNotify point announcement vote) AsClient StBusy StIdle m C
  receiver stop =
    ReceiverAwait ReflServerAgency $ \msg -> case msg of
      MsgLeiosBlockAnnouncement{} -> handler stop k0 msg
      MsgLeiosBlockOffer{} -> handler stop k0 msg
      MsgLeiosBlockTxsOffer{} -> handler stop k0 msg
      MsgLeiosVotes{} -> handler stop k0 msg

  handler ::
    MutVar (PrimState m) WhetherDraining ->
    m (msg -> m ()) ->
    msg ->
    Receiver (LeiosNotify point announcement vote) AsClient StIdle StIdle m C
  handler stop k x = ReceiverEffect $ do
    Prim.readMutVar stop >>= \case
      AlreadyDraining -> pure ()
      NotYetDraining -> k >>= ($ x)
    pure $ ReceiverDone MkC

  drainThePipe :: a -> Nat n -> X point announcement vote m a n
  drainThePipe x = \case
    Zero ->
      Yield ReflClientAgency MsgDone $
        Done ReflNobodyAgency x
    Succ m ->
      Collect
        Nothing
        (\MkC -> drainThePipe x m)

leiosNotifyServerPeerAntiPipelined ::
  forall m point announcement vote.
  Monad m =>
  m () ->
  -- ^ increments the number of outstanding requests
  m (Message (LeiosNotify point announcement vote) StBusy StIdle) ->
  -- ^ blocks until the next reply (announcement\/offer\/vote) is ready
  PeerAntiPipelined (LeiosNotify point announcement vote) AsServer StIdle m ()
leiosNotifyServerPeerAntiPipelined incr next =
    PeerAntiPipelined (go Zero)
  where
    responder :: Sender (LeiosNotify point announcement vote) AsServer StBusy StIdle m
    responder = SenderEffect $ next <&> \msg -> SenderYield ReflServerAgency msg SenderDone

    go :: forall n.
      Nat n ->
      Peer (LeiosNotify point announcement vote) AsServer (AntiPipelined n) StIdle m ()
    go n =
      Await ReflClientAgency $ \case
        MsgDone                         -> drain n
        MsgLeiosNotificationRequestNext ->
          Effect $ do
            incr
            pure $ YieldAntiPipelined ReflServerAgency responder (go (Succ n))

    -- on termination, flush the sends we've handed off, then Done.
    drain :: forall k.
      Nat k ->
      Peer (LeiosNotify point announcement vote) AsServer (AntiPipelined k) StDone m ()
    drain = \case
      Zero   -> Done ReflNobodyAgency ()
      Succ j -> AntiCollect (drain j) Nothing

-----

-- | Run an anti-pipelined peer with the given channel via the given codec.
--
-- The anti-pipelined dual of 'runPipelinedPeerWithLimits': the peer receives
-- ahead and its sends are performed by a parallel thread, hence the
-- 'MonadAsync' constraint.
--
-- TODO: upstream this to ouroboros-network
runAntiPipelinedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , BearerBytes bytes
     , NFData a
     , NFData failure
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerAntiPipelined ps pr st m a
  -> m (a, Maybe bytes)
runAntiPipelinedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runAntiPipelinedPeerWithDriver driver peer
