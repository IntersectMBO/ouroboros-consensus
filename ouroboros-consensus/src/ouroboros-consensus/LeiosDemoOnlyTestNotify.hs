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
{-# LANGUAGE TypeApplications #-}
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
  , LeiosNotifyServerPeerLookahead
  , leiosNotifyClientPeer
  , leiosNotifyClientPeerPipelined
  , leiosNotifyServerPeer
  , leiosNotifyServerPeerLookahead
  , WhetherExcessiveRequests (..)
  , toLeiosNotifyClientPeerPipelined
  , runLookaheadFixedSenderPeerWithLimits
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (join, replicateM)
-- for runLookaheadFixedSenderPeerWithLimits
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Tracer (Tracer (..))
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Network.Mux.Timeout (withTimeoutSerial)
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
  , SenderVariability (..)
  , StateAgency
  , natToInt
  )
import Network.TypedProtocol.Driver (runLookaheadFixedSenderPeerWithDriver)
import Network.TypedProtocol.Peer
  ( Peer (..)
  , PeerLookaheadFixedSender (..)
  , PeerPipelined (..)
  , Receiver (..)
  , Sender (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits (TraceSendRecv, driverWithLimits)
import Ouroboros.Network.Protocol.Limits
  ( BearerBytes
  , ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , longWait
  , smallByteLimit
  , waitForever
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Text.Printf (printf)

-----

leiosNotifyMiniProtocolNum :: Mux.MiniProtocolNum
leiosNotifyMiniProtocolNum = Mux.MiniProtocolNum 18

type LeiosNotify :: Type -> Type -> Type -> Type
data LeiosNotify point announcement vote where
  StIdle :: LeiosNotify point announcement vote
  StBusy :: LeiosNotify point announcement vote
  StQuit :: LeiosNotify point announcement vote
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
instance ShowProxy (StQuit :: LeiosNotify point announcement vote) where
  showProxy _ = "StQuit"
instance ShowProxy (StDone :: LeiosNotify point announcement vote) where
  showProxy _ = "StDone"

type SingLeiosNotify ::
  LeiosNotify point announcement vote ->
  Type
data SingLeiosNotify st where
  SingIdle :: SingLeiosNotify StIdle
  SingBusy :: SingLeiosNotify StBusy
  SingQuit :: SingLeiosNotify StQuit
  SingDone :: SingLeiosNotify StDone

deriving instance Show (SingLeiosNotify st)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI StBusy where stateToken = SingBusy
instance StateTokenI StQuit where stateToken = SingQuit
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
      -- The point alone would not say which announcement's size is being
      -- offered: two announcements, even from different elections, can name
      -- one endorser block in one slot at different sizes. Suppressing the
      -- contrasting announcement is not open to us either, since L_hdr is
      -- precisely the window in which an honest peer relays both, before it
      -- can know which size is right.
      --
      -- There would be two ways to drop it. Name the announcing block's RbHash,
      -- instead of or alongside the endorser block's hash: one announcing block
      -- is one announcement, hence one size. Or inflate the outstanding-bytes
      -- budget conservatively --- charge each request the codec limit rather
      -- than a claimed size --- which needs no size at all, at the cost of a
      -- budget denominated in the worst case.
      --
      -- TODO update the CIP/blueprint, which still has this message carrying
      -- the point alone. Both alternatives change the mini-protocol message or
      -- its accounting, so the choice belongs in that discussion.
      !Word32 ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosBlockTxsOffer ::
      !point ->
      -- the closure byte range [start, end) on offer; an end of maxBound runs
      -- to the end of the closure
      !Word32 ->
      !Word32 ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosVotes ::
      -- TODO: non-empty
      [vote] ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgDone ::
      Message (LeiosNotify point announcement vote) StQuit StDone
    MsgQuit ::
      Message (LeiosNotify point announcement vote) StIdle StQuit
    MsgCanceled ::
      Message (LeiosNotify point announcement vote) StBusy StIdle

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StQuit = ServerAgency
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
    MsgQuit -> ()
    MsgCanceled -> ()

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
  SingQuit -> smallByteLimit
  st@SingDone -> notActiveState st

timeLimitsLeiosNotify ::
  ProtocolTimeLimits (LeiosNotify point announcement vote)
timeLimitsLeiosNotify = ProtocolTimeLimits $ \case
  SingIdle -> waitForever
  SingBusy -> waitForever
  SingQuit -> longWait
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
    MsgDone ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 0
    MsgQuit ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 1
    MsgCanceled ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 2
    MsgLeiosNotificationRequestNext ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 3
    MsgLeiosBlockAnnouncement x ->
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 4
        <> encodeA x
    MsgLeiosBlockOffer p sz ->
      CBOR.encodeListLen 3
        <> CBOR.encodeWord 5
        <> encodeP p
        <> CBOR.encodeWord32 sz
    MsgLeiosBlockTxsOffer p start end ->
      CBOR.encodeListLen 4
        <> CBOR.encodeWord 6
        <> encodeP p
        <> CBOR.encodeWord32 start
        <> CBOR.encodeWord32 end
    MsgLeiosVotes vs ->
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 7
        <> encodeVotes
     where
      encodeVotes =
        CBOR.encodeListLen (fromIntegral $ length vs)
          <> foldMap encodeV vs

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
      (SingQuit, 1, 0) ->
        return $ SomeMessage MsgDone
      (SingIdle, 1, 1) ->
        return $ SomeMessage MsgQuit
      (SingBusy, 1, 2) ->
        return $ SomeMessage MsgCanceled
      (SingIdle, 1, 3) ->
        return $ SomeMessage MsgLeiosNotificationRequestNext
      (SingBusy, 2, 4) -> do
        x <- decodeA
        return $ SomeMessage $ MsgLeiosBlockAnnouncement x
      (SingBusy, 3, 5) -> do
        p <- decodeP
        sz <- CBOR.decodeWord32
        return $ SomeMessage $ MsgLeiosBlockOffer p sz
      (SingBusy, 4, 6) -> do
        p <- decodeP
        start <- CBOR.decodeWord32
        end <- CBOR.decodeWord32
        return $ SomeMessage $ MsgLeiosBlockTxsOffer p start end
      (SingBusy, 2, 7) -> do
        vs <- decodeVotes
        return $ SomeMessage $ MsgLeiosVotes vs
       where
        decodeVotes = do
          n <- CBOR.decodeListLen
          replicateM n decodeV
      (SingDone, _, _) -> notActiveState stok
      -- failures per protocol state
      (SingIdle, _, _) ->
        fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len
      (SingBusy, _, _) ->
        fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len
      (SingQuit, _, _) ->
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
      (SingQuit, Just (AnyMessage msg@MsgDone)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@MsgQuit)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBusy, Just (AnyMessage msg@MsgCanceled)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingDone, _) ->
        notActiveState stok
      (_, _) ->
        DecodeFail $ CodecFailure "codecLeiosNotifyId: no matching message"

-----

leiosNotifyClientPeer ::
  forall m announcement point vote a.
  Monad m =>
  -- | INVARIANT: this will only be 'MsgCanceled' if the peer sent that before
  -- we sent 'MsgQuit'
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
          Yield ReflClientAgency MsgQuit $
            Await ReflServerAgency $
              \MsgDone -> Done ReflNobodyAgency x
        Right k ->
          Yield ReflClientAgency MsgLeiosNotificationRequestNext $
            Await ReflServerAgency $ \msg -> case msg of
              MsgLeiosBlockAnnouncement{} -> react $ k msg
              MsgLeiosBlockOffer{} -> react $ k msg
              MsgLeiosBlockTxsOffer{} -> react $ k msg
              MsgLeiosVotes{} -> react $ k msg
              MsgCanceled{} -> react $ k msg

  react action = Effect $ fmap (\() -> go) action

-----

type LeiosNotifyServerPeer point announcement vote m a =
  Peer (LeiosNotify point announcement vote) AsServer NonPipelined StIdle m ()

type LeiosNotifyServerPeerLookahead point announcement vote m a =
  PeerLookaheadFixedSender (LeiosNotify point announcement vote) AsServer StIdle m ()

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
    MsgLeiosNotificationRequestNext -> Effect $ do
      msg <- handler
      pure $
        Yield ReflServerAgency msg $
          go
    MsgQuit -> Yield ReflServerAgency MsgDone $ Done ReflNobodyAgency ()

-----

-- | Merely an abbreviation local to this module
type X point announcement vote st m a n =
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined n C) st m a

type LeiosNotifyClientPeerPipelined point announcement vote m a =
  PeerPipelined (LeiosNotify point announcement vote) AsClient StIdle m a

toLeiosNotifyClientPeerPipelined ::
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a ->
  LeiosNotifyClientPeerPipelined point announcement vote m a
toLeiosNotifyClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining

-- | Whether incrementing the count of outstanding LeiosNotify requests found the
-- peer already at its pipelining bound, i.e. requesting more than it is allowed.
data WhetherExcessiveRequests = ExcessiveRequests | NotExcessiveRequests

leiosNotifyClientPeerPipelined ::
  forall m point announcement vote a.
  (PrimMonad m, MonadSTM m) =>
  -- | either the return value or else the current max pipelining depth
  STM m (Either a Int) ->
  -- | INVARIANT: this will only be 'MsgCanceled' if the peer sent that before
  -- we sent 'MsgQuit'
  m (Message (LeiosNotify point announcement vote) StBusy StIdle -> m ()) ->
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a
leiosNotifyClientPeerPipelined checkDone k0 =
  Effect $ do
    stop <- Prim.newMutVar NotYetDraining
    arrived <- newTVarIO 0
    pure $ go stop arrived Zero
 where
  go ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Nat n ->
    X point announcement vote StIdle m a n
  go stop arrived !n =
    Effect $
      join @m $
        atomically $
          checkDone >>= \case
            Left x -> pure @(STM m) $ do
              Prim.writeMutVar stop AlreadyDraining
              pure @m $ case n of
                Zero ->
                  Yield ReflClientAgency MsgQuit $
                    Await ReflServerAgency $ \MsgDone ->
                      Done ReflNobodyAgency x
                Succ _ ->
                  YieldPipelined
                    ReflClientAgency
                    MsgQuit
                    (ReceiverDone MkC) -- note that this Receiver doesn't await the MsgDone response
                    $ drainThePipe x (Succ n)
            Right maxDepth ->
              case n of
                Zero -> pure @(STM m) $ pure @m $ sendAnother stop arrived n
                Succ p
                  | natToInt n < maxDepth ->
                      pure @(STM m) $ pure @m $ Collect (Just $ sendAnother stop arrived n) (collectOne p)
                  | otherwise -> do
                      readTVar arrived >>= check . (> 0)
                      -- We can only call Collect if it would only block
                      -- ephemerally. That way we're always able to send MsgQuit as
                      -- soon as the Diffusion Layer commands us to.
                      --
                      -- This STM noise should be upstreamed into a CollectSTM that
                      -- runs handles whichever happens first: the continuation
                      -- becomes known or a pipelined request's reply arrives.
                      pure @(STM m) $ pure @m $ Collect Nothing (collectOne p)
   where
    collectOne :: Nat p -> C -> X point announcement vote StIdle m a p
    collectOne p MkC = Effect $ do
      atomically $ modifyTVar arrived (subtract 1)
      pure $ go stop arrived p

  sendAnother ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Nat n ->
    X point announcement vote StIdle m a n
  sendAnother stop arrived !n =
    YieldPipelined
      ReflClientAgency
      MsgLeiosNotificationRequestNext
      (receiver stop arrived)
      (go stop arrived $ Succ n)

  receiver ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Receiver (LeiosNotify point announcement vote) AsClient StBusy StIdle m C
  receiver stop arrived =
    ReceiverAwait ReflServerAgency $ \msg -> case msg of
      MsgLeiosBlockAnnouncement{} -> handler stop arrived k0 msg
      MsgLeiosBlockOffer{} -> handler stop arrived k0 msg
      MsgLeiosBlockTxsOffer{} -> handler stop arrived k0 msg
      MsgLeiosVotes{} -> handler stop arrived k0 msg
      MsgCanceled{} -> handler stop arrived k0 msg

  handler ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    m (msg -> m ()) ->
    msg ->
    Receiver (LeiosNotify point announcement vote) AsClient StIdle StIdle m C
  handler stop arrived k x = ReceiverEffect $ do
    Prim.readMutVar stop >>= \case
      AlreadyDraining -> pure ()
      NotYetDraining -> k >>= ($ x)
    atomically $ modifyTVar arrived (+ 1)
    pure $ ReceiverDone MkC

  drainThePipe :: a -> Nat n -> X point announcement vote StQuit m a n
  drainThePipe x = \case
    Zero ->
      Await ReflServerAgency $ \MsgDone ->
        Done ReflNobodyAgency x
    Succ m ->
      Collect
        Nothing -- OK to block, since we're entirely passive now
        (\MkC -> drainThePipe x m)

leiosNotifyServerPeerLookahead ::
  forall m point announcement vote.
  (MonadThrow m, MonadSTM m) =>
  m WhetherExcessiveRequests ->
  -- | blocks until the next reply (announcement\/offer\/vote) is ready
  STM m (Message (LeiosNotify point announcement vote) StBusy StIdle) ->
  m (PeerLookaheadFixedSender (LeiosNotify point announcement vote) AsServer StIdle m ())
leiosNotifyServerPeerLookahead incr next = do
  quitVar <- newTVarIO False
  pure $ PeerLookaheadFixedSender (responder quitVar) (start quitVar)
 where
  responder ::
    StrictTVar m Bool ->
    Sender (LeiosNotify point announcement vote) AsServer VariableSender StBusy StIdle m
  responder quitVar =
    SenderEffect $
      atomically $
        orElse
          (do readTVar quitVar >>= check; pure $ SenderYield ReflServerAgency MsgCanceled SenderDone)
          (next <&> \msg -> SenderYield ReflServerAgency msg SenderDone)

  -- StIdle with nothing outstanding: receive the first request (or quit). A
  -- plain 'Await' is required here; 'AwaitLookahead' defers the StBusy->StIdle
  -- send, so it is only usable once we hold a request (i.e. are at StBusy).
  start ::
    StrictTVar m Bool ->
    Peer
      (LeiosNotify point announcement vote)
      AsServer
      (Lookahead Z (FixedSender StBusy StIdle))
      StIdle
      m
      ()
  start quitVar =
    Await ReflClientAgency $ \case
      MsgQuit -> Effect $ do
        atomically $ writeTVar quitVar True
        pure $ Yield ReflServerAgency MsgDone $ Done ReflNobodyAgency ()
      MsgLeiosNotificationRequestNext ->
        Effect $ do
          incr >>= \case
            ExcessiveRequests -> throwIO MkExnLeiosNotifyExcessiveRequests
            NotExcessiveRequests -> pure ()
          pure $ busy quitVar Zero

  -- StBusy with @n@ deferred sends outstanding: hand this reply off to the
  -- responder and look ahead to the next request (or done) in one step.
  busy ::
    forall n.
    StrictTVar m Bool ->
    Nat n ->
    Peer
      (LeiosNotify point announcement vote)
      AsServer
      (Lookahead n (FixedSender StBusy StIdle))
      StBusy
      m
      ()
  busy quitVar n =
    AwaitLookahead ReflClientAgency TheSender $ \case
      MsgQuit -> Effect $ do
        atomically (writeTVar quitVar True)
        pure $ drain (Succ n)
      MsgLeiosNotificationRequestNext ->
        Effect $ do
          incr >>= \case
            ExcessiveRequests -> throwIO MkExnLeiosNotifyExcessiveRequests
            NotExcessiveRequests -> pure ()
          pure $ busy quitVar (Succ n)

  -- on termination, flush the sends we've spawned, then send MsgDone.
  --
  -- Those sends are unblocked because the MsgQuit handler which lead us to this
  -- already set the quitVar flag.
  drain ::
    forall n.
    Nat n ->
    Peer
      (LeiosNotify point announcement vote)
      AsServer
      (Lookahead n (FixedSender StBusy StIdle))
      StQuit
      m
      ()
  drain = \case
    Zero -> Yield ReflServerAgency MsgDone $ Done ReflNobodyAgency ()
    Succ j -> FlushSender Nothing (drain j)

data ExnLeiosNotifyExcessiveRequests = MkExnLeiosNotifyExcessiveRequests
  deriving Show

instance Exception ExnLeiosNotifyExcessiveRequests

-----

-- | Run a lookahead (fixed-sender) peer with the given channel via the given codec.
--
-- The lookahead dual of 'runPipelinedPeerWithLimits': the peer receives ahead
-- and its sends are performed by a parallel thread, hence the 'MonadAsync'
-- constraint.
--
-- TODO: upstream this to ouroboros-network
runLookaheadFixedSenderPeerWithLimits ::
  forall ps (st :: ps) pr failure bytes m a.
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
  ) =>
  Tracer m (TraceSendRecv ps) ->
  Codec ps failure m bytes ->
  ProtocolSizeLimits ps bytes ->
  ProtocolTimeLimits ps ->
  Channel m bytes ->
  PeerLookaheadFixedSender ps pr st m a ->
  m (a, Maybe bytes)
runLookaheadFixedSenderPeerWithLimits tracer codec slimits tlimits channel peer =
  withTimeoutSerial $ \timeoutFn ->
    let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
     in runLookaheadFixedSenderPeerWithDriver driver peer
