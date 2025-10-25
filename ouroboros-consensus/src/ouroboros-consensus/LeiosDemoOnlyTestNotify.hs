{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LeiosDemoOnlyTestNotify
  ( LeiosNotify (..)
  , SingLeiosNotify (..)
  , Message (..)
  , leiosNotifyMiniProtocolNum
  -- *
  , byteLimitsLeiosNotify
  , timeLimitsLeiosNotify
  , codecLeiosNotify
  , codecLeiosNotifyId
  -- *
  , LeiosNotifyClientPeerPipelined
  , LeiosNotifyServerPeer
  , leiosNotifyClientPeer
  , leiosNotifyClientPeerPipelined
  , leiosNotifyServerPeer
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.Concurrent.Class.MonadMVar (MVar, MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.DeepSeq (NFData (..))
import           Control.Monad (when)
import           Control.Monad.Class.MonadST
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.Singletons
import           Data.Word (Word32)
import qualified Network.Mux.Types as Mux
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import           Text.Printf


-----

leiosNotifyMiniProtocolNum :: Mux.MiniProtocolNum
leiosNotifyMiniProtocolNum = Mux.MiniProtocolNum 18

type LeiosNotify :: Type -> Type -> Type
data LeiosNotify point announcement where
  StIdle :: LeiosNotify point announcement
  StBusy :: LeiosNotify point announcement
  StDone :: LeiosNotify point announcement

instance ( ShowProxy point
         , ShowProxy announcement
         )
      => ShowProxy (LeiosNotify point announcement) where
  showProxy _ =
    concat
      [ "LeiosNotify ",
        showProxy (Proxy :: Proxy point),
        " ",
        showProxy (Proxy :: Proxy announcement)
      ]

instance ShowProxy (StIdle :: LeiosNotify point announcement) where
  showProxy _ = "StIdle"
instance ShowProxy (StBusy :: LeiosNotify point announcement) where
  showProxy _ = "StBusy"
instance ShowProxy (StDone :: LeiosNotify point announcement) where
  showProxy _ = "StDone"

type SingLeiosNotify
  :: LeiosNotify point announcement
  -> Type
data SingLeiosNotify st where
  SingIdle :: SingLeiosNotify StIdle
  SingBusy :: SingLeiosNotify StBusy
  SingDone :: SingLeiosNotify StDone

deriving instance Show (SingLeiosNotify st)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI StBusy where stateToken = SingBusy
instance StateTokenI StDone where stateToken = SingDone

-----

instance Protocol (LeiosNotify point announcement) where
  data Message (LeiosNotify point announcement) from to where
    MsgLeiosNotificationRequestNext
      :: Message (LeiosNotify point announcement) StIdle StBusy

    MsgLeiosBlockAnnouncement
      :: !announcement
      -> Message (LeiosNotify point announcement) StBusy StIdle
    MsgLeiosBlockOffer
      :: !point
      -> !Word32   -- TODO this size should be redundant, determined by the announcement
      -> Message (LeiosNotify point announcement) StBusy StIdle
    MsgLeiosBlockTxsOffer
      :: !point
      -> Message (LeiosNotify point announcement) StBusy StIdle
    -- votes offer

    MsgDone
      :: Message (LeiosNotify point announcement) StIdle StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingLeiosNotify

instance NFData (Message (LeiosNotify point announcement) from to) where
    rnf = \case
        MsgLeiosNotificationRequestNext -> ()
        MsgLeiosBlockAnnouncement{} -> ()
        MsgLeiosBlockOffer{} -> ()
        MsgLeiosBlockTxsOffer{} -> ()
        -- votes offer
        MsgDone -> ()

deriving instance (Eq point, Eq announcement)
               => Eq (Message (LeiosNotify point announcement) from to)

deriving instance (Show point, Show announcement)
               => Show (Message (LeiosNotify point announcement) from to)

-----

byteLimitsLeiosNotify
  :: (bytes -> Word) -> ProtocolSizeLimits (LeiosNotify point announcement) bytes
byteLimitsLeiosNotify = ProtocolSizeLimits $ \case
  SingIdle    -> smallByteLimit
  SingBusy    -> smallByteLimit
  st@SingDone -> notActiveState st

timeLimitsLeiosNotify
  :: ProtocolTimeLimits (LeiosNotify point announcement)
timeLimitsLeiosNotify = ProtocolTimeLimits $ \case
  SingIdle    -> waitForever
  SingBusy    -> waitForever
  st@SingDone -> notActiveState st

-----

codecLeiosNotify
  :: forall (point :: Type) (announcement :: Type) m.
     (MonadST m)
  => (point -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s point)
  -> (announcement -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s announcement)
  -> Codec (LeiosNotify point announcement) CBOR.DeserialiseFailure m ByteString
codecLeiosNotify encodeP decodeP encodeA decodeA =
  mkCodecCborLazyBS
    (encodeLeiosNotify encodeP encodeA)
    decode
    where
      decode
        :: forall (st :: LeiosNotify point announcement).
           (ActiveState st)
        => StateToken st
        -> forall s. CBOR.Decoder s (SomeMessage st)
      decode stok = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        decodeLeiosNotify decodeP decodeA stok len key

encodeLeiosNotify
  :: forall (point :: Type) (announcement :: Type)
            (st  :: LeiosNotify point announcement)
            (st' :: LeiosNotify point announcement).
     (point -> CBOR.Encoding)
  -> (announcement -> CBOR.Encoding)
  -> Message (LeiosNotify point announcement) st st'
  -> CBOR.Encoding
encodeLeiosNotify encodeP encodeA = encode
  where
    encode
      :: forall st0 st1.
         Message (LeiosNotify point announcement) st0 st1
      -> CBOR.Encoding
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
        -- votes offer
      MsgDone ->
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 5

decodeLeiosNotify
  :: forall (point :: Type) (announcement :: Type)
            (st :: LeiosNotify point announcement) s.
     (ActiveState st)
  => (forall s'. CBOR.Decoder s' point)
  -> (forall s'. CBOR.Decoder s' announcement)
  -> StateToken st
  -> Int
  -> Word
  -> CBOR.Decoder s (SomeMessage st)
decodeLeiosNotify decodeP decodeA = decode
  where
    decode
      :: forall (st' :: LeiosNotify point announcement).
         (ActiveState st')
      => StateToken st'
      -> Int
      -> Word
      -> CBOR.Decoder s (SomeMessage st')
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
        -- votes offer
        (SingIdle, 1, 5) ->
          return $ SomeMessage MsgDone
        (SingDone, _, _) -> notActiveState stok
        -- failures per protocol state
        (SingIdle, _, _) ->
          fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len
        (SingBusy, _, _) ->
          fail $ printf "codecLeiosNotify (%s) unexpected key (%d, %d)" (show stok) key len

codecLeiosNotifyId
  :: forall (point :: Type) (announcement :: Type) m.
     (Monad m)
  => Codec
       (LeiosNotify point announcement)
       CodecFailure
       m
       (AnyMessage (LeiosNotify point announcement))
codecLeiosNotifyId = Codec {encode, decode}
  where
    encode
      :: forall st st'.
         ( ActiveState st
         , StateTokenI st
         )
      => Message (LeiosNotify point announcement) st st'
      -> AnyMessage (LeiosNotify point announcement)
    encode = AnyMessage

    decode
      :: forall (st :: LeiosNotify point announcement).
         (ActiveState st)
      => StateToken st
      -> m (DecodeStep
             (AnyMessage (LeiosNotify point announcement))
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
        (SingIdle, Just (AnyMessage msg@MsgDone)) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingDone, _) ->
          notActiveState stok
        (_, _) ->
          DecodeFail $ CodecFailure "codecLeiosNotifyId: no matching message"

-----

leiosNotifyClientPeer ::
  forall m announcement point a.
     Monad m
  =>
     m (Either a (Message (LeiosNotify point announcement) StBusy StIdle -> m ()))
  ->
     Peer (LeiosNotify point announcement) AsClient NonPipelined StIdle m a
leiosNotifyClientPeer checkDone =
    go
  where
    go :: Peer (LeiosNotify point announcement) AsClient NonPipelined StIdle m a
    go = Effect $ checkDone <&> \case
        Left x ->
            Yield ReflClientAgency MsgDone
          $ Done ReflNobodyAgency x
        Right k ->
            Yield ReflClientAgency MsgLeiosNotificationRequestNext
          $ Await ReflServerAgency $ \msg -> case msg of
                MsgLeiosBlockAnnouncement{} -> react $ k msg
                MsgLeiosBlockOffer{} -> react $ k msg
                MsgLeiosBlockTxsOffer{} -> react $ k msg

    react action = Effect $ fmap (\() -> go) action

-----

type LeiosNotifyServerPeer point announcement m a =
    Peer (LeiosNotify point announcement) AsServer NonPipelined StIdle m ()

leiosNotifyServerPeer ::
  forall m announcement point.
     Monad m
  =>
     m (Message (LeiosNotify point announcement) StBusy StIdle)
  ->
     Peer (LeiosNotify point announcement) AsServer NonPipelined StIdle m ()
leiosNotifyServerPeer handler =
    go
  where
    go :: Peer (LeiosNotify point announcement) AsServer NonPipelined StIdle m ()
    go = Await ReflClientAgency $ \case
        MsgDone -> Done ReflNobodyAgency ()
        MsgLeiosNotificationRequestNext -> Effect $ do
            msg <- handler
            pure
              $ Yield ReflServerAgency msg
              $ go

-----

-- | Merely an abbreviation local to this module
type X point announcement m a n =
    Peer (LeiosNotify point announcement) AsClient (Pipelined n C) StIdle m a

type LeiosNotifyClientPeerPipelined point announcement m a =
    PeerPipelined (LeiosNotify point announcement) AsClient StIdle m a

data C = MkC

leiosNotifyClientPeerPipelined ::
  forall m point announcement a.
     MonadMVar m
  =>
     m (Either a Int)
     -- ^ either the return value or else the current max pipelining depth
  ->
     m (Message (LeiosNotify point announcement) StBusy StIdle -> m ())
  ->
    PeerPipelined (LeiosNotify point announcement) AsClient StIdle m a
leiosNotifyClientPeerPipelined checkDone k0 =
    PeerPipelined $ Effect $ do
        stop <- MVar.newEmptyMVar   -- would be IORef if io-classes had it
        pure $ go stop Zero
  where
    go :: MVar m () -> Nat n -> X point announcement m a n
    go stop !n = Effect $ checkDone <&> \case
        Left x -> Effect $ do
            MVar.putMVar stop ()
            pure $ drainThePipe x n
        Right maxDepth ->
            case n of
                Zero -> sendAnother stop n
                Succ m ->
                    Collect
                        (if natToInt n >= maxDepth then Nothing else Just $ sendAnother stop n)
                        (\MkC -> go stop m)

    sendAnother :: MVar m () -> Nat n -> X point announcement m a n
    sendAnother stop !n =
        YieldPipelined
            ReflClientAgency
            MsgLeiosNotificationRequestNext
            (receiver stop)
            (go stop $ Succ n)

    receiver :: MVar m () -> Receiver (LeiosNotify point announcement) AsClient StBusy StIdle m C
    receiver stop =
        ReceiverAwait ReflServerAgency $ \msg -> case msg of
            MsgLeiosBlockAnnouncement{} -> handler stop k0 msg
            MsgLeiosBlockOffer{} -> handler stop k0 msg
            MsgLeiosBlockTxsOffer{} -> handler stop k0 msg

    handler ::
        MVar m ()
     ->
        m (msg -> m ())
     ->
        msg
     ->
        Receiver (LeiosNotify point announcement) AsClient StIdle StIdle m C
    handler stop k x = ReceiverEffect $ do
        b <- MVar.isEmptyMVar stop
        when b $ k >>= ($ x)
        pure $ ReceiverDone MkC

    drainThePipe :: a -> Nat n -> X point announcement m a n
    drainThePipe x = \case
        Zero ->
            Yield ReflClientAgency MsgDone
          $ Done ReflNobodyAgency x
        Succ m ->
            Collect
                Nothing
                (\MkC -> drainThePipe x m)
