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

-- | The LeiosNotify mini-protocol's types and codec.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
  ( LeiosNotify (..)
  , Message (..)
  , SingLeiosNotify (..)
  , byteLimitsLeiosNotify
  , codecLeiosNotify
  , codecLeiosNotifyId
  , timeLimitsLeiosNotify
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Control.Monad.Class.MonadST (MonadST)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Network.TypedProtocol.Codec.CBOR
  ( ActiveState
  , AnyMessage (..)
  , Codec
  , CodecF (..)
  , CodecFailure (..)
  , DecodeStep (..)
  , SomeMessage (..)
  , StateTokenI (..)
  , mkCodecCborLazyBS
  , notActiveState
  )
import Network.TypedProtocol.Core
  ( Agency (..)
  , Protocol (..)
  , StateAgency
  )
import Ouroboros.Network.Protocol.Limits
  ( ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , longWait
  , smallByteLimit
  , waitForever
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Text.Printf (printf)

-----

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
      -- Carry the size so that an upstream can offer an EB as long as they've
      -- sent an announcement of this hash, even if that announcement had the
      -- wrong size. This is cheap to do and makes the LeiosFetch logic simpler
      -- to reason about.
      --
      -- TODO update the CIP/blueprint, which still has this message carrying
      -- the point alone. Both alternatives change the mini-protocol message or
      -- its accounting, so the choice belongs in that discussion.
      !Word32 ->
      Message (LeiosNotify point announcement vote) StBusy StIdle
    MsgLeiosBlockTxsOffer ::
      !point ->
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
  (bytes -> Word) ->
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
    MsgQuit ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 6
    MsgCanceled ->
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 7

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
      (SingQuit, 1, 5) ->
        return $ SomeMessage MsgDone
      (SingIdle, 1, 6) ->
        return $ SomeMessage MsgQuit
      (SingBusy, 1, 7) ->
        return $ SomeMessage MsgCanceled
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
