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

module LeiosDemoOnlyTestFetch
  ( LeiosFetch (..)
  , SingLeiosFetch (..)
  , Message (..)
  , byteLimitsLeiosFetch
  , timeLimitsLeiosFetch
  , codecLeiosFetch
  , codecLeiosFetchId
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.Singletons
import Data.Word (Word16, Word64)
import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Text.Printf

-----

type LeiosFetch :: Type -> Type -> Type -> Type
data LeiosFetch point eb tx where
  StIdle     :: LeiosFetch point eb tx
  StBlock    :: LeiosFetch point eb tx
  StBlockTxs :: LeiosFetch point eb tx
  StDone     :: LeiosFetch point eb tx

instance ( ShowProxy point
         , ShowProxy eb
         , ShowProxy tx
         )
      => ShowProxy (LeiosFetch point eb tx) where
  showProxy _ =
    concat
      [ "LeiosFetch ",
        showProxy (Proxy :: Proxy point),
        " ",
        showProxy (Proxy :: Proxy eb),
        " ",
        showProxy (Proxy :: Proxy tx)
      ]

instance ShowProxy (StIdle :: LeiosFetch point eb tx) where
  showProxy _ = "StIdle"
instance ShowProxy (StBlock :: LeiosFetch point eb tx) where
  showProxy _ = "StBlock"
instance ShowProxy (StBlockTxs :: LeiosFetch point eb tx) where
  showProxy _ = "StBlockTxs"
instance ShowProxy (StDone :: LeiosFetch point eb tx) where
  showProxy _ = "StDone"

type SingLeiosFetch
  :: LeiosFetch point eb tx
  -> Type
data SingLeiosFetch st where
  SingIdle     :: SingLeiosFetch StIdle
  SingBlock    :: SingLeiosFetch StBlock
  SingBlockTxs :: SingLeiosFetch StBlockTxs
  SingDone     :: SingLeiosFetch StDone

deriving instance Show (SingLeiosFetch st)

instance StateTokenI StIdle     where stateToken = SingIdle
instance StateTokenI StBlock    where stateToken = SingBlock
instance StateTokenI StBlockTxs where stateToken = SingBlockTxs
instance StateTokenI StDone     where stateToken = SingDone

-----

instance Protocol (LeiosFetch point eb tx) where
  data Message (LeiosFetch point eb tx) from to where
    MsgLeiosBlockRequest
      :: !point
      -> Message (LeiosFetch point eb tx) StIdle StBlock
    MsgLeiosBlock
      :: !eb
      -> Message (LeiosFetch point eb tx) StBlock StIdle

    MsgLeiosBlockTxsRequest
      :: !point
      -> [(Word16, Word64)]
      -> Message (LeiosFetch point eb tx) StIdle StBlockTxs
    MsgLeiosBlockTxs
      :: ![tx]
      -> Message (LeiosFetch point eb tx) StBlockTxs StIdle

    -- MsgLeiosVotesRequest
    -- MsgLeiosVoteDelivery

    -- MsgLeiosBlockRangeRequest
    -- MsgLeiosNextBlockAndTxsInRange
    -- MsgLeiosLastBlockAndTxsInRange

    MsgDone
      :: Message (LeiosFetch point eb tx) StIdle StDone

  type StateAgency StIdle     = ClientAgency
  type StateAgency StBlock    = ServerAgency
  type StateAgency StBlockTxs = ServerAgency
  type StateAgency StDone     = NobodyAgency

  type StateToken = SingLeiosFetch

instance NFData (Message (LeiosFetch point eb tx) from to) where
    rnf = \case
      MsgLeiosBlockRequest{} -> ()
      MsgLeiosBlock{} -> ()
      MsgLeiosBlockTxsRequest _p bitmaps -> rnf bitmaps
      MsgLeiosBlockTxs{} -> ()
      -- MsgLeiosVotesRequest
      -- MsgLeiosVoteDelivery
      -- MsgLeiosBlockRangeRequest
      -- MsgLeiosNextBlockAndTxsInRange
      -- MsgLeiosLastBlockAndTxsInRange
      MsgDone -> ()

deriving instance (Eq point, Eq eb, Eq tx)
               => Eq (Message (LeiosFetch point eb tx) from to)

deriving instance (Show point, Show eb, Show tx)
               => Show (Message (LeiosFetch point eb tx) from to)

-----

byteLimitsLeiosFetch
  :: (bytes -> Word) -> ProtocolSizeLimits (LeiosFetch point eb tx) bytes
byteLimitsLeiosFetch = ProtocolSizeLimits $ \case
  SingIdle     -> smallByteLimit
  SingBlock    -> largeByteLimit
  SingBlockTxs -> largeByteLimit
  st@SingDone  -> notActiveState st

timeLimitsLeiosFetch
  :: ProtocolTimeLimits (LeiosFetch point eb tx)
timeLimitsLeiosFetch = ProtocolTimeLimits $ \case
  SingIdle     -> waitForever
  SingBlock    -> longWait
  SingBlockTxs -> longWait
  st@SingDone  -> notActiveState st

-----

codecLeiosFetch
  :: forall (point :: Type) (eb :: Type) (tx :: Type) m.
     (MonadST m)
  => (point -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s point)
  -> (eb -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s eb)
  -> (tx -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tx)
  -> Codec (LeiosFetch point eb tx) CBOR.DeserialiseFailure m ByteString
codecLeiosFetch encodeP decodeP encodeEb decodeEb encodeTx decodeTx =
  mkCodecCborLazyBS
    (encodeLeiosFetch encodeP encodeEb encodeTx)
    decode
    where
      decode
        :: forall (st :: LeiosFetch point eb tx).
           (ActiveState st)
        => StateToken st
        -> forall s. CBOR.Decoder s (SomeMessage st)
      decode stok = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        decodeLeiosFetch decodeP decodeEb decodeTx stok len key

encodeLeiosFetch
  :: forall (point :: Type) (eb :: Type) (tx :: Type)
            (st  :: LeiosFetch point eb tx)
            (st' :: LeiosFetch point eb tx).
     (point -> CBOR.Encoding)
  -> (eb -> CBOR.Encoding)
  -> (tx -> CBOR.Encoding)
  -> Message (LeiosFetch point eb tx) st st'
  -> CBOR.Encoding
encodeLeiosFetch encodeP encodeEb encodeTx = encode
  where
    encode
      :: forall st0 st1.
         Message (LeiosFetch point eb tx) st0 st1
      -> CBOR.Encoding
    encode = \case
      MsgLeiosBlockRequest p ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> encodeP p
      MsgLeiosBlock x -> 
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 1
        <> encodeEb x
      MsgLeiosBlockTxsRequest p bitmaps ->
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 2
        <> encodeP p
        <> encodeBitmaps bitmaps
      MsgLeiosBlockTxs txs ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 3
        <> CBOR.encodeListLenIndef <> foldr (\tx r -> encodeTx tx <> r) CBOR.encodeBreak txs
      -- MsgLeiosVotesRequest
      -- MsgLeiosVoteDelivery
      -- MsgLeiosBlockRangeRequest
      -- MsgLeiosNextBlockAndTxsInRange
      -- MsgLeiosLastBlockAndTxsInRange
      MsgDone ->
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 8

decodeLeiosFetch
  :: forall (point :: Type) (eb :: Type) (tx :: Type)
            (st :: LeiosFetch point eb tx) s.
     (ActiveState st)
  => (forall s'. CBOR.Decoder s' point)
  -> (forall s'. CBOR.Decoder s' eb)
  -> (forall s'. CBOR.Decoder s' tx)
  -> StateToken st
  -> Int
  -> Word
  -> CBOR.Decoder s (SomeMessage st)
decodeLeiosFetch decodeP decodeEb decodeTx = decode
  where
    decode
      :: forall (st' :: LeiosFetch point eb tx).
         (ActiveState st')
      => StateToken st'
      -> Int
      -> Word
      -> CBOR.Decoder s (SomeMessage st')
    decode stok len key = do
      case (stok, len, key) of
        (SingIdle, 1, 0) -> do
          p <- decodeP
          return $ SomeMessage $ MsgLeiosBlockRequest p
        (SingBlock, 2, 1) -> do
          x <- decodeEb
          return $ SomeMessage $ MsgLeiosBlock x
        (SingIdle, 3, 2) -> do
          p <- decodeP
          bitmaps <- decodeBitmaps
          return $ SomeMessage $ MsgLeiosBlockTxsRequest p bitmaps
        (SingBlockTxs, 2, 3) -> do
          txs <- CBOR.decodeListLenIndef *> CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTx
          return $ SomeMessage $ MsgLeiosBlockTxs txs
        -- MsgLeiosVotesRequest
        -- MsgLeiosVoteDelivery
        -- MsgLeiosBlockRangeRequest
        -- MsgLeiosNextBlockAndTxsInRange
        -- MsgLeiosLastBlockAndTxsInRange
        (SingIdle, 1, 9) ->
          return $ SomeMessage MsgDone
        (SingDone, _, _) -> notActiveState stok
        -- failures per protocol state
        (SingIdle, _, _) ->
          fail $ printf "codecLeiosFetch (%s) unexpected key (%d, %d)" (show stok) key len
        (SingBlock, _, _) ->
          fail $ printf "codecLeiosFetch (%s) unexpected key (%d, %d)" (show stok) key len
        (SingBlockTxs, _, _) ->
          fail $ printf "codecLeiosFetch (%s) unexpected key (%d, %d)" (show stok) key len

codecLeiosFetchId
  :: forall (point :: Type) (eb :: Type) (tx :: Type) m.
     (Monad m)
  => Codec
       (LeiosFetch point eb tx)
       CodecFailure
       m
       (AnyMessage (LeiosFetch point eb tx))
codecLeiosFetchId = Codec {encode, decode}
  where
    encode
      :: forall st st'.
         ( ActiveState st
         , StateTokenI st
         )
      => Message (LeiosFetch point eb tx) st st'
      -> AnyMessage (LeiosFetch point eb tx)
    encode = AnyMessage

    decode
      :: forall (st :: LeiosFetch point eb tx).
         (ActiveState st)
      => StateToken st
      -> m (DecodeStep
             (AnyMessage (LeiosFetch point eb tx))
             CodecFailure
             m
             (SomeMessage st)
           )
    decode stok = return $ DecodePartial $ \bytes ->
      return $ case (stok, bytes) of
        (SingIdle, Just (AnyMessage msg@MsgLeiosBlockRequest{})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingBlock, Just (AnyMessage msg@MsgLeiosBlock{})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@MsgLeiosBlockTxsRequest{})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingBlockTxs, Just (AnyMessage msg@MsgLeiosBlockTxs{})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@MsgDone{})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingDone, _) ->
          notActiveState stok
        (_, _) ->
          DecodeFail $ CodecFailure "codecLeiosFetchId: no matching message"

-----

encodeBitmaps :: [(Word16, Word64)] -> CBOR.Encoding
encodeBitmaps bitmaps =
     CBOR.encodeMapLenIndef
  <> foldr
       (\(index, bitmap) r -> CBOR.encodeWord16 index <> CBOR.encodeWord64 bitmap <> r)
       CBOR.encodeBreak
       bitmaps

decodeBitmaps :: CBOR.Decoder s [(Word16, Word64)]
decodeBitmaps =
     CBOR.decodeMapLenIndef
  *> CBOR.decodeSequenceLenIndef
       (flip (:))
       []
       reverse
       ((,) <$> CBOR.decodeWord16 <*> CBOR.decodeWord64)
