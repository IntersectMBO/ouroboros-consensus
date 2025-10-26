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

module LeiosDemoOnlyTestFetch
  ( LeiosFetch (..)
  , SingLeiosFetch (..)
  , Message (..)
  , leiosFetchMiniProtocolNum
  -- *
  , byteLimitsLeiosFetch
  , timeLimitsLeiosFetch
  , codecLeiosFetch
  , codecLeiosFetchId
  -- *
  , LeiosFetchClientPeer
  , LeiosFetchClientPeerPipelined
  , LeiosFetchServerPeer
  , LeiosFetchRequestHandler (..)
  , SomeLeiosFetchJob (..)
  , leiosFetchClientPeer
  , leiosFetchClientPeerPipelined
  , leiosFetchServerPeer
  , toLeiosFetchClientPeerPipelined
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.DeepSeq (NFData (..))
import           Control.Monad.Class.MonadST
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
import           Data.Singletons
import qualified Data.Vector as V
import           Data.Word (Word16, Word64)
import qualified Network.Mux.Types as Mux
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import           Text.Printf

-----

leiosFetchMiniProtocolNum :: Mux.MiniProtocolNum
leiosFetchMiniProtocolNum = Mux.MiniProtocolNum 19

data LeiosFetch point eb tx = StIdle | StBusy (LeiosFetchBusy point eb tx) | StDone

data LeiosFetchBusy point eb tx = StBlock | StBlockTxs

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

instance ShowProxy (StIdle            :: LeiosFetch point eb tx) where
  showProxy _ = "StIdle"
instance ShowProxy (StBusy StBlock    :: LeiosFetch point eb tx) where
  showProxy _ = "(StBusy StBlock)"
instance ShowProxy (StBusy StBlockTxs :: LeiosFetch point eb tx) where
  showProxy _ = "(StBusy StBlockTxs)"
instance ShowProxy (StDone            :: LeiosFetch point eb tx) where
  showProxy _ = "StDone"

type SingLeiosFetch
  :: LeiosFetch point eb tx
  -> Type
data SingLeiosFetch st where
  SingIdle     :: SingLeiosFetch StIdle
  SingBlock    :: SingLeiosFetch (StBusy StBlock)
  SingBlockTxs :: SingLeiosFetch (StBusy StBlockTxs)
  SingDone     :: SingLeiosFetch StDone

deriving instance Show (SingLeiosFetch st)

instance StateTokenI StIdle              where stateToken = SingIdle
instance StateTokenI (StBusy StBlock)    where stateToken = SingBlock
instance StateTokenI (StBusy StBlockTxs) where stateToken = SingBlockTxs
instance StateTokenI StDone              where stateToken = SingDone

-----

instance Protocol (LeiosFetch point eb tx) where
  data Message (LeiosFetch point eb tx) from to where
    MsgLeiosBlockRequest
      :: !point
      -> Message (LeiosFetch point eb tx) StIdle (StBusy StBlock)
    MsgLeiosBlock
      :: !eb
      -> Message (LeiosFetch point eb tx) (StBusy StBlock) StIdle

    MsgLeiosBlockTxsRequest
      :: !point
      -> [(Word16, Word64)]
      -> Message (LeiosFetch point eb tx) StIdle (StBusy StBlockTxs)
    MsgLeiosBlockTxs
      :: !(V.Vector tx)
      -> Message (LeiosFetch point eb tx) (StBusy StBlockTxs) StIdle

    -- MsgLeiosVotesRequest
    -- MsgLeiosVoteDelivery

    -- MsgLeiosBlockRangeRequest
    -- MsgLeiosNextBlockAndTxsInRange
    -- MsgLeiosLastBlockAndTxsInRange

    MsgDone
      :: Message (LeiosFetch point eb tx) StIdle StDone

  type StateAgency StIdle      = ClientAgency
  type StateAgency (StBusy st) = ServerAgency
  type StateAgency StDone      = NobodyAgency

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
        <> CBOR.encodeListLen (fromIntegral $ V.length txs)
        <> foldMap encodeTx txs
      -- MsgLeiosVotesRequest
      -- MsgLeiosVoteDelivery
      -- MsgLeiosBlockRangeRequest
      -- MsgLeiosNextBlockAndTxsInRange
      -- MsgLeiosLastBlockAndTxsInRange
      MsgDone ->
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 9

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
        (SingIdle, 2, 0) -> do
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
          n <- CBOR.decodeListLen
          -- TODO does V.generateM allocate exacly one buffer, via the hint?
          --
          -- If not, we could do so manually by relying on the fact that
          -- Decoder is ultimate in ST.
          txs <- V.generateM n $ \_i -> decodeTx
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

-----

data SomeLeiosFetchJob point eb tx m =
    forall st'.
        StateTokenI (StBusy st')
     =>
        MkSomeLeiosFetchJob
            (Message (LeiosFetch point eb tx) StIdle (StBusy st'))
            (m (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ()))

type LeiosFetchClientPeer point eb tx m a =
     Peer (LeiosFetch point eb tx) AsClient NonPipelined StIdle m a

leiosFetchClientPeer ::
  forall m point eb tx a.
     Monad m
  =>
     m (Either a (SomeLeiosFetchJob point eb tx m))
  ->
     Peer (LeiosFetch point eb tx) AsClient NonPipelined StIdle m a
leiosFetchClientPeer checkDone =
    go
  where
    go :: Peer (LeiosFetch point eb tx) AsClient NonPipelined StIdle m a
    go = Effect $ checkDone <&> \case
        Left x ->
            Yield ReflClientAgency MsgDone
          $ Done ReflNobodyAgency x
        Right (MkSomeLeiosFetchJob req k) -> case req of
            MsgLeiosBlockRequest{} -> do
                Yield ReflClientAgency req
              $ Await ReflServerAgency $ \rsp -> case rsp of
                    MsgLeiosBlock{} -> react $ k >>= ($ rsp)
            MsgLeiosBlockTxsRequest{} -> do
                Yield ReflClientAgency req
              $ Await ReflServerAgency $ \rsp -> case rsp of
                    MsgLeiosBlockTxs{} -> react $ k >>= ($ rsp)

    react action = Effect $ fmap (\() -> go) action

-----

type LeiosFetchServerPeer point eb tx m a =
    Peer (LeiosFetch point eb tx) AsServer NonPipelined StIdle m ()

newtype LeiosFetchRequestHandler point eb tx m = MkLeiosFetchRequestHandler (
    forall st'.
        Message (LeiosFetch point eb tx) StIdle (StBusy st')
     ->
        m (Message (LeiosFetch point eb tx) (StBusy st') StIdle)
  )

leiosFetchServerPeer ::
  forall m point eb tx.
     Monad m
  =>
     m (LeiosFetchRequestHandler point eb tx m)
  ->
     Peer (LeiosFetch point eb tx) AsServer NonPipelined StIdle m ()
leiosFetchServerPeer handler =
    go
  where
    go :: Peer (LeiosFetch point eb tx) AsServer NonPipelined StIdle m ()
    go = Await ReflClientAgency $ \req -> case req of
        MsgDone -> Done ReflNobodyAgency ()
        MsgLeiosBlockRequest{} -> Effect $ do
            MkLeiosFetchRequestHandler f <- handler
            rsp <- f req
            pure
              $ Yield ReflServerAgency rsp
              $ go
        MsgLeiosBlockTxsRequest{} -> Effect $ do
            MkLeiosFetchRequestHandler f <- handler
            rsp <- f req
            pure
              $ Yield ReflServerAgency rsp
              $ go

-----

-- | Merely an abbreviation local to this module
type X point eb tx m a n =
    Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a

type LeiosFetchClientPeerPipelined point eb tx m a =
    PeerPipelined (LeiosFetch point eb tx) AsClient StIdle m a

toLeiosFetchClientPeerPipelined ::
     Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a
  -> LeiosFetchClientPeerPipelined point eb tx m a
toLeiosFetchClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining

leiosFetchClientPeerPipelined ::
  forall m point eb tx a.
     PrimMonad m
  =>
     m (Either
         (m  (Either a (SomeLeiosFetchJob point eb tx m)))
             (Either a (SomeLeiosFetchJob point eb tx m))
       )
     -- ^ either the return value or the next job, or a blocking request for those two
  ->
    Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a
leiosFetchClientPeerPipelined tryNext =
    Effect $ do
        stop <- Prim.newMutVar NotYetDraining
        pure $ go1 stop Zero
  where
    go1 :: MutVar (PrimState m) WhetherDraining -> Nat n -> X point eb tx m a n
    go1 stop !n =
        Effect $ tryNext >>= \case
            -- no next instruction yet
            Left next ->
                case n of
                    Zero -> next <&> go2 stop Zero
                    Succ m ->
                        pure
                      $ Collect
                            Nothing
                            (\MkC -> go1 stop m)
            Right x -> pure $ go2 stop n x

    go2 :: MutVar (PrimState m) WhetherDraining -> Nat n -> Either a (SomeLeiosFetchJob point eb tx m) -> X point eb tx m a n
    go2 stop !n = \case
        Left x -> Effect $ do
            Prim.writeMutVar stop AlreadyDraining
            pure $ drainThePipe x n
        Right job ->
            case n of
                Zero -> send stop n job
                Succ m ->
                    Collect
                        (Just $ send stop n job)
                        (\MkC -> go1 stop m)

    send :: MutVar (PrimState m) WhetherDraining -> Nat n -> SomeLeiosFetchJob point eb tx m -> X point eb tx m a n
    send stop !n (MkSomeLeiosFetchJob req k) =
        YieldPipelined
            ReflClientAgency
            req
            (receiver stop k)
            (go1 stop $ Succ n)

    receiver ::
        StateTokenI (StBusy st')
     =>
        MutVar (PrimState m) WhetherDraining
     ->
        m (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ())
     ->
        Receiver (LeiosFetch point eb tx) AsClient (StBusy st') StIdle m C
    receiver stop k =
        ReceiverAwait ReflServerAgency $ \msg -> case msg of
            MsgLeiosBlock{} -> handler stop k msg
            MsgLeiosBlockTxs{} -> handler stop k msg

    handler ::
        MutVar (PrimState m) WhetherDraining
     ->
        m (msg -> m ())
     ->
        msg
     ->
        Receiver (LeiosFetch point eb tx) AsClient StIdle StIdle m C
    handler stop k x = ReceiverEffect $ do
        Prim.readMutVar stop >>= \case
            AlreadyDraining -> pure ()
            NotYetDraining -> k >>= ($ x)
        pure $ ReceiverDone MkC

    drainThePipe :: a -> Nat n -> X point eb tx m a n
    drainThePipe x = \case
        Zero ->
            Yield ReflClientAgency MsgDone
          $ Done ReflNobodyAgency x
        Succ m ->
            Collect
                Nothing
                (\MkC -> drainThePipe x m)
