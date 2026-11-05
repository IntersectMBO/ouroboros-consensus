{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests that the LeiosDB and LeiosFetch serve what a LeiosFetch client needs:
-- a client that fetches an endorser block from the node's server, over the
-- node's codec, as the fetch logic will, and stores it in its own LeiosDB.
--
-- The node runs no such client yet. These tests pin the contract it will rely
-- on: it checks the bytes it receives against the hash it requested before it
-- decodes them, decodes them as of the era of the announcement, writes the
-- body with the fills of the txs its node holds, requests only the txs the
-- write did not fill, and then holds, and serves, the endorser block in exactly
-- the bytes it received.
module Test.Consensus.Leios.LeiosFetch.Client (tests) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , atomically
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Monad.Class.MonadAsync (concurrently)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Bits (bit, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (Codec, PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised
  , TxLocation (..)
  , TxOffset
  , decodeLeiosPoint
  , decodeLeiosTx
  , decodeSerialisedEndorserBlock
  , deserialiseEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , encodeSerialisedEndorserBlock
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , endorserBlockReferences
  , hashSerialisedEndorserBlock
  , maxTxsPerEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (leiosFetchServer)
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LocalFill
  , Promise (..)
  , newLeiosDBInMemory
  , withReader
  , withWriter
  )
import Ouroboros.Network.Channel (createConnectedBufferedChannels)
import Ouroboros.Network.Driver.Simple (runPeer)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "a client fetching into its own LeiosDB"
    [ testCase "holds and serves the endorser block in the bytes it received" $ do
        let Fetched{fetchedHeld, fetchedRequested, fetchedDropped, fetchedBody, fetchedClosure, fetchedComplete} =
              runSimOrThrow $ fetchInto (\_writer -> pure ()) []
        fetchedHeld @?= []
        fetchedRequested @?= [0, 1, 2]
        fetchedDropped @?= []
        fetchedBody @?= Just (serialiseEndorserBlock eb)
        fetchedClosure @?= Just txs
        fetchedComplete @?= [point]
    , testCase "requests only the txs its write did not fill from what it holds" $ do
        let Fetched{fetchedHeld, fetchedRequested, fetchedClosure, fetchedComplete} =
              runSimOrThrow $
                fetchInto
                  (holdWhole otherPoint otherEb otherTxs)
                  [(1, MkTxLocation (endorserBlockHash otherEb) 0)]
        fetchedHeld @?= [1]
        fetchedRequested @?= [0, 2]
        fetchedClosure @?= Just txs
        fetchedComplete @?= [otherPoint, point]
    , testCase "requests no txs of an endorser block it holds at another point" $ do
        let Fetched{fetchedHeld, fetchedRequested, fetchedClosure, fetchedComplete} =
              runSimOrThrow $
                fetchInto
                  (holdWhole earlierPoint eb txs)
                  [(i, MkTxLocation (endorserBlockHash eb) i) | i <- [0, 1, 2]]
        fetchedHeld @?= [0, 1, 2]
        fetchedRequested @?= []
        fetchedClosure @?= Just txs
        fetchedComplete @?= [earlierPoint, point]
    ]

-- | The endorser block the server holds, its txs, and its point.
eb :: EndorserBlock
eb = endorserBlockFromTxBytes (nonEmpty' txs)

txs :: [BS.ByteString]
txs = [BS.replicate 10 1, BS.replicate 20 2, BS.replicate 30 3]

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 10) (endorserBlockHash eb)

-- | The same endorser block, announced earlier.
earlierPoint :: LeiosPoint
earlierPoint = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)

-- | Another endorser block, sharing the tx at offset 1 of 'eb' at its offset 0.
otherEb :: EndorserBlock
otherEb = endorserBlockFromTxBytes (nonEmpty' otherTxs)

otherTxs :: [BS.ByteString]
otherTxs = [BS.replicate 20 2, BS.replicate 40 4]

otherPoint :: LeiosPoint
otherPoint = MkLeiosPoint (SlotNo 7) (endorserBlockHash otherEb)

-- | What the client's write and its LeiosDB report once it is done.
data Fetched = Fetched
  { fetchedHeld :: [TxOffset]
  -- ^ The offsets its body write reported held.
  , fetchedRequested :: [TxOffset]
  -- ^ The offsets it then requested.
  , fetchedDropped :: [TxOffset]
  -- ^ The offsets its tx write dropped for their size.
  , fetchedBody :: Maybe (Serialised EndorserBlock)
  -- ^ What its own server would send.
  , fetchedClosure :: Maybe [BS.ByteString]
  , fetchedComplete :: [LeiosPoint]
  }

-- | The server's node holds 'eb' at 'point'. The client's node holds what the
-- action writes, then fetches 'eb' at 'point' with these fills.
fetchInto ::
  (LeiosDbWriter (IOSim s) -> IOSim s ()) ->
  [LocalFill] ->
  IOSim s Fetched
fetchInto prepare fills = do
  serverDb <- newLeiosDBInMemory
  withWriter serverDb $ holdWhole point eb txs
  clientDb <- newLeiosDBInMemory
  withWriter clientDb prepare
  resultVar <- newTVarIO Nothing
  (clientChan, serverChan) <- createConnectedBufferedChannels 10
  _ <-
    withReader serverDb $ \reader ->
      withWriter clientDb $ \writer ->
        concurrently
          (runPeer nullTracer codec clientChan (fetchingClient fills writer resultVar))
          (runPeer nullTracer codec serverChan (leiosFetchServer nullTracer reader))
  (held, requested, dropped) <-
    readTVarIO resultVar >>= maybe (error "the client did not finish") pure
  withReader clientDb $ \reader -> do
    body <- lookupEbBody reader point
    closure <- lookupEbClosure reader point
    complete <- scanCompleteEbClosuresNotOlderThanSlot reader 0
    pure
      Fetched
        { fetchedHeld = held
        , fetchedRequested = requested
        , fetchedDropped = dropped
        , fetchedBody = body
        , fetchedClosure = closure
        , fetchedComplete = complete
        }

-- | A client fetching 'eb' at 'point', as the fetch logic will: it requests the
-- endorser block, checks its bytes against the requested hash before decoding
-- them, as of the era of the announcement, writes the body with the fills,
-- requests the txs the write did not fill, and writes them. It keeps the
-- offsets held, requested and dropped.
fetchingClient ::
  [LocalFill] ->
  LeiosDbWriter (IOSim s) ->
  StrictTVar (IOSim s) (Maybe ([TxOffset], [TxOffset], [TxOffset])) ->
  Peer
    (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx)
    AsClient
    NonPipelined
    StIdle
    (IOSim s)
    ()
fetchingClient fills writer resultVar =
  Yield ReflClientAgency (MsgLeiosBlockRequest point) $
    Await ReflServerAgency $ \case
      MsgLeiosBlock bytes
        | hashSerialisedEndorserBlock bytes /= pointEbHash point ->
            error "the server sent another endorser block"
        | otherwise -> Effect $ do
            received <-
              either (error . show) pure $ deserialiseEndorserBlock (Proxy @DijkstraEra) bytes
            held <- await =<< writeEbBody writer point received fills
            let wanted = [0 .. length (endorserBlockReferences received) - 1] \\ held
                finish dropped = do
                  atomically $ writeTVar resultVar (Just (held, wanted, dropped))
                  pure $ Yield ReflClientAgency MsgDone $ Done ReflNobodyAgency ()
            if null wanted
              then finish []
              else pure $
                Yield ReflClientAgency (MsgLeiosBlockTxsRequest point (bitmapsOf wanted)) $
                  Await ReflServerAgency $ \case
                    MsgLeiosBlockTxs fetched -> Effect $ do
                      dropped <-
                        await
                          =<< writeTxs writer point (zip wanted (map leiosTxBytes (V.toList fetched)))
                      finish dropped

-- | The node's codec of LeiosFetch.
codec ::
  Codec
    (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx)
    DeserialiseFailure
    (IOSim s)
    ByteString
codec =
  codecLeiosFetch
    maxTxsPerEndorserBlock
    encodeLeiosPoint
    decodeLeiosPoint
    encodeSerialisedEndorserBlock
    decodeSerialisedEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

-- | Write the endorser block at the point, with all its txs.
holdWhole :: LeiosPoint -> EndorserBlock -> [BS.ByteString] -> LeiosDbWriter (IOSim s) -> IOSim s ()
holdWhole p body bytes writer = do
  _ <- await =<< writeEbBody writer p body []
  _ <- await =<< writeTxs writer p (zip [0 ..] bytes)
  pure ()

-- | A list of at least one element, as 'NonEmpty'.
nonEmpty' :: [a] -> NonEmpty a
nonEmpty' = \case
  x : xs -> x :| xs
  [] -> error "nonEmpty': empty list"

-- | The bitmaps naming these offsets.
bitmapsOf :: [TxOffset] -> TxBitmaps
bitmapsOf offsets =
  Map.toAscList $
    Map.fromListWith (.|.) [(fromIntegral (o `div` 64), bit (63 - o `mod` 64)) | o <- offsets]
