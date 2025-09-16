{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.CertConjuring where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict (STM, StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import Control.Monad (forever, join, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), ToJSONKey (..), Value (Null), object, (.=))
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import Data.Data (Proxy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Word (Word64)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Ouroboros.Consensus.Block.Abstract
  ( BlockNo (..)
  , ConvertRawHash
  , HeaderHash
  , SlotNo (..)
  , fromRawHash
  , toRawHash
  )
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..), PerasWeight (..))
import Web.Scotty (file, get, json, middleware, pathParam, scotty)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | To avoid having to thread type variables everywhere
newtype RawHeaderHash = RawHeaderHash {unRawHeaderHash :: ByteString}
  deriving (Eq, Ord, Show)

reifyHeaderHash :: ConvertRawHash blk => Proxy blk -> RawHeaderHash -> HeaderHash blk
reifyHeaderHash p = fromRawHash p . Base16.decodeLenient . unRawHeaderHash

reflectHeaderHash :: ConvertRawHash blk => Proxy blk -> HeaderHash blk -> RawHeaderHash
reflectHeaderHash p = RawHeaderHash . Base16.encode . toRawHash p

instance ToJSON RawHeaderHash where
  toJSON (RawHeaderHash bs) = toJSON (BS.unpack bs)

instance ToJSONKey RawHeaderHash where
  toJSONKey = toJSONKeyText (Text.pack . BS.unpack . unRawHeaderHash)

-- Chain tips (ours and theirs)

newtype Peer = Peer {unPeer :: String}
  deriving (Eq, Ord, Show)

data ChainTip
  = OurTip (Maybe RawHeaderHash)
  | TheirTip Peer (Maybe RawHeaderHash)
  deriving (Eq, Ord, Show)

instance ToJSON ChainTip where
  toJSON (OurTip hash) =
    object
      [ "peer" .= ("us" :: String)
      , "block" .= maybe Null toJSON hash
      ]
  toJSON (TheirTip (Peer peer) hash) =
    object
      [ "peer" .= ("them(" <> peer <> ")")
      , "block" .= maybe Null toJSON hash
      ]

-- | Server blocks
data ServerBlock = ServerBlock
  { sbHash :: RawHeaderHash
  , sbParent :: Maybe RawHeaderHash
  , sbBoost :: PerasWeight
  , sbSlot :: SlotNo
  , sbNumber :: BlockNo
  }
  deriving Show

instance ToJSON ServerBlock where
  toJSON (ServerBlock hash parent boost slot number) =
    object
      [ "hash" .= toJSON hash
      , "parent" .= maybe Null toJSON parent
      , "boost" .= unPerasWeight boost
      , "slot" .= unSlotNo slot
      , "number" .= unBlockNo number
      ]

-- | Server state
data ServerState = ServerState
  { ssBlocks :: Map RawHeaderHash ServerBlock
  , ssAnchor :: Maybe RawHeaderHash
  , ssTips :: [ChainTip]
  , sbRound :: PerasRoundNo
  }
  deriving Show

instance ToJSON ServerState where
  toJSON st =
    object
      [ "blocks" .= ssBlocks st
      , "anchor" .= maybe Null toJSON (ssAnchor st)
      , "tips" .= ssTips st
      , "round" .= sbRound st
      ]

instance ToJSON PerasRoundNo where
  toJSON (PerasRoundNo n) = toJSON n

initServerState :: ServerState
initServerState = ServerState mempty Nothing mempty (PerasRoundNo 0)

addBlock :: ServerBlock -> ServerState -> ServerState
addBlock blk st =
  st
    { ssBlocks = Map.insert (sbHash blk) blk (ssBlocks st)
    }

setAnchor :: Maybe RawHeaderHash -> ServerState -> ServerState
setAnchor mbHash st = st{ssAnchor = mbHash}

setTips :: [ChainTip] -> ServerState -> ServerState
setTips tips st = st{ssTips = tips}

incRound :: ServerState -> ServerState
incRound st = st{sbRound = PerasRoundNo (unPerasRoundNo (sbRound st) + 1)}

-- Callbacks to be provided by the client
data ServerCallbacks = ServerCallbacks
  { onUpdateServerState ::
      ServerState ->
      STM IO (ServerState, IO ())
  , onCertConjuring ::
      ServerBlock ->
      PerasWeight ->
      ServerState ->
      STM IO (ServerState, IO ())
  }

emptyServerCallbacks :: ServerCallbacks
emptyServerCallbacks =
  ServerCallbacks
    { onUpdateServerState = \st -> pure (st, pure ())
    , onCertConjuring = \_ _ st -> pure (st, pure ())
    }

{-------------------------------------------------------------------------------
  HTTP server
-------------------------------------------------------------------------------}

type Port = Int

type ServerStateRef = StrictTVar IO ServerState

withServerState :: ServerStateRef -> (ServerState -> STM IO (ServerState, a)) -> STM IO a
withServerState ref stm = do
  st <- STM.readTVar ref
  (st', a) <- stm st
  STM.writeTVar ref st'
  return a

forkServer :: FilePath -> Port -> ServerCallbacks -> IO ()
forkServer static port callbacks = void $ do
  ref <- STM.newTVarIO initServerState
  void $ forkIO $ forever $ do
    join $ STM.atomically $ withServerState ref $ onUpdateServerState callbacks
    threadDelay 100000 -- 100ms
  void $ forkIO $ do
    runServer static port callbacks ref

runServer :: FilePath -> Port -> ServerCallbacks -> ServerStateRef -> IO ()
runServer static port callbacks ref = scotty port $ do
  middleware logStdout
  get "/" $ do
    file (static <> "/" <> "index.html")
  get "/state" $ do
    st <- liftIO $ STM.readTVarIO ref
    json st
  get "/boost/:hash/:boost" $ do
    hash <- RawHeaderHash <$> pathParam @ByteString "hash"
    boost <- PerasWeight <$> pathParam @Word64 "boost"
    liftIO $ join $ STM.atomically $ do
      withServerState ref $ \st -> do
        case Map.lookup hash (ssBlocks st) of
          Nothing -> do
            return (st, pure ())
          Just block -> do
            onCertConjuring callbacks block boost st
