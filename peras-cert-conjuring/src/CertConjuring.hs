{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module CertConjuring where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status200)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (file, get, json, middleware, pathParam, scotty, status)
import Data.Aeson (ToJSON (..), (.=), object)
import Data.Tree (Tree (..))
import Test.QuickCheck (Gen, elements, generate)
import Control.Concurrent (threadDelay)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type BlockHash = Int

type BlockBoost = Int

type SlotNo = Int

-- | Server block representation
data ServerBlock = ServerBlock
  { sbHash :: BlockHash,
    sbParent :: BlockHash,
    sbBoost :: BlockBoost,
    sbSlot :: SlotNo
  }
  deriving (Show)

instance ToJSON ServerBlock where
  toJSON (ServerBlock hash parent boost slot) =
    object
      [ "hash" .= hash,
        "parent" .= parent,
        "boost" .= boost,
        "slot" .= slot
      ]

genesisBlock :: ServerBlock
genesisBlock = ServerBlock 0 0 0 0

-- | Vizualization state
data ServerState = ServerState
  { ssChain :: Tree ServerBlock
  }
  deriving (Show)

instance ToJSON ServerState where
  toJSON (ServerState chain) =
    object
      [ "chain" .= toHierarchy chain
      ]
    where
      toHierarchy (Node blk subtrees) =
        object
          [ "name" .= sbHash blk,
            "block" .= blk,
            "children" .= map toHierarchy subtrees
          ]

initServerState :: ServerState
initServerState = ServerState (Node genesisBlock [])

addServerBlock :: ServerBlock -> ServerState -> ServerState
addServerBlock blk st = ServerState (go (ssChain st))
  where
    go (Node other subtrees)
      | sbHash other == sbParent blk
          && sbHash blk `notElem` (sbHash . rootLabel <$> subtrees) =
          Node other (Node blk [] : subtrees)
      | otherwise =
          Node other (go <$> subtrees)

boostServerBlock :: BlockHash -> BlockBoost -> ServerState -> ServerState
boostServerBlock hash boost st = ServerState (go (ssChain st))
  where
    go (Node blk subtrees)
      | sbHash blk == hash =
          Node (blk {sbBoost = sbBoost blk + boost}) subtrees
      | otherwise =
          Node blk (go <$> subtrees)

{-------------------------------------------------------------------------------
  Mocked state generation
-------------------------------------------------------------------------------}

genHash :: Gen BlockHash
genHash = elements [0 .. 100]

genServerBlock :: SlotNo -> Gen ServerBlock
genServerBlock slot = do
  hash <- genHash
  parent <- genHash
  return
    ServerBlock
      { sbHash = hash
      , sbParent = parent
      , sbBoost = 0
      , sbSlot = slot
      }

mockUpdateServerState :: MVar ServerState -> IO ()
mockUpdateServerState st = go 0
 where
  go slot = do
    blk <- generate (genServerBlock slot)
    putStrLn $ "Generated block: " <> show blk
    modifyMVar_ st (pure . addServerBlock blk)
    threadDelay 1000000
    go (slot + 1)

{-------------------------------------------------------------------------------
  Mocked state generation
-------------------------------------------------------------------------------}

-- In case we want ot embed the index.html file directly into the binary.
-- Not very convenient during development, though.
-- index :: Text
-- index = Text.pack $(
--   embedOneStringFileOf
--     [ prefix <> "static/index.html"
--     | prefix <- ["", "peras-cert-conjuring/"]
--     ]
--   )

index :: FilePath
index = "peras-cert-conjuring/static/index.html"

runServer :: MVar ServerState -> IO ()
runServer st = scotty 3000 $ do
  middleware logStdout

  get "/" $ do
    file index
  get "/state" $ do
    state <- liftIO (readMVar st)
    liftIO $ putStrLn $ "Serving state to "
    json state
  get "/boost/:hash/:boost" $ do
    blockHash <- pathParam @Int "hash"
    boost <- pathParam @Int "boost"
    liftIO $ boostBlock blockHash boost st
    status status200

boostBlock :: Int -> Int -> MVar ServerState -> IO ()
boostBlock hash boost st = do
  putStrLn $ "Boosting block " <> show hash <> " by " <> show boost
  modifyMVar_ st (pure . boostServerBlock hash boost)
