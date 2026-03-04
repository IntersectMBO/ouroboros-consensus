{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis
import Control.ResourceRegistry
import Data.Aeson
import Data.ByteString.Char8 (putStr)
import Data.ByteString.Lazy
import Data.ByteString.Short
import Data.Maybe
import qualified Data.Text as T
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import System.Exit
import System.FS.API
import System.FS.IO
import Text.Hex

data Opts = Opts
  { immDBDir :: FilePath
  , slot :: SlotNo
  , configFile :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Output the block after a given one"

  parse = do
    immDBDir <-
      strOption $
        mconcat
          [ long "db"
          , help "Path to the ImmutableDB"
          , metavar "PATH"
          ]
    slot <-
      fmap SlotNo $
        option auto $
          mconcat
            [ long "slot"
            , help "Slot of the parent block, we will look for the NEXT block after this one"
            , metavar "SLOT"
            ]
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    pure Opts{immDBDir, slot, configFile}

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  Opts{immDBDir, slot, configFile} <- execParser optsParser
  let args = Cardano.CardanoBlockArgs configFile Nothing
  ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
  run immDBDir pInfoConfig (RealPoint (SlotNo $ 1 + unSlotNo slot) dummyHash)

dummyHash :: HeaderHash (CardanoBlock StandardCrypto)
dummyHash =
  fromShortRawHash (Proxy @(CardanoBlock StandardCrypto))
    . toShort
    . fromJust
    . decodeHex
    $ "b287995b9b0b3b0f834077f28327e8a276a673642da2a4c38ed937100074f153"

run ::
  FilePath ->
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  RealPoint (CardanoBlock StandardCrypto) ->
  IO ()
run immDBDir cfg p = withRegistry \registry ->
  ImmutableDB.withDB
    (ImmutableDB.openDB (immDBArgs registry))
    \immDB -> do
      ImmutableDB.getBlockAtOrAfterPoint immDB p >>= \case
        Left{} -> exitFailure
        Right pt ->
          ImmutableDB.getBlockComponent immDB ChainDB.GetHeader pt >>= \case
            Left{} -> exitFailure
            Right hdr ->
              Data.ByteString.Char8.putStr $
                toStrict $
                  ( encode @Object $
                      mconcat ["blockNo" .= blockNo hdr, "hash" .= String (T.pack (show (realPointHash pt)))]
                  )
 where
  immDBArgs registry =
    ImmutableDB.defaultArgs
      { ImmutableDB.immCheckIntegrity = Node.nodeCheckIntegrity storageCfg
      , ImmutableDB.immChunkInfo = Node.nodeImmutableDbChunkInfo storageCfg
      , ImmutableDB.immCodecConfig = codecCfg
      , ImmutableDB.immRegistry = registry
      , ImmutableDB.immHasFS = SomeHasFS $ ioHasFS $ MountPoint immDBDir
      }

  codecCfg = configCodec cfg
  storageCfg = configStorage cfg
