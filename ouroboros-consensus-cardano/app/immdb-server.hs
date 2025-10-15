{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import           Data.Void (absurd)
import           Main.Utf8 (withStdTerminalHandles)
import           Network.Socket (AddrInfo (addrFlags, addrSocketType))
import qualified Network.Socket as Socket
import           Options.Applicative (ParserInfo, execParser, fullDesc, help,
                     helper, info, long, metavar, progDesc, showDefault,
                     strOption, value)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    Opts {immDBDir, port, address, configFile} <- execParser optsParser

    let hints = Socket.defaultHints { addrFlags = [Socket.AI_NUMERICHOST], addrSocketType = Socket.Stream}
    addrInfo <- do
      addrInfos <- Socket.getAddrInfo (Just hints) (Just address) (Just port)
      case addrInfos of
        []         -> error "Invalid address or port"
        addrInfo:_ -> return addrInfo

    let args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    absurd <$> ImmDBServer.run immDBDir (Socket.addrAddress addrInfo) pInfoConfig

data Opts = Opts {
    immDBDir   :: FilePath
  , port       :: String
  , address    :: String
  , configFile :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> parse) $ fullDesc <> progDesc desc
  where
    desc = "Serve an ImmutableDB via ChainSync and BlockFetch"

    parse = do
      immDBDir <- strOption $ mconcat
        [ long "db"
        , help "Path to the ImmutableDB"
        , metavar "PATH"
        ]
      port <- strOption $ mconcat
        [ long "port"
        , help "Port to serve on"
        , value "3001"
        , showDefault
        ]
      address <- strOption $ mconcat
        [ long "address"
        , help "Address to serve on"
        , value "127.0.0.1"
        , showDefault
        ]
      configFile <- strOption $ mconcat
        [ long "config"
        , help "Path to config file, in the same format as for the node or db-analyser"
        , metavar "PATH"
        ]
      pure Opts {immDBDir, port, address, configFile}
