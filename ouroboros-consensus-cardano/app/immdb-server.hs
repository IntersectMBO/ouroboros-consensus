{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import           Data.Void
import           Main.Utf8 (withStdTerminalHandles)
import qualified Network.Socket as Socket
import           Options.Applicative
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    Opts {immDBDir, port, configFile} <- execParser optsParser
    let sockAddr = Socket.SockAddrInet port hostAddr
          where
            -- could also be passed in
            hostAddr = Socket.tupleToHostAddress (127, 0, 0, 1)
        args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    absurd <$> ImmDBServer.run immDBDir sockAddr pInfoConfig

data Opts = Opts {
    immDBDir   :: FilePath
  , port       :: Socket.PortNumber
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
      port <- option auto $ mconcat
        [ long "port"
        , help "Port to serve on"
        , value 3001
        , showDefault
        ]
      configFile <- strOption $ mconcat
        [ long "config"
        , help "Path to config file, in the same format as for the node or db-analyser"
        , metavar "PATH"
        ]
      pure Opts {immDBDir, port, configFile}
