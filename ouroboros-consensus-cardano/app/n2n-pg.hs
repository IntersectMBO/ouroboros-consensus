{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.N2NPG.Run (Opts (..))
import qualified Cardano.Tools.N2NPG.Run as N2NPG
import           Main.Utf8 (withStdTerminalHandles)
import           Options.Applicative
import           Ouroboros.Consensus.Block

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    N2NPG.run =<< execParser optsParser

optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> parse) $ fullDesc <> progDesc desc
  where
    desc = "Download headers and blocks via the N2N protocol"

    parse = do
      configFile <- strOption $ mconcat
        [ long "config"
        , help "Path to config file, in the same format as for the node or db-analyser"
        , metavar "PATH"
        ]
      immutableDBDir <- strOption $ mconcat
        [ long "db"
        , help "Path to the ImmutableDB, only used for hash lookups"
        , metavar "PATH"
        ]
      let readHostPort = do
            hostPort <- str
            case break (== ':') hostPort of
              (host, ':' : port) -> pure (host, port)
              _ -> fail $ "Could not read host and port: " ++ show hostPort
      serverAddr <- option readHostPort $ mconcat
        [ long "server"
        , help "Server address"
        , metavar "HOST:PORT"
        ]
      startSlot <- fmap withOriginFromMaybe $ optional $ option (SlotNo <$> auto) $ mconcat
        [ long "start-from"
        , metavar "SLOT_NUMBER"
        , help "Start downloading from this slot (must be in the ImmutableDB)"
        ]
      numBlocks <- option auto $ mconcat
        [ long "num-blocks"
        , metavar "NUM_BLOCKS"
        , help "Number of blocks to download"
        ]
      sweepCommand <- subparser $
        command "sweep" (info (pure True) (progDesc "Sweeps the cache of the server"))
        <>
        command "single" (info (pure False) (progDesc "Requests a single range of blocks"))
      pure Opts {
          configFile
        , immutableDBDir
        , serverAddr
        , startSlot
        , numBlocks
        , sweepCommand
        }
