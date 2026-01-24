{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.N2NPG.Run (Opts (..), StartFrom (..))
import qualified Cardano.Tools.N2NPG.Run as N2NPG
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC8
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
import Ouroboros.Consensus.Block
import Text.Read (readEither)

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
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    immutableDBDir <-
      optional $
        strOption $
          mconcat
            [ long "db"
            , help "Path to the ImmutableDB, only used for hash lookups"
            , metavar "PATH"
            ]
    let readHostPort = do
          hostPort <- str
          case break (== ':') hostPort of
            (host, ':' : port) -> pure (host, port)
            _ -> fail $ "Could not read host and port: " ++ show hostPort
    serverAddr <-
      option readHostPort $
        mconcat
          [ long "server"
          , help "Server address"
          , metavar "HOST:PORT"
          ]
    let readStartFrom = eitherReader $ \sf -> case break (== '@') sf of
          (h, '@' : s) -> do
            hash <- B16.decode $ BC8.pack h
            slot <- readEither s
            pure $ StartFromPoint (SlotNo slot) hash
          (s, _) -> StartFromSlot . SlotNo <$> readEither s
    startFrom <-
      some $
        option readStartFrom $
          mconcat
            [ long "start-from"
            , metavar "SLOT_NUMBER or HASH@SLOT_NUMBER"
            , help $
                "Start downloading from this slot (must be in the ImmutableDB) "
                  <> "or the given point (hash and slot)"
            ]
    numBlocks <-
      option auto $
        mconcat
          [ long "num-blocks"
          , metavar "NUM_BLOCKS"
          , help "Number of blocks to download"
          ]
    pure
      Opts
        { configFile
        , immutableDBDir
        , serverAddr
        , startFrom
        , numBlocks
        }
