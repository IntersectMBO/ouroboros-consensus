module DBTruncater.Parsers
  ( commandLineParser
  ) where

import Ouroboros.Consensus.Block.Abstract
import Options.Applicative
import Cardano.Tools.DBTruncater.Types
import DBAnalyser.Parsers

commandLineParser :: Parser DBTruncaterConfig
commandLineParser = DBTruncaterConfig
  <$> parseChainDBPath
  <*> parseTruncatePoint
  <*> blockTypeParser
  <*> parseVerbose
  where
    parseChainDBPath = strOption $
      mconcat 
        [ long "db"
        , help "Path of the chain DB"
        , metavar "PATH"
        ]
    parseVerbose = switch (long "verbose" <> help "Enable verbose logging")

parseTruncatePoint :: Parser TruncatePoint
parseTruncatePoint = TruncatePoint <$> slotNoOption

slotNoOption :: Parser SlotNo
slotNoOption = 
  SlotNo <$> option auto (long "truncate-point" <> metavar "SLOT_NUMBER")
