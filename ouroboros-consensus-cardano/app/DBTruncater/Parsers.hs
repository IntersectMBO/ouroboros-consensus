module DBTruncater.Parsers (commandLineParser) where

import           Cardano.Tools.DBTruncater.Types
import           DBAnalyser.Parsers
import           Options.Applicative
import           Ouroboros.Consensus.Block.Abstract

commandLineParser :: Parser DBTruncaterConfig
commandLineParser = DBTruncaterConfig
  <$> parseChainDBPath
  <*> parseTruncateAfter
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

parseTruncateAfter :: Parser TruncateAfter
parseTruncateAfter =
  fmap TruncateAfterSlot slotNoOption <|> fmap TruncateAfterBlock blockNoOption

slotNoOption :: Parser SlotNo
slotNoOption =
    SlotNo <$> option auto mods
  where
    mods = mconcat
      [ long "truncate-after-slot"
      , metavar "SLOT_NUMBER"
      , help "The slot number of the intended new tip of the chain after truncation"
      ]

blockNoOption :: Parser BlockNo
blockNoOption =
    BlockNo <$> option auto mods
  where
    mods = mconcat
      [ long "truncate-after-block"
      , metavar "BLOCK_NUMBER"
      , help "The block number of the intended new tip of the chain after truncation"
      ]
