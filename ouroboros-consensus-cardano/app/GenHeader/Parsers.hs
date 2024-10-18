module GenHeader.Parsers where

import Cardano.Tools.Headers (Options (..))
import Data.Version (showVersion)
import Options.Applicative (Parser, ParserInfo, execParser, helper, info, progDesc, (<**>))
import Paths_ouroboros_consensus_cardano (version)

parseOptions :: IO Options
parseOptions = execParser argsParser

argsParser :: ParserInfo Options
argsParser =
    info
        (optionsParser <**> helper)
        ( progDesc $
            unlines
                [ "gen-header - A utility to generate valid and invalid Praos headers for testing purpose"
                , "version: " <> showVersion version
                ]
        )

optionsParser :: Parser Options
optionsParser = pure Options
