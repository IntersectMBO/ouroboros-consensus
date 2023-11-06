{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBAnalyser.Run
import           Cardano.Tools.DBAnalyser.Types
import           Control.Monad (void)
import           DBAnalyser.Parsers
import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))


main :: IO ()
main = do
    cryptoInit
    (conf, blocktype) <- getCmdLine
    void $ case blocktype of
      LegacyCardanoBlock args -> analyse conf args

getCmdLine :: IO (DBAnalyserConfig, BlockType)
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to analyse a Chain DB"
        ])
