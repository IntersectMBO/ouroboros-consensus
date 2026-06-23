{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database analysis tool.
module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.Block.Cardano
import Cardano.Tools.DBAnalyser.Run
import Cardano.Tools.DBAnalyser.Types
import Cardano.Tools.GitRev (gitRev)
import Control.Monad (void)
import DBAnalyser.Parsers
import qualified Data.Text as T
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
  ( execParser
  , footer
  , fullDesc
  , helper
  , info
  , progDesc
  , (<**>)
  )

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  void $ uncurry analyse =<< getCmdLine

getCmdLine :: IO (DBAnalyserConfig, CardanoBlockArgs)
getCmdLine = execParser opts
 where
  opts =
    info
      (parseCmdLine <**> helper)
      ( mconcat
          [ fullDesc
          , progDesc "Simple framework used to analyse a Chain DB"
          , footer $ "ouroboros-consensus commit: " <> T.unpack gitRev
          ]
      )
