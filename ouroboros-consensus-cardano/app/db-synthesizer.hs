{-# LANGUAGE NamedFieldPuns #-}

-- | This tool synthesizes a valid ChainDB, replicating cardano-node's UX
--
-- Usage: db-synthesizer --config FILE --db PATH
--                       [--byron-delegation-certificate FILE]
--                       [--byron-signing-key FILE]
--                       [--shelley-kes-key FILE | --shelley-kes-agent-socket PATH]
--                       [--shelley-vrf-key FILE]
--                       [--shelley-operational-certificate FILE]
--                       [--bulk-credentials-file FILE]
--                       ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
--                         (-e|--epochs NUMBER)) [-f | -a]
--
-- Available options:
--   --config FILE            Path to the node's config.json
--   --db PATH                Path to the Chain DB
--   -s,--slots NUMBER        Amount of slots to process
--   -b,--blocks NUMBER       Amount of blocks to forge
--   -e,--epochs NUMBER       Amount of epochs to process
--   -f                       Force overwrite an existing Chain DB
--   -a                       Append to an existing Chain DB
--
-- The credential options are cardano-node's own (they come from
-- @cardano-config@), so @--help@ documents them the way the node does.
module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.Config (withConfigErrorHandling)
import Cardano.Tools.DBSynthesizer.Run
import DBSynthesizer.Parsers
import Main.Utf8 (withStdTerminalHandles)

main :: IO ()
main = withStdTerminalHandles $ withConfigErrorHandling $ do
  cryptoInit
  Options{configFile, chainDBDir, credentials, synthOptions} <- parseCommandLine
  (shelleyGenesis, protocol) <- initialize configFile credentials
  result <- synthesize genTxs synthOptions shelleyGenesis chainDBDir protocol
  putStrLn $ "--> done; result: " ++ show result
 where
  genTxs _ _ _ _ = pure []
