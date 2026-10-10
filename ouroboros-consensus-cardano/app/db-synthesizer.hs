{-# LANGUAGE NamedFieldPuns #-}

-- | This tool synthesizes a valid ChainDB, replicating cardano-node's UX
--
-- Usage: db-synthesizer --config FILE --db PATH [--payment-signing-key FILE]
--                       [--byron-delegation-certificate FILE]
--                       [--byron-signing-key FILE]
--                       [--shelley-kes-key FILE | --shelley-kes-agent-socket PATH]
--                       [--shelley-vrf-key FILE]
--                       [--shelley-operational-certificate FILE]
--                       [--bulk-credentials-file FILE]
--                       [--shelley-bls-key FILE] [--tx-generator NAME]
--                       [--tx-file FILE]
--                       ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
--                         (-e|--epochs NUMBER)) [-f | -a]
--
-- Available options:
--   --config FILE            Path to the node's config.json
--   --db PATH                Path to the Chain DB
--   --payment-signing-key FILE
--                            Path to the payment signing key
--   --shelley-bls-key FILE   Path to the pool's BLS signing key
--   --tx-generator NAME      Which transaction generator fills the blocks:
--                            "respend" (default) or "file"
--   --tx-file FILE           Transaction file that --tx-generator file replays
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
import Cardano.Tools.DBSynthesizer.BlsKey (readBlsSigningKey)
import Cardano.Tools.DBSynthesizer.Run
import Cardano.Tools.DBSynthesizer.TxGen.File (mkFileTxGen)
import Cardano.Tools.DBSynthesizer.TxGen.Respend (mkRespendTxGen)
import Control.Monad ((<=<))
import DBSynthesizer.Parsers
  ( Options (..)
  , TxGenSource (FromFile, FromRespend)
  , parseCommandLine
  )
import Main.Utf8 (withStdTerminalHandles)
import System.Exit (die)

main :: IO ()
main = withStdTerminalHandles $ withConfigErrorHandling $ do
  cryptoInit
  (Options{configFile, chainDBDir, paymentKeyFile, credentials, blsKeyFile, synthOptions}, txGen) <-
    parseCommandLine
  genTxs <-
    either die pure =<< case txGen of
      FromRespend -> mkRespendTxGen paymentKeyFile
      -- The file already holds the transactions, so no payment key is needed.
      FromFile path -> mkFileTxGen path
  votingKey <- traverse (either die pure <=< readBlsSigningKey) blsKeyFile
  (shelleyGenesis, protocol) <- initialize configFile credentials
  result <- synthesize genTxs votingKey synthOptions shelleyGenesis chainDBDir protocol
  putStrLn $ "--> done; result: " ++ show result
