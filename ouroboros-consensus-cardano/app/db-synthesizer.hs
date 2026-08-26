-- | This tool synthesizes a valid ChainDB, replicating cardano-node's UX
--
-- Usage: db-synthesizer --config FILE --db PATH [--payment-signing-key FILE]
--                       [--shelley-operational-certificate FILE]
--                       [--shelley-vrf-key FILE] [--shelley-kes-key FILE]
--                       [--bulk-credentials-file FILE]
--                       [--shelley-bls-key FILE]
--                       ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
--                         (-e|--epochs NUMBER)) [-f | -a]
--
-- Available options:
--   --config FILE            Path to the node's config.json
--   --db PATH                Path to the Chain DB
--   --payment-signing-key FILE
--                            Path to the payment signing key
--   --shelley-operational-certificate FILE
--                            Path to the delegation certificate
--   --shelley-vrf-key FILE   Path to the VRF signing key
--   --shelley-kes-key FILE   Path to the KES signing key
--   --bulk-credentials-file FILE
--                            Path to the bulk credentials file
--   --shelley-bls-key FILE   Path to the pool's BLS signing key
--   -s,--slots NUMBER        Amount of slots to process
--   -b,--blocks NUMBER       Amount of blocks to forge
--   -e,--epochs NUMBER       Amount of epochs to process
--   -f                       Force overwrite an existing Chain DB
--   -a                       Append to an existing Chain DB
module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBSynthesizer.Run
import Cardano.Tools.DBSynthesizer.TxGen (mkRespendTxGen)
import Cardano.Tools.DBSynthesizer.Types (NodeFilePaths (nfpPaymentKey))
import DBSynthesizer.Parsers
import Main.Utf8 (withStdTerminalHandles)
import System.Exit

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (paths, creds, forgeOpts) <- parseCommandLine
  genTxs <- either die pure =<< mkRespendTxGen (nfpPaymentKey paths)
  result <- initialize paths creds forgeOpts >>= either die (uncurry (synthesize genTxs))
  putStrLn $ "--> done; result: " ++ show result
