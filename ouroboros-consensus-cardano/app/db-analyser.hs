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
import qualified Ouroboros.Consensus.Backends as Backends
import qualified Ouroboros.Consensus.Backends.LSM as LSM
import System.FS.API (mkFsPath)
import System.Random (genWord64, newStdGen)

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (cfg, blockArgs) <- getCmdLine
  backendArgs <- case ldbBackend cfg of
    V2InMem -> pure Backends.inMemoryBackendArgs
    V2LSM -> do
      salt <- fst . genWord64 <$> newStdGen
      pure $ LSM.lsmBackendArgsIO (mkFsPath ["lsm"]) (dbDir cfg) salt
  void $ analyse cfg blockArgs backendArgs

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
