-- |

module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL8
import qualified Test.Cardano.Ledger.Allegra.Binary.Cddl as Allegra
import qualified Test.Cardano.Ledger.Alonzo.Binary.Cddl as Alonzo
import qualified Test.Cardano.Ledger.Babbage.Binary.Cddl as Babbage
import qualified Paths_cardano_ledger_byron as Byron
import qualified Test.Cardano.Ledger.Conway.Binary.Cddl as Conway
import qualified Test.Cardano.Ledger.Mary.Binary.Cddl as Mary
import qualified Test.Cardano.Ledger.Shelley.Binary.Cddl as Shelley
import qualified Data.List as L
import qualified System.Process.ByteString.Lazy as P
import qualified System.Environment as E
import qualified System.FilePath as F

main :: IO ()
main = do
  byron <- forwardize . (:[]) <$> Byron.getDataFileName "cddl-spec/byron.cddl"
  shelley <- forwardize <$> Shelley.readShelleyCddlFileNames
  allegra <- forwardize <$> Allegra.readAllegraCddlFileNames
  mary <- forwardize <$> Mary.readMaryCddlFileNames
  alonzo <- forwardize <$> Alonzo.readAlonzoCddlFileNames
  babbage <- forwardize <$> Babbage.readBabbageCddlFileNames
  conway <- forwardize <$> Conway.readConwayCddlFileNames

  let include_path = mconcat $ L.intersperse ":" $ [byron, shelley, allegra, mary, alonzo, babbage, conway]
  E.setEnv "CDDL_INCLUDE_PATH" (include_path <> ":")

  putStrLn . BL8.toString =<< cddlc "ouroboros-consensus-cardano/cddl/disk/block.cddl"
 where
   forwardize [x] =
     let f = [ if c /= '\\' then c else '/' | c <- F.takeDirectory x ]
     in if "C:" `L.isPrefixOf` f
        then drop 2 f
        else f
   forwardize x = error $ "match: " <> show x


-- | A 'CDDL' specifcation for a protocol 'ps'.
--
-- newtype CDDLSpec ps = CDDLSpec BL.ByteString

cddlc :: FilePath -> IO BL.ByteString
cddlc path = do
  (_, cddl, err) <- P.readProcessWithExitCode "ruby" ["C:/msys64/clang64/bin/cddlc", "-u", "-2", "-t", "cddl", path] mempty
  putStrLn $ BL8.toString err
  return cddl
