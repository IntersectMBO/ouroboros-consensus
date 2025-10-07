{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Cardano.SnapshotConversion.External () where

import Cardano.Chain.Slotting
import Control.Monad.Except
import Data.SOP.Strict
import Foreign.C.String
import Ouroboros.Consensus.Byron.Ledger.Config
import qualified Ouroboros.Consensus.Cardano.SnapshotConversion as Impl
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.Shelley.Ledger.Config
import System.FilePath

foreign export ccall ffiConvertSnapshot :: CString -> CString -> CString -> IO ()

ffiConvertSnapshot :: CString -> CString -> CString -> IO ()
ffiConvertSnapshot cfgFileC outDirC snapNameC = do
  [cfgFile, outDir, snapName] <- mapM peekCString [cfgFileC, outDirC, snapNameC]
  Impl.FromConfigFile inFmt lgrPath <- Impl.getFormatFromConfig $ Impl.FP cfgFile

  res <-
    runExceptT $
      Impl.convertSnapshot
        False
        ( HardForkCodecConfig $
            PerEraCodecConfig $
              ByronCodecConfig (EpochSlots $ 2160 * 10)
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* ShelleyCodecConfig
                :* Nil
        )
        (inFmt (Impl.FP @"Snapshot" $ Impl.rawFilePath lgrPath </> snapName))
        (Impl.Mem $ outDir </> snapName)
  case res of
    Left err -> putStrLn $ show err
    Right () -> pure ()
