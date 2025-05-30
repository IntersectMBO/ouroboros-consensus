{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.Golden (tests) where

import Cardano.Ledger.Core
import Cardano.Slotting.Slot
import Control.Exception
import Control.Tracer
import qualified Data.Map.Strict as Map
import Debug.Trace
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.Ledger.Query (QueryVersion)
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB
import qualified System.Directory as Dir
import System.Exit
import System.FS.API hiding ((</>))
import qualified System.FS.IO as FSIO
import System.FilePath ((</>))
import System.IO.Temp
import qualified System.Process.ByteString as P
import Test.Consensus.Cardano.Examples
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Paths
import Test.Util.Serialisation.Golden

tests :: TestTree
tests =
  let goldenDir = ($(getGoldenDir) </> "cardano")
   in testGroup
        "Cardano golden"
        [ goldenTest_all codecConfig goldenDir examples
        , testCase "Golden LMDB tables" $
            testGoldenDiskLedgerTables goldenDir
        ]

instance
  CardanoHardForkConstraints c =>
  ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras c))
  where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    _ -> error $ "Unknown version: " <> show v

instance
  CardanoHardForkConstraints c =>
  ToGoldenDirectory (QueryVersion, HardForkNodeToClientVersion (CardanoEras c))
  where
  toGoldenDirectory (queryVersion, blockVersion) =
    show queryVersion </> case blockVersion of
      CardanoNodeToClientVersion12 -> "CardanoNodeToClientVersion12"
      CardanoNodeToClientVersion13 -> "CardanoNodeToClientVersion13"
      CardanoNodeToClientVersion14 -> "CardanoNodeToClientVersion14"
      CardanoNodeToClientVersion15 -> "CardanoNodeToClientVersion15"
      CardanoNodeToClientVersion16 -> "CardanoNodeToClientVersion16"
      _ -> error $ "Unknown version: " <> show blockVersion

{-
data ShelleyTxOut era = TxOutCompact
  { txOutCompactAddr :: {-# UNPACK #-} !CompactAddr
  , txOutCompactValue :: !(CompactForm (Value era))
  }
-}
exampleShelleyTxOut :: Cardano.Ledger.Core.TxOut ShelleyEra
exampleShelleyTxOut = undefined

exampleAllegraTxOut :: Cardano.Ledger.Core.TxOut AllegraEra
exampleAllegraTxOut = undefined

exampleMaryTxOut :: Cardano.Ledger.Core.TxOut MaryEra
exampleMaryTxOut = undefined

{-
data AlonzoTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      !DataHash
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
-}
exampleAlonzoTxOutCompact :: Cardano.Ledger.Core.TxOut AlonzoEra
exampleAlonzoTxOutCompact = undefined

exampleAlonzoTxOutCompactDH :: Cardano.Ledger.Core.TxOut AlonzoEra
exampleAlonzoTxOutCompactDH = undefined

exampleAlonzoTxOutAdaOnly :: Cardano.Ledger.Core.TxOut AlonzoEra
exampleAlonzoTxOutAdaOnly = undefined

exampleAlonzoTxOutAdaOnly_DataHash32 :: Cardano.Ledger.Core.TxOut AlonzoEra
exampleAlonzoTxOutAdaOnly_DataHash32 = undefined

{-
data BabbageTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      !DataHash
  | TxOutCompactDatum
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      !(Datum era)
      !(Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
  deriving (Generic)
-}
exampleBabbageTxOutCompact :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutCompact = undefined

exampleBabbageTxOutCompactDH :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutCompactDH = undefined

exampleBabbageTxOutCompactDatum :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutCompactDatum = undefined

exampleBabbageTxOutCompactRefScript :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutCompactRefScript = undefined

exampleBabbageTxOutAdaOnly :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutAdaOnly = undefined

exampleBabbageTxOutAdaOnly_DataHash32 :: Cardano.Ledger.Core.TxOut BabbageEra
exampleBabbageTxOutAdaOnly_DataHash32 = undefined

exampleConwayTxOutCompact :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutCompact = undefined

exampleConwayTxOutCompactDH :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutCompactDH = undefined

exampleConwayTxOutCompactDatum :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutCompactDatum = undefined

exampleConwayTxOutCompactRefScript :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutCompactRefScript = undefined

exampleConwayTxOutAdaOnly :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutAdaOnly = undefined

exampleConwayTxOutAdaOnly_DataHash32 :: Cardano.Ledger.Core.TxOut ConwayEra
exampleConwayTxOutAdaOnly_DataHash32 = undefined

exampleTxIns :: [CanonicalTxIn (CardanoEras StandardCrypto)]
exampleTxIns =
  [ CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  , CardanoTxIn undefined
  ]

exampleTxOuts :: [CardanoTxOut StandardCrypto]
exampleTxOuts =
  [ ShelleyTxOut exampleShelleyTxOut
  , AllegraTxOut exampleAllegraTxOut
  , MaryTxOut exampleMaryTxOut
  , AlonzoTxOut exampleAlonzoTxOutCompact
  , AlonzoTxOut exampleAlonzoTxOutCompactDH
  , AlonzoTxOut exampleAlonzoTxOutAdaOnly
  , AlonzoTxOut exampleAlonzoTxOutAdaOnly_DataHash32
  , BabbageTxOut exampleBabbageTxOutCompact
  , BabbageTxOut exampleBabbageTxOutCompactDH
  , BabbageTxOut exampleBabbageTxOutCompactDatum
  , BabbageTxOut exampleBabbageTxOutCompactRefScript
  , BabbageTxOut exampleBabbageTxOutAdaOnly
  , BabbageTxOut exampleBabbageTxOutAdaOnly_DataHash32
  , ConwayTxOut exampleConwayTxOutCompact
  , ConwayTxOut exampleConwayTxOutCompactDH
  , ConwayTxOut exampleConwayTxOutCompactDatum
  , ConwayTxOut exampleConwayTxOutCompactRefScript
  , ConwayTxOut exampleConwayTxOutAdaOnly
  , ConwayTxOut exampleConwayTxOutAdaOnly_DataHash32
  ]

exampleLedgerState :: LedgerState (CardanoBlock StandardCrypto) EmptyMK
exampleLedgerState = undefined

setupFS :: IO (SomeHasFS IO, FilePath, IO ())
setupFS = do
  systmpdir <- Dir.getTemporaryDirectory
  tmpdir <- createTempDirectory systmpdir "init_standalone_db"
  pure (SomeHasFS $ FSIO.ioHasFS $ MountPoint tmpdir, tmpdir, Dir.removeDirectoryRecursive tmpdir)

testGoldenDiskLedgerTables :: FilePath -> IO ()
testGoldenDiskLedgerTables goldenDir = do
  let goldenTables :: LedgerTables (LedgerState (CardanoBlock StandardCrypto)) ValuesMK
      goldenTables =
        LedgerTables $
          ValuesMK $
            Map.fromList $
              zip
                exampleTxIns
                exampleTxOuts
  let goldenFile = goldenDir </> "disk/LMDBLedgerTables"
  (e1, out1, err1) <-
    P.readProcessWithExitCode
      "mdb_dump"
      (traceShowId ["-a", goldenFile])
      mempty

  exists <- Dir.doesFileExist goldenFile
  if exists
    then case e1 of
      ExitFailure _ -> assertFailure $ "mdb_dump1 failed with error: " <> show err1
      ExitSuccess -> do
        bracket
          setupFS
          (\(_, _, io) -> io)
          ( \(fs, fp, _) -> do
              bs <-
                newLMDBBackingStore
                  nullTracer
                  (LMDBLimits (1024 * 1024 * 1024) 10 16)
                  (LiveLMDBFS fs)
                  (SnapshotsFS fs)
                  ( InitFromValues
                      Origin
                      exampleLedgerState
                      goldenTables
                  )
              bsCopy bs (error "LMDB does not use this") $ mkFsPath ["snap"]
              (e2, out2, err2) <-
                P.readProcessWithExitCode "mdb_dump" (traceShowId ["-a", fp </> "snap"]) mempty
              case e2 of
                ExitFailure _ -> assertFailure $ "mdb_dump2 failed with error: " <> show err2
                ExitSuccess ->
                  if out1 == out2
                    then pure ()
                    else
                      assertFailure $
                        unlines
                          [ "Mismatch on dumped values: "
                          , show out1
                          , "/="
                          , show out2
                          ]
          )
    else
      bracket
        setupFS
        (\(_, _, io) -> io)
        ( \(fs, fp, _) -> do
            bs <-
              newLMDBBackingStore
                nullTracer
                (LMDBLimits (1024 * 1024 * 1024) 10 16)
                (LiveLMDBFS fs)
                (SnapshotsFS fs)
                ( InitFromValues
                    Origin
                    exampleLedgerState
                    goldenTables
                )
            bsCopy bs (error "LMDB does not use this") $ mkFsPath ["snap"]
            Dir.renamePath (fp </> "snap") goldenFile
        )
