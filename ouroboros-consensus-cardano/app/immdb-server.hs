{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import qualified Data.Aeson as JSON
import Data.Time.Clock (DiffTime)
import qualified Data.Time.Clock.POSIX as POSIX
import Data.Void (absurd)
import Main.Utf8 (withStdTerminalHandles)
import Network.Socket (AddrInfo (addrFlags, addrSocketType))
import qualified Network.Socket as Socket
import Options.Applicative
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import System.Exit (die)
import qualified System.IO as SIO

main :: IO ()
main = withStdTerminalHandles $ do
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  cryptoInit
  Opts
    { immDBDir
    , port
    , address
    , configFile
    , refSlotNr
    , refTimeForRefSlot
    , leiosDbFile
    , leiosScheduleFile
    } <-
    execParser optsParser
  let hints = Socket.defaultHints{addrFlags = [Socket.AI_NUMERICHOST], addrSocketType = Socket.Stream}
  addrInfo <- do
    addrInfos <- Socket.getAddrInfo (Just hints) (Just address) (Just port)
    case addrInfos of
      [] -> error "Invalid address or port"
      addrInfo : _ -> return addrInfo

  let args = Cardano.CardanoBlockArgs configFile Nothing
  ProtocolInfo{pInfoConfig} <- mkProtocolInfo args

  leiosSchedule <-
    JSON.eitherDecodeFileStrict leiosScheduleFile >>= \case
      Left err -> die $ "Failed to decode LeiosSchedule: " ++ err
      Right x -> pure x

  absurd
    <$> ImmDBServer.run
      immDBDir
      (Socket.addrAddress addrInfo)
      pInfoConfig
      (mkGetSlotDelay refSlotNr refTimeForRefSlot)
      leiosDbFile
      (leiosSchedule :: ImmDBServer.LeiosSchedule)
 where
  -- 1-second slot duration is hard-coded; supporting variable durations would
  -- mean threading the era summary through and running the slot-to-wallclock
  -- query.
  mkGetSlotDelay :: SlotNo -> POSIX.POSIXTime -> Double -> IO DiffTime
  mkGetSlotDelay refSlotNr refTimeForRefSlot =
    let slotToPosix :: Double -> POSIX.POSIXTime
        slotToPosix = realToFrac
     in \slotDbl -> do
          let slotTime = refTimeForRefSlot + (slotToPosix slotDbl - slotToPosix (fromIntegral $ unSlotNo refSlotNr))
          currentTime <- POSIX.getPOSIXTime
          pure $
            if currentTime < slotTime
              then realToFrac $ slotTime - currentTime
              else 0

data Opts = Opts
  { immDBDir :: FilePath
  , port :: String
  , address :: String
  , configFile :: FilePath
  , refSlotNr :: SlotNo
  , refTimeForRefSlot :: POSIX.POSIXTime
  , leiosDbFile :: FilePath
  , leiosScheduleFile :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Serve an ImmutableDB via ChainSync, BlockFetch and schedule-driven Leios"

  parse = do
    immDBDir <-
      strOption $
        mconcat
          [ long "db"
          , help "Path to the ImmutableDB"
          , metavar "PATH"
          ]
    port <-
      strOption $
        mconcat
          [ long "port"
          , help "Port to serve on"
          , value "3001"
          , showDefault
          ]
    address <-
      strOption $
        mconcat
          [ long "address"
          , help "Address to serve on"
          , value "127.0.0.1"
          , showDefault
          ]
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    refSlotNr <-
      fmap SlotNo $
        option auto $
          mconcat
            [ long "initial-slot"
            , help
                "Reference slot number (SlotNo). With initial-time this drives the slot-to-wallclock translation."
            , metavar "SLOT_NO"
            ]
    refTimeForRefSlot <-
      fmap (fromInteger @POSIX.POSIXTime) $
        option auto $
          mconcat
            [ long "initial-time"
            , help "UTC time for the reference slot, provided as POSIX seconds (Unix timestamp)"
            , metavar "POSIX_SECONDS"
            ]
    leiosDbFile <-
      strOption $
        mconcat
          [ long "leios-db"
          , help "Path to the Leios SQLite database (created by leios-schedule-gen)"
          , metavar "PATH"
          ]
    leiosScheduleFile <-
      strOption $
        mconcat
          [ long "leios-schedule"
          , help
              "Path to the JSON schedule (produced by leios-schedule-gen) specifying when to send Leios offers"
          , metavar "PATH"
          ]
    pure
      Opts
        { immDBDir
        , port
        , address
        , configFile
        , refSlotNr
        , refTimeForRefSlot
        , leiosDbFile
        , leiosScheduleFile
        }
