{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.Aeson as JSON
import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import           Data.Time.Clock (DiffTime)
import qualified Data.Time.Clock.POSIX as POSIX
import           Data.Void (absurd)
import           Main.Utf8 (withStdTerminalHandles)
import           Network.Socket (AddrInfo (addrFlags, addrSocketType))
import qualified Network.Socket as Socket
import           Options.Applicative (ParserInfo, execParser, fullDesc, help,
                     helper, info, long, metavar, progDesc, showDefault,
                     strOption, value, auto, option)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           System.Exit (die)
import qualified System.IO as SIO

main :: IO ()
main = withStdTerminalHandles $ do
    SIO.hSetBuffering SIO.stdout SIO.LineBuffering
    cryptoInit
    Opts {immDBDir, port, address, configFile, refSlotNr, refTimeForRefSlot, leiosDbFile, leiosScheduleFile} <- execParser optsParser
    let hints = Socket.defaultHints { addrFlags = [Socket.AI_NUMERICHOST], addrSocketType = Socket.Stream}
    addrInfo <- do
      addrInfos <- Socket.getAddrInfo (Just hints) (Just address) (Just port)
      case addrInfos of
        []         -> error "Invalid address or port"
        addrInfo:_ -> return addrInfo

    let args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args

    leiosSchedule <- JSON.eitherDecodeFileStrict leiosScheduleFile >>= \case
        Left err -> die $ "Failed to decode LeiosSchedule: " ++ err
        Right x -> pure x

    absurd <$> ImmDBServer.run immDBDir
                               (Socket.addrAddress addrInfo)
                               pInfoConfig
                               (mkGetSlotDelay refSlotNr refTimeForRefSlot)
                               leiosDbFile
                               (leiosSchedule :: ImmDBServer.LeiosSchedule)
    where
      -- NB we assume for now the slot duration is 1 second.
      --
      -- If we want to this in the actual chain we will need to access
      -- the information from the configuration file to run the
      -- Qry.slotToWallclock query.
      mkGetSlotDelay :: SlotNo -> POSIX.POSIXTime -> Double -> IO DiffTime
      mkGetSlotDelay refSlotNr refTimeForRefSlot =
        -- If slot < refSlotNr, we need to subtract to
        -- refTimeForRefSlot.
        let slotToPosix :: Double -> POSIX.POSIXTime
            slotToPosix = realToFrac -- TODO: here is where we assume the slot duration of 1 second.
        in \slotDbl -> do
                   let slotTime = refTimeForRefSlot + (slotToPosix slotDbl - slotToPosix (fromIntegral $ unSlotNo refSlotNr))
                   currentTime <- POSIX.getPOSIXTime
                   pure $ if currentTime < slotTime
                          then realToFrac $ slotTime - currentTime
                          else 0

data Opts = Opts {
    immDBDir   :: FilePath
  , port       :: String
  , address    :: String
  , configFile :: FilePath
  , refSlotNr :: SlotNo
  -- ^ Reference slot number. This, in combination with the reference
  -- time will be used to convert between slot number and wallclock time.
  -- N.B.: for now we assume the slot duration to be 1 second.
  , refTimeForRefSlot  :: POSIX.POSIXTime
  -- ^ Reference slot onset. Wallclock time that corresponds to the
  -- reference slot.
  , leiosDbFile :: FilePath
  -- ^ SQLite3 file storing the Leios data
  , leiosScheduleFile :: FilePath
  -- ^ JSON file encoding the 'ImmDBServer.LeiosSchedule'
  }

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Serve an ImmutableDB via ChainSync and BlockFetch"

    parse = do
      immDBDir <- strOption $ mconcat
        [ long "db"
        , help "Path to the ImmutableDB"
        , metavar "PATH"
        ]
      port <- strOption $ mconcat
        [ long "port"
        , help "Port to serve on"
        , value "3001"
        , showDefault
        ]
      address <- strOption $ mconcat
        [ long "address"
        , help "Address to serve on"
        , value "127.0.0.1"
        , showDefault
        ]
      configFile <- strOption $ mconcat
        [ long "config"
        , help "Path to config file, in the same format as for the node or db-analyser"
        , metavar "PATH"
        ]
      refSlotNr <- fmap SlotNo $ option auto $ mconcat
        [ long "initial-slot"
        , help "Reference slot number (SlotNo). This, together with the initial-time will be used for time translations."
        , metavar "SLOT_NO"
        ]
      refTimeForRefSlot <- fmap (fromInteger @POSIX.POSIXTime) $ option auto $ mconcat
        [ long "initial-time"
        , help "UTC time for the reference slot, provided as POSIX seconds (Unix timestamp)"
        , metavar "POSIX_SECONDS"
        ]
      leiosDbFile <- strOption $ mconcat
        [ long "leios-db"
        , help "Path to the Leios database"
        , metavar "PATH"
        ]
      leiosScheduleFile <- strOption $ mconcat
        [ long "leios-schedule"
        , help "Path to json file specifying when to send Leios offers"
        , metavar "PATH"
        ]
      pure Opts {immDBDir, port, address, configFile, refSlotNr, refTimeForRefSlot, leiosDbFile, leiosScheduleFile}
