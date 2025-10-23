{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import           Data.Void (absurd)
import           Main.Utf8 (withStdTerminalHandles)
import           Network.Socket (AddrInfo (addrFlags, addrSocketType))
import qualified Network.Socket as Socket
import           Options.Applicative (ParserInfo, execParser, fullDesc, help,
                     helper, info, long, metavar, progDesc, showDefault,
                     strOption, value, auto, option)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (At, Origin))
import qualified Data.Time.Clock.POSIX as POSIX
import Data.Time.Clock (DiffTime)
import Data.Int (Int64)

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    Opts {immDBDir, port, address, configFile, refSlotNr, refTimeForRefSlot} <- execParser optsParser

    let hints = Socket.defaultHints { addrFlags = [Socket.AI_NUMERICHOST], addrSocketType = Socket.Stream}
    addrInfo <- do
      addrInfos <- Socket.getAddrInfo (Just hints) (Just address) (Just port)
      case addrInfos of
        []         -> error "Invalid address or port"
        addrInfo:_ -> return addrInfo

    let args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    absurd <$> ImmDBServer.run immDBDir
                               (Socket.addrAddress addrInfo)
                               pInfoConfig
                               (mkGetSlotDelay refSlotNr refTimeForRefSlot)
    where
      -- NB we assume for now the slot duration is 1 second.
      --
      -- If we want to this in the actual chain we will need to access
      -- the information from the configuration file to run the
      -- Qry.slotToWallclock query.
      mkGetSlotDelay :: SlotNo -> POSIX.POSIXTime -> WithOrigin SlotNo -> IO DiffTime
      mkGetSlotDelay refSlotNr refTimeForRefSlot =
        -- If slot < refSlotNr, we need to subtract to
        -- refTimeForRefSlot. To simplify the calculations we work
        -- with Int64
        let iRefSlotNr :: Int64
            iRefSlotNr = fromIntegral $ unSlotNo refSlotNr

            -- TODO: here is where we assume the slot duration of 1 second.
            toSeconds :: Int64 -> POSIX.POSIXTime
            toSeconds iSlot =  realToFrac iSlot
        in \case Origin  -> pure 0 -- TODO: I'm not sure what we want to do here.
                 At slot -> do
                   let iSlot = fromIntegral $ unSlotNo slot
                       slotTime = refTimeForRefSlot + toSeconds (iSlot - iRefSlotNr)

                   currentTime <- POSIX.getPOSIXTime
                   pure $ if currentTime <= slotTime
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
      refTimeForRefSlot <- fmap asPOSIXseconds $ option auto $ mconcat
        [ long "initial-time"
        , help "UTC time for the reference slot, provided as POSIX seconds (Unix timestamp)"
        , metavar "POSIX_SECONDS"
        ]
      pure Opts {immDBDir, port, address, configFile, refSlotNr, refTimeForRefSlot}
        where
          asPOSIXseconds :: Double -> POSIX.POSIXTime
          asPOSIXseconds = realToFrac
