{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.DBSynthesizer.Run
  ( CardanoProtocol
  , initialize
  , synthesize
  ) where

import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Configuration as Cfg
import qualified Cardano.Configuration.CliArgs as CLI
import qualified Cardano.Ledger.Api.Era as L
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Tools.Config
  ( mkHardForkTriggers
  , mkInitialNonce
  , mkTransitionConfig
  , resolveNodeConfigurationWith
  , throwConfigError
  )
import Cardano.Tools.Credentials (LeaderCredentials)
import qualified Cardano.Tools.Credentials as Creds
import Cardano.Tools.DBSynthesizer.Forging
import Cardano.Tools.DBSynthesizer.Types
import Control.Monad (filterM)
import Control.ResourceRegistry
import Control.Tracer
import Data.Bool (bool)
import Data.Functor (($>))
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Block.Forging as BlockForging
import Ouroboros.Consensus.Cardano
  ( ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  )
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
  ( CardanoHardForkTriggers
  , CardanoProtocolParams (..)
  , protocolInfoCardano
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configStorage
  , emptyCheckpointsMap
  )
import qualified Ouroboros.Consensus.Node as Node (stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
  ( nodeImmutableDbChunkInfo
  )
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
  ( ShelleyGenesis (..)
  , validateGenesis
  )
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (getTipPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.Block hiding (GenesisHash)
import Ouroboros.Network.Point (WithOrigin (..))
import System.Directory
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (MountPoint))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen)

-- | A Cardano protocol ready to forge with: the 'ProtocolInfo' and the block
-- forgers that 'protocolInfoCardano' produces alongside it.
type CardanoProtocol =
  ( ProtocolInfo (CardanoBlock StandardCrypto)
  , Tracer IO KESAgentClientTrace ->
    IO [BlockForging.MkBlockForging IO (CardanoBlock StandardCrypto)]
  )

-- | Build the protocol to forge with from a node configuration file and the
-- forging credentials it is given on the command line.
--
-- Everything that reads a file is @cardano-config@'s: it parses and resolves
-- the configuration, loads every era's genesis, and decodes the credentials.
-- What happens here is the mapping onto the consensus types, which is the part
-- that cannot live below the consensus layer.
--
-- Any problem with the configuration or the credentials is thrown as a
-- 'Cardano.Tools.Config.ConfigError', like in the other tools, so that the user
-- gets a plain message rather than a backtrace.
--
-- The Shelley genesis is returned alongside the protocol because 'synthesize'
-- needs its epoch length.
initialize ::
  -- | The node configuration file.
  FilePath ->
  -- | The credential files, as named on the command line.
  Cfg.Credentials ->
  IO (ShelleyGenesis, CardanoProtocol)
initialize configFile creds = do
  (nc, warns) <- resolveNodeConfigurationWith cliArgs
  mapM_ (hPutStrLn stderr . ("WARNING: " ++) . show) warns

  triggers <- either throwConfigError pure $ mkHardForkTriggers (Cfg.testingConfiguration nc)
  let shelleyGenesis = Cfg.shelleyGenesisConfig nc
  either throwConfigError pure $ validateGenesis shelleyGenesis
  leaderCredentials <-
    either throwConfigError pure
      =<< Creds.readLeaderCredentials (Cfg.byronGenesisConfig nc) (Cfg.credentials nc)

  -- The same filesystem db-analyser mounts: rooted at the configuration file's
  -- directory, which is what the paths in a node configuration are relative to.
  configDir <- takeDirectory <$> makeAbsolute configFile
  let fs = SomeHasFS (ioHasFS (MountPoint configDir))

  protocol <- protocolInfoCardano fs (protocolParams nc triggers leaderCredentials)
  pure (shelleyGenesis, protocol)
 where
  -- db-synthesizer has no node command line of its own beyond the credentials,
  -- so everything else in the configuration comes from the file.
  cliArgs = (CLI.defaultCliArgs configFile){CLI.credentials = creds}

protocolParams ::
  Cfg.NodeConfiguration ->
  CardanoHardForkTriggers ->
  LeaderCredentials ->
  CardanoProtocolParams StandardCrypto
protocolParams nc triggers leaderCredentials =
  CardanoProtocolParams
    ProtocolParamsByron
      { byronGenesis = Cfg.byronGenesisConfig nc
      , -- Not modelled by cardano-config; the node's own default is to leave the
        -- genesis-imposed threshold alone.
        byronPbftSignatureThreshold = Nothing
      , -- These two are what a forged Byron block announces about the software
        -- that made it. cardano-config no longer models them either, so we
        -- announce what a stock cardano-node announces.
        byronProtocolVersion = Byron.Update.ProtocolVersion 3 0 0
      , byronSoftwareVersion =
          Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "cardano-sl") 1
      , byronLeaderCredentials = Creds.byronLeaderCredentials leaderCredentials
      }
    ProtocolParamsShelleyBased
      { shelleyBasedInitialNonce = mkInitialNonce nc
      , shelleyBasedLeaderCredentials = Creds.shelleyLeaderCredentials leaderCredentials
      }
    triggers
    (mkTransitionConfig nc)
    emptyCheckpointsMap
    -- The greatest protocol version we can forge in, ie the latest era we know
    -- about. db-analyser uses the same, so that it can validate what we forge.
    (ProtVer (L.eraProtVerHigh @L.LatestKnownEra) 0)

-- | Forge a ChainDB from a ready-made Cardano 'ProtocolInfo' and its block
-- forgers (as produced by 'initialize'). Constructing the protocol from a node
-- configuration is the caller's responsibility, keeping this function free of
-- any configuration machinery. In particular, the caller is also responsible
-- for having validated the genesis (see
-- 'Ouroboros.Consensus.Shelley.Node.validateGenesis').
synthesize ::
  ( TopLevelConfig (CardanoBlock StandardCrypto) ->
    GenTxs (CardanoBlock StandardCrypto)
  ) ->
  DBSynthesizerOptions ->
  -- | The same Shelley genesis the 'ProtocolInfo' was built from; only its
  -- epoch length is used, to interpret a 'ForgeLimitEpoch'.
  ShelleyGenesis ->
  -- | The directory of the ChainDB to forge into.
  FilePath ->
  CardanoProtocol ->
  IO ForgeResult
synthesize genTxs confOptions shelleyGenesis confDbDir (ProtocolInfo{pInfoConfig, pInfoInitLedger}, mkForgers) =
  withRegistry $ \registry -> do
    snapshotDelayRng <- newStdGen
    let
      epochSize = sgEpochLength shelleyGenesis
      chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage pInfoConfig)
      flavargs = LedgerDB.LedgerDbBackendArgsV2 $ SomeBackendArgs InMemArgs
      dbArgs =
        ChainDB.completeChainDbArgs
          registry
          pInfoConfig
          pInfoInitLedger
          chunkInfo
          (const True)
          (Node.stdMkChainDbHasFS confDbDir)
          (Node.stdMkChainDbHasFS confDbDir)
          snapshotDelayRng
          flavargs
          $ ChainDB.defaultArgs

    mbfs <- mkForgers nullTracer
    allocatedForgers <-
      traverse
        (\mbf -> allocate registry (const (BlockForging.mkBlockForging mbf)) BlockForging.finalize)
        mbfs
    let forgers = snd <$> allocatedForgers
    let fCount = length forgers
    putStrLn $ "--> forger count: " ++ show fCount
    r <-
      if fCount > 0
        then do
          putStrLn $ "--> opening ChainDB on file system with mode: " ++ show synthOpenMode
          preOpenChainDB synthOpenMode confDbDir
          let dbTracer = nullTracer
          ChainDB.withDB (ChainDB.updateTracer dbTracer dbArgs) $ \chainDB -> do
            slotNo <- do
              tip <- atomically (ChainDB.getTipPoint chainDB)
              pure $ case pointSlot tip of
                Origin -> 0
                At s -> succ s

            putStrLn $ "--> starting at: " ++ show slotNo
            runForge epochSize slotNo synthLimit chainDB forgers pInfoConfig $ genTxs pInfoConfig
        else do
          putStrLn "--> no forgers found; leaving possibly existing ChainDB untouched"
          pure $ ForgeResult 0
    mapM_ (release . fst) allocatedForgers $> r
 where
  DBSynthesizerOptions
    { synthOpenMode
    , synthLimit
    } = confOptions

preOpenChainDB :: DBSynthesizerOpenMode -> FilePath -> IO ()
preOpenChainDB mode db =
  doesDirectoryExist db >>= bool create checkMode
 where
  checkIsDB ls = Set.fromList ls `Set.isSubsetOf` chainDBDirs
  chainDBDirs = Set.fromList ["immutable", "ledger", "volatile", "gsm"]
  loc = "preOpenChainDB: '" ++ db ++ "'"
  create = createDirectoryIfMissing True db
  checkMode = do
    isChainDB <- checkIsDB <$> listSubdirectories db
    case mode of
      OpenCreate ->
        throwConfigError $ loc ++ " already exists. Use -f to overwrite or -a to append."
      OpenAppend
        | isChainDB ->
            pure ()
      OpenCreateForce
        | isChainDB ->
            removePathForcibly db >> create
      _ ->
        throwConfigError $
          loc
            ++ " is non-empty and does not look like a ChainDB"
              <> " (i.e. it contains directories other than"
              <> " 'immutable'/'ledger'/'volatile'/'gsm'). Aborting."

  listSubdirectories path = filterM isDir =<< listDirectory path
   where
    isDir p = doesDirectoryExist (path </> p)
