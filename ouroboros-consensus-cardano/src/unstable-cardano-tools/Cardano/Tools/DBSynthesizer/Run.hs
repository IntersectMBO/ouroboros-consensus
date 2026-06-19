{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Run
  ( initialize
  , synthesize
  ) where

import Cardano.Api.Any (displayError)
import qualified Cardano.Chain.Update as Byron (ApplicationName (..))
import qualified Cardano.Configuration.File as Cfg
import qualified Cardano.Configuration.File.Protocol as Cfg
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Node.Protocol.Cardano (mkConsensusProtocolCardano)
import Cardano.Node.Types
import qualified Cardano.Slotting.Slot as Slot
import Cardano.Tools.DBSynthesizer.Forging
import Cardano.Tools.DBSynthesizer.Types
import Control.Monad (filterM)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  , runExceptT
  )
import Control.ResourceRegistry
import Control.Tracer
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Bool (bool)
import Data.Functor (($>))
import Data.Functor.Identity (runIdentity)
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Block.Forging as BlockForging
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config (TopLevelConfig, configStorage)
import qualified Ouroboros.Consensus.Node as Node (stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
  ( nodeImmutableDbChunkInfo
  )
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node (validateGenesis)
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
import System.FilePath (takeDirectory, (</>))
import System.Random (newStdGen)

initialize ::
  NodeFilePaths ->
  NodeCredentials ->
  DBSynthesizerOptions ->
  IO (Either String (DBSynthesizerConfig, CardanoProtocolParams StandardCrypto))
initialize NodeFilePaths{nfpConfig, nfpChainDB} creds synthOptions = do
  configDir <- takeDirectory <$> makeAbsolute nfpConfig
  let relativeToConfig :: FilePath -> FilePath
      relativeToConfig = (configDir </>)
  runExceptT $ do
    -- The node configuration is parsed by the shared cardano-config package.
    -- Genesis file paths in the configuration are relative to the
    -- configuration file's directory.
    ncff <- handleIOExceptT show (Cfg.parseConfigurationFiles nfpConfig)
    let protoCfg = runIdentity (Cfg.protocolConfiguration ncff)
        testCfg = runIdentity (Cfg.testingConfiguration ncff)

    shelleyGenesis <-
      readFileJson (relativeToConfig (Cfg.hashed (Cfg.shelleyGenesis protoCfg)))
    _ <- hoistEither $ validateGenesis shelleyGenesis

    let protocolCredentials =
          ProtocolFilepaths
            { byronCertFile = Nothing
            , byronKeyFile = Nothing
            , shelleyKESFile = credKESFile creds
            , shelleyVRFFile = credVRFFile creds
            , shelleyCertFile = credCertFile creds
            , shelleyBulkCredsFile = credBulkFile creds
            }
        conf =
          DBSynthesizerConfig
            { confOptions = synthOptions
            , confProtocolCredentials = protocolCredentials
            , confShelleyGenesis = shelleyGenesis
            , confDbDir = nfpChainDB
            , confNodeConfigDir = configDir
            }

    proto <-
      firstExceptT displayError $
        mkConsensusProtocolCardano
          (byronProtocolConfiguration relativeToConfig protoCfg)
          (NodeShelleyProtocolConfiguration (genesisFile relativeToConfig (Cfg.shelleyGenesis protoCfg)) Nothing)
          (NodeAlonzoProtocolConfiguration (genesisFile relativeToConfig (Cfg.alonzoGenesis protoCfg)) Nothing)
          (NodeConwayProtocolConfiguration (genesisFile relativeToConfig (Cfg.conwayGenesis protoCfg)) Nothing)
          ( (\h -> NodeDijkstraProtocolConfiguration (genesisFile relativeToConfig h) Nothing)
              <$> Cfg.experimentalGenesis testCfg
          )
          (hardForkProtocolConfiguration testCfg)
          (Just protocolCredentials)
    pure (conf, proto)

readFileJson :: FromJSON a => FilePath -> ExceptT String IO a
readFileJson f = handleIOExceptT show (eitherDecodeFileStrict' f) >>= hoistEither

-- | The genesis file path from a 'Cfg.Hashed' entry, made relative to the
-- configuration file's directory.
genesisFile :: (FilePath -> FilePath) -> Cfg.Hashed FilePath -> GenesisFile
genesisFile relativeToConfig = GenesisFile . relativeToConfig . Cfg.hashed

-- | Adapt the @cardano-config@ byron-era configuration into the
-- 'NodeByronProtocolConfiguration' that 'mkConsensusProtocolCardano' expects.
-- The byron software version is hard-coded, mirroring the node.
byronProtocolConfiguration ::
  (FilePath -> FilePath) ->
  Cfg.ProtocolConfiguration Maybe ->
  NodeByronProtocolConfiguration
byronProtocolConfiguration relativeToConfig protoCfg =
  NodeByronProtocolConfiguration
    { npcByronGenesisFile = genesisFile relativeToConfig (Cfg.byronGenesisFile byronCfg)
    , npcByronGenesisFileHash = GenesisHash <$> Cfg.hash (Cfg.byronGenesisFile byronCfg)
    , npcByronReqNetworkMagic = parseRequiresNetworkMagic (Cfg.byronReqNetworkMagic byronCfg)
    , npcByronPbftSignatureThresh = Cfg.byronPbftSignatureThresh byronCfg
    , npcByronApplicationName = Byron.ApplicationName "cardano-sl"
    , npcByronApplicationVersion = 1
    , npcByronSupportedProtocolVersionMajor = Cfg.byronSupportedProtocolVersionMajor byronCfg
    , npcByronSupportedProtocolVersionMinor = Cfg.byronSupportedProtocolVersionMinor byronCfg
    , npcByronSupportedProtocolVersionAlt = Cfg.byronSupportedProtocolVersionAlt byronCfg
    }
 where
  byronCfg = Cfg.byronGenesis protoCfg

-- | Adapt the @cardano-config@ testing configuration into the
-- 'NodeHardForkProtocolConfiguration' that 'mkConsensusProtocolCardano'
-- expects.
hardForkProtocolConfiguration ::
  Cfg.TestingConfiguration -> NodeHardForkProtocolConfiguration
hardForkProtocolConfiguration testCfg =
  NodeHardForkProtocolConfiguration
    { npcTestEnableDevelopmentHardForkEras = Cfg.experimentalHardForksEnabled testCfg
    , npcTestShelleyHardForkAtEpoch = Slot.EpochNo <$> Cfg.testShelleyHardForkAtEpoch testCfg
    , npcTestAllegraHardForkAtEpoch = Slot.EpochNo <$> Cfg.testAllegraHardForkAtEpoch testCfg
    , npcTestMaryHardForkAtEpoch = Slot.EpochNo <$> Cfg.testMaryHardForkAtEpoch testCfg
    , npcTestAlonzoHardForkAtEpoch = Slot.EpochNo <$> Cfg.testAlonzoHardForkAtEpoch testCfg
    , npcTestBabbageHardForkAtEpoch = Slot.EpochNo <$> Cfg.testBabbageHardForkAtEpoch testCfg
    , npcTestConwayHardForkAtEpoch = Slot.EpochNo <$> Cfg.testConwayHardForkAtEpoch testCfg
    , npcTestDijkstraHardForkAtEpoch = Slot.EpochNo <$> Cfg.testDijkstraHardForkAtEpoch testCfg
    }

parseRequiresNetworkMagic :: String -> RequiresNetworkMagic
parseRequiresNetworkMagic magic = case magic of
  "RequiresNoMagic" -> RequiresNoMagic
  "RequiresMagic" -> RequiresMagic
  other -> error $ "initialize: unknown RequiresNetworkMagic value: " <> other

-- | Forge a ChainDB from a ready-made Cardano 'ProtocolInfo' and its block
-- forgers (as produced by 'protocolInfoCardano'). Constructing the protocol
-- from a node configuration is the caller's responsibility, keeping this
-- function free of any node/api configuration machinery.
synthesize ::
  ( TopLevelConfig (CardanoBlock StandardCrypto) ->
    GenTxs (CardanoBlock StandardCrypto)
  ) ->
  DBSynthesizerOptions ->
  Slot.EpochSize ->
  -- | The directory of the ChainDB to forge into.
  FilePath ->
  ( ProtocolInfo (CardanoBlock StandardCrypto)
  , Tracer IO KESAgentClientTrace ->
    IO [BlockForging.MkBlockForging IO (CardanoBlock StandardCrypto)]
  ) ->
  IO ForgeResult
synthesize genTxs confOptions epochSize confDbDir (ProtocolInfo{pInfoConfig, pInfoInitLedger}, mkForgers) =
  withRegistry $ \registry -> do
    snapshotDelayRng <- newStdGen
    let
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
        fail $ loc ++ " already exists. Use -f to overwrite or -a to append."
      OpenAppend
        | isChainDB ->
            pure ()
      OpenCreateForce
        | isChainDB ->
            removePathForcibly db >> create
      _ ->
        fail $
          loc
            ++ " is non-empty and does not look like a ChainDB"
              <> " (i.e. it contains directories other than"
              <> " 'immutable'/'ledger'/'volatile'/'gsm'). Aborting."

  listSubdirectories path = filterM isDir =<< listDirectory path
   where
    isDir p = doesDirectoryExist (path </> p)
