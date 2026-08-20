{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Run
  ( initialize
  , synthesize
  ) where

import Cardano.Api.Any (displayError)
import Cardano.Node.Protocol.Cardano (mkConsensusProtocolCardano)
import Cardano.Node.Types
import Cardano.Tools.DBSynthesizer.BlsKey (readBlsSigningKey)
import Cardano.Tools.DBSynthesizer.Forging
import Cardano.Tools.DBSynthesizer.Orphans ()
import Cardano.Tools.DBSynthesizer.Types
import Control.Monad (filterM)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  , runExceptT
  )
import Control.ResourceRegistry
import Control.Tracer
import Data.Aeson as Aeson
  ( FromJSON
  , Result (..)
  , Value
  , eitherDecodeFileStrict'
  , eitherDecodeStrict'
  , fromJSON
  )
import Data.Bool (bool)
import Data.ByteString as BS (ByteString, readFile)
import Data.Functor (($>))
import qualified Data.Set as Set
import LeiosDemoDb (newLeiosDBSQLite, withLeiosDb)
import qualified Ouroboros.Consensus.Block.Forging as BlockForging
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configStorage
  , topLevelConfigVotingKey
  )
import qualified Ouroboros.Consensus.Node as Node (stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
  ( nodeImmutableDbChunkInfo
  )
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosCanBeLeader (praosCanBeLeaderSignKeyBLS)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
  ( ProtocolParamsShelleyBased (shelleyBasedLeaderCredentials)
  , ShelleyGenesis (..)
  , ShelleyLeaderCredentials (shelleyLeaderCredentialsCanBeLeader)
  , validateGenesis
  )
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (getTipPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.Block
import Ouroboros.Network.Point (WithOrigin (..))
import System.Directory
import System.FS.API (MountPoint (..), SomeHasFS (..))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory, (</>))

initialize ::
  NodeFilePaths ->
  NodeCredentials ->
  DBSynthesizerOptions ->
  IO (Either String (DBSynthesizerConfig, CardanoProtocolParams StandardCrypto))
initialize NodeFilePaths{nfpConfig, nfpChainDB, nfpBlsKey} creds synthOptions = do
  relativeToConfig :: (FilePath -> FilePath) <-
    (</>) . takeDirectory <$> makeAbsolute nfpConfig
  runExceptT $ do
    conf <- initConf relativeToConfig
    proto <- initProtocol relativeToConfig conf >>= withVotingKey
    pure (conf, proto)
 where
  -- 'mkPraosLeaderCredentials' builds every credential with
  -- 'praosCanBeLeaderSignKeyBLS = Nothing', and the files it reads hold no BLS
  -- key. 'CardanoProtocolParams' is a plain record, so the key goes in here
  -- instead of in that vendored module. 'protocolInfoCardano' then puts it in
  -- 'topLevelConfigVotingKey'.
  withVotingKey ::
    CardanoProtocolParams StandardCrypto ->
    ExceptT String IO (CardanoProtocolParams StandardCrypto)
  withVotingKey proto = case nfpBlsKey of
    Nothing -> pure proto
    Just path -> do
      votingKey <- ExceptT (readBlsSigningKey path)
      let shelleyBased = shelleyBasedProtocolParams proto
          setKey credentials =
            credentials
              { shelleyLeaderCredentialsCanBeLeader =
                  (shelleyLeaderCredentialsCanBeLeader credentials)
                    { praosCanBeLeaderSignKeyBLS = Just votingKey
                    }
              }
      pure
        proto
          { shelleyBasedProtocolParams =
              shelleyBased
                { shelleyBasedLeaderCredentials =
                    map setKey (shelleyBasedLeaderCredentials shelleyBased)
                }
          }

  initConf :: (FilePath -> FilePath) -> ExceptT String IO DBSynthesizerConfig
  initConf relativeToConfig = do
    inp <- handleIOExceptT show (BS.readFile nfpConfig)
    configStub <- adjustFilePaths relativeToConfig <$> readJson inp
    shelleyGenesis <- readFileJson $ ncsShelleyGenesisFile configStub
    _ <- hoistEither $ validateGenesis shelleyGenesis
    let
      protocolCredentials =
        ProtocolFilepaths
          { byronCertFile = Nothing
          , byronKeyFile = Nothing
          , shelleyKESFile = credKESFile creds
          , shelleyVRFFile = credVRFFile creds
          , shelleyCertFile = credCertFile creds
          , shelleyBulkCredsFile = credBulkFile creds
          }
    pure
      DBSynthesizerConfig
        { confConfigStub = configStub
        , confOptions = synthOptions
        , confProtocolCredentials = protocolCredentials
        , confShelleyGenesis = shelleyGenesis
        , confDbDir = nfpChainDB
        }

  initProtocol ::
    (FilePath -> FilePath) ->
    DBSynthesizerConfig ->
    ExceptT String IO (CardanoProtocolParams StandardCrypto)
  initProtocol relativeToConfig DBSynthesizerConfig{confConfigStub, confProtocolCredentials} = do
    hfConfig :: NodeHardForkProtocolConfiguration <-
      hoistEither hfConfig_
    byronConfig :: NodeByronProtocolConfiguration <-
      adjustFilePaths relativeToConfig <$> hoistEither byConfig_

    firstExceptT displayError $
      mkConsensusProtocolCardano
        byronConfig
        shelleyConfig
        alonzoConfig
        conwayConfig
        dijkstraConfig
        hfConfig
        (Just confProtocolCredentials)
   where
    shelleyConfig = NodeShelleyProtocolConfiguration (GenesisFile $ ncsShelleyGenesisFile confConfigStub) Nothing
    alonzoConfig = NodeAlonzoProtocolConfiguration (GenesisFile $ ncsAlonzoGenesisFile confConfigStub) Nothing
    conwayConfig = NodeConwayProtocolConfiguration (GenesisFile $ ncsConwayGenesisFile confConfigStub) Nothing
    dijkstraConfig =
      fmap
        (\x -> NodeDijkstraProtocolConfiguration (GenesisFile x) Nothing)
        (ncsDijkstraGenesisFile confConfigStub)
    hfConfig_ = eitherParseJson $ ncsNodeConfig confConfigStub
    byConfig_ = eitherParseJson $ ncsNodeConfig confConfigStub

readJson :: (Monad m, FromJSON a) => ByteString -> ExceptT String m a
readJson = hoistEither . eitherDecodeStrict'

readFileJson :: FromJSON a => FilePath -> ExceptT String IO a
readFileJson f = handleIOExceptT show (eitherDecodeFileStrict' f) >>= hoistEither

eitherParseJson :: FromJSON a => Aeson.Value -> Either String a
eitherParseJson v = case fromJSON v of
  Error err -> Left err
  Success a -> Right a

synthesize ::
  ( TopLevelConfig (CardanoBlock StandardCrypto) ->
    GenTxs (CardanoBlock StandardCrypto)
  ) ->
  DBSynthesizerConfig ->
  (CardanoProtocolParams StandardCrypto) ->
  IO ForgeResult
synthesize genTxs DBSynthesizerConfig{confOptions, confShelleyGenesis, confDbDir} runP =
  withRegistry $ \registry -> do
    -- The node writes its LeiosDb next to the other ChainDB files.
    -- The tool derives that path from --db. That is also where
    -- db-analyser looks for it.
    leiosDbHandle <- newLeiosDBSQLite nullTracer (confDbDir </> "leios.db")
    (ProtocolInfo{pInfoConfig, pInfoInitLedger}, mkForgers) <-
      protocolInfoCardano (SomeHasFS (ioHasFS (MountPoint confDbDir))) runP
    let
      epochSize = sgEpochLength confShelleyGenesis
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
          flavargs
          leiosDbHandle
          $ ChainDB.defaultArgs

    putStrLn $
      "--> voting key: "
        ++ maybe "absent" (const "present") (topLevelConfigVotingKey pInfoConfig)

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
          -- Open after 'preOpenChainDB'. That call creates the db directory, and
          -- with -f it deletes and recreates it. An earlier open loses the file
          -- with no error.
          withLeiosDb leiosDbHandle $ \leiosDb ->
            ChainDB.withDB (ChainDB.updateTracer dbTracer dbArgs) $ \chainDB -> do
              slotNo <- do
                tip <- atomically (ChainDB.getTipPoint chainDB)
                pure $ case pointSlot tip of
                  Origin -> 0
                  At s -> succ s

              putStrLn $ "--> starting at: " ++ show slotNo
              runForge epochSize slotNo synthLimit chainDB forgers pInfoConfig (genTxs pInfoConfig) leiosDb
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
