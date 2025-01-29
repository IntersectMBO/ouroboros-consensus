{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Run
  ( initialize
  , synthesize
  ) where

import Cardano.Api.Any (displayError)
import Cardano.Node.Protocol.Cardano (mkConsensusProtocolCardano)
import Cardano.Node.Types
import Cardano.Tools.DBSynthesizer.Forging
import Cardano.Tools.DBSynthesizer.Orphans ()
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
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
  ( ShelleyGenesis (..)
  , validateGenesis
  )
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (getTipPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.V1.Args as LedgerDB.V1
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.Block
import Ouroboros.Network.Point (WithOrigin (..))
import System.Directory
import System.FilePath (takeDirectory, (</>))

initialize ::
  NodeFilePaths ->
  NodeCredentials ->
  DBSynthesizerOptions ->
  IO (Either String (DBSynthesizerConfig, CardanoProtocolParams StandardCrypto))
initialize NodeFilePaths{nfpConfig, nfpChainDB} creds synthOptions = do
  relativeToConfig :: (FilePath -> FilePath) <-
    (</>) . takeDirectory <$> makeAbsolute nfpConfig
  runExceptT $ do
    conf <- initConf relativeToConfig
    proto <- initProtocol relativeToConfig conf
    pure (conf, proto)
 where
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
    dijkstraConfig = NodeDijkstraProtocolConfiguration (GenesisFile $ ncsDijkstraGenesisFile confConfigStub) Nothing
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
    GenTxs (CardanoBlock StandardCrypto) mk
  ) ->
  DBSynthesizerConfig ->
  (CardanoProtocolParams StandardCrypto) ->
  IO ForgeResult
synthesize genTxs DBSynthesizerConfig{confOptions, confShelleyGenesis, confDbDir} runP =
  withRegistry $ \registry -> do
    let
      epochSize = sgEpochLength confShelleyGenesis
      chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage pInfoConfig)
      bss = LedgerDB.V1.V1Args LedgerDB.V1.DisableFlushing InMemoryBackingStoreArgs
      flavargs = LedgerDB.LedgerDbFlavorArgsV1 bss
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
          $ ChainDB.defaultArgs

    (_, forgers) <- allocate registry (const $ mkForgers nullTracer) (mapM_ BlockForging.finalize)
    let fCount = length forgers
    putStrLn $ "--> forger count: " ++ show fCount
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
 where
  DBSynthesizerOptions
    { synthOpenMode
    , synthLimit
    } = confOptions
  ( ProtocolInfo
      { pInfoConfig
      , pInfoInitLedger
      }
    , mkForgers
    ) = protocolInfoCardano runP

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
