{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , encode
  , fromJSON
  , (.=)
  )
import Data.Bool (bool)
import Data.ByteString as BS (ByteString, readFile)
import qualified Data.ByteString.Lazy.Char8 as BSL8 (unpack)
import Data.Functor (($>))
import qualified Data.Set as Set
import LeiosDemoDb (newLeiosDBSQLite, withLeiosDb)
import LeiosDemoTypes
  ( TraceLeiosKernel (TraceLeiosDb)
  , traceLeiosKernelToObject
  )
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
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
import Ouroboros.Consensus.Util.IOLike
  ( atomically
  , bracket_
  , diffTime
  , getMonotonicTime
  , newMVar
  , putMVar
  , takeMVar
  )
import Ouroboros.Network.Block
import Ouroboros.Network.Point (WithOrigin (..))
import System.Directory
import System.FS.API (MountPoint (..), SomeHasFS (..))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hPutStrLn, stderr)
import Text.Printf (printf)

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
    votingKey <- traverse (ExceptT . readBlsSigningKey) (credBlsFile creds)
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
        , confVotingKey = votingKey
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

-- | Write each Leios event to stderr as one JSON object.
--
-- The object is the one cardano-node writes to its log. This tracer
-- adds one field, @at@. It holds the seconds from the start of the
-- run to the event.
mkLeiosTracer :: IO (Tracer IO TraceLeiosKernel)
mkLeiosTracer = do
  startTime <- getMonotonicTime
  -- The ChainDB gives this tracer to every LeiosDb connection it opens, so a
  -- thread other than the forge loop can reach it. One write per lock keeps
  -- each event on a line of its own.
  lock <- newMVar ()
  let withLock = bracket_ (takeMVar lock) (putMVar lock ())
  pure $ Tracer . emit $ \ev -> withLock $ do
    now <- getMonotonicTime
    let at = realToFrac (diffTime now startTime) :: Double
    hPutStrLn stderr $
      BSL8.unpack $
        Aeson.encode $
          -- The seconds are a string with six decimals, because
          -- aeson writes a number below 0.1 in exponent form.
          ("at" .= (printf "%.6f" at :: String)) <> traceLeiosKernelToObject ev
    hFlush stderr

synthesize ::
  ( TopLevelConfig (CardanoBlock StandardCrypto) ->
    GenTxs (CardanoBlock StandardCrypto)
  ) ->
  DBSynthesizerConfig ->
  (CardanoProtocolParams StandardCrypto) ->
  IO ForgeResult
synthesize genTxs DBSynthesizerConfig{confOptions, confShelleyGenesis, confDbDir, confVotingKey} runP =
  withRegistry $ \registry -> do
    -- The node writes its LeiosDb next to the other ChainDB files.
    -- The tool derives that path from --db. That is also where
    -- db-analyser looks for it.
    leiosTracer <- mkLeiosTracer
    leiosDbHandle <-
      newLeiosDBSQLite (TraceLeiosDb >$< leiosTracer) (confDbDir </> "leios.db")
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
              runForge
                epochSize
                slotNo
                synthLimit
                chainDB
                forgers
                pInfoConfig
                confVotingKey
                (genTxs pInfoConfig)
                leiosDb
                leiosTracer
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
