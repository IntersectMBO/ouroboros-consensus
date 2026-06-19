{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Run
  ( synthesize
  ) where

import qualified Cardano.Slotting.Slot as Slot
import Cardano.Tools.DBSynthesizer.Forging
import Cardano.Tools.DBSynthesizer.Types
import Control.Monad (filterM)
import Control.ResourceRegistry
import Control.Tracer
import Data.Bool (bool)
import Data.Functor (($>))
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Block.Forging as BlockForging
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (TopLevelConfig, configStorage)
import qualified Ouroboros.Consensus.Node as Node (stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
  ( nodeImmutableDbChunkInfo
  )
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
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
import System.FilePath ((</>))
import System.Random (newStdGen)

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
