{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.ImmDBServer.Diffusion (run, LeiosSchedule (..)) where

import qualified Data.Aeson as Aeson
import qualified Cardano.Tools.ImmDBServer.MiniProtocols as MP
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.ResourceRegistry
import           Control.Tracer
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Void (Void)
import           Data.Word (Word32, Word64)
import qualified Database.SQLite3.Direct as DB
import           GHC.Generics (Generic)
import qualified Network.Mux as Mux
import           Network.Socket (SockAddr (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Node.InitStorage
                     (NodeInitStorage (nodeCheckIntegrity, nodeImmutableDbChunkInfo))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                     (decodeRemoteAddress, encodeRemoteAddress)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (configureSocket)
import qualified System.Directory as Dir
import           System.Exit (die)
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)

import qualified LeiosDemoLogic as LeiosLogic
import qualified LeiosDemoTypes as Leios

-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
     SockAddr
  -> N2N.Versions N2N.NodeToNodeVersion N2N.NodeToNodeVersionData
       (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode SockAddr BL.ByteString IO Void ())
  -> IO Void
serve sockAddr application = withIOManager \iocp -> do
    let sn     = Snocket.socketSnocket iocp
        family = Snocket.addrFamily sn sockAddr
    bracket (Snocket.open sn family) (Snocket.close sn) \socket -> do
      networkMutableState <- N2N.newNetworkMutableState
      configureSocket socket (Just sockAddr)
      Snocket.bind sn socket sockAddr
      Snocket.listen sn socket
      N2N.withServer
        sn
        N2N.nullNetworkServerTracers {
          N2N.nstHandshakeTracer   = show >$< stdoutTracer
        , N2N.nstErrorPolicyTracer = show >$< stdoutTracer
        }
        networkMutableState
        acceptedConnectionsLimit
        socket
        application
        nullErrorPolicies
  where
    acceptedConnectionsLimit = N2N.AcceptedConnectionsLimit {
          N2N.acceptedConnectionsHardLimit = maxBound
        , N2N.acceptedConnectionsSoftLimit = maxBound
        , N2N.acceptedConnectionsDelay     = 0
        }

run ::
     forall blk.
     ( GetPrevHash blk
     , ShowProxy blk
     , SupportedNetworkProtocolVersion blk
     , SerialiseNodeToNodeConstraints blk
     , ImmutableDB.ImmutableDbSerialiseConstraints blk
     , NodeInitStorage blk
     , ConfigSupportsNode blk
     )
  => FilePath
  -> SockAddr
  -> TopLevelConfig blk
  -> (Double -> IO DiffTime)
  -> FilePath
  -> LeiosSchedule
  -> IO Void
run immDBDir sockAddr cfg getSlotDelay leiosDbFile leiosSchedule = withRegistry \registry ->do
    let mkLeiosNotifyContext registry' = do
          -- each LeiosNotify server calls this when it initializes
          leiosMailbox <- MVar.newEmptyMVar
          let leiosNotifyContext = MP.MkLeiosNotifyContext { MP.leiosMailbox }
          _threadId <- forkLinkedThread registry' "LeiosScheduler" (leiosScheduler getSlotDelay leiosNotifyContext leiosSchedule)
          pure leiosNotifyContext
    let mkLeiosFetchContext = do
          -- each LeiosFetch server calls this when it initializes
          Dir.doesFileExist leiosDbFile >>= \case
              False -> die $ "The Leios database must already exist: " <> show leiosDbFile
              True -> pure ()
          leiosDb <- DB.open (fromString leiosDbFile) >>= \case
              Left (_err, utf8) -> die $ show utf8
              Right x -> pure $ Leios.leiosDbFromSqliteDirect x
          leiosEbBodies <- LeiosLogic.loadEbBodies leiosDb
          fmap LeiosLogic.MkSomeLeiosFetchContext
            $ LeiosLogic.newLeiosFetchContext
                  leiosDb
                  (pure leiosEbBodies)
    ImmutableDB.withDB
      (ImmutableDB.openDB (immDBArgs registry) runWithTempRegistry)
      \immDB -> serve sockAddr $ MP.immDBServer
        codecCfg
        encodeRemoteAddress
        decodeRemoteAddress
        immDB
        networkMagic
        (getSlotDelay . fromIntegral . unSlotNo)
        mkLeiosNotifyContext
        mkLeiosFetchContext
  where
    immDBArgs registry = ImmutableDB.defaultArgs {
          immCheckIntegrity = nodeCheckIntegrity storageCfg
        , immChunkInfo      = nodeImmutableDbChunkInfo storageCfg
        , immCodecConfig    = codecCfg
        , immRegistry       = registry
        , immHasFS          = SomeHasFS $ ioHasFS $ MountPoint immDBDir
        }

    codecCfg     = configCodec cfg
    storageCfg   = configStorage cfg
    networkMagic = getNetworkMagic . configBlock $ cfg

-----

data LeiosSchedule = MkLeiosSchedule [(Double, (Word64, T.Text, Maybe Word32))]
  deriving (Generic)

-- | Deriving via "GHC.Generics"
instance Aeson.FromJSON LeiosSchedule

leiosScheduler ::
    (Double -> IO DiffTime)
 ->
    MP.LeiosNotifyContext IO
 ->
    LeiosSchedule
 ->
    IO ()
leiosScheduler getSlotDelay leiosContext =
    \(MkLeiosSchedule x) -> do
        y <-
            traverse (traverse cnv . reverse)
          $ Map.fromListWith (++) [ (k, [v]) | (k, v) <- x ]
        flip mapM_ (Map.toAscList y) $ \(slotDbl, msgs) -> do
            getSlotDelay slotDbl >>= threadDelay
            mapM_ (MVar.putMVar (MP.leiosMailbox leiosContext)) msgs
  where
    cnv (ebSlot, ebHashText, !mbEbBytesSize) = do
        ebHash <-
            case BS16.decode (T.encodeUtf8 ebHashText) of
                Left err -> die $ "bad hash in Leios schedule! " ++ T.unpack ebHashText ++ " " ++ err
                Right y -> pure y
        let !rp = Leios.MkLeiosPoint (SlotNo ebSlot) (Leios.MkEbHash ebHash)
        pure (rp, mbEbBytesSize)
