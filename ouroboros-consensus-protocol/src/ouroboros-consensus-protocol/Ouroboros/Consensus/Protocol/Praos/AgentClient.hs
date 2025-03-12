{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Ouroboros.Consensus.Protocol.Praos.AgentClient (
  runKESAgentClient,
  AgentCrypto (..),
  MonadKESAgent (..),
  KESAgentContext,
  KESAgentClientTrace (..),
)
where

import qualified Cardano.KESAgent.KES.Bundle as Agent
import qualified Cardano.KESAgent.KES.Crypto as Agent
import qualified Cardano.KESAgent.KES.OCert as Agent
import qualified Cardano.KESAgent.Processes.ServiceClient as Agent
import qualified Cardano.KESAgent.Protocols.RecvResult as Agent
import qualified Cardano.KESAgent.Protocols.VersionedProtocol as Agent
import qualified Cardano.KESAgent.Protocols.StandardCrypto as Agent
import qualified Cardano.KESAgent.Serialization.DirectCodec as Agent
import           Cardano.KESAgent.Util.RefCounting
import qualified Cardano.Protocol.TPraos.OCert as OCert
import           Cardano.Ledger.Crypto (Crypto, KES, DSIGN, VRF, StandardCrypto)

import Cardano.Crypto.KES.Class
import Cardano.Crypto.VRF.Class
import Cardano.Crypto.DirectSerialise (DirectSerialise, DirectDeserialise)

import           Ouroboros.Network.RawBearer
import           Ouroboros.Network.Snocket
import           Ouroboros.Consensus.Util.IOLike

import Control.Monad (forever)
import Control.Monad.Class.MonadAsync
import Control.Tracer
import Data.Coerce (coerce)
import Network.Socket
import System.IOManager
import Data.Typeable
import Data.Kind
import Data.SerDoc.Class as SerDoc
import Control.Monad.IOSim
import qualified Simulation.Network.Snocket as SimSnocket
import Test.Ouroboros.Network.Data.AbsBearerInfo as ABI

type KESAgentContext c m =
      ( AgentCrypto c
      , MonadKESAgent m
      , SerDoc.HasInfo (Agent.DirectCodec m) (VerKeyKES (KES c))
      , SerDoc.HasInfo (Agent.DirectCodec m) (SignKeyKES (KES c))
      , IOLike m
      )

data KESAgentClientTrace
  = KESAgentClientException SomeException
  | KESAgentClientTrace Agent.ServiceClientTrace
  deriving (Show)

class ( Crypto c
      , Agent.Crypto (ACrypto c)
      , Agent.NamedCrypto (ACrypto c)
      , Agent.KES (ACrypto c) ~ KES c
      , Agent.DSIGN (ACrypto c) ~ DSIGN c
      , ContextKES (KES c) ~ ()
      , ContextVRF (VRF c) ~ ()
      , Typeable (ACrypto c)
      , Agent.ServiceClientDrivers (ACrypto c)
      , DirectSerialise (SignKeyKES (KES c))
      , DirectDeserialise (SignKeyKES (KES c))
      )
  => AgentCrypto c where
        type ACrypto c :: Type

instance AgentCrypto StandardCrypto where
  type ACrypto StandardCrypto = Agent.StandardCrypto

convertOCert :: ( AgentCrypto c
                )
              => Agent.OCert (ACrypto c) -> OCert.OCert c
convertOCert oca =
  OCert.OCert
    { OCert.ocertVkHot = Agent.ocertVkHot oca
    , OCert.ocertN = Agent.ocertN oca
    , OCert.ocertKESPeriod = OCert.KESPeriod (Agent.unKESPeriod $ Agent.ocertKESPeriod oca)
    , OCert.ocertSigma = coerce (Agent.ocertSigma oca)
    }

class (MonadFail m, Show (Addr m)) => MonadKESAgent m where
  type FD m :: Type
  type Addr m :: Type
  withAgentContext :: (Snocket m (FD m) (Addr m) -> m a) -> m a
  makeRawBearer :: MakeRawBearer m (FD m)
  makeAddress :: Proxy m -> FilePath -> Addr m

instance MonadKESAgent IO where
  type FD IO = Socket
  type Addr IO = SockAddr
  withAgentContext inner =
    withIOManager $ \ioManager ->
      inner (socketSnocket ioManager)
  makeRawBearer = makeSocketRawBearer
  makeAddress _ = SockAddrUnix

instance MonadKESAgent (IOSim s) where
  type FD (IOSim s) = SimSnocket.FD (IOSim s) (TestAddress FilePath)
  type Addr (IOSim s) = TestAddress FilePath
  withAgentContext inner = do
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo $ absNoAttenuation {abiConnectionDelay = SmallDelay})
      mempty
      $ \snocket _observe -> inner snocket
  makeRawBearer = SimSnocket.makeFDRawBearer nullTracer
  makeAddress _ = TestAddress

instance SimSnocket.GlobalAddressScheme FilePath where
  getAddressType = const SimSnocket.IPv4Address
  ephemeralAddress _ty num = TestAddress $ "simSnocket_" <> show num

runKESAgentClient :: forall m c.
                     ( KESAgentContext c m
                     )
                  => Tracer m KESAgentClientTrace
                  -> FilePath
                  -> (OCert.OCert c -> SignKeyKES (KES c) -> Word -> m ())
                  -> m ()
runKESAgentClient tracer path handleKey = do
  withAgentContext $ \snocket -> do
    forever $ do
      Agent.runServiceClient
        (Proxy @(ACrypto c))
        makeRawBearer
        (Agent.ServiceClientOptions
          { Agent.serviceClientSnocket = snocket
          , Agent.serviceClientAddress = makeAddress (Proxy @m) path
          } :: Agent.ServiceClientOptions m (FD m) (Addr m)
        )
        (\(Agent.Bundle skpRef ocert) -> do
            -- We take ownership of the key, so we acquire one extra reference,
            -- preventing the key from being discarded after `handleKey`
            -- finishes.
            _ <- acquireCRef skpRef
            withCRefValue skpRef $ \(SignKeyWithPeriodKES sk p) ->
              handleKey (convertOCert ocert) sk p
            return Agent.RecvOK
        )
        (contramap KESAgentClientTrace tracer)
        `catch` ( \(_e :: AsyncCancelled) ->
                    return ()
                )
        `catch` ( \(e :: SomeException) ->
                    traceWith tracer (KESAgentClientException e)
                )
      threadDelay 10000000

toBearerInfo :: ABI.AbsBearerInfo -> SimSnocket.BearerInfo
toBearerInfo abi =
  SimSnocket.BearerInfo
    { SimSnocket.biConnectionDelay = ABI.delay (ABI.abiConnectionDelay abi)
    , SimSnocket.biInboundAttenuation = attenuation (ABI.abiInboundAttenuation abi)
    , SimSnocket.biOutboundAttenuation = attenuation (ABI.abiOutboundAttenuation abi)
    , SimSnocket.biInboundWriteFailure = ABI.abiInboundWriteFailure abi
    , SimSnocket.biOutboundWriteFailure = ABI.abiOutboundWriteFailure abi
    , SimSnocket.biAcceptFailures =
        ( \(errDelay, errType) ->
            ( ABI.delay errDelay
            , errType
            )
        )
          <$> abiAcceptFailure abi
    , SimSnocket.biSDUSize = toSduSize (ABI.abiSDUSize abi)
    }

