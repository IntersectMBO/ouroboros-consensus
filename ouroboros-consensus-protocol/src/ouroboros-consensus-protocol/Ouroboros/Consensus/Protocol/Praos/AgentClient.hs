{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ouroboros.Consensus.Protocol.Praos.AgentClient (
    AgentCrypto (..)
  , KESAgentClientTrace (..)
  , KESAgentContext
  , MonadKESAgent (..)
  , runKESAgentClient
  ) where

import           Cardano.Crypto.DirectSerialise (DirectDeserialise,
                     DirectSerialise)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import qualified Cardano.KESAgent.KES.Bundle as Agent
import qualified Cardano.KESAgent.KES.Crypto as Agent
import qualified Cardano.KESAgent.KES.OCert as Agent
import qualified Cardano.KESAgent.Processes.ServiceClient as Agent
import qualified Cardano.KESAgent.Protocols.RecvResult as Agent
import qualified Cardano.KESAgent.Protocols.StandardCrypto as Agent
import qualified Cardano.KESAgent.Protocols.VersionedProtocol as Agent
import qualified Cardano.KESAgent.Serialization.DirectCodec as Agent
import           Cardano.KESAgent.Util.RefCounting
import           Cardano.Ledger.Keys (DSIGN)
import           Cardano.Protocol.Crypto (Crypto, KES, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.IOSim
import           Control.Tracer
import           Data.Coerce (coerce)
import           Data.Kind
import           Data.SerDoc.Class as SerDoc
import           Data.Typeable
import           Network.Socket
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.RawBearer
import           Ouroboros.Network.Snocket
import qualified Simulation.Network.Snocket as SimSnocket
import           System.IOManager
import           Test.Ouroboros.Network.Data.AbsBearerInfo as ABI

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

convertOCert :: (AgentCrypto c, Agent.DSIGN (ACrypto c) ~ DSIGN) => Agent.OCert (ACrypto c) -> OCert.OCert c
convertOCert oca =
    OCert.OCert
      { OCert.ocertVkHot = Agent.ocertVkHot oca
      , OCert.ocertN = Agent.ocertN oca
      , OCert.ocertKESPeriod = OCert.KESPeriod (Agent.unKESPeriod $ Agent.ocertKESPeriod oca)
      , OCert.ocertSigma = coerce (Agent.ocertSigma oca)
      }

convertPeriod :: Agent.KESPeriod -> OCert.KESPeriod
convertPeriod (Agent.KESPeriod p) = OCert.KESPeriod p

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
                     , Agent.DSIGN (ACrypto c) ~ DSIGN
                     )
                  => Tracer m KESAgentClientTrace
                  -> FilePath
                  -> (OCert.OCert c -> SignKeyKES (KES c) -> Word -> OCert.KESPeriod -> m ())
                  -> m ()
                  -> m ()
runKESAgentClient tracer path handleKey handleDropKey = do
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
        (\(Agent.TaggedBundle mBundle _) -> do
            case mBundle of
              Just (Agent.Bundle skpRef ocert) -> do
                -- We take ownership of the key, so we acquire one extra reference,
                -- preventing the key from being discarded after `handleKey`
                -- finishes.
                _ <- acquireCRef skpRef
                withCRefValue skpRef $ \(SignKeyWithPeriodKES sk p) ->
                  handleKey (convertOCert ocert) sk p (convertPeriod $ Agent.ocertKESPeriod ocert)
                return Agent.RecvOK
              _ -> do
                handleDropKey
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
