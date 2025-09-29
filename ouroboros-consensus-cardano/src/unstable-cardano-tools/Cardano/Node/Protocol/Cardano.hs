{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol/Cardano.hs

module Cardano.Node.Protocol.Cardano
  ( mkConsensusProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError (..)
  ) where

import Cardano.Api.Any (Error (..))
import qualified Cardano.Chain.Update as Byron
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import qualified Cardano.Node.Protocol.Alonzo as Alonzo
import qualified Cardano.Node.Protocol.Byron as Byron
import qualified Cardano.Node.Protocol.Conway as Conway
import Cardano.Node.Protocol.Shelley (readGenesisAny)
import qualified Cardano.Node.Protocol.Shelley as Shelley
import Cardano.Node.Types
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Cardano.Node (CardanoProtocolParams (..))
import Ouroboros.Consensus.Config (emptyCheckpointsMap)
import Ouroboros.Consensus.HardFork.Combinator.Condense ()
import Ouroboros.Consensus.Shelley.Crypto (StandardCrypto)

------------------------------------------------------------------------------
-- Real Cardano protocol
--

-- | Make 'SomeConsensusProtocol' using the Cardano instance.
--
-- The Cardano protocol instance is currently the sequential composition of
-- the Byron and Shelley protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
mkConsensusProtocolCardano ::
  NodeByronProtocolConfiguration ->
  NodeShelleyProtocolConfiguration ->
  NodeAlonzoProtocolConfiguration ->
  NodeConwayProtocolConfiguration ->
  NodeDijkstraProtocolConfiguration ->
  NodeHardForkProtocolConfiguration ->
  Maybe ProtocolFilepaths ->
  ExceptT CardanoProtocolInstantiationError IO (CardanoProtocolParams StandardCrypto)
mkConsensusProtocolCardano
  NodeByronProtocolConfiguration
    { npcByronGenesisFile
    , npcByronGenesisFileHash
    , npcByronReqNetworkMagic
    , npcByronPbftSignatureThresh
    , npcByronApplicationName
    , npcByronApplicationVersion
    , npcByronSupportedProtocolVersionMajor
    , npcByronSupportedProtocolVersionMinor
    , npcByronSupportedProtocolVersionAlt
    }
  NodeShelleyProtocolConfiguration
    { npcShelleyGenesisFile
    , npcShelleyGenesisFileHash
    }
  NodeAlonzoProtocolConfiguration
    { npcAlonzoGenesisFile
    , npcAlonzoGenesisFileHash
    }
  NodeConwayProtocolConfiguration
    { npcConwayGenesisFile
    , npcConwayGenesisFileHash
    }
  NodeDijkstraProtocolConfiguration
    { npcDijkstraGenesisFile
    , npcDijkstraGenesisFileHash
    }
  NodeHardForkProtocolConfiguration
    { npcTestEnableDevelopmentHardForkEras = _
    , -- During testing of the latest unreleased era, we conditionally
    -- declared that we knew about it. We do so only when a config option
    -- for testing development/unstable eras is used. This lets us include
    -- not-yet-ready eras in released node versions without mainnet nodes
    -- prematurely advertising that they could hard fork into the new era.
    npcTestShelleyHardForkAtEpoch
    , npcTestAllegraHardForkAtEpoch
    , npcTestMaryHardForkAtEpoch
    , npcTestAlonzoHardForkAtEpoch
    , npcTestBabbageHardForkAtEpoch
    , npcTestConwayHardForkAtEpoch
    , npcTestDijkstraHardForkAtEpoch
    }
  files = do
    byronGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readGenesis
          npcByronGenesisFile
          npcByronGenesisFileHash
          npcByronReqNetworkMagic

    byronLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readLeaderCredentials byronGenesis files

    (shelleyGenesis, shelleyGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationShelleyGenesisReadError $
        Shelley.readGenesis
          npcShelleyGenesisFile
          npcShelleyGenesisFileHash

    (alonzoGenesis, _alonzoGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationAlonzoGenesisReadError $
        Alonzo.readGenesis
          npcAlonzoGenesisFile
          npcAlonzoGenesisFileHash

    (conwayGenesis, _conwayGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationConwayGenesisReadError $
        Conway.readGenesis
          npcConwayGenesisFile
          npcConwayGenesisFileHash

    (dijkstraGenesis, _dijkstraGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationDijkstraGenesisReadError $
        readGenesisAny
          npcDijkstraGenesisFile
          npcDijkstraGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationPraosLeaderCredentialsError $
        Shelley.readLeaderCredentials files

    let transitionLedgerConfig =
          SL.mkLatestTransitionConfig shelleyGenesis alonzoGenesis conwayGenesis dijkstraGenesis

    -- TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      CardanoProtocolParams
        Consensus.ProtocolParamsByron
          { byronGenesis = byronGenesis
          , byronPbftSignatureThreshold =
              PBftSignatureThreshold <$> npcByronPbftSignatureThresh
          , -- This is /not/ the Byron protocol version. It is the protocol
            -- version that this node will use in blocks it creates. It is used
            -- in the Byron update mechanism to signal that this block-producing
            -- node is ready to move to the new protocol. For example, when the
            -- protocol version (according to the ledger state) is 0, this setting
            -- should be 1 when we are ready to move. Similarly when the current
            -- protocol version is 1, this should be 2 to indicate we are ready
            -- to move into the Shelley era.
            byronProtocolVersion =
              Byron.ProtocolVersion
                npcByronSupportedProtocolVersionMajor
                npcByronSupportedProtocolVersionMinor
                npcByronSupportedProtocolVersionAlt
          , byronSoftwareVersion =
              Byron.SoftwareVersion
                npcByronApplicationName
                npcByronApplicationVersion
          , byronLeaderCredentials = byronLeaderCredentials
          }
        Consensus.ProtocolParamsShelleyBased
          { shelleyBasedInitialNonce =
              Shelley.genesisHashToPraosNonce
                shelleyGenesisHash
          , shelleyBasedLeaderCredentials = shelleyLeaderCredentials
          }
        -- The 'CardanoHardForkTriggers' specify the parameters needed to
        -- transition between two eras. The comments below also apply for all
        -- subsequent hard forks.
        --
        -- Byron to Shelley hard fork parameters
        Consensus.CardanoHardForkTriggers'
          { triggerHardForkShelley =
              -- What will trigger the Byron -> Shelley hard fork?
              case npcTestShelleyHardForkAtEpoch of
                -- This specifies the major protocol version number update that will
                -- trigger us moving to the Shelley protocol.
                --
                -- Version 0 is Byron with Ouroboros classic
                -- Version 1 is Byron with Ouroboros Permissive BFT
                -- Version 2 is Shelley
                -- Version 3 is Allegra
                -- Version 4 is Mary
                -- Version 5 is Alonzo
                -- Version 6 is Alonzo (intra era hardfork)
                -- Version 7 is Babbage
                -- Version 8 is Babbage (intra era hardfork)
                -- Version 9 is Conway
                -- Version 10 is Conway (intra era hardfork)
                -- Version 11 is Conway (intra era hardfork)
                -- Version 12 is Dijkstra
                --
                -- But we also provide an override to allow for simpler test setups
                -- such as triggering at the 0 -> 1 transition .
                --
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                -- Alternatively, for testing we can transition at a specific epoch.
                --
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Shelley to Allegra hard fork parameters
            triggerHardForkAllegra =
              case npcTestAllegraHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Allegra to Mary hard fork parameters
            triggerHardForkMary =
              case npcTestMaryHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Mary to Alonzo hard fork parameters
            triggerHardForkAlonzo =
              case npcTestAlonzoHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Alonzo to Babbage hard fork parameters
            triggerHardForkBabbage =
              case npcTestBabbageHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Babbage to Conway hard fork parameters
            triggerHardForkConway =
              case npcTestConwayHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          , -- Conway to Dijkstra hard fork parameters
            triggerHardForkDijkstra =
              case npcTestDijkstraHardForkAtEpoch of
                Nothing -> Consensus.CardanoTriggerHardForkAtDefaultVersion
                Just epochNo -> Consensus.CardanoTriggerHardForkAtEpoch epochNo
          }
        transitionLedgerConfig
        emptyCheckpointsMap
        (ProtVer (L.eraProtVerHigh @L.LatestKnownEra) 0)

------------------------------------------------------------------------------
-- Errors
--

data CardanoProtocolInstantiationError
  = CardanoProtocolInstantiationErrorByron
      Byron.ByronProtocolInstantiationError
  | CardanoProtocolInstantiationShelleyGenesisReadError
      Shelley.GenesisReadError
  | CardanoProtocolInstantiationAlonzoGenesisReadError
      Shelley.GenesisReadError
  | CardanoProtocolInstantiationConwayGenesisReadError
      Shelley.GenesisReadError
  | CardanoProtocolInstantiationDijkstraGenesisReadError
      Shelley.GenesisReadError
  | CardanoProtocolInstantiationPraosLeaderCredentialsError
      Shelley.PraosLeaderCredentialsError
  | CardanoProtocolInstantiationErrorAlonzo
      Alonzo.AlonzoProtocolInstantiationError
  deriving Show

instance Error CardanoProtocolInstantiationError where
  displayError (CardanoProtocolInstantiationErrorByron err) =
    displayError err
  displayError (CardanoProtocolInstantiationShelleyGenesisReadError err) =
    "Shelley related: " <> displayError err
  displayError (CardanoProtocolInstantiationAlonzoGenesisReadError err) =
    "Alonzo related: " <> displayError err
  displayError (CardanoProtocolInstantiationConwayGenesisReadError err) =
    "Conway related: " <> displayError err
  displayError (CardanoProtocolInstantiationDijkstraGenesisReadError err) =
    "Dijkstra related: " <> displayError err
  displayError (CardanoProtocolInstantiationPraosLeaderCredentialsError err) =
    displayError err
  displayError (CardanoProtocolInstantiationErrorAlonzo err) =
    displayError err
