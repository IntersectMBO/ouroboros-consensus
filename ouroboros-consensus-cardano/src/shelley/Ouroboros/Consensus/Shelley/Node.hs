{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node
  ( MaxMajorProtVer (..)
  , ProtocolParamsShelleyBased (..)
  , SL.Nonce (..)
  , SL.ProtVer (..)
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , SL.emptyGenesisStaking
  , ShelleyLeaderCredentials (..)
  , protocolClientInfoShelley
  , protocolInfoShelley
  , protocolInfoTPraosShelleyBased
  , validateGenesis
  ) where

import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (Crypto)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.TPraos
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Node.DiffusionPipelining ()
import Ouroboros.Consensus.Shelley.Node.Serialisation ()
import Ouroboros.Consensus.Shelley.Node.TPraos
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , pHeaderIssuer
  )

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

protocolClientInfoShelley :: ProtocolClientInfo (ShelleyBlock proto era)
protocolClientInfoShelley =
  ProtocolClientInfo
    { -- No particular codec configuration is needed for Shelley
      pClientInfoCodecConfig = ShelleyCodecConfig
    }

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => BlockSupportsMetrics (ShelleyBlock proto era) where
  -- \| Premature optimisation: we assume everywhere that metrics are
  -- cheap, so micro-optimise checking whether the issuer vkey is one of our
  -- own vkeys.
  --
  -- \* Equality of vkeys takes roughly 40ns
  -- \* Hashing a vkey takes roughly 850ns
  -- \* Equality of hashes takes roughly 10ns
  --
  -- We want to avoid the hashing of a vkey as it is more expensive than
  -- simply doing a linear search, comparing vkeys for equality. Only when
  -- we have to do a linear search across a large number of vkeys does it
  -- become more efficient to first hash the vkey and look up its hash in
  -- the map.
  --
  -- We could try to be clever and estimate the number of keys after which
  -- we switch from a linear search to hashing + a O(log n) map lookup, but
  -- we keep it (relatively) simple and optimise for the common case: 0 or 1
  -- key.
  isSelfIssued cfg (ShelleyHeader shdr _) = case Map.size issuerVKeys of
    -- The most common case: a non-block producing node
    0 -> IsNotSelfIssued
    -- A block producing node with a single set of credentials: just do an
    -- equality check of the single VKey, skipping the more expensive
    -- computation of the hash.
    1
      | pHeaderIssuer shdr `elem` issuerVKeys ->
          IsSelfIssued
      | otherwise ->
          IsNotSelfIssued
    -- When we are running with multiple sets of credentials, which should
    -- only happen when benchmarking, do a hash lookup, as the number of
    -- keys can grow to 100-250.
    _
      | SL.hashKey (pHeaderIssuer shdr) `Map.member` issuerVKeys ->
          IsSelfIssued
      | otherwise ->
          IsNotSelfIssued
   where
    issuerVKeys ::
      Map
        (SL.KeyHash 'SL.BlockIssuer)
        (SL.VKey 'SL.BlockIssuer)
    issuerVKeys = shelleyBlockIssuerVKeys cfg

instance ConsensusProtocol proto => BlockSupportsSanityCheck (ShelleyBlock proto era) where
  configAllSecurityParams = pure . protocolSecurityParam . topLevelConfigProtocol

instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , BlockSupportsSanityCheck (ShelleyBlock proto era)
  , TxLimits (ShelleyBlock proto era)
  , SerialiseNodeToClientConstraints (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  ) =>
  RunNode (ShelleyBlock proto era)
