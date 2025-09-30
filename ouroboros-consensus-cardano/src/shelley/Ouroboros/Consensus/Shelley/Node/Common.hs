{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Node configuration common to all (era, protocol) combinations deriving from
-- Shelley.
module Ouroboros.Consensus.Shelley.Node.Common
  ( ProtocolParamsShelleyBased (..)
  , ShelleyEraWithCrypto
  , ShelleyLeaderCredentials (..)
  , shelleyBlockIssuerVKey
  , validateGenesis
  ) where

import Cardano.Ledger.BaseTypes (unNonZero)
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Slot
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Ouroboros.Consensus.Block
  ( CannotForge
  , ForgeStateInfo
  , ForgeStateUpdateError
  )
import Ouroboros.Consensus.Config (maxRollbacks)
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits)
import Ouroboros.Consensus.Node.InitStorage
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosCanBeLeader (praosCanBeLeaderColdVerKey)
  )
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , shelleyNetworkMagic
  , shelleyStorageConfigSecurityParam
  , shelleyStorageConfigSlotsPerKESPeriod
  , shelleySystemStart
  , verifyBlockIntegrity
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsProtocol (CannotForgeError)
  )
import Ouroboros.Consensus.Storage.ImmutableDB

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data ShelleyLeaderCredentials c = ShelleyLeaderCredentials
  { shelleyLeaderCredentialsCanBeLeader :: PraosCanBeLeader c
  , shelleyLeaderCredentialsLabel :: Text
  -- ^ Identifier for this set of credentials.
  --
  -- Useful when the node is running with multiple sets of credentials.
  }

shelleyBlockIssuerVKey ::
  ShelleyLeaderCredentials c -> SL.VKey 'SL.BlockIssuer
shelleyBlockIssuerVKey =
  praosCanBeLeaderColdVerKey . shelleyLeaderCredentialsCanBeLeader

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge (ShelleyBlock proto era) = CannotForgeError proto

type instance ForgeStateInfo (ShelleyBlock proto era) = HotKey.KESInfo

type instance ForgeStateUpdateError (ShelleyBlock proto era) = HotKey.KESEvolutionError

-- | Needed in '*SharedBlockForging' because we can't partially apply
-- equality constraints.
class
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), ProtoCrypto proto ~ c) =>
  ShelleyEraWithCrypto c proto era

instance
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), ProtoCrypto proto ~ c) =>
  ShelleyEraWithCrypto c proto era

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (ShelleyBlock proto era) where
  getSystemStart = shelleySystemStart
  getNetworkMagic = shelleyNetworkMagic

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => NodeInitStorage (ShelleyBlock proto era) where
  -- We fix the chunk size to @10k@ so that we have the same chunk size as
  -- Byron. Consequently, a Shelley net will have the same chunk size as the
  -- Byron-to-Shelley net with the same @k@.
  nodeImmutableDbChunkInfo =
    simpleChunkInfo
      . EpochSize
      . (* 10)
      . unNonZero
      . maxRollbacks
      . shelleyStorageConfigSecurityParam

  nodeCheckIntegrity cfg =
    verifyBlockIntegrity (shelleyStorageConfigSlotsPerKESPeriod cfg)

{-------------------------------------------------------------------------------
  Protocol parameters
-------------------------------------------------------------------------------}

-- | Parameters common to all Shelley-based ledgers.
--
-- When running a chain with multiple Shelley-based eras, in addition to the
-- per-era protocol parameters, one value of 'ProtocolParamsShelleyBased' will
-- be needed, which is shared among all Shelley-based eras.
data ProtocolParamsShelleyBased c = ProtocolParamsShelleyBased
  { shelleyBasedInitialNonce :: SL.Nonce
  -- ^ The initial nonce, typically derived from the hash of Genesis
  -- config JSON file.
  --
  -- WARNING: chains using different values of this parameter will be
  -- mutually incompatible.
  , shelleyBasedLeaderCredentials :: [ShelleyLeaderCredentials c]
  }

-- | Check the validity of the genesis config. To be used in conjunction with
-- 'assertWithMsg'.
validateGenesis :: SL.ShelleyGenesis -> Either String ()
validateGenesis = first errsToString . SL.validateGenesis
 where
  errsToString :: [SL.ValidationErr] -> String
  errsToString errs =
    Text.unpack $
      Text.unlines
        ("Invalid genesis config:" : map SL.describeValidationErr errs)
