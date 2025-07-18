{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Protocol.ModChainSel
  ( ModChainSel

    -- * Type family instances
  , ConsensusConfig (..)
  ) where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Protocol.Abstract

data ModChainSel p t

newtype instance ConsensusConfig (ModChainSel p t) = McsConsensusConfig
  { mcsConfigP :: ConsensusConfig p
  }
  deriving Generic

instance
  ( ConsensusProtocol p
  , ChainOrder t
  , Show t
  , Typeable t
  , NoThunks t
  ) =>
  ConsensusProtocol (ModChainSel p t)
  where
  type TiebreakerView (ModChainSel p t) = t

  type ChainDepState (ModChainSel p t) = ChainDepState p
  type IsLeader (ModChainSel p t) = IsLeader p
  type CanBeLeader (ModChainSel p t) = CanBeLeader p
  type LedgerView (ModChainSel p t) = LedgerView p
  type ValidationErr (ModChainSel p t) = ValidationErr p
  type ValidateView (ModChainSel p t) = ValidateView p

  checkIsLeader = checkIsLeader . mcsConfigP
  tickChainDepState = tickChainDepState . mcsConfigP
  updateChainDepState = updateChainDepState . mcsConfigP
  reupdateChainDepState = reupdateChainDepState . mcsConfigP
  protocolSecurityParam = protocolSecurityParam . mcsConfigP

instance ConsensusProtocol p => NoThunks (ConsensusConfig (ModChainSel p t))
