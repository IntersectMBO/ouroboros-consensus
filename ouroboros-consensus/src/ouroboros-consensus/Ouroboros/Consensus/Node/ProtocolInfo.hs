{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Node.ProtocolInfo
  ( NumCoreNodes (..)
  , ProtocolClientInfo (..)
  , ProtocolInfo (..)
  , enumCoreNodes
  ) where

import Data.Word
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, Values)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.NodeId

{-------------------------------------------------------------------------------
  Number of core nodes
-------------------------------------------------------------------------------}

newtype NumCoreNodes = NumCoreNodes Word64
  deriving (Show, NoThunks)

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes 0) = []
enumCoreNodes (NumCoreNodes numNodes) =
  [CoreNodeId n | n <- [0 .. numNodes - 1]]

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the specified protocol.
data ProtocolInfo b = ProtocolInfo
  { pInfoConfig :: !(TopLevelConfig b)
  , pInfoInitLedger :: ExtLedgerState b EmptyMK
  -- ^ The ledger state at genesis.
  , pInfoInitLedgerTables :: Values b
  -- ^ The UTxO values at genesis, carried alongside 'pInfoInitLedger'.
  --
  -- The ledger state at genesis does not store the UTxO, so the initial values
  -- are carried separately rather than inside it. For Cardano these are the
  -- values for the era the chain lands in after the genesis-time era
  -- transitions; the initial era is Byron, whose tables are void.
  }

-- | Data required by clients of a node running the specified protocol.
data ProtocolClientInfo b = ProtocolClientInfo
  { pClientInfoCodecConfig :: CodecConfig b
  }
