{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ouroboros.Consensus.Protocol.Translate (TranslateProto (..)) where

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked (Ticked)

-- | Translate across protocols
class TranslateProto protoFrom protoTo
  where
  translateConsensusConfig ::
    ConsensusConfig protoFrom -> ConsensusConfig protoTo
  -- | Translate the ticked ledger view.
  translateTickedLedgerView ::
    Ticked (LedgerView protoFrom) -> Ticked (LedgerView protoTo)
  translateChainDepState ::
    Ticked (ChainDepState protoFrom) -> ChainDepState protoTo
