{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for the mock block.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Mock.Node.Serialisation' needs some of the these
-- instances, but defining them there would be too confusing.
module Ouroboros.Consensus.Mock.Node.Peras () where

import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock, SimpleCrypto)
import Ouroboros.Consensus.Peras.Context (StateSupportsPerasEpochContext (..))
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepState
  , ChainDepStateSupportsPeras
  )
import Ouroboros.Consensus.Ticked (Ticked)

instance
  ( SimpleCrypto c
  , Typeable ext
  , ChainDepStateSupportsPeras (ChainDepState (BlockProtocol (SimpleBlock c ext)))
  , ChainDepStateSupportsPeras (Ticked (ChainDepState (BlockProtocol (SimpleBlock c ext))))
  ) =>
  StateSupportsPerasEpochContext (SimpleBlock c ext)
  where
  type MaybeEraIndexedEpochToPerasRoundInfo (SimpleBlock c ext) = EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: SimpleBlock does not support Peras"

instance
  ( SimpleCrypto c
  , Typeable ext
  ) =>
  BlockSupportsPeras (SimpleBlock c ext)
