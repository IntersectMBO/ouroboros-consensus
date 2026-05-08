{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for the mock block.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Mock.Node.Serialisation' needs this instance, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.Mock.Node.Peras () where

import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock, SimpleCrypto)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- NOTE: The mock block does not support Peras, so we can use the empty instance here.
instance
  (SimpleCrypto c, Typeable ext) =>
  BlockSupportsPeras (SimpleBlock c ext)
