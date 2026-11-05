{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion (..)
  , SupportedNetworkProtocolVersion (..)
  , latestReleasedNodeVersionDefault
  , withExperimentalAtomicFetchVersion

    -- * Re-exports
  , NodeToClientVersion (..)
  , NodeToNodeVersion (..)
  ) where

import Cardano.Network.NodeToClient.Version
import Cardano.Network.NodeToNode.Version
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy

{-------------------------------------------------------------------------------
  Protocol versioning
-------------------------------------------------------------------------------}

-- | Protocol versioning
--
-- IMPORTANT Note that this is entirely independent of the
-- 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer' field et al.
--
-- Its primary purpose is to control the details of on-the-wire codecs. And
-- additionally which queries are allowed, in the case of
-- 'BlockNodeToClienVersion' (this use is already handled by
-- 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer' in the NTN case).
class
  ( Show (BlockNodeToNodeVersion blk)
  , Show (BlockNodeToClientVersion blk)
  , Eq (BlockNodeToNodeVersion blk)
  , Eq (BlockNodeToClientVersion blk)
  ) =>
  HasNetworkProtocolVersion blk
  where
  type BlockNodeToNodeVersion blk :: Type
  type BlockNodeToClientVersion blk :: Type

  -- Defaults

  type BlockNodeToNodeVersion blk = ()
  type BlockNodeToClientVersion blk = ()

class HasNetworkProtocolVersion blk => SupportedNetworkProtocolVersion blk where
  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions ::
    Proxy blk -> Map NodeToNodeVersion (BlockNodeToNodeVersion blk)

  -- | Enumerate all supported node-to-client versions
  supportedNodeToClientVersions ::
    Proxy blk -> Map NodeToClientVersion (BlockNodeToClientVersion blk)

  -- | The latest released version
  --
  -- This is the latest version intended for deployment.
  --
  -- IMPORTANT Note that this is entirely independent of the
  -- 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer' field et al.
  latestReleasedNodeVersion ::
    Proxy blk -> (Maybe NodeToNodeVersion, Maybe NodeToClientVersion)

-- | A default for 'latestReleasedNodeVersion'
--
-- Chooses the greatest in 'supportedNodeToNodeVersions' and
-- 'supportedNodeToClientVersions'.
latestReleasedNodeVersionDefault ::
  SupportedNetworkProtocolVersion blk =>
  Proxy blk ->
  (Maybe NodeToNodeVersion, Maybe NodeToClientVersion)
latestReleasedNodeVersionDefault prx =
  ( fmap fst $ Map.lookupMax $ supportedNodeToNodeVersions prx
  , fmap fst $ Map.lookupMax $ supportedNodeToClientVersions prx
  )

-- | Explicit development/fixture opt-in to version 17, using the same block
-- serialization as version 16. Ordinary supported/released maps remain unchanged.
-- The atomic fetch client/server must exist before enabling a live application.
withExperimentalAtomicFetchVersion :: Map NodeToNodeVersion a -> Map NodeToNodeVersion a
withExperimentalAtomicFetchVersion versions = case Map.lookup NodeToNodeV_16 versions of
  Nothing -> versions
  Just blockVersion -> Map.insert NodeToNodeV_17 blockVersion versions
