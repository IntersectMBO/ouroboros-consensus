{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion (
    HasNetworkProtocolVersion (..)
  , SupportedNetworkProtocolVersion (..)
  , latestReleasedNodeVersionDefault
    -- * Re-exports
  , NodeToClientVersion (..)
  , NodeToNodeVersion (..)
  ) where

import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Ouroboros.Network.NodeToClient.Version
import           Ouroboros.Network.NodeToNode.Version

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
class ( Show (BlockNodeToNodeVersion   blk)
      , Show (BlockNodeToClientVersion blk)
      , Eq   (BlockNodeToNodeVersion   blk)
      , Eq   (BlockNodeToClientVersion blk)
      ) => HasNetworkProtocolVersion blk where
  type BlockNodeToNodeVersion   blk :: Type
  type BlockNodeToClientVersion blk :: Type

  -- Defaults

  type BlockNodeToNodeVersion   blk = ()
  type BlockNodeToClientVersion blk = ()

class HasNetworkProtocolVersion blk => SupportedNetworkProtocolVersion blk where
  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions
    :: Proxy blk -> Map NodeToNodeVersion (BlockNodeToNodeVersion blk)

  -- | Enumerate all supported node-to-client versions
  supportedNodeToClientVersions
    :: Proxy blk -> Map NodeToClientVersion (BlockNodeToClientVersion blk)

  -- | The latest released version
  --
  -- This is the latest version intended for deployment.
  --
  -- IMPORTANT Note that this is entirely independent of the
  -- 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer' field et al.
  latestReleasedNodeVersion
    :: Proxy blk -> (Maybe NodeToNodeVersion, Maybe NodeToClientVersion)

-- | A default for 'latestReleasedNodeVersion'
--
-- Chooses the greatest in 'supportedNodeToNodeVersions' and
-- 'supportedNodeToClientVersions'.
latestReleasedNodeVersionDefault ::
     SupportedNetworkProtocolVersion blk
  => Proxy blk
  -> (Maybe NodeToNodeVersion, Maybe NodeToClientVersion)
latestReleasedNodeVersionDefault prx =
    ( fmap fst $ Map.lookupMax $ supportedNodeToNodeVersions   prx
    , fmap fst $ Map.lookupMax $ supportedNodeToClientVersions prx
    )
