{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Mock.Node (
    CodecConfig (..)
  , simpleBlockForging
  ) where

import           Codec.Serialise (Serialise)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.SupportsMempool (txForgetValidated)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Node.Serialisation ()
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance SupportedNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  supportedNodeToNodeVersions   _ = Map.singleton maxBound ()
  supportedNodeToClientVersions _ = Map.singleton maxBound ()

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

instance NodeInitStorage (SimpleBlock SimpleMockCrypto ext) where
  nodeImmutableDbChunkInfo (SimpleStorageConfig secParam) = simpleChunkInfo $
      EpochSize $ 10 * maxRollbacks secParam

  nodeCheckIntegrity _ _ = True

instance BlockSupportsMetrics (SimpleBlock c ext) where
  isSelfIssued = isSelfIssuedConstUnknown

deriving via SelectViewDiffusionPipelining (SimpleBlock c ext) instance
  ( BlockSupportsProtocol (SimpleBlock c ext)
  , Show (SelectView (BlockProtocol (SimpleBlock c ext)))
  ) => BlockSupportsDiffusionPipelining (SimpleBlock c ext)
  
instance ConsensusProtocol (BlockProtocol (SimpleBlock c ext)) => BlockSupportsSanityCheck (SimpleBlock c ext) where
  configAllSecurityParams = pure . configSecurityParam

instance ( LedgerSupportsProtocol      (SimpleBlock SimpleMockCrypto ext)
         , Show (CannotForge           (SimpleBlock SimpleMockCrypto ext))
         , Show (ForgeStateInfo        (SimpleBlock SimpleMockCrypto ext))
         , Show (ForgeStateUpdateError (SimpleBlock SimpleMockCrypto ext))
         , Serialise ext
         , RunMockBlock SimpleMockCrypto ext
         ) => RunNode (SimpleBlock SimpleMockCrypto ext)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Can be used when 'CanBeLeader' is static
simpleBlockForging ::
     forall c ext m.
     ( RunMockBlock c ext
     , CannotForge           (SimpleBlock c ext) ~ Void
     , ForgeStateInfo        (SimpleBlock c ext) ~ ()
     , ForgeStateUpdateError (SimpleBlock c ext) ~ Void
     , Monad m
     )
  => CanBeLeader (BlockProtocol (SimpleBlock c ext))
  -> ForgeExt c ext
  -> BlockForging m (SimpleBlock c ext)
simpleBlockForging aCanBeLeader aForgeExt = BlockForging {
      forgeLabel       = "simpleBlockForging"
    , canBeLeader      = aCanBeLeader
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge    = \_ _ _ _ _ -> return ()
    , forgeBlock       = \cfg bno slot lst txs proof ->
        return
          $ forgeSimple
              aForgeExt
              cfg
              bno
              slot
              lst
              (map txForgetValidated txs)
              proof
    }
  where
    _ = keepRedundantConstraint (Proxy @(ForgeStateUpdateError (SimpleBlock c ext) ~ Void))
