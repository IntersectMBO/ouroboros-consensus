{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Data.Map.Strict as Map
import           Data.SOP.Index (Index (..))
import           Data.Void (Void, absurd)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Byron as the single era in the hard fork combinator
type ByronBlockHFC = HardForkBlock '[ByronBlock]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance NoHardForks ByronBlock where
  getEraParams cfg =
      byronEraParamsNeverHardForks (byronGenesisConfig (configBlock cfg))
  toPartialLedgerConfig _ cfg = ByronPartialLedgerConfig {
        byronLedgerConfig    = cfg
      , byronTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ByronBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ByronBlock'.
instance SupportedNetworkProtocolVersion ByronBlockHFC where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @ByronBlock)

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @ByronBlock)

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Forward to the ByronBlock instance, this means we don't add an era
-- wrapper around blocks on disk. This makes sure we're compatible with the
-- existing Byron blocks.
instance SerialiseHFC '[ByronBlock] where
  encodeDiskHfcBlock (DegenCodecConfig ccfg) (DegenBlock b) =
      encodeDisk ccfg b
  decodeDiskHfcBlock (DegenCodecConfig ccfg) =
      fmap DegenBlock <$> decodeDisk ccfg
  reconstructHfcPrefixLen _ =
      reconstructPrefixLen (Proxy @(Header ByronBlock))
  reconstructHfcNestedCtxt _ prefix blockSize =
      mapSomeNestedCtxt NCZ $
        reconstructNestedCtxt (Proxy @(Header ByronBlock)) prefix blockSize
  getHfcBinaryBlockInfo (DegenBlock b) =
      getBinaryBlockInfo b

{-------------------------------------------------------------------------------
  Canonical TxIn
-------------------------------------------------------------------------------}

instance HasCanonicalTxIn '[ByronBlock] where
  newtype instance CanonicalTxIn '[ByronBlock] = ByronHFCTxIn {
      getByronHFCTxIn :: Void
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NoThunks, FromCBOR, ToCBOR)

  injectCanonicalTxIn IZ key      = absurd key
  injectCanonicalTxIn (IS idx') _ = case idx' of {}

  distribCanonicalTxIn _ key = absurd $ getByronHFCTxIn key

  encodeCanonicalTxIn = toCBOR

  decodeCanonicalTxIn = fromCBOR

instance BlockSupportsHFLedgerQuery '[ByronBlock] where
  answerBlockQueryHFLookup IZ      _cfg  (q :: BlockQuery ByronBlock QFLookupTables result) _dlv = case q of {}
  answerBlockQueryHFLookup (IS is) _cfg _q _dlv = case is of {}

  answerBlockQueryHFTraverse IZ      _cfg  (q :: BlockQuery ByronBlock QFTraverseTables result) _dlv = case q of {}
  answerBlockQueryHFTraverse (IS is) _cfg _q _dlv = case is of {}

  queryLedgerGetTraversingFilter IZ (q :: BlockQuery ByronBlock QFTraverseTables result) = case q of {}
  queryLedgerGetTraversingFilter (IS is) _q = case is of {}
