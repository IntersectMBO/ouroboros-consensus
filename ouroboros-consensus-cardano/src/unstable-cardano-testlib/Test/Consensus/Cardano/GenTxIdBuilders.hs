{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Build Cardano 'OneEraGenTxId' values from raw hash bytes, one per era.
--
-- Shared by the txid @Eq@\/@Ord@ benchmark and the txid @Eq@\/@Ord@
-- equivalence test: both need to place a chosen 32-byte hash at a chosen era
-- position, without going through the network or the ledger.
module Test.Consensus.Cardano.GenTxIdBuilders
  ( BuildGenTxId (..)
  , apInjs
  , buildNP
  , oneEraGenTxIds
  ) where

import Cardano.Crypto.Hashing (unsafeAbstractHashFromShort)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import qualified Cardano.Ledger.Shelley.API as SL
import Data.ByteString.Short (ShortByteString)
import Data.SOP (All, Proxy (..), SListI)
import Data.SOP.Strict (NP, NS, hap, hcollapse, hcpure, injections)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (ByronTxId))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraGenTxId (..)
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (ShelleyTxId))
import Ouroboros.Consensus.TypeFamilyWrappers (WrapGenTxId (..))
import Ouroboros.Consensus.Util (hashFromBytesShortE)

-- | Build a transaction id for one era from raw hash bytes. Each era wraps its
-- hash differently, hence one instance per era shape.
class BuildGenTxId blk where
  buildGenTxId :: ShortByteString -> GenTxId blk

instance BuildGenTxId (ShelleyBlock proto era) where
  buildGenTxId bs = ShelleyTxId (SL.TxId (unsafeMakeSafeHash (hashFromBytesShortE bs)))

instance BuildGenTxId ByronBlock where
  buildGenTxId bs = ByronTxId (unsafeAbstractHashFromShort bs)

-- | One leaf per era, every era sharing the given hash bytes.
buildNP :: All BuildGenTxId xs => ShortByteString -> NP WrapGenTxId xs
buildNP bs = hcpure (Proxy @BuildGenTxId) (WrapGenTxId (buildGenTxId bs))

-- | Inject each leaf into the sum at its own era position. The strict 'NS' has
-- no @apInjs_NP@, so we go through 'injections'.
apInjs :: SListI xs => NP f xs -> [NS f xs]
apInjs np = hcollapse (hap injections np)

-- | One 'OneEraGenTxId' per era, list index equal to the era position, every
-- entry carrying the given hash bytes.
oneEraGenTxIds :: All BuildGenTxId xs => ShortByteString -> [OneEraGenTxId xs]
oneEraGenTxIds bs = OneEraGenTxId <$> apInjs (buildNP bs)
