{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LedgerDB (
    -- * API
    module Ouroboros.Consensus.Storage.LedgerDB.API
  , module Ouroboros.Consensus.Storage.LedgerDB.API.Config
  , module Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
    -- * Impl
  , openDB
  ) where

import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Init as Init
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Init as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Init as V2
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike

openDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , InspectLedger blk
  , HasCallStack
  , HasHardForkHistory blk
  )
  => Complete LedgerDbArgs m blk
  -- ^ Stateless initializaton arguments
  -> StreamAPI m blk blk
  -- ^ Stream source for blocks.
  --
  -- After reading a snapshot from disk, the ledger DB will be brought up to
  -- date with the tip of this steam of blocks. The corresponding ledger state
  -- can then be used as the starting point for chain selection in the ChainDB
  -- driver.
  -> Point blk
  -- ^ The Replay goal i.e. the tip of the stream of blocks.
  -> ResolveBlock m blk
  -- ^ How to get blocks from the ChainDB
  -> m (LedgerDB' m blk, Word64)
openDB
  args
  stream
  replayGoal
  getBlock = case lgrFlavorArgs args of
    LedgerDbFlavorArgsV1 bss ->
      let initDb = V1.mkInitDb
                       args
                       bss
                       getBlock
        in
          Init.openDB args initDb stream replayGoal
    LedgerDbFlavorArgsV2 bss ->
        let initDb = V2.mkInitDb
                       args
                       bss
                       getBlock
        in
          Init.openDB args initDb stream replayGoal
