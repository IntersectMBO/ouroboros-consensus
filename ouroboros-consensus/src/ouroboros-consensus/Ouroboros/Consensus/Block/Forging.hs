{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.Forging
  ( BlockForging (..)
  , MkBlockForging (..)
  , CannotForge
  , ForgeStateInfo
  , ForgeStateUpdateError
  , ForgeStateUpdateInfo (..)
  , ShouldForge (..)
  , castForgeStateUpdateInfo
  , checkShouldForge
  , forgeStateUpdateInfoFromUpdateInfo

    -- * 'UpdateInfo'
  , UpdateInfo (..)

    -- * 'ForgeBlockArgs'
  , ForgeBlockArgs (..)

    -- * 'ForgedBlock'
  , ForgedBlock (..)

    -- * Selecting transactions
  , selectBlockTxs
  ) where

import Control.Tracer (Tracer, traceWith)
import Data.Kind (Type)
import qualified Data.Measure
import Data.Text (Text)
import GHC.Stack
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.SupportsPeras (PerasCert)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API (MempoolMeasure, MempoolSnapshot (..))
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Ticked

-- | Information about why we /cannot/ forge a block, although we are a leader
--
-- This should happen only rarely. An example might be that our hot key
-- does not (yet/anymore) match the delegation state.
type family CannotForge blk :: Type

-- | Returned when a call to 'updateForgeState' succeeded and caused the forge
-- state to change. This info is traced.
type family ForgeStateInfo blk :: Type

-- | Returned when a call 'updateForgeState' failed, e.g., because the KES key
-- is no longer valid. This info is traced.
type family ForgeStateUpdateError blk :: Type

-- | The result of 'updateForgeState'.
--
-- Note: the forge state itself is implicit and not reflected in the types.
data ForgeStateUpdateInfo blk
  = -- | NB The update might have not changed the forge state.
    ForgeStateUpdated (ForgeStateInfo blk)
  | ForgeStateUpdateFailed (ForgeStateUpdateError blk)
  | -- | A node was prevented from forging for an artificial reason, such as
    -- testing, benchmarking, etc. It's /artificial/ in that this constructor
    -- should never occur in a production deployment.
    ForgeStateUpdateSuppressed

deriving instance
  (Show (ForgeStateInfo blk), Show (ForgeStateUpdateError blk)) =>
  Show (ForgeStateUpdateInfo blk)

castForgeStateUpdateInfo ::
  ( ForgeStateInfo blk ~ ForgeStateInfo blk'
  , ForgeStateUpdateError blk ~ ForgeStateUpdateError blk'
  ) =>
  ForgeStateUpdateInfo blk -> ForgeStateUpdateInfo blk'
castForgeStateUpdateInfo = \case
  ForgeStateUpdated x -> ForgeStateUpdated x
  ForgeStateUpdateFailed x -> ForgeStateUpdateFailed x
  ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

-- | Stateful wrapper around block production
--
-- NOTE: do not refer to the consensus or ledger config in the closure of this
-- record because they might contain an @EpochInfo Identity@, which will be
-- incorrect when used as part of the hard fork combinator.
data BlockForging m blk = BlockForging
  { forgeLabel :: Text
  -- ^ Identifier used in the trace messages produced for this
  -- 'BlockForging' record.
  --
  -- Useful when the node is running with multiple sets of credentials.
  , canBeLeader :: CanBeLeader (BlockProtocol blk)
  -- ^ Proof that the node can be a leader
  --
  -- NOTE: the other fields of this record may refer to this value (or a
  -- value derived from it) in their closure, which means one should not
  -- override this field independently from the others.
  , updateForgeState ::
      TopLevelConfig blk ->
      SlotNo ->
      Ticked (ChainDepState (BlockProtocol blk)) ->
      m (ForgeStateUpdateInfo blk)
  -- ^ Update the forge state.
  --
  -- When the node can be a leader, this will be called at the start of
  -- each slot, right before calling 'checkCanForge'.
  --
  -- When 'Updated' is returned, we trace the 'ForgeStateInfo'.
  --
  -- When 'UpdateFailed' is returned, we trace the 'ForgeStateUpdateError'
  -- and don't call 'checkCanForge'.
  , checkCanForge ::
      TopLevelConfig blk ->
      SlotNo ->
      Ticked (ChainDepState (BlockProtocol blk)) ->
      IsLeader (BlockProtocol blk) ->
      ForgeStateInfo blk -> -- Proof that 'updateForgeState' did not fail
      Either (CannotForge blk) ()
  -- ^ After checking that the node indeed is a leader ('checkIsLeader'
  -- returned 'Just') and successfully updating the forge state
  -- ('updateForgeState' did not return 'UpdateFailed'), do another check
  -- to see whether we can actually forge a block.
  --
  -- When 'CannotForge' is returned, we don't call 'forgeBlock'.
  , forgeBlock :: ForgeBlockArgs blk -> m (ForgedBlock blk)
  -- ^ Forge a block
  --
  -- NOTE: do not refer to the consensus or ledger config in the closure,
  -- because they might contain an @EpochInfo Identity@, which will be
  -- incorrect when used as part of the hard fork combinator. Use the
  -- given 'fbConfig' instead, as it is guaranteed to be correct
  -- even when used as part of the hard fork combinator.
  --
  -- PRECONDITION: 'checkCanForge' returned @Right ()@.
  , finalize :: m ()
  -- ^ Clean up any unmanaged resources.
  --
  -- Such resources may include KES keys that require explicit erasing
  -- ("secure forgetting"), and threads that connect to a KES agent.
  -- This method will be run once when the block forging thread
  -- terminates, whether cleanly or due to an exception.
  }

-- | 'MkBlockForging' is a wrapper around a monadic action that allocates a
-- 'BlockForging', potentially allocating other linked resources like KES
-- HotKeys, that *MUST* be finalized when the 'BlockForging' is no longer in
-- use. Users of this code must call the 'finalize' function on the returned 'BlockForging' at least once after terminating otherwise allocated resources
-- may leak.
newtype MkBlockForging m blk
  = MkBlockForging {mkBlockForging :: m (BlockForging m blk)}

data ShouldForge blk
  = -- | Before check whether we are a leader in this slot, we tried to update
    --  our forge state ('updateForgeState'), but it failed. We will not check
    --  whether we are leader and will thus not forge a block either.
    --
    -- E.g., we could not evolve our KES key.
    ForgeStateUpdateError (ForgeStateUpdateError blk)
  | -- | We are a leader in this slot, but we cannot forge for a certain
    -- reason.
    --
    -- E.g., our KES key is not yet valid in this slot or we are not the
    -- current delegate of the genesis key we have a delegation certificate
    -- from.
    CannotForge (CannotForge blk)
  | -- | We are not a leader in this slot
    NotLeader
  | -- | We are a leader in this slot and we should forge a block.
    ShouldForge (IsLeader (BlockProtocol blk))

checkShouldForge ::
  forall m blk.
  ( Monad m
  , ConsensusProtocol (BlockProtocol blk)
  , HasCallStack
  ) =>
  BlockForging m blk ->
  Tracer m (ForgeStateInfo blk) ->
  TopLevelConfig blk ->
  SlotNo ->
  Ticked (ChainDepState (BlockProtocol blk)) ->
  m (ShouldForge blk)
checkShouldForge
  BlockForging{..}
  forgeStateInfoTracer
  cfg
  slot
  tickedChainDepState =
    updateForgeState cfg slot tickedChainDepState >>= \updateInfo ->
      case updateInfo of
        ForgeStateUpdated info -> handleUpdated info
        ForgeStateUpdateFailed err -> return $ ForgeStateUpdateError err
        ForgeStateUpdateSuppressed -> return NotLeader
   where
    mbIsLeader :: Maybe (IsLeader (BlockProtocol blk))
    mbIsLeader =
      -- WARNING: It is critical that we do not depend on the 'BlockForging'
      -- record for the implementation of 'checkIsLeader'. Doing so would
      -- make composing multiple 'BlockForging' values responsible for also
      -- composing the 'checkIsLeader' checks, but that should be the
      -- responsibility of the 'ConsensusProtocol' instance for the
      -- composition of those blocks.
      checkIsLeader
        (configConsensus cfg)
        canBeLeader
        slot
        tickedChainDepState

    handleUpdated :: ForgeStateInfo blk -> m (ShouldForge blk)
    handleUpdated info = do
      traceWith forgeStateInfoTracer info
      return $ case mbIsLeader of
        Nothing -> NotLeader
        Just isLeader ->
          case checkCanForge cfg slot tickedChainDepState isLeader info of
            Left cannotForge -> CannotForge cannotForge
            Right () -> ShouldForge isLeader

{-------------------------------------------------------------------------------
  UpdateInfo
-------------------------------------------------------------------------------}

-- | The result of updating something, e.g., the forge state.
data UpdateInfo updated failed
  = -- | NOTE: The update may have induced no change.
    Updated updated
  | UpdateFailed failed
  deriving Show

-- | Embed 'UpdateInfo' into 'ForgeStateUpdateInfo'
forgeStateUpdateInfoFromUpdateInfo ::
  UpdateInfo (ForgeStateInfo blk) (ForgeStateUpdateError blk) ->
  ForgeStateUpdateInfo blk
forgeStateUpdateInfoFromUpdateInfo = \case
  Updated info -> ForgeStateUpdated info
  UpdateFailed err -> ForgeStateUpdateFailed err

{-------------------------------------------------------------------------------
  ForgeBlockArgs
-------------------------------------------------------------------------------}

-- | Arguments to 'forgeBlock' aggregated into a single record.
data ForgeBlockArgs blk = ForgeBlockArgs
  { fbConfig :: !(TopLevelConfig blk)
  -- ^ The node's top-level config.
  , fbCurrentBlockNo :: !BlockNo
  -- ^ The block number of the block to be forged.
  , fbCurrentSlotNo :: !SlotNo
  -- ^ The slot number of the block to be forged.
  , fbPerasCert :: !(Maybe (PerasCert blk))
  -- ^ Optional Peras certificate to include in the forged block
  --
  -- For 'blk' that supports Peras 'Nothing' means no certificate.
  -- For 'blk' that doesn't support Peras it's always 'Nothing'.
  , fbCurrentTickedLedgerState :: !(TickedLedgerState blk EmptyMK)
  -- ^ The current ledger state ticked to 'fbCurrentSlotNo'.
  , fbMempoolSnapshot :: !(MempoolSnapshot blk)
  -- ^ The mempool snapshot for 'fbCurrentTickedLedgerState'.
  --
  -- Its transactions apply in order to that state. 'forgeBlock' selects the
  -- transactions for the block and returns them in 'forgedTxs'.
  , fbIsLeader :: !(IsLeader (BlockProtocol blk))
  -- ^ Proof that the node is the slot leader.
  }

{-------------------------------------------------------------------------------
  ForgedBlock
-------------------------------------------------------------------------------}

-- | The result of 'forgeBlock'.
data ForgedBlock blk = ForgedBlock
  { forgedBlock :: !blk
  -- ^ The forged block.
  , forgedTxs :: ![Validated (GenTx blk)]
  -- ^ The transactions that 'forgeBlock' selected for 'forgedBlock'.
  --
  -- A Byron block holds at most one update proposal, so for Byron this list
  -- can hold an update proposal that the block leaves out.
  --
  -- @Ouroboros.Consensus.NodeKernel.Forge.forge@ removes them from the
  -- mempool if the ChainDB finds the block invalid. It traces them when the
  -- ChainDB adopts the block.
  , forgedTxsMeasure :: !(MempoolMeasure blk)
  -- ^ The total measure of 'forgedTxs'.
  --
  -- For a hard fork block it is the era's measure.
  -- 'Ouroboros.Consensus.HardFork.Combinator.Forging.hardForkBlockForging'
  -- injects it with the @hardForkInj*@ methods of
  -- 'Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork.CanHardFork'.
  -- It can differ from the sum of the combined measures that the mempool
  -- computes for the same transactions. For example,
  -- 'Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork.hardForkTxEbMeasure'
  -- can count more than the injection of the era's endorser-block measure.
  }

{-------------------------------------------------------------------------------
  Selecting transactions
-------------------------------------------------------------------------------}

-- | The ranking-block part of 'snapshotPartition' and its total measure.
--
-- A 'forgeBlock' that never builds an endorser block calls it.
-- 'selectBlockTxs' passes a zero endorser-block capacity and drops the
-- endorser-block part. The ranking-block part does not depend on that capacity.
--
-- The block capacity comes from 'fbCurrentTickedLedgerState', the state that
-- the block extends. The ledger checks the block against that state, which can
-- differ from the ledger state that the mempool last synced with.
selectBlockTxs ::
  TxLimits blk =>
  ForgeBlockArgs blk ->
  ([Validated (GenTx blk)], MempoolMeasure blk)
selectBlockTxs ForgeBlockArgs{..} =
  (txs, txsMeasure)
 where
  (txs, txsMeasure, _, _) =
    snapshotPartition
      fbMempoolSnapshot
      (blockCapacityTxMeasure (configLedger fbConfig) fbCurrentTickedLedgerState)
      Data.Measure.zero
