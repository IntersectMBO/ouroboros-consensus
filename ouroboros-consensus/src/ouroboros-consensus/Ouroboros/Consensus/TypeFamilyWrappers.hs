{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Newtypes around type families so that they can be partially applied
module Ouroboros.Consensus.TypeFamilyWrappers
  ( -- * Block based
    WrapApplyTxErr (..)
  , WrapCannotForge (..)
  , WrapEnvelopeErr (..)
  , WrapForgeStateInfo (..)
  , WrapForgeStateUpdateError (..)
  , WrapGenTxId (..)
  , WrapHeaderHash (..)
  , WrapLedgerConfig (..)
  , WrapLedgerErr (..)
  , WrapLedgerEvent (..)
  , WrapLedgerUpdate (..)
  , WrapLedgerWarning (..)
  , WrapTentativeHeaderState (..)
  , WrapTentativeHeaderView (..)
  , WrapTipInfo (..)
  , WrapTxIn (..)
  , WrapTxMeasure (..)
  , WrapTxOut (..)
  , WrapValidatedGenTx (..)

    -- * Protocol based
  , WrapCanBeLeader (..)
  , WrapChainDepState (..)
  , WrapChainOrderConfig (..)
  , WrapConsensusConfig (..)
  , WrapIsLeader (..)
  , WrapLedgerView (..)
  , WrapTiebreakerView (..)
  , WrapValidateView (..)
  , WrapValidationErr (..)

    -- * Versioning
  , WrapNodeToClientVersion (..)
  , WrapNodeToNodeVersion (..)

    -- * Type family instances
  , Ticked (..)
  ) where

import Codec.Serialise (Serialise)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Block based
-------------------------------------------------------------------------------}

newtype WrapApplyTxErr blk = WrapApplyTxErr {unwrapApplyTxErr :: ApplyTxErr blk}
newtype WrapCannotForge blk = WrapCannotForge {unwrapCannotForge :: CannotForge blk}
newtype WrapEnvelopeErr blk = WrapEnvelopeErr {unwrapEnvelopeErr :: OtherHeaderEnvelopeError blk}
newtype WrapForgeStateInfo blk = WrapForgeStateInfo {unwrapForgeStateInfo :: ForgeStateInfo blk}
newtype WrapForgeStateUpdateError blk = WrapForgeStateUpdateError {unwrapForgeStateUpdateError :: ForgeStateUpdateError blk}
newtype WrapGenTxId blk = WrapGenTxId {unwrapGenTxId :: GenTxId blk}
newtype WrapHeaderHash blk = WrapHeaderHash {unwrapHeaderHash :: HeaderHash blk}
newtype WrapLedgerConfig blk = WrapLedgerConfig {unwrapLedgerConfig :: LedgerConfig blk}
newtype WrapLedgerEvent blk = WrapLedgerEvent {unwrapLedgerEvent :: AuxLedgerEvent (LedgerState blk)}
newtype WrapLedgerErr blk = WrapLedgerErr {unwrapLedgerErr :: LedgerError blk}
newtype WrapLedgerUpdate blk = WrapLedgerUpdate {unwrapLedgerUpdate :: LedgerUpdate blk}
newtype WrapLedgerWarning blk = WrapLedgerWarning {unwrapLedgerWarning :: LedgerWarning blk}
newtype WrapTentativeHeaderState blk = WrapTentativeHeaderState {unwrapTentativeHeaderState :: TentativeHeaderState blk}
newtype WrapTentativeHeaderView blk = WrapTentativeHeaderView {unwrapTentativeHeaderView :: TentativeHeaderView blk}
newtype WrapTipInfo blk = WrapTipInfo {unwrapTipInfo :: TipInfo blk}

-- | A data family wrapper for @'Validated' . 'GenTx'@
--
-- 'Validated' is is data family, so this is an outlier in this module full of
-- type family wrappers. However, the standard functor composition operator @f
-- :.: g@ incurs some type classes instances that are inappropriate when the
-- outer type constructor @f@ is a family and hence non-parametric (eg @'Eq' (f
-- :.: g)@ requires @'Data.Functor.Classes.Eq1' f)). The bespoke composition
-- 'WrapValidatedGenTx' therefore serves much the same purpose as the other
-- wrappers in this module.
newtype WrapValidatedGenTx blk = WrapValidatedGenTx {unwrapValidatedGenTx :: Validated (GenTx blk)}

newtype WrapTxMeasure blk = WrapTxMeasure {unwrapTxMeasure :: TxMeasure blk}

newtype WrapTxIn blk = WrapTxIn {unwrapTxIn :: TxIn blk}
newtype WrapTxOut blk = WrapTxOut {unwrapTxOut :: TxOut blk}

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype WrapCanBeLeader blk = WrapCanBeLeader {unwrapCanBeLeader :: CanBeLeader (BlockProtocol blk)}
newtype WrapChainDepState blk = WrapChainDepState {unwrapChainDepState :: ChainDepState (BlockProtocol blk)}
newtype WrapChainOrderConfig blk = WrapChainOrderConfig
  {unwrapChainOrderConfig :: ChainOrderConfig (TiebreakerView (BlockProtocol blk))}
newtype WrapConsensusConfig blk = WrapConsensusConfig {unwrapConsensusConfig :: ConsensusConfig (BlockProtocol blk)}
newtype WrapIsLeader blk = WrapIsLeader {unwrapIsLeader :: IsLeader (BlockProtocol blk)}
newtype WrapLedgerView blk = WrapLedgerView {unwrapLedgerView :: LedgerView (BlockProtocol blk)}
newtype WrapTiebreakerView blk = WrapTiebreakerView {unwrapTiebreakerView :: TiebreakerView (BlockProtocol blk)}
newtype WrapValidateView blk = WrapValidateView {unwrapValidateView :: ValidateView (BlockProtocol blk)}
newtype WrapValidationErr blk = WrapValidationErr {unwrapValidationErr :: ValidationErr (BlockProtocol blk)}

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

newtype WrapNodeToNodeVersion blk = WrapNodeToNodeVersion {unwrapNodeToNodeVersion :: BlockNodeToNodeVersion blk}
newtype WrapNodeToClientVersion blk = WrapNodeToClientVersion {unwrapNodeToClientVersion :: BlockNodeToClientVersion blk}

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance Eq (ApplyTxErr blk) => Eq (WrapApplyTxErr blk)
deriving instance Eq (GenTxId blk) => Eq (WrapGenTxId blk)
deriving instance Eq (LedgerError blk) => Eq (WrapLedgerErr blk)
deriving instance Eq (LedgerUpdate blk) => Eq (WrapLedgerUpdate blk)
deriving instance Eq (LedgerWarning blk) => Eq (WrapLedgerWarning blk)
deriving instance Eq (OtherHeaderEnvelopeError blk) => Eq (WrapEnvelopeErr blk)
deriving instance Eq (TentativeHeaderState blk) => Eq (WrapTentativeHeaderState blk)
deriving instance Eq (TentativeHeaderView blk) => Eq (WrapTentativeHeaderView blk)
deriving instance Eq (TipInfo blk) => Eq (WrapTipInfo blk)
deriving instance Eq (Validated (GenTx blk)) => Eq (WrapValidatedGenTx blk)

deriving instance Ord (GenTxId blk) => Ord (WrapGenTxId blk)
deriving instance Ord (TentativeHeaderState blk) => Ord (WrapTentativeHeaderState blk)

deriving instance Show (ApplyTxErr blk) => Show (WrapApplyTxErr blk)
deriving instance Show (CannotForge blk) => Show (WrapCannotForge blk)
deriving instance Show (ForgeStateInfo blk) => Show (WrapForgeStateInfo blk)
deriving instance Show (ForgeStateUpdateError blk) => Show (WrapForgeStateUpdateError blk)
deriving instance Show (GenTxId blk) => Show (WrapGenTxId blk)
deriving instance Show (LedgerError blk) => Show (WrapLedgerErr blk)
deriving instance Show (LedgerUpdate blk) => Show (WrapLedgerUpdate blk)
deriving instance Show (LedgerWarning blk) => Show (WrapLedgerWarning blk)
deriving instance Show (OtherHeaderEnvelopeError blk) => Show (WrapEnvelopeErr blk)
deriving instance Show (TentativeHeaderState blk) => Show (WrapTentativeHeaderState blk)
deriving instance Show (TentativeHeaderView blk) => Show (WrapTentativeHeaderView blk)
deriving instance Show (TipInfo blk) => Show (WrapTipInfo blk)
deriving instance Show (Validated (GenTx blk)) => Show (WrapValidatedGenTx blk)

deriving instance
  NoThunks (GenTxId blk) => NoThunks (WrapGenTxId blk)
deriving instance
  NoThunks (LedgerError blk) => NoThunks (WrapLedgerErr blk)
deriving instance
  NoThunks (OtherHeaderEnvelopeError blk) => NoThunks (WrapEnvelopeErr blk)
deriving instance
  NoThunks (TentativeHeaderState blk) => NoThunks (WrapTentativeHeaderState blk)
deriving instance
  NoThunks (TipInfo blk) => NoThunks (WrapTipInfo blk)
deriving instance
  NoThunks (Validated (GenTx blk)) => NoThunks (WrapValidatedGenTx blk)

deriving instance Show (TxIn blk) => Show (WrapTxIn blk)
deriving instance Eq (TxIn blk) => Eq (WrapTxIn blk)
deriving instance Ord (TxIn blk) => Ord (WrapTxIn blk)
deriving instance NoThunks (TxIn blk) => NoThunks (WrapTxIn blk)

deriving instance Show (TxOut blk) => Show (WrapTxOut blk)
deriving instance Eq (TxOut blk) => Eq (WrapTxOut blk)
deriving instance Ord (TxOut blk) => Ord (WrapTxOut blk)
deriving instance NoThunks (TxOut blk) => NoThunks (WrapTxOut blk)

{-------------------------------------------------------------------------------
  .. consensus based
-------------------------------------------------------------------------------}

deriving instance Eq (ChainDepState (BlockProtocol blk)) => Eq (WrapChainDepState blk)
deriving instance Eq (TiebreakerView (BlockProtocol blk)) => Eq (WrapTiebreakerView blk)
deriving instance Eq (ValidationErr (BlockProtocol blk)) => Eq (WrapValidationErr blk)

deriving instance Ord (TiebreakerView (BlockProtocol blk)) => Ord (WrapTiebreakerView blk)

deriving instance
  ChainOrder (TiebreakerView (BlockProtocol blk)) => ChainOrder (WrapTiebreakerView blk)

deriving instance Show (ChainDepState (BlockProtocol blk)) => Show (WrapChainDepState blk)
deriving instance Show (LedgerView (BlockProtocol blk)) => Show (WrapLedgerView blk)
deriving instance Show (TiebreakerView (BlockProtocol blk)) => Show (WrapTiebreakerView blk)
deriving instance Show (ValidationErr (BlockProtocol blk)) => Show (WrapValidationErr blk)

deriving instance NoThunks (ChainDepState (BlockProtocol blk)) => NoThunks (WrapChainDepState blk)
deriving instance NoThunks (TiebreakerView (BlockProtocol blk)) => NoThunks (WrapTiebreakerView blk)
deriving instance NoThunks (ValidationErr (BlockProtocol blk)) => NoThunks (WrapValidationErr blk)

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

deriving instance Show (BlockNodeToNodeVersion blk) => Show (WrapNodeToNodeVersion blk)
deriving instance Show (BlockNodeToClientVersion blk) => Show (WrapNodeToClientVersion blk)

deriving instance Eq (BlockNodeToNodeVersion blk) => Eq (WrapNodeToNodeVersion blk)
deriving instance Eq (BlockNodeToClientVersion blk) => Eq (WrapNodeToClientVersion blk)

{-------------------------------------------------------------------------------
  Serialise instances

  These are primarily useful in testing.
-------------------------------------------------------------------------------}

deriving instance Serialise (GenTxId blk) => Serialise (WrapGenTxId blk)
deriving instance Serialise (ChainDepState (BlockProtocol blk)) => Serialise (WrapChainDepState blk)
deriving instance Serialise (TipInfo blk) => Serialise (WrapTipInfo blk)

{-------------------------------------------------------------------------------
  Ticking

  These are just forwarding instances
-------------------------------------------------------------------------------}

newtype instance Ticked (WrapChainDepState blk) = WrapTickedChainDepState
  { unwrapTickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
  }
