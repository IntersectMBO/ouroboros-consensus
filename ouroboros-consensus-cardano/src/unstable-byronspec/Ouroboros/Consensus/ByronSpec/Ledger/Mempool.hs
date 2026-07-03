{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Mempool
  ( -- * Type family instances
    GenTx (..)
  , Validated (..)
  ) where

import Codec.Serialise
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunk (..), NoThunks)
import Ouroboros.Consensus.ByronSpec.Ledger.Block
import Ouroboros.Consensus.ByronSpec.Ledger.GenTx
  ( ByronSpecGenTx (..)
  , ByronSpecGenTxErr (..)
  )
import qualified Ouroboros.Consensus.ByronSpec.Ledger.GenTx as GenTx
import Ouroboros.Consensus.ByronSpec.Ledger.Ledger
import Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()
import Ouroboros.Consensus.Ledger.SupportsMempool

newtype instance GenTx ByronSpecBlock = ByronSpecGenTx
  { unByronSpecGenTx :: ByronSpecGenTx
  }
  deriving stock (Show, Generic)
  deriving anyclass Serialise
  deriving NoThunks via AllowThunk (GenTx ByronSpecBlock)

newtype instance Validated (GenTx ByronSpecBlock) = ValidatedByronSpecGenTx
  { forgetValidatedByronSpecGenTx :: GenTx ByronSpecBlock
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

type instance ApplyTxErr ByronSpecBlock = ByronSpecGenTxErr

instance LedgerSupportsMempool ByronSpecBlock where
  applyTx cfg _wti _slot tx _values (TickedByronSpecLedgerState tip st) =
    fmap
      ( \st' ->
          ( TickedByronSpecLedgerState tip st'
          , () -- ByronSpec has no on-disk tables, so applying produces no diff
          , ValidatedByronSpecGenTx tx
          )
      )
      $ GenTx.apply cfg (unByronSpecGenTx tx) st

  -- Byron spec doesn't have multiple validation modes
  reapplyTx cfg slot vtx values st = do
    (st', diff, _vtx) <-
      applyTx cfg DoNotIntervene slot (forgetValidatedByronSpecGenTx vtx) values st
    pure (st', diff)

  txForgetValidated = forgetValidatedByronSpecGenTx

  getTransactionKeySets _ = ()

  mkMempoolApplyTxError = nothingMkMempoolApplyTxError

instance TxLimits ByronSpecBlock where
  type TxMeasurePhase1 ByronSpecBlock = IgnoringOverflow ByteSize32
  type TxMeasurePhase2 ByronSpecBlock = TrivialTxMeasurePhase2

  -- Dummy values, as these are not used in practice.
  txWireSize = const . fromIntegral $ (0 :: Int)
  blockCapacityTxMeasure _cfg _st = TxMeasure (IgnoringOverflow $ ByteSize32 1) TrivialTxMeasurePhase2

  txMeasurePhase1 _cfg _st _tx = pure $ IgnoringOverflow $ ByteSize32 0
  txMeasurePhase2 _cfg _values _st _tx = pure TrivialTxMeasurePhase2
