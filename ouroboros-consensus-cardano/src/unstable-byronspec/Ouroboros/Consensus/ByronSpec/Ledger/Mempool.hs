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
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils

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
  applyTx cfg _wti tx st =
    fmap
      ( \st' ->
          ( st{byronSpecLedgerState = st'}
          , ValidatedByronSpecGenTx tx
          )
      )
      $ GenTx.apply cfg (unByronSpecGenTx tx) (byronSpecLedgerState st)

  -- Byron spec doesn't have multiple validation modes
  reapplyTxBoth mode cfg _ vtx st =
    case mode of
      ReapplyLedgerState ->
        applyDiffs st . fst
          <$> applyTx cfg DoNotIntervene (forgetValidatedByronSpecGenTx vtx) st
      ReapplyTickedLedgerState ->
        fmap
          ( WrapTickedLedgerState
              . applyDiffs (unWrapTickedLedgerState st)
              . (\st' -> (unWrapTickedLedgerState st){tickedByronSpecLedgerState = st'})
          )
          $ GenTx.apply
            cfg
            (unByronSpecGenTx $ forgetValidatedByronSpecGenTx vtx)
            (tickedByronSpecLedgerState $ unWrapTickedLedgerState st)

  txForgetValidated = forgetValidatedByronSpecGenTx

  getTransactionKeySets _ = emptyLedgerTables

  mkMempoolApplyTxError = nothingMkMempoolApplyTxError

instance TxLimits ByronSpecBlock where
  type TxMeasure ByronSpecBlock = IgnoringOverflow ByteSize32

  -- Dummy values, as these are not used in practice.
  txWireSize = const . fromIntegral $ (0 :: Int)
  blockCapacityTxMeasure _mode _cfg _st = IgnoringOverflow $ ByteSize32 1

  txMeasure _cfg _st _tx = pure $ IgnoringOverflow $ ByteSize32 0
