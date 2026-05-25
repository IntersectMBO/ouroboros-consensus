{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Mempool
  ( -- * Type family instances
    GenTx (..)
  , MempoolAcc (..)
  , TxLocalData (..)
  , Validated (..)
  ) where

import Codec.Serialise
import Control.Monad.Except (runExcept, throwError)
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

-- | The Byron spec ledger has no on-disk tables; nothing to read per-tx.
data instance TxLocalData ByronSpecBlock = ByronSpecTxLocalData
  deriving stock Generic
  deriving anyclass NoThunks

-- | The mempool acc /is/ the ticked ledger state for the Byron spec: each tx is
-- applied in place to that state.
newtype instance MempoolAcc ByronSpecBlock = ByronSpecMempoolAcc
  {unByronSpecMempoolAcc :: Ticked LedgerState ByronSpecBlock}
  deriving stock Generic
  deriving anyclass NoThunks

instance LedgerSupportsMempool ByronSpecBlock where
  emptyAcc = ByronSpecMempoolAcc
  accTickedState = unByronSpecMempoolAcc

  prepareTx _cfg _slot _ts _acc _tx = pure ByronSpecTxLocalData

  applyTx cfg _wti _slot (ByronSpecMempoolAcc (TickedByronSpecLedgerState tip st)) tx _tld =
    case runExcept (GenTx.apply cfg (unByronSpecGenTx tx) st) of
      Left err -> throwError err
      Right st' ->
        pure
          ( ValidatedByronSpecGenTx tx
          , ByronSpecMempoolAcc (TickedByronSpecLedgerState tip st')
          )

  -- Byron spec doesn't have multiple validation modes
  reapplyTx cfg slot acc vtx tld = snd <$> applyTx cfg DoNotIntervene slot acc (forgetValidatedByronSpecGenTx vtx) tld

  txForgetValidated = forgetValidatedByronSpecGenTx

  mkMempoolApplyTxError = nothingMkMempoolApplyTxError

instance TxLimits ByronSpecBlock where
  type TxMeasure ByronSpecBlock = IgnoringOverflow ByteSize32

  -- Dummy values, as these are not used in practice.
  txWireSize = const . fromIntegral $ (0 :: Int)
  blockCapacityTxMeasure _cfg _st = IgnoringOverflow $ ByteSize32 1

  txMeasure _cfg _st _tld _tx = pure $ IgnoringOverflow $ ByteSize32 0
