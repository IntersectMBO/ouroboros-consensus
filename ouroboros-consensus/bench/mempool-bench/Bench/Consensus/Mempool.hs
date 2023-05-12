{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Bench.Consensus.Mempool (
    -- * Commands
    MempoolCmd (..)
    -- ** Queries on commands
  , getCmdTx
  , getCmdsTxs
  , getCmdTxId
  , getCmdsTxIds
    -- * Commands execution
  , run
  ) where

import           Bench.Consensus.Mempool.TestBlock ()
import           Bench.Consensus.MempoolWithMockedLedgerItf
                     (MempoolWithMockedLedgerItf, addTx)
import           Control.DeepSeq (NFData)
import           Control.Monad (void)
import           Data.Foldable (traverse_)
import           GHC.Generics (Generic)
import           Data.Maybe (mapMaybe)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Ouroboros.Consensus.Mempool.API (AddTxOnBehalfOf (..))

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data MempoolCmd blk =
    -- | Add a transaction.
    --
    -- NB: if the mempool is full, the execution of this command will block
    -- until the mempool has at least one byte free. As a consenquence, if these
    -- commands are run sequentially the benchmarks or tests will deadlock if
    -- the maxium mempool capacity is reached.
    AddTx (Ledger.GenTx blk)
  deriving (Generic)

deriving anyclass instance (NFData (Ledger.GenTx blk)) => NFData (MempoolCmd blk)

getCmdTx :: MempoolCmd blk -> Maybe (Ledger.GenTx blk)
getCmdTx (AddTx tx) = Just tx

getCmdsTxs :: [MempoolCmd blk] -> [Ledger.GenTx blk]
getCmdsTxs = mapMaybe getCmdTx

getCmdTxId ::
     Ledger.HasTxId (Ledger.GenTx blk)
  => MempoolCmd blk -> Maybe (Ledger.TxId (Ledger.GenTx blk))
getCmdTxId = fmap Ledger.txId . getCmdTx

getCmdsTxIds ::
     Ledger.HasTxId (Ledger.GenTx blk)
  => [MempoolCmd blk] -> [Ledger.TxId (Ledger.GenTx blk)]
getCmdsTxIds = mapMaybe getCmdTxId

{-------------------------------------------------------------------------------
  Commands execution
-------------------------------------------------------------------------------}

-- TODO: the interpretation of running the command should be defined elsewhere,
-- and tested by state-mathine tests.
run ::
     Monad m
  => MempoolWithMockedLedgerItf m blk -> [MempoolCmd blk] -> m ()
run mempool = traverse_ (runCmd mempool)

runCmd ::
     Monad m
  => MempoolWithMockedLedgerItf m blk -> MempoolCmd blk -> m ()
runCmd mempool = \case
    AddTx tx -> void $ addTx mempool AddTxForRemotePeer tx -- TODO: we might want to benchmark the 'Intervene' case
