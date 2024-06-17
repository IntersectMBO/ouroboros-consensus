{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.ToExpr () where

import           Data.TreeDiff
import qualified Data.TreeDiff.OMap as TD
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Network.Point
import           Test.Cardano.Slotting.TreeDiff ()

{-------------------------------------------------------------------------------
  ouroboros-network
-------------------------------------------------------------------------------}

instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
instance ToExpr (HeaderHash blk) => ToExpr (RealPoint blk)
instance (ToExpr slot, ToExpr hash) => ToExpr (Block slot hash)

{-------------------------------------------------------------------------------
  ouroboros-consensus
-------------------------------------------------------------------------------}

instance ( ToExpr (LedgerState blk EmptyMK)
         , ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (ExtLedgerState blk EmptyMK)

instance ( ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (HeaderState blk)

instance ToExpr (TipInfo blk) => ToExpr (AnnTip blk)

{-------------------------------------------------------------------------------
  Mempool and transactions
-------------------------------------------------------------------------------}

deriving newtype instance ToExpr TicketNo

instance Show (TxId (GenTx blk)) => ToExpr (TxId (GenTx blk)) where
  toExpr x = App (show x) []

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         ) => ToExpr (TxTicket (Validated (GenTx blk))) where
  toExpr tkt =
    Rec "Ticket"
    $ TD.fromList [ ("number", toExpr $ txTicketNo tkt)
                  , ("tx", toExpr $ txForgetValidated $ txTicketTx tkt)
                  , ("size", toExpr $ txTicketTxSizeInBytes tkt)]

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         , ToExpr (Validated (GenTx blk))
         ) => ToExpr (MempoolAddTxResult blk) where
  toExpr (MempoolTxAdded vtx)     = App "Added" [toExpr vtx]
  toExpr (MempoolTxRejected tx e) = App "Rejected" [toExpr tx, App (show e) [] ]
