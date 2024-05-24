{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.ToExpr () where

import qualified Control.Monad.Class.MonadTime.SI as SI
import           Data.TreeDiff
import qualified Data.TreeDiff.OMap as TD
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB (InvalidBlockReason)
import           Ouroboros.Consensus.Storage.ImmutableDB
import           Ouroboros.Consensus.Util.STM (Fingerprint, WithFingerprint)
import           Ouroboros.Network.Block (MaxSlotNo)
import           Ouroboros.Network.Mock.Chain
import           Ouroboros.Network.Mock.ProducerState
import           Ouroboros.Network.Point
import           System.FS.API
import           Test.Cardano.Slotting.TreeDiff ()
import           Test.Util.ToExpr ()

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

instance ToExpr SecurityParam

instance ToExpr ChunkSize
instance ToExpr ChunkNo
instance ToExpr ChunkSlot
instance ToExpr RelativeSlot
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e, ToExpr f, ToExpr g,
          ToExpr h, ToExpr i, ToExpr j)
      => ToExpr (a, b, c, d, e, f, g, h, i, j) where
    toExpr (a, b, c, d, e, f, g, h, i, j) = App "_×_×_×_×_×_×_×_×_x_"
      [ toExpr a, toExpr b, toExpr c, toExpr d, toExpr e, toExpr f, toExpr g
      , toExpr h, toExpr i, toExpr j
      ]

instance ToExpr ChunkInfo where
  toExpr = defaultExprViaShow
instance ToExpr FsError where
  toExpr fsError = App (show fsError) []


{-------------------------------------------------------------------------------
  si-timers
--------------------------------------------------------------------------------}

instance ToExpr SI.Time where toExpr = defaultExprViaShow


deriving anyclass instance ToExpr Fingerprint
deriving anyclass instance ToExpr FollowerNext
deriving anyclass instance ToExpr MaxSlotNo

deriving instance ToExpr (HeaderHash blk) => ToExpr (ChainHash blk)
deriving instance ToExpr (HeaderHash blk) => ToExpr (FollowerState blk)

deriving instance Generic FollowerNext
deriving instance Generic (Chain blk)
deriving instance Generic (ChainProducerState blk)
deriving instance Generic (FollowerState blk)

deriving instance ToExpr blk => ToExpr (Chain blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash blk)
                  )
                 => ToExpr (ChainProducerState blk)
deriving instance ToExpr a => ToExpr (WithFingerprint a)
deriving instance ( ToExpr (HeaderHash blk)
                  , ToExpr (ExtValidationError blk)
                  )
                 => ToExpr (InvalidBlockReason blk)

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
