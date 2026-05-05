{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.ToExpr (mapExpr, setExpr, vvoteBody) where

import qualified Control.Monad.Class.MonadTime.SI as SI
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.TreeDiff
import qualified Data.TreeDiff.OMap as OMap
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), WithArrivalTime (..))
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.TxSeq
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API (LoE (..))
import Ouroboros.Consensus.Storage.ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.PerasVoteDB.API (PerasVoteTicketNo)
import Ouroboros.Consensus.Util.STM (Fingerprint, WithFingerprint)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import Ouroboros.Network.Block (MaxSlotNo)
import Ouroboros.Network.Mock.Chain
import Ouroboros.Network.Mock.ProducerState
import Ouroboros.Network.Point
import System.FS.API
import System.FS.CRC (CRC (..))
import Test.Cardano.Slotting.TreeDiff ()
import Test.Util.ToExpr ()

{-------------------------------------------------------------------------------
  ouroboros-network
-------------------------------------------------------------------------------}

instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
instance ToExpr (HeaderHash blk) => ToExpr (RealPoint blk)
instance (ToExpr slot, ToExpr hash) => ToExpr (Block slot hash)

deriving instance
  ( ToExpr blk
  , ToExpr (HeaderHash blk)
  ) =>
  ToExpr (Fragment.Anchor blk)

instance (ToExpr blk, ToExpr (HeaderHash blk)) => ToExpr (AnchoredFragment blk) where
  toExpr f = toExpr (Fragment.anchor f, Fragment.toOldestFirst f)

{-------------------------------------------------------------------------------
  ouroboros-consensus
-------------------------------------------------------------------------------}

instance
  ( ToExpr (LedgerState blk EmptyMK)
  , ToExpr (ChainDepState (BlockProtocol blk))
  , ToExpr (TipInfo blk)
  ) =>
  ToExpr (ExtLedgerState blk EmptyMK)

instance
  ( ToExpr (ChainDepState (BlockProtocol blk))
  , ToExpr (TipInfo blk)
  ) =>
  ToExpr (HeaderState blk)

instance ToExpr SecurityParam where
  toExpr = defaultExprViaShow

instance ToExpr CRC
instance ToExpr DiskSnapshot

instance ToExpr ChunkSize
instance ToExpr ChunkNo
instance ToExpr ChunkSlot
instance ToExpr RelativeSlot
instance
  ( ToExpr a
  , ToExpr b
  , ToExpr c
  , ToExpr d
  , ToExpr e
  , ToExpr f
  , ToExpr g
  , ToExpr h
  , ToExpr i
  , ToExpr j
  ) =>
  ToExpr (a, b, c, d, e, f, g, h, i, j)
  where
  toExpr (a, b, c, d, e, f, g, h, i, j) =
    App
      "_×_×_×_×_×_×_×_×_x_"
      [ toExpr a
      , toExpr b
      , toExpr c
      , toExpr d
      , toExpr e
      , toExpr f
      , toExpr g
      , toExpr h
      , toExpr i
      , toExpr j
      ]

instance ToExpr RelativeTime where
  toExpr = defaultExprViaShow
instance ToExpr ChunkInfo where
  toExpr = defaultExprViaShow
instance ToExpr FsError where
  toExpr fsError = App (show fsError) []

deriving instance ToExpr a => ToExpr (LoE a)

instance ToExpr PerasRoundNo where
  toExpr (PerasRoundNo r) = App (show r) []

instance ToExpr PerasWeight where
  toExpr (PerasWeight w) = App (show w) []

-- | Format a point as @#hash|slot@ or @genesis@.
formatPoint :: StandardHash blk => Point blk -> String
formatPoint GenesisPoint = "genesis"
formatPoint (BlockPoint slot hash) = extractHash (show hash) ++ "|" ++ show (unSlotNo slot)
 where
  extractHash s = stripParens (unwords (tail (words s)))
  stripParens ('(':rest) | not (Prelude.null rest) && last rest == ')' = init rest
  stripParens s = s

-- | Format a vote target as @<round = N, point = ...>@.
formatTarget :: StandardHash blk => PerasRoundNo -> Point blk -> String
formatTarget (PerasRoundNo r) pt = "<round = " ++ show r ++ ", point = " ++ formatPoint pt ++ ">"

-- | Format a 'NominalDiffTime' without the trailing @s@.
formatTime :: Show a => a -> String
formatTime t = init (show t)

-- | Extract the first 8 hex chars from a 'PerasVoterId' show output.
formatVoterId :: Show a => a -> String
formatVoterId v = "#" ++ take 8 (extractQuoted (show v))
 where
  extractQuoted s = case dropWhile (/= '"') s of
    '"':rest -> Prelude.takeWhile (/= '"') rest
    _ -> s

-- | Build the body fields of a 'ValidatedPerasVote' as a string.
vvoteBody :: StandardHash blk => ValidatedPerasVote blk -> String
vvoteBody (ValidatedPerasVote vote (PerasVoteStake stake)) =
  "for = " ++ formatTarget (pvVoteRound vote) (pvVoteBlock vote)
    ++ ", by = " ++ formatVoterId (pvVoteVoterId vote)
    ++ ", weight = " ++ showFFloat (Just 2) (fromRational stake :: Double) ""

-- | Build the body fields of a 'ValidatedPerasCert' as a string.
vcertBody :: StandardHash blk => ValidatedPerasCert blk -> String
vcertBody (ValidatedPerasCert cert _boost) =
  "for = " ++ formatTarget (pcCertRound cert) (pcCertBoostedBlock cert)

instance StandardHash blk => ToExpr (PerasVoteTarget blk) where
  toExpr (PerasVoteTarget roundNo blkPt) = App (formatTarget roundNo blkPt) []

instance StandardHash blk => ToExpr (PerasVote blk) where
  toExpr (PerasVote roundNo blkPt voterId) =
    App ("Vote { for = " ++ formatTarget roundNo blkPt ++ ", by = " ++ formatVoterId voterId ++ " }") []

instance StandardHash blk => ToExpr (ValidatedPerasVote blk) where
  toExpr v = App ("VVote { " ++ vvoteBody v ++ " }") []

instance StandardHash blk => ToExpr (PerasCert blk) where
  toExpr (PerasCert roundNo blkPt) =
    App ("Cert { for = " ++ formatTarget roundNo blkPt ++ " }") []

instance StandardHash blk => ToExpr (ValidatedPerasCert blk) where
  toExpr c = App ("VCert { " ++ vcertBody c ++ " }") []

instance {-# OVERLAPPING #-} StandardHash blk => ToExpr (WithArrivalTime (ValidatedPerasVote blk)) where
  toExpr (WithArrivalTime (RelativeTime t) v) =
    App ("WatVVote { " ++ vvoteBody v ++ ", at = " ++ formatTime t ++ " }") []

instance {-# OVERLAPPING #-} StandardHash blk => ToExpr (WithArrivalTime (ValidatedPerasCert blk)) where
  toExpr (WithArrivalTime (RelativeTime t) c) =
    App ("WatVCert { " ++ vcertBody c ++ ", at = " ++ formatTime t ++ " }") []

instance {-# OVERLAPPABLE #-} ToExpr a => ToExpr (WithArrivalTime a) where
  toExpr (WithArrivalTime (RelativeTime t) a) =
    App ("WithArrivalTime " ++ formatTime t) [toExpr a]

instance ToExpr PerasVoteTicketNo where
  toExpr t = App (last (words (show t))) []

instance ToExpr PerasParams where
  toExpr p =
    Rec "PerasParams" $ OMap.fromList
      [ ("ignoranceRounds", App (show (unPerasIgnoranceRounds (perasIgnoranceRounds p))) [])
      , ("cooldownRounds", App (show (unPerasCooldownRounds (perasCooldownRounds p))) [])
      , ("blockMinSlots", App (show (unPerasBlockMinSlots (perasBlockMinSlots p))) [])
      , ("certMaxRounds", App (show (unPerasCertMaxRounds (perasCertMaxRounds p))) [])
      , ("certArrivalThreshold", App (show (unPerasCertArrivalThreshold (perasCertArrivalThreshold p))) [])
      , ("roundLength", App (show (unPerasRoundLength (perasRoundLength p))) [])
      , ("weight", App (show (unPerasWeight (perasWeight p))) [])
      , ("quorumStake", App (show (unPerasQuorumStakeThreshold (perasQuorumStakeThreshold p))) [])
      , ("quorumMargin", App (show (unPerasQuorumStakeThresholdSafetyMargin (perasQuorumStakeThresholdSafetyMargin p))) [])
      ]

-- | Render a 'Map' as a list of key => value entries.
mapExpr :: (ToExpr k, ToExpr v) => Map.Map k v -> Expr
mapExpr m = Lst
  [App (exprFieldName (toExpr k) ++ " =>") [toExpr v] | (k, v) <- Map.toList m]
 where
  exprFieldName (App s _) = s
  exprFieldName (Rec s _) = s
  exprFieldName (Lst _) = "[]"

-- | Render a 'Set' as a list.
setExpr :: ToExpr a => Set.Set a -> Expr
setExpr s = Lst [toExpr x | x <- Set.toList s]

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
deriving instance
  ( ToExpr blk
  , ToExpr (HeaderHash blk)
  ) =>
  ToExpr (ChainProducerState blk)
deriving instance ToExpr a => ToExpr (WithFingerprint a)

instance ToExpr (TipInfo blk) => ToExpr (AnnTip blk)

{-------------------------------------------------------------------------------
  Mempool and transactions
-------------------------------------------------------------------------------}

deriving newtype instance ToExpr TicketNo

instance Show (TxId (GenTx blk)) => ToExpr (TxId (GenTx blk)) where
  toExpr x = App (show x) []

deriving instance
  ( ToExpr (GenTx blk)
  , LedgerSupportsMempool blk
  , measure ~ TxMeasure blk
  , ToExpr measure
  , ToExpr (Validated (GenTx blk))
  ) =>
  ToExpr (TxTicket measure (Validated (GenTx blk)))

instance
  ( ToExpr (GenTx blk)
  , LedgerSupportsMempool blk
  , ToExpr (Validated (GenTx blk))
  ) =>
  ToExpr (MempoolAddTxResult blk)
  where
  toExpr (MempoolTxAdded vtx _) = App "Added" [toExpr vtx]
  toExpr (MempoolTxRejected tx e) = App "Rejected" [toExpr tx, App (show e) []]
