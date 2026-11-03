{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adaptive endorsements: endorse less after announcements miss their
-- quorum, and creep back to the full capacity while they make it.
--
-- The vote weight an announcement collected by the time the next block
-- arrives is its participation, a share of total stake like the quorum
-- threshold. Each announcement that reached the quorum adds a step back to the
-- endorser-block capacity, which the forge applies before it cuts the mempool
-- snapshot. Only one whose next block came late enough to certify it can miss
-- the quorum, and each that did cuts the capacity by a factor: an earlier
-- block cut its votes short.
--
-- The controller does not anticipate: a quorum made with any margin never
-- cuts. The cut is deep and the recovery gradual, because a missed quorum loses
-- a whole endorser block while too small a one loses a share of it, and a
-- backlog keeps endorser blocks full until the cut gets them under the cliff.
module LeiosAdaptiveEndorsements
  ( LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , stepAdaptiveEb
  , forkAdaptiveEndorsements
  ) where

import Control.Monad (guard, void)
import Control.ResourceRegistry (ResourceRegistry, forkLinkedThread)
import Control.Tracer (Tracer, traceWith)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import LeiosDemoTypes
  ( AnnouncementFields
  , HasLeiosVoting (..)
  , LeiosPoint (..)
  , RbHash (..)
  , TraceLeiosKernel (..)
  , announcementLeiosPoint
  )
import LeiosVoteState (LeiosVoteState (..))
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , ConvertRawHash (..)
  , Header
  , HeaderHash
  , SlotNo (..)
  , WithOrigin (..)
  , headerHash
  , headerPrevHash
  )
import Ouroboros.Consensus.HeaderValidation
  ( annTipHash
  , annTipSlotNo
  , headerStateChainDep
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
import Ouroboros.Consensus.Ledger.Extended (headerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  , leiosCommitteeOfTip
  )
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTVar
  , atomically
  , newTVarIO
  , readTVar
  , retry
  , writeTVar
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | How the endorser-block capacity follows missed quorums.
data LeiosAdaptiveEbConfig = MkLeiosAdaptiveEbConfig
  { aebDecrease :: !Double
  -- ^ The factor a missed quorum scales the capacity by, in @(0, 1)@.
  , aebIncrease :: !Double
  -- ^ The share of the protocol's capacity a quorum made adds back.
  , aebMinScale :: !Double
  -- ^ The capacity never shrinks below this share of what the protocol allows.
  }
  deriving (Eq, Show)

defaultLeiosAdaptiveEbConfig :: LeiosAdaptiveEbConfig
defaultLeiosAdaptiveEbConfig =
  MkLeiosAdaptiveEbConfig
    { aebDecrease = 0.8
    , aebIncrease = 0.02
    , aebMinScale = 0.1
    }

-- | Take whether an announcement whose certification was possible missed its
-- quorum, and move the share of the protocol's endorser-block capacity to
-- forge with.
stepAdaptiveEb :: LeiosAdaptiveEbConfig -> Bool -> Double -> Double
stepAdaptiveEb cfg missed scale
  | missed = max (aebMinScale cfg) (scale * aebDecrease cfg)
  | otherwise = min 1 (scale + aebIncrease cfg)

-- | The block at the tip of the selection, and whether it announced an
-- endorser block.
data LastTip blk = LastTip
  { ltHash :: !(HeaderHash blk)
  , ltSlot :: !SlotNo
  , ltAnnounced :: !(Maybe AnnouncementFields)
  }

-- | Start the controller, and give the forge what to read the capacity scale
-- from: the share of the protocol's endorser-block capacity to forge with.
forkAdaptiveEndorsements ::
  forall m blk.
  ( IOLike m
  , ConvertRawHash blk
  , HasLeiosVoting blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  ) =>
  ResourceRegistry m ->
  Tracer m TraceLeiosKernel ->
  LeiosAdaptiveEbConfig ->
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosVoteState m ->
  -- | Whether the node is caught up.
  STM m Bool ->
  m (STM m Rational)
forkAdaptiveEndorsements registry tracer cfg lcfg chainDB voteState caughtUp = do
  scaleVar <- newTVarIO 1
  void . forkLinkedThread registry "NodeKernel.leiosAdaptiveEndorsements" $
    runAdaptiveEndorsements tracer cfg lcfg chainDB voteState caughtUp scaleVar
  pure (readTVar scaleVar)

-- | Follow the selection, and on each block that extends one which announced
-- an endorser block, take whether that announcement collected its quorum and
-- move the capacity scale in the given variable.
--
-- Samples only while the node is caught up: a node syncing past old
-- announcements has none of their votes.
runAdaptiveEndorsements ::
  forall m blk.
  ( IOLike m
  , ConvertRawHash blk
  , HasLeiosVoting blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  ) =>
  Tracer m TraceLeiosKernel ->
  LeiosAdaptiveEbConfig ->
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosVoteState m ->
  -- | Whether the node is caught up.
  STM m Bool ->
  StrictTVar m Rational ->
  m Void
runAdaptiveEndorsements tracer cfg lcfg chainDB voteState caughtUp scaleVar =
  go Nothing 1
 where
  go :: Maybe (LastTip blk) -> Double -> m Void
  go lastTip scale = do
    (tip, prevHash, ext, isCaughtUp) <- atomically $ do
      chain <- ChainDB.getCurrentChain chainDB
      ext <- ChainDB.getCurrentLedger chainDB
      case (newHead chain, headerStateTip (headerState ext)) of
        -- The selection and its ledger state agree on the tip, and it moved.
        (Just hdr, NotOrigin tip)
          | annTipHash tip == headerHash hdr
          , Just (annTipHash tip) /= (ltHash <$> lastTip) -> do
              isCaughtUp <- caughtUp
              pure (tip, headerPrevHash hdr, ext, isCaughtUp)
        _ -> retry
    let slot = annTipSlotNo tip
        announced = do
          ann <- protocolStateLeiosAnnouncement @blk (headerStateChainDep (headerState ext))
          -- The announcement the header state carries may be an older block's.
          guard (pointSlotNo (announcementLeiosPoint ann) == slot)
          pure ann
    scale' <- case lastTip of
      Just LastTip{ltHash, ltSlot, ltAnnounced = Just ann}
        | prevHash == BlockHash ltHash
        , isCaughtUp ->
            sample
              ltHash
              ltSlot
              ann
              slot
              (getMinCertificationGap lcfg (ledgerState ext))
              (snd <$> leiosCommitteeOfTip lcfg (ledgerState ext))
              scale
      _ -> pure scale
    go (Just LastTip{ltHash = annTipHash tip, ltSlot = slot, ltAnnounced = announced}) scale'

  newHead :: AnchoredFragment (Header blk) -> Maybe (Header blk)
  newHead = either (const Nothing) Just . AF.head

  sample announcer announcerSlot ann slot minGap mThreshold scale = do
    let rbHash = MkRbHash (toRawHash (Proxy @blk) announcer)
        certifiable = case (minGap, mThreshold) of
          (Just gap, Just _) -> unSlotNo slot - unSlotNo announcerSlot >= unSlotNo gap
          _ -> False
    voters <- queryVoters voteState rbHash
    let participation = fromRational (sum voters)
        missedQuorum = any (sum voters <) mThreshold
        reachedQuorum = any (sum voters >=) mThreshold
        scale'
          | reachedQuorum = stepAdaptiveEb cfg False scale
          | certifiable = stepAdaptiveEb cfg missedQuorum scale
          | otherwise = scale
    atomically $ writeTVar scaleVar (toRational scale')
    traceWith tracer $
      TraceLeiosAdaptiveEb
        { ebPoint = announcementLeiosPoint ann
        , participation
        , missedQuorum
        , certifiable
        , ebCapacityScale = scale'
        }
    pure scale'
