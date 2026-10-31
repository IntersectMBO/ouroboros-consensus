{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adaptive endorsements: endorse less while the votes for recent
-- announcements fall short of a target, and more again as they recover.
--
-- The vote weight an announcement collected by the time the next block
-- arrives is its participation, a share of total stake like the quorum
-- threshold. Only announcements whose next block came late enough to certify
-- them count. A positional PD controller turns the filtered shortfall into a
-- scale on the endorser-block capacity, which the forge applies before it
-- cuts the mempool snapshot.
--
-- Positional, so without an I term: nobody knows the participation honest
-- stake reaches, and an integral would drag the capacity to its minimum on
-- any target above it. Here a persistent shortfall holds the capacity at a
-- fixed cut instead.
module LeiosAdaptiveEndorsements
  ( LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , AdaptiveEbState (..)
  , initialAdaptiveEbState
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
  , GetPrevHash
  , Header
  , HeaderHash
  , SlotNo (..)
  , WithOrigin (..)
  , headerHash
  , headerPrevHash
  )
import Ouroboros.Consensus.HeaderValidation
  ( HasAnnTip
  , annTipHash
  , annTipSlotNo
  , headerStateChainDep
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
import Ouroboros.Consensus.Ledger.Extended (headerState, ledgerState)
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

-- | How the endorser-block capacity follows participation.
data LeiosAdaptiveEbConfig = MkLeiosAdaptiveEbConfig
  { aebTargetParticipation :: !Double
  -- ^ Participation, as a share of total stake, below which the capacity
  -- shrinks. It can be 1: a shortfall that persists only holds a fixed cut.
  , aebGainP :: !Double
  -- ^ How hard a shortfall cuts: missing a share @s@ of the stake scales the
  -- capacity by @exp (-gainP * s)@.
  , aebGainD :: !Double
  -- ^ How much the change of the filtered shortfall adds, so that a falling
  -- participation cuts before its level has caught up.
  , aebAttack :: !Double
  -- ^ Filter gain while participation falls, in @(0, 1]@.
  , aebRelease :: !Double
  -- ^ Filter gain while participation recovers, in @(0, 1]@.
  , aebMinScale :: !Double
  -- ^ The capacity never shrinks below this share of what the protocol allows.
  }
  deriving (Eq, Show)

defaultLeiosAdaptiveEbConfig :: LeiosAdaptiveEbConfig
defaultLeiosAdaptiveEbConfig =
  MkLeiosAdaptiveEbConfig
    { aebTargetParticipation = 0.95
    , aebGainP = 4
    , aebGainD = 1
    , aebAttack = 0.3
    , aebRelease = 0.08
    , aebMinScale = 0.1
    }

data AdaptiveEbState = MkAdaptiveEbState
  { aebFilteredError :: !Double
  -- ^ Participation minus the target, filtered.
  , aebScale :: !Double
  -- ^ The share of the protocol's endorser-block capacity to forge with.
  }
  deriving (Eq, Show)

initialAdaptiveEbState :: AdaptiveEbState
initialAdaptiveEbState = MkAdaptiveEbState{aebFilteredError = 0, aebScale = 1}

-- | Take the participation of one announcement whose certification was
-- possible.
stepAdaptiveEb :: LeiosAdaptiveEbConfig -> Double -> AdaptiveEbState -> AdaptiveEbState
stepAdaptiveEb cfg participation st =
  MkAdaptiveEbState{aebFilteredError = filtered, aebScale = scale}
 where
  err = participation - aebTargetParticipation cfg
  prev = aebFilteredError st
  gain
    | err < prev = aebAttack cfg
    | otherwise = aebRelease cfg
  filtered = prev + gain * (err - prev)
  scale =
    max (aebMinScale cfg) . min 1 . exp $
      aebGainP cfg * min 0 filtered + aebGainD cfg * (filtered - prev)

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
  , GetPrevHash blk
  , HasAnnTip blk
  , ConvertRawHash blk
  , HasLeiosVoting blk
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
-- an endorser block, take the weight that announcement collected and move the
-- capacity scale in the given variable.
--
-- Samples only while the node is caught up: a node syncing past old
-- announcements has none of their votes.
runAdaptiveEndorsements ::
  forall m blk.
  ( IOLike m
  , GetPrevHash blk
  , HasAnnTip blk
  , ConvertRawHash blk
  , HasLeiosVoting blk
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
  go Nothing initialAdaptiveEbState
 where
  go :: Maybe (LastTip blk) -> AdaptiveEbState -> m Void
  go lastTip st = do
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
    st' <- case lastTip of
      Just LastTip{ltHash, ltSlot, ltAnnounced = Just ann}
        | prevHash == BlockHash ltHash
        , isCaughtUp ->
            sample ltHash ltSlot ann slot (getMinCertificationGap lcfg (ledgerState ext)) st
      _ -> pure st
    go (Just LastTip{ltHash = annTipHash tip, ltSlot = slot, ltAnnounced = announced}) st'

  newHead :: AnchoredFragment (Header blk) -> Maybe (Header blk)
  newHead = either (const Nothing) Just . AF.head

  sample announcer announcerSlot ann slot minGap st = do
    participation <-
      fromRational <$> queryTally voteState (MkRbHash (toRawHash (Proxy @blk) announcer))
    let certifiable = case minGap of
          Just gap -> unSlotNo slot - unSlotNo announcerSlot >= unSlotNo gap
          Nothing -> False
        st'
          | certifiable = stepAdaptiveEb cfg participation st
          | otherwise = st
    atomically $ writeTVar scaleVar (toRational (aebScale st'))
    traceWith tracer $
      TraceLeiosAdaptiveEb
        { ebPoint = announcementLeiosPoint ann
        , participation
        , certifiable
        , ebCapacityScale = aebScale st'
        }
    pure st'
