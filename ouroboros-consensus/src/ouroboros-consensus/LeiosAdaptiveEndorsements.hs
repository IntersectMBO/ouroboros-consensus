{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adaptive endorsements: endorse less while recent announcements lose
-- their quorum margin, and more again as it recovers.
--
-- The vote weight an announcement collected by the time the next block
-- arrives is its participation, a share of total stake like the quorum
-- threshold. The stake that voted on any recent sample is alive, and the
-- error is the share of the alive stake's quorum margin the announcement
-- lost. Only announcements whose next block came late enough to certify them
-- count. A proportional controller turns the filtered error into a scale on
-- the endorser-block capacity, which the forge applies before it cuts the
-- mempool snapshot.
--
-- The error never rises above zero, so there is no setpoint and no I term: a
-- persistent loss holds the capacity at a fixed cut. Stake that withholds
-- votes, or EBs that collect none, cut it by at most @exp (-gainP)@ until the
-- withheld stake reaches the margin, where it fails every certificate anyway.
module LeiosAdaptiveEndorsements
  ( LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , AdaptiveEbState (..)
  , initialAdaptiveEbState
  , lostMarginShare
  , stepAdaptiveEb
  , forkAdaptiveEndorsements
  ) where

import Control.Monad (guard, void)
import Control.ResourceRegistry (ResourceRegistry, forkLinkedThread)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
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

-- | How the endorser-block capacity follows the quorum margin.
data LeiosAdaptiveEbConfig = MkLeiosAdaptiveEbConfig
  { aebGainP :: !Double
  -- ^ How hard a lost margin cuts: losing a share @e@ of it, sustained,
  -- scales the capacity by @exp (-gainP * e)@.
  , aebAttack :: !Double
  -- ^ Filter gain while the margin shrinks, in @(0, 1]@.
  , aebRelease :: !Double
  -- ^ Filter gain while the margin recovers, in @(0, 1]@.
  , aebMinScale :: !Double
  -- ^ The capacity never shrinks below this share of what the protocol allows.
  , aebAliveWindow :: !Int
  -- ^ Stake that voted on any of this many recent samples counts as alive.
  }
  deriving (Eq, Show)

defaultLeiosAdaptiveEbConfig :: LeiosAdaptiveEbConfig
defaultLeiosAdaptiveEbConfig =
  MkLeiosAdaptiveEbConfig
    { aebGainP = 2
    , aebAttack = 0.1
    , aebRelease = 0.03
    , aebMinScale = 0.1
    , aebAliveWindow = 20
    }

data AdaptiveEbState = MkAdaptiveEbState
  { aebFilteredError :: !Double
  -- ^ The margin share, filtered.
  , aebScale :: !Double
  -- ^ The share of the protocol's endorser-block capacity to forge with.
  }
  deriving (Eq, Show)

initialAdaptiveEbState :: AdaptiveEbState
initialAdaptiveEbState = MkAdaptiveEbState{aebFilteredError = 0, aebScale = 1}

-- | The share of the alive stake's quorum margin an announcement lost, in
-- @[-1, 0]@ when the participation is at most the alive stake. With no margin
-- left nobody can certify, which reads as the whole margin lost.
lostMarginShare ::
  -- | The quorum threshold.
  Double ->
  -- | The alive stake.
  Double ->
  -- | The participation.
  Double ->
  Double
lostMarginShare threshold alive participation
  | alive > threshold = max (-1) ((participation - alive) / (alive - threshold))
  | otherwise = -1

-- | Take the margin share of one announcement whose certification was
-- possible.
stepAdaptiveEb :: LeiosAdaptiveEbConfig -> Double -> AdaptiveEbState -> AdaptiveEbState
stepAdaptiveEb cfg err st =
  MkAdaptiveEbState{aebFilteredError = filtered, aebScale = scale}
 where
  prev = aebFilteredError st
  gain
    | err < prev = aebAttack cfg
    | otherwise = aebRelease cfg
  filtered = prev + gain * (err - prev)
  scale = max (aebMinScale cfg) . min 1 . exp $ aebGainP cfg * min 0 filtered

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
-- an endorser block, take the weight that announcement collected against the
-- alive stake and move the capacity scale in the given variable.
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
  go Nothing Seq.empty initialAdaptiveEbState
 where
  -- The window holds the most recent samples, whose voters are alive.
  go :: Maybe (LastTip blk) -> Seq RbHash -> AdaptiveEbState -> m Void
  go lastTip window st = do
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
    (window', st') <- case lastTip of
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
              window
              st
      _ -> pure (window, st)
    go (Just LastTip{ltHash = annTipHash tip, ltSlot = slot, ltAnnounced = announced}) window' st'

  newHead :: AnchoredFragment (Header blk) -> Maybe (Header blk)
  newHead = either (const Nothing) Just . AF.head

  sample announcer announcerSlot ann slot minGap mThreshold window st = do
    let rbHash = MkRbHash (toRawHash (Proxy @blk) announcer)
        certifiable = case (minGap, mThreshold) of
          (Just gap, Just _) -> unSlotNo slot - unSlotNo announcerSlot >= unSlotNo gap
          _ -> False
        window'
          | certifiable = Seq.drop (Seq.length window + 1 - aebAliveWindow cfg) (window |> rbHash)
          | otherwise = window
    voters <- queryVoters voteState rbHash
    -- Read now rather than kept from each sample, so late votes count too.
    alive <- Map.unions <$> traverse (queryVoters voteState) window'
    let participation = fromRational (sum voters)
        aliveStake = fromRational (sum alive)
        threshold = maybe 1 fromRational mThreshold
        err = lostMarginShare threshold aliveStake participation
        st'
          | certifiable = stepAdaptiveEb cfg err st
          | otherwise = st
    atomically $ writeTVar scaleVar (toRational (aebScale st'))
    traceWith tracer $
      TraceLeiosAdaptiveEb
        { ebPoint = announcementLeiosPoint ann
        , participation
        , aliveStake
        , marginShare = err
        , certifiable
        , ebCapacityScale = aebScale st'
        }
    pure (window', st')
