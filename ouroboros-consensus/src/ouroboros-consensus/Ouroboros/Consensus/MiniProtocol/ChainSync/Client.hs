{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-strictness #-}
-- NOTE: With @-fstrictness@ optimisation (enabled by default for -O1), we get
-- an unexplained thunk in 'KnownIntersectionState' and thus a space leak. See
-- #1356.

-- | The ChainSync client logic
--
-- Its core specification is found in "The Shelley Networking Protocol",
-- currently found at
-- <https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf>.
--
-- It would be difficult to maintain or extrend this module without
-- understanding the @typed-protocols@ architecture; eg see
-- <https://github.com/input-output-hk/typed-protocols>.
--
-- This module is intended for qualified import, aliased as either CSC,
-- CSClient, or CsClient.

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client (
    -- * ChainSync client
    bracketChainSyncClient
  , chainSyncClient
    -- * Arguments
  , ChainDbView (..)
  , ConfigEnv (..)
  , DynamicEnv (..)
  , InternalEnv (..)
  , defaultChainDbView
    -- * Results
  , ChainSyncClientException (..)
  , ChainSyncClientResult (..)
    -- * Misc
  , Consensus
  , Our (..)
  , Their (..)
    -- * Genesis configuration
  , CSJConfig (..)
  , CSJEnabledConfig (..)
  , ChainSyncLoPBucketConfig (..)
  , ChainSyncLoPBucketEnabledConfig (..)
    -- * Trace events
  , InvalidBlockReason
  , TraceChainSyncClientEvent (..)
    -- * State shared with other components
  , ChainSyncClientHandle (..)
  , ChainSyncState (..)
  , ChainSyncStateView (..)
  , Jumping.noJumping
  , chainSyncStateFor
  , noIdling
  , noLoPBucket
  , viewChainSyncState
  ) where

import           Control.Monad (join, void)
import           Control.Monad.Except (runExcept, throwError)
import           Control.Tracer
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Network.TypedProtocol.Pipelined
import           NoThunks.Class (unsafeNoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History
                     (PastHorizonException (PastHorizon))
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..), validateHeader)
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import           Ouroboros.Consensus.HeaderValidation hiding (validateHeader)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping as Jumping
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB,
                     InvalidBlockReason)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.AnchoredFragment (cross)
import           Ouroboros.Consensus.Util.Assert (assertWithMsg)
import           Ouroboros.Consensus.Util.EarlyExit (WithEarlyExit, exitEarly)
import qualified Ouroboros.Consensus.Util.EarlyExit as EarlyExit
import           Ouroboros.Consensus.Util.IOLike hiding (handle)
import qualified Ouroboros.Consensus.Util.LeakyBucket as LeakyBucket
import           Ouroboros.Consensus.Util.STM (Fingerprint, Watcher (..),
                     WithFingerprint (..), withWatcher)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (Tip (..), getTipBlockNo)
import           Ouroboros.Network.BlockFetch.ConsensusInterface
                     (WhetherReceivingTentativeBlocks (..))
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
import           Ouroboros.Network.NodeToNode.Version (isPipeliningEnabled)
import           Ouroboros.Network.PeerSelection.PeerMetric.Type
                     (HeaderMetricsTracer)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

-- | Merely a helpful abbreviation
type Consensus
        (client :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type)
        blk
        m
  = client (Header blk) (Point blk) (Tip blk) m ChainSyncClientResult

-- | Abstract over the ChainDB
data ChainDbView m blk = ChainDbView {
    getCurrentChain :: STM m (AnchoredFragment (Header blk))
  ,
    getHeaderStateHistory :: STM m (HeaderStateHistory blk)
  ,
    getPastLedger :: Point blk -> STM m (Maybe (ExtLedgerState blk))
  ,
    getIsInvalidBlock ::
      STM m
          (WithFingerprint
              (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
  }

-- | Configuration of the leaky bucket when it is enabled.
data ChainSyncLoPBucketEnabledConfig = ChainSyncLoPBucketEnabledConfig {
    -- | The capacity of the bucket (think number of tokens).
    csbcCapacity :: Integer,
    -- | The rate of the bucket (think tokens per second).
    csbcRate     :: Rational
  }

-- | Configuration of the leaky bucket.
data ChainSyncLoPBucketConfig
  =
    -- | Fully disable the leaky bucket. The background thread that is used to
    -- run it will not even be started.
    ChainSyncLoPBucketDisabled
  |
    -- | Enable the leaky bucket.
    ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig

-- | Configuration of ChainSync Jumping
data CSJConfig
  =
    -- Disable ChainSync Jumping. All clients will fully synchronize with
    -- the chain of its peer.
    CSJDisabled
  |
    -- | Enable ChainSync Jumping
    CSJEnabled CSJEnabledConfig

data CSJEnabledConfig = CSJEnabledConfig {
  -- | The _ideal_ size for ChainSync jumps. Note that the algorithm
  -- is best-effort: there might not be exactly `csjcJumpSize` slots between two
  -- jumps, depending on the chain.
  --
  -- There can be a few less slots between jumps if there is not a block exactly
  -- at the boundary. Jumps are often made when a block is announced after the
  -- jump boundary.
  --
  -- There can be even less slots if a dynamo is elected and it requires an
  -- initial jump regardless of how far we are from the next jump boundary.
  --
  -- csjcJumpSize must be greater than 0 and smaller or equal to the genesis
  -- window size. The larger the jump size, the less jumps are made and peers
  -- are less involved in the syncing. A jump size as large as the genesis
  -- window has a higher change that dishonest peers can delay syncing by a
  -- small margin (around 2 minutes per dishonest peer with mainnet parameters).
  csjcJumpSize       :: SlotNo
}

defaultChainDbView ::
     (IOLike m, LedgerSupportsProtocol blk)
  => ChainDB m blk -> ChainDbView m blk
defaultChainDbView chainDB = ChainDbView {
    getCurrentChain       = ChainDB.getCurrentChain       chainDB
  , getHeaderStateHistory = ChainDB.getHeaderStateHistory chainDB
  , getPastLedger         = ChainDB.getPastLedger         chainDB
  , getIsInvalidBlock     = ChainDB.getIsInvalidBlock     chainDB
  }

-- | A newtype wrapper to avoid confusing our tip with their tip.
newtype Their a = Their { unTheir :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoThunks)

-- | A newtype wrapper to avoid confusing our tip with their tip.
newtype Our a = Our { unOur :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoThunks)

-- | Convenience function for reading a nested set of TVars and extracting some
-- data from 'ChainSyncState'.
viewChainSyncState ::
  IOLike m =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  (ChainSyncState blk -> a) ->
  STM m (Map peer a)
viewChainSyncState varHandles f =
  Map.map f <$> (traverse (readTVar . cschState) =<< readTVar varHandles)

-- | Convenience function for reading the 'ChainSyncState' for a single peer
-- from a nested set of TVars.
chainSyncStateFor ::
  Ord peer =>
  IOLike m =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  STM m (ChainSyncState blk)
chainSyncStateFor varHandles peer =
  readTVar . cschState . (Map.! peer) =<< readTVar varHandles

-- | Interface for the ChainSync client to manipulate the idling flag in
-- 'ChainSyncState'.
data Idling m = Idling {
    -- | Mark the peer as being idle.
    idlingStart :: !(m ())

    -- | Mark the peer as not being idle.
  , idlingStop  :: !(m ())
  }
  deriving stock (Generic)

deriving anyclass instance IOLike m => NoThunks (Idling m)

-- | No-op implementation, for tests.
noIdling :: Applicative m => Idling m
noIdling =
  Idling {
      idlingStart = pure ()
    , idlingStop  = pure ()
    }

-- | Interface to the LoP implementation for the ChainSync client.
data LoPBucket m = LoPBucket {
    -- | Pause the bucket, because the peer is alert and we're waiting for some
    -- condition.
    lbPause      :: !(m ())

    -- | Resume the bucket after pausing it.
  , lbResume     :: !(m ())

    -- | Notify the bucket that the peer has sent an interesting header.
  , lbGrantToken :: !(m ())
  }
  deriving stock (Generic)

deriving anyclass instance IOLike m => NoThunks (LoPBucket m)

-- | No-op implementation, for tests.
noLoPBucket :: Applicative m => LoPBucket m
noLoPBucket =
  LoPBucket {
      lbPause      = pure ()
    , lbResume     = pure ()
    , lbGrantToken = pure ()
    }

-- | Interface for the ChainSync client to its state allocated by
-- 'bracketChainSyncClient'.
data ChainSyncStateView m blk = ChainSyncStateView {
    -- | The current candidate fragment
    csvSetCandidate  :: !(AnchoredFragment (Header blk) -> STM m ())

    -- | Update the slot of the latest received header
  , csvSetLatestSlot :: !(WithOrigin SlotNo -> STM m ())

    -- | (Un)mark the peer as idling.
  , csvIdling        :: !(Idling m)

    -- | Control the 'LeakyBucket' for the LoP.
  , csvLoPBucket     :: !(LoPBucket m)

    -- | Jumping-related API.
  , csvJumping       :: !(Jumping.Jumping m blk)
  }
  deriving stock (Generic)

deriving anyclass instance (
  IOLike m,
  HasHeader blk,
  NoThunks (Header blk)
  ) => NoThunks (ChainSyncStateView m blk)

bracketChainSyncClient :: forall m peer blk a.
    ( IOLike m
    , Ord peer
    , LedgerSupportsProtocol blk
    )
 => Tracer m (TraceChainSyncClientEvent blk)
 -> ChainDbView m blk
 -> StrictTVar m (Map peer (ChainSyncClientHandle m blk))
    -- ^ The kill handle and states for each peer, we need the whole map because we
    -- (de)register nodes (@peer@).
 -> peer
 -> NodeToNodeVersion
 -> ChainSyncLoPBucketConfig
 -> CSJConfig
 -> (ChainSyncStateView m blk -> m a)
 -> m a
bracketChainSyncClient
    tracer
    ChainDbView { getIsInvalidBlock }
    varHandles
    peer
    version
    csBucketConfig
    csjConfig
    body
  = mkChainSyncClientHandleState >>= \csHandleState ->
    withCSJCallbacks csHandleState csjConfig $ \csjCallbacks ->
        withWatcher
            "ChainSync.Client.rejectInvalidBlocks"
            (invalidBlockWatcher csHandleState)
      $ LeakyBucket.execAgainstBucket lopBucketConfig
      $ \lopBucket ->
            body ChainSyncStateView {
              csvSetCandidate =
              modifyTVar csHandleState . \ c s -> s {csCandidate = c}
            , csvSetLatestSlot =
              modifyTVar csHandleState . \ ls s -> s {csLatestSlot = Just $! ls}
            , csvIdling = Idling {
                idlingStart = atomically $ modifyTVar csHandleState $ \ s -> s {csIdling = True}
              , idlingStop = atomically $ modifyTVar csHandleState $ \ s -> s {csIdling = False}
              }
            , csvLoPBucket = LoPBucket {
                lbPause = LeakyBucket.setPaused lopBucket True
              , lbResume = LeakyBucket.setPaused lopBucket False
              , lbGrantToken = void $ LeakyBucket.fill lopBucket 1
              }
            , csvJumping = csjCallbacks
            }
  where
    mkChainSyncClientHandleState =
      newTVarIO ChainSyncState {
          csCandidate = AF.Empty AF.AnchorGenesis
        , csLatestSlot = Nothing
        , csIdling = False
        }

    withCSJCallbacks ::
      StrictTVar m (ChainSyncState blk) ->
      CSJConfig ->
      (Jumping.Jumping m blk -> m a) ->
      m a
    withCSJCallbacks cschState CSJDisabled f = do
      tid <- myThreadId
      cschJumpInfo <- newTVarIO Nothing
      cschJumping <- newTVarIO Disengaged
      let handle = ChainSyncClientHandle {
              cschGDDKill = throwTo tid DensityTooLow
            , cschState
            , cschJumping
            , cschJumpInfo
            }
          insertHandle = atomically $ modifyTVar varHandles $ Map.insert peer handle
          deleteHandle = atomically $ modifyTVar varHandles $ Map.delete peer
      bracket_ insertHandle deleteHandle $ f Jumping.noJumping

    withCSJCallbacks csHandleState (CSJEnabled csjEnabledConfig) f =
      bracket (acquireContext csHandleState csjEnabledConfig) releaseContext $ \peerContext ->
        f $ Jumping.mkJumping peerContext
    acquireContext cschState (CSJEnabledConfig jumpSize) = do
        tid <- myThreadId
        atomically $ do
          cschJumpInfo <- newTVar Nothing
          context <- Jumping.makeContext varHandles jumpSize
          Jumping.registerClient context peer cschState $ \cschJumping -> ChainSyncClientHandle
            { cschGDDKill = throwTo tid DensityTooLow
            , cschState
            , cschJumping
            , cschJumpInfo
            }

    releaseContext = atomically . Jumping.unregisterClient

    invalidBlockWatcher varState =
        invalidBlockRejector
            tracer version getIsInvalidBlock (csCandidate <$> readTVar varState)

    -- | Wrapper around 'LeakyBucket.execAgainstBucket' that handles the
    -- disabled bucket by running the given action with dummy handlers.
    lopBucketConfig :: LeakyBucket.Config m
    lopBucketConfig =
      case csBucketConfig of
        ChainSyncLoPBucketDisabled -> LeakyBucket.dummyConfig
        ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig {csbcCapacity, csbcRate} ->
            LeakyBucket.Config
              { capacity = fromInteger $ csbcCapacity,
                rate = csbcRate,
                onEmpty = throwIO EmptyBucket,
                fillOnOverflow = True
              }

-- Our task: after connecting to an upstream node, try to maintain an
-- up-to-date header-only fragment representing their chain. We maintain
-- such candidate fragments in a map with upstream nodes as keys.
--
-- The block fetch logic will use these candidate fragments to download
-- blocks from, prioritising certain candidate fragments over others using
-- the consensus protocol. Whenever such a block has been downloaded and
-- added to the local 'ChainDB', the 'ChainDB' will perform chain
-- selection.
--
-- We also validate the headers of a candidate fragment by advancing the
-- 'ChainDepState' with the headers, which returns an error when validation
-- failed. Thus, in addition to the chain fragment of each candidate, we also
-- store a 'ChainDepState' corresponding to the head of the candidate fragment.
--
-- We must keep the candidate fragment synchronised with the corresponding
-- upstream chain. The upstream node's chain might roll forward or
-- backwards, and they will inform us about this. When we get these
-- messages, we will replicate these actions on the candidate fragment.
--
-- INVARIANT:
--
-- >           our tip
-- >             v
-- >   /--* .... *
-- >   |
-- > --*
-- >   |
-- >   \--* .... *
-- >        fragment tip
--
-- The distance from our tip to the intersection between our chain and the
-- fragment maintained for the upstream node cannot exceed @k@ blocks. When
-- this invariant cannot be maintained, the upstream node is on a fork that
-- is too distant and we should disconnect.
--
-- TODO #423 rate-limit switching chains, otherwise we can't place blame (we
-- don't know which candidate's chain included the point that was
-- poisoned). E.g. two rollbacks per time slot -> make it configurable ->
-- just a simple argument for now.
--
-- TODO #467 if the 'theirTip' that they sent us is on our chain, just
-- switch to it.


-- = Candidate fragment size
-- -------------------------
--
-- The size of the downloaded candidate fragment ('theirFrag') and the
-- corresponding header state history ('theirHeaderStateHistory', which has the
-- same size as 'theirFrag') is limited by how far in the future the ledger
-- view can forecast.
--
-- For PBFT (Byron), we can forecast up to @2k@ slots ahead. Assuming a chain
-- density of 100%, this means the look-ahead is @2k@ headers. For mainnet this
-- means @2 * 2160 = 4320@ headers.
--
-- For TPraos (Shelley), we can forecast up to @3k/f@ slots ahead. Assuming a
-- density of @f@, this means the look-ahead is @3k@ headers. For mainnet, this
-- means @3 * 2160 = 6480@ headers.
--
-- The figure below shows the relation between 'ourFrag' and 'theirFrag':
--
-- >                       k headers or less, when A is genesis
-- >              <--------------------->
-- >            anchor    header       tip
-- >              |         |           |
-- >              V         V           V
-- > 'ourFrag':   A-H-H-H-H-H-H-H-H-...-H
-- >                     \
-- > 'theirFrag':         H-H-H-H-...   ...   ...
-- >                    ^
-- >                    |
-- >           most recent intersection (between A and the tip)
--
-- Note that the 'ourFrag' and 'theirFrag' share anchors /at all times/. In the
-- figure above, the first three headers on 'ourFrag' are thus also on
-- 'theirFrag'. The further away the most recent intersection is from the
-- anchor point, the more headers 'theirFrag' and 'ourFrag' will have in
-- common.
--
-- In the \"worst\" case 'theirFrag' has the following length:
--
-- >                        k
-- >              <--------------------->
-- > 'ourFrag':   A-H-H-H-H-H-H-H-H-...-H
-- >                                    \
-- > 'theirFrag':                        H-H-H-H-H-H-H-H-H-H-H-H-H-H-H...-H
-- >                                     <-------------------------------->
-- >                                               max look-ahead
-- > max length   <------------------------------------------------------->
-- > of 'theirFrag'         k + max look-ahead
--
-- For PBFT this is @2160 + 4320 = 6480@ headers, for TPraos this is @2160 +
-- 6480 = 8640@ headers. The header state history will have the same length.
--
-- This worst case can happen when:
-- * We are more than 6480 or respectively 8640 blocks behind, bulk syncing,
--   and the BlockFetch client and/or the ChainDB can't keep up with the
--   ChainSync client.
-- * When our clock is running behind such that we are not adopting the
--   corresponding blocks because we think they are from the future.
-- * When an attacker is serving us headers from the future.
--
-- When we are in sync with the network, the fragment will typically be @k@ to
-- @k + 1@ headers long.

-- | State used when the intersection between the candidate and the current
-- chain is unknown.
data UnknownIntersectionState blk = UnknownIntersectionState {
    ourFrag               :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment. Note that this might be
    -- temporarily out of date w.r.t. the actual current chain until we update
    -- it again.
    --
    -- This fragment is used to select points from to find an intersection
    -- with the candidate.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
  ,
    ourHeaderStateHistory :: !(HeaderStateHistory blk)
    -- ^ 'HeaderStateHistory' corresponding to the tip (most recent block) of
    -- 'ourFrag'.
  ,
    uBestBlockNo          :: !BlockNo
    -- ^ The best block number of any header sent by this peer, to be used by
    -- the limit on patience.
  }
  deriving (Generic)

instance
     LedgerSupportsProtocol blk
  => NoThunks (UnknownIntersectionState blk) where
    showTypeOf _ = show $ typeRep (Proxy @(UnknownIntersectionState blk))

-- | State used when the intersection between the candidate and the current
-- chain is known.
data KnownIntersectionState blk = KnownIntersectionState {
    mostRecentIntersection  :: !(Point blk)
    -- ^ The most recent intersection point between 'theirFrag' and 'ourFrag'.
    -- Note that this is not necessarily the anchor point of both 'theirFrag'
    -- and 'ourFrag', they might have many more headers in common.
    --
    -- INVARIANT:
    -- @
    -- (==)
    --     (Just 'mostRecentIntersection')
    --     ('AF.intersectionPoint' 'theirFrag' 'ourFrag')
    -- @
    --
    -- It follows from the invariants on 'ourFrag' that this point is within
    -- the last @k@ headers of the current chain fragment, at time of
    -- computing the 'KnownIntersectionState'.
  ,
    ourFrag                 :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment used to maintain the invariants
    -- with. Note that this might be temporarily out of date w.r.t. the actual
    -- current chain until we update it again.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
    --
    -- INVARIANT: 'theirFrag' and 'ourFrag' have the same anchor point. From
    -- this follows that both fragments intersect. This also means that
    -- 'theirFrag' forks off within the last @k@ headers/blocks of the
    -- 'ourFrag'.
  ,
    theirFrag               :: !(AnchoredFragment (Header blk))
    -- ^ The candidate, the synched fragment of their chain.
    --
    -- See the \"Candidate fragment size\" note above.
  ,
    theirHeaderStateHistory :: !(HeaderStateHistory blk)
    -- ^ 'HeaderStateHistory' corresponding to the tip (most recent block) of
    -- 'theirFrag'.
    --
    -- INVARIANT: the tips in 'theirHeaderStateHistory' correspond to the
    -- headers in 'theirFrag', including the anchor.
    --
    -- See the \"Candidate fragment size\" note above.
  ,
    kBestBlockNo            :: !BlockNo
    -- ^ The best block number of any header sent by this peer, to be used by
    -- the limit on patience.
  }
  deriving (Generic)

instance
     LedgerSupportsProtocol blk
  => NoThunks (KnownIntersectionState blk) where
    showTypeOf _ = show $ typeRep (Proxy @(KnownIntersectionState blk))

checkKnownIntersectionInvariants ::
    ( HasHeader blk
    , HasHeader (Header blk)
    , HasAnnTip blk
    , ConsensusProtocol (BlockProtocol blk)
    )
 => ConsensusConfig (BlockProtocol blk)
 -> KnownIntersectionState blk
 -> Either String ()
checkKnownIntersectionInvariants cfg kis
    -- 'theirHeaderStateHistory' invariant
    | let HeaderStateHistory snapshots = theirHeaderStateHistory
          historyTips  = headerStateTip        <$> AS.toOldestFirst snapshots
          fragmentTips = NotOrigin . getAnnTip <$> AF.toOldestFirst theirFrag

          fragmentAnchorPoint = castPoint $ AF.anchorPoint theirFrag
          historyAnchorPoint  =
              withOriginRealPointToPoint
            $ annTipRealPoint <$> headerStateTip (AS.anchor snapshots)
    ,    historyTips        /= fragmentTips
      ||
         historyAnchorPoint /= fragmentAnchorPoint
    = throwError $ unwords
      [ "The tips in theirHeaderStateHistory"
      , "didn't match the headers in theirFrag:"
      , show historyTips
      , "vs"
      , show fragmentTips
      , "with anchors"
      , show historyAnchorPoint
      , "vs"
      , show fragmentAnchorPoint
      ]

    -- 'ourFrag' invariants
    | let nbHeaders      = AF.length ourFrag
          ourAnchorPoint = AF.anchorPoint ourFrag
    , nbHeaders < fromIntegral k
    , ourAnchorPoint /= GenesisPoint
    = throwError $ unwords
      [ "ourFrag contains fewer than k headers and not close to genesis:"
      , show nbHeaders
      , "vs"
      , show k
      , "with anchor"
      , show ourAnchorPoint
      ]

    | let ourFragAnchor   = AF.anchorPoint ourFrag
          theirFragAnchor = AF.anchorPoint theirFrag
    , ourFragAnchor /= theirFragAnchor
    = throwError $ unwords
      [ "ourFrag and theirFrag have different anchor points:"
      , show ourFragAnchor
      , "vs"
      , show theirFragAnchor
      ]

    -- 'mostRecentIntersection' invariant
    | let actualMostRecentIntersection =
              castPoint <$> AF.intersectionPoint theirFrag ourFrag
    , Just mostRecentIntersection /= actualMostRecentIntersection
    = throwError $ unwords
      [ "mostRecentIntersection not the most recent intersection"
      , "of theirFrag and ourFrag:"
      , show mostRecentIntersection
      , "vs"
      , show actualMostRecentIntersection
      ]

    | otherwise
    = return ()
  where
    SecurityParam k = protocolSecurityParam cfg

    KnownIntersectionState {
        mostRecentIntersection
      , ourFrag
      , theirFrag
      , theirHeaderStateHistory
      } = kis

assertKnownIntersectionInvariants ::
    ( HasHeader blk
    , HasHeader (Header blk)
    , HasAnnTip blk
    , ConsensusProtocol (BlockProtocol blk)
    , HasCallStack
    )
 => ConsensusConfig (BlockProtocol blk)
 -> KnownIntersectionState blk
 -> KnownIntersectionState blk
assertKnownIntersectionInvariants cfg kis =
    assertWithMsg (checkKnownIntersectionInvariants cfg kis) kis

{-------------------------------------------------------------------------------
  The ChainSync client definition
-------------------------------------------------------------------------------}

-- | Arguments determined by configuration
--
-- These are available before the diffusion layer is online.
data ConfigEnv m blk = ConfigEnv {
    mkPipelineDecision0     :: MkPipelineDecision
    -- ^ The pipelining decider to use after 'MsgFoundIntersect' arrives
  , tracer                  :: Tracer m (TraceChainSyncClientEvent blk)
  , cfg                     :: TopLevelConfig blk
  , someHeaderInFutureCheck :: InFutureCheck.SomeHeaderInFutureCheck m blk
  , chainDbView             :: ChainDbView m blk
  }

-- | Arguments determined dynamically
--
-- These are available only after the diffusion layer is online and/or on per
-- client basis.
data DynamicEnv m blk = DynamicEnv {
    version             :: NodeToNodeVersion
  , controlMessageSTM   :: ControlMessageSTM m
  , headerMetricsTracer :: HeaderMetricsTracer m
  , setCandidate        :: AnchoredFragment (Header blk) -> STM m ()
  , setLatestSlot       :: WithOrigin SlotNo -> STM m ()
  , idling              :: Idling m
  , loPBucket           :: LoPBucket m
  , jumping             :: Jumping.Jumping m blk
  }

-- | General values collectively needed by the top-level entry points
data InternalEnv m blk arrival judgment = InternalEnv {
    drainThePipe ::
      forall s n.
         NoThunks s
      => Nat n
      -> Stateful m blk s (ClientPipelinedStIdle 'Z)
      -> Stateful m blk s (ClientPipelinedStIdle n)
    -- ^ "Drain the pipe": collect and discard all in-flight responses and
    -- finally execute the given action.
  ,
    disconnect ::
      forall m' a.
         MonadThrow m'
      => ChainSyncClientException
      -> m' a
    -- ^ Disconnect from the upstream node by throwing the given exception.
    -- The cleanup is handled in 'bracketChainSyncClient'.
  ,
    headerInFutureCheck ::
        InFutureCheck.HeaderInFutureCheck m blk arrival judgment
  ,
    intersectsWithCurrentChain ::
        KnownIntersectionState blk
     -> STM m (UpdatedIntersectionState blk ())
    -- ^ A combinator necessary whenever relying on a
    -- 'KnownIntersectionState', since it's always possible that that
    -- intersection will go stale.
    --
    -- Look at the current chain fragment that may have been updated in the
    -- background. Check whether the candidate fragment still intersects with
    -- it. If so, update the 'KnownIntersectionState' and trim the candidate
    -- fragment to the new current chain fragment's anchor point. If not,
    -- return 'Nothing'.
    --
    -- INVARIANT: This a read-only STM transaction.
  ,
    terminate ::
        ChainSyncClientResult
     -> m (Consensus (ClientPipelinedStIdle 'Z) blk m)
    -- ^ Gracefully terminate the connection with the upstream node with the
    -- given result.
  ,
    terminateAfterDrain ::
      forall n.
         Nat n
      -> ChainSyncClientResult
      -> m (Consensus (ClientPipelinedStIdle n) blk m)
    -- ^ Same as 'terminate', but first 'drainThePipe'.
  ,
    traceException :: forall a. m a -> m a
    -- ^ Trace any 'ChainSyncClientException' if thrown.
  }

-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ChainSyncClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
chainSyncClient :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> Consensus ChainSyncClientPipelined blk m
chainSyncClient cfgEnv dynEnv =
    case someHeaderInFutureCheck cfgEnv of
        InFutureCheck.SomeHeaderInFutureCheck headerInFutureCheck ->
            ChainSyncClientPipelined
          $ continueWithState ()
          $ -- Start ChainSync by looking for an intersection between our
            -- current chain fragment and their chain.
            findIntersectionTop
                cfgEnv
                dynEnv
                (mkIntEnv headerInFutureCheck)
                (BlockNo 0)
                (ForkTooDeep GenesisPoint)
  where
    ConfigEnv {
        cfg
      , chainDbView
      , tracer
      } = cfgEnv

    ChainDbView {
        getCurrentChain
      } = chainDbView

    DynamicEnv {
        idling
      } = dynEnv

    mkIntEnv ::
        InFutureCheck.HeaderInFutureCheck m blk arrival judgment
     -> InternalEnv                       m blk arrival judgment
    mkIntEnv hifc = InternalEnv {
        drainThePipe
      ,
        disconnect = throwIO
      ,
        headerInFutureCheck = hifc
      ,
        intersectsWithCurrentChain
      ,
        terminate
      ,
        terminateAfterDrain = \n result ->
            continueWithState ()
          $ drainThePipe n
          $ Stateful $ \() -> terminate result
      ,
        traceException = \m -> do
            m `catch` \(e :: ChainSyncClientException) -> do
                traceWith tracer $ TraceException e
                throwIO e
      }

    drainThePipe ::
      forall s n.
         NoThunks s
      => Nat n
      -> Stateful m blk s (ClientPipelinedStIdle 'Z)
      -> Stateful m blk s (ClientPipelinedStIdle n)
    drainThePipe n0 m =
      let go ::
            forall n'.
               Nat n'
            -> s
            -> m (Consensus (ClientPipelinedStIdle n') blk m)
          go n s = case n of
              Zero    -> continueWithState s m
              Succ n' -> return $ CollectResponse Nothing $ ClientStNext {
                  recvMsgRollForward  = \_hdr _tip -> go n' s
                , recvMsgRollBackward = \_pt  _tip -> go n' s
                }
      in Stateful $ \s -> idlingStop idling >> go n0 s

    terminate ::
        ChainSyncClientResult
     -> m (Consensus (ClientPipelinedStIdle 'Z) blk m)
    terminate res = do
        traceWith tracer (TraceTermination res)
        pure (SendMsgDone res)

    intersectsWithCurrentChain ::
        KnownIntersectionState blk
     -> STM m (UpdatedIntersectionState blk ())
    intersectsWithCurrentChain kis = do
        let KnownIntersectionState {
                ourFrag
              , theirFrag
              , theirHeaderStateHistory
              , kBestBlockNo
              } = kis
        ourFrag' <- getCurrentChain

        -- Our current chain didn't change, and changes to their chain that
        -- might affect the intersection point are handled elsewhere
        -- ('rollBackward'), so we have nothing to do.
        let noChange = AF.headPoint ourFrag == AF.headPoint ourFrag'

        return $ if noChange then StillIntersects () kis else
            case cross ourFrag' theirFrag of
                Nothing -> NoLongerIntersects

                Just (intersection, trimmedCandidate) ->
                    -- Even though our current chain changed it still
                    -- intersects with candidate fragment, so update the
                    -- 'ourFrag' field and trim the candidate fragment to the
                    -- same anchor point.
                    --
                    -- Note that this is the only place we need to trim.
                    -- Headers on their chain can only become unnecessary
                    -- (eligible for trimming) in two ways: 1. we adopted them,
                    -- i.e., our chain changed (handled in this function); 2.
                    -- we will /never/ adopt them, which is handled in the "no
                    -- more intersection case".
                    StillIntersects ()
                  $ assertKnownIntersectionInvariants (configConsensus cfg)
                  $ KnownIntersectionState {
                        mostRecentIntersection  = castPoint intersection
                      , ourFrag                 = ourFrag'
                      , theirFrag               = trimmedCandidate
                      , theirHeaderStateHistory =
                            -- We trim the 'HeaderStateHistory' to the same
                            -- size as our fragment so they keep in sync.
                            HeaderStateHistory.trim
                                (AF.length trimmedCandidate)
                                theirHeaderStateHistory
                      , kBestBlockNo
                      }

{-------------------------------------------------------------------------------
  (Re-)Establishing a common intersection
-------------------------------------------------------------------------------}

findIntersectionTop ::
  forall m blk arrival judgment.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> InternalEnv m blk arrival judgment
  -> BlockNo
     -- ^ Peer's best block; needed to build an 'UnknownIntersectionState'.
  -> (Our (Tip blk) -> Their (Tip blk) -> ChainSyncClientResult)
     -- ^ Exception to throw when no intersection is found.
  -> Stateful m blk () (ClientPipelinedStIdle 'Z)
findIntersectionTop cfgEnv dynEnv intEnv =
    findIntersection
  where
    ConfigEnv {
        tracer
      , cfg
      , chainDbView
      } = cfgEnv

    ChainDbView {
        getCurrentChain
      , getHeaderStateHistory
      } = chainDbView

    DynamicEnv {
        setCandidate
      , jumping
      } = dynEnv

    InternalEnv {
        disconnect
      , terminate
      , traceException
      } = intEnv

    -- Try to find an intersection by sending points of our current chain to
    -- the server, if any of them intersect with their chain, roll back our
    -- chain to that point and start synching using that fragment. If none
    -- intersect, disconnect by throwing the exception obtained by calling the
    -- passed function.
    findIntersection ::
        BlockNo
        -- ^ Peer's best block; needed to build an 'UnknownIntersectionState'.
     -> (Our (Tip blk) -> Their (Tip blk) -> ChainSyncClientResult)
        -- ^ Exception to throw when no intersection is found.
     -> Stateful m blk () (ClientPipelinedStIdle 'Z)
    findIntersection uBestBlockNo mkResult = Stateful $ \() -> do
        (ourFrag, ourHeaderStateHistory) <- atomically $ (,)
            <$> getCurrentChain
            <*> getHeaderStateHistory
        -- This means that if an intersection is found for one of these points,
        -- it was an intersection within the last @k@ blocks of our current
        -- chain. If not, we could never switch to this candidate chain anyway.
        let maxOffset = fromIntegral (AF.length ourFrag)
            k         = protocolSecurityParam (configConsensus cfg)
            offsets   = mkOffsets k maxOffset
            points    =
                map castPoint
              $ AF.selectPoints (map fromIntegral offsets) ourFrag

            uis = UnknownIntersectionState {
                ourFrag               = ourFrag
              , ourHeaderStateHistory = ourHeaderStateHistory
              , uBestBlockNo
              }

        return
          $ SendMsgFindIntersect points
          $ ClientPipelinedStIntersect {
                recvMsgIntersectFound    = \i theirTip' ->
                    continueWithState uis
                  $ intersectFound (castPoint i) (Their theirTip')
              ,
                recvMsgIntersectNotFound = \theirTip' ->
                    terminate
                  $ mkResult (ourTipFromChain ourFrag) (Their theirTip')
              }

    -- One of the points we sent intersected our chain. This intersection point
    -- will become the new tip of the candidate fragment.
    intersectFound ::
        Point blk  -- ^ Intersection
     -> Their (Tip blk)
     -> Stateful m blk
            (UnknownIntersectionState blk)
            (ClientPipelinedStIdle 'Z)
    intersectFound intersection theirTip = Stateful $ \uis -> do
        let UnknownIntersectionState {
                ourFrag
              , ourHeaderStateHistory
              , uBestBlockNo
              } = uis
        traceWith tracer $
            TraceFoundIntersection
                intersection (ourTipFromChain ourFrag) theirTip
        traceException $ do
            -- Roll back the current chain fragment to the @intersection@.
            --
            -- While the primitives in the ChainSync protocol are "roll back",
            -- "roll forward (apply block)", etc. The /real/ primitive is
            -- "switch to fork", which means that a roll back is always
            -- followed by applying at least as many blocks that we rolled
            -- back.
            --
            -- This is important for 'rewindHeaderStateHistory', which can only
            -- roll back up to @k@ blocks, /once/, i.e., we cannot keep rolling
            -- back the same chain state multiple times, because that would
            -- mean that we store the chain state for the /whole chain/, all
            -- the way to genesis.
            --
            -- So the rewind below is fine when we are switching to a fork
            -- (i.e. it is followed by rolling forward again), but we need some
            -- guarantees that the ChainSync protocol /does/ in fact give us a
            -- switch-to-fork instead of a true rollback.
            (theirFrag, theirHeaderStateHistory) <- do
                case attemptRollback
                         intersection
                         (ourFrag, ourHeaderStateHistory)
                  of
                    Just (c, d) -> return (c, d)
                    Nothing ->
                        -- The @intersection@ is not on our fragment, even
                        -- though we sent only points from our fragment to find
                        -- an intersection with. The node must have sent us an
                        -- invalid intersection point.
                        disconnect
                      $ InvalidIntersection
                            intersection (ourTipFromChain ourFrag) theirTip
            let kis =
                   assertKnownIntersectionInvariants (configConsensus cfg)
                 $ KnownIntersectionState {
                       mostRecentIntersection  = intersection
                     , ourFrag
                     , theirFrag
                     , theirHeaderStateHistory
                     , kBestBlockNo            = uBestBlockNo
                     }
            atomically $ do
              updateJumpInfoSTM jumping kis
              setCandidate theirFrag
              setLatestSlot dynEnv (AF.headSlot theirFrag)
            continueWithState kis $
                knownIntersectionStateTop cfgEnv dynEnv intEnv theirTip

{-------------------------------------------------------------------------------
  Processing 'MsgRollForward' and 'MsgRollBackward'
-------------------------------------------------------------------------------}

knownIntersectionStateTop ::
  forall m blk arrival judgment.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> InternalEnv m blk arrival judgment
  -> Their (Tip blk)
  -> Stateful m blk
         (KnownIntersectionState blk)
         (ClientPipelinedStIdle 'Z)
knownIntersectionStateTop cfgEnv dynEnv intEnv =
    nextStep mkPipelineDecision0 Zero
      -- The 'MkPiplineDecision' and @'Nat' n@ arguments below could safely be
      -- merged into the 'KnownIntersectionState' record type, but it's
      -- unfortunately quite awkward to do so.
  where
    ConfigEnv {
        mkPipelineDecision0
      , tracer
      , cfg
      } = cfgEnv

    DynamicEnv {
        controlMessageSTM
      , headerMetricsTracer
      , idling
      , loPBucket
      , setCandidate
      , jumping
      } = dynEnv

    InternalEnv {
        drainThePipe
      , headerInFutureCheck
      , intersectsWithCurrentChain
      , terminateAfterDrain
      , traceException
      } = intEnv

    InFutureCheck.HeaderInFutureCheck {
        recordHeaderArrival
      } = headerInFutureCheck

    -- Request the next message (roll forward or backward), unless our chain
    -- has changed such that it no longer intersects with the candidate, in
    -- which case we initiate the intersection finding part of the protocol.
    --
    -- This is the main place we check whether our current chain has changed.
    -- We also check it in 'rollForward' to make sure we have an up-to-date
    -- intersection before calling 'getLedgerView'.
    --
    -- This is also the place where we checked whether we're asked to terminate
    -- by the mux layer or to wait and perform a CSJ jump.
    nextStep ::
        MkPipelineDecision
     -> Nat n
     -> Their (Tip blk)
     -> Stateful m blk
            (KnownIntersectionState blk)
            (ClientPipelinedStIdle n)
    nextStep mkPipelineDecision n theirTip = Stateful $ \kis ->
        atomically controlMessageSTM >>= \case
            -- We have been asked to terminate the client
            Terminate -> terminateAfterDrain n $ AskedToTerminate
            _continue -> do
                -- Wait until next jumping instruction, which can be either to
                -- jump or to run normal ChainSync.
                -- Pause LoP while waiting, we'll resume it if we get `RunNormally`
                traceWith tracer TraceJumpingWaitingForNextInstruction
                lbPause loPBucket
                instruction <- Jumping.jgNextInstruction jumping
                traceWith tracer $ TraceJumpingInstructionIs instruction
                case instruction of
                    Jumping.JumpInstruction jumpInstruction ->
                      continueWithState kis
                        $ drainThePipe n
                        $ offerJump mkPipelineDecision jumpInstruction
                    Jumping.RunNormally -> do
                      lbResume loPBucket
                      continueWithState kis
                        $ nextStep' mkPipelineDecision n theirTip

    nextStep' ::
        MkPipelineDecision
     -> Nat n
     -> Their (Tip blk)
     -> Stateful m blk
            (KnownIntersectionState blk)
            (ClientPipelinedStIdle n)
    nextStep' mkPipelineDecision n theirTip =
        Stateful $ \kis ->
                atomically (intersectsWithCurrentChain kis) >>= \case
                    -- Our chain (tip) didn't change or if it did, it still
                    -- intersects with the candidate fragment, so we can
                    -- continue requesting the next block.
                    StillIntersects () kis' -> do
                        let KnownIntersectionState {
                                theirFrag
                              } = kis'
                        atomically $ do
                          updateJumpInfoSTM jumping kis'
                          setCandidate theirFrag
                        return $
                            requestNext
                                kis'
                                mkPipelineDecision
                                n
                                theirTip
                                (AF.headBlockNo theirFrag)
                    -- Our chain (tip) has changed and it no longer intersects
                    -- with the candidate fragment, so we have to find a new
                    -- intersection, but first drain the pipe.
                    NoLongerIntersects ->
                        continueWithState ()
                      $ drainThePipe n
                      $ findIntersectionTop
                          cfgEnv
                          dynEnv
                          intEnv
                          (kBestBlockNo kis)
                          NoMoreIntersection

    offerJump ::
        MkPipelineDecision
     -> Jumping.JumpInstruction blk
     -> Stateful m blk
            (KnownIntersectionState blk)
            (ClientPipelinedStIdle Z)
    offerJump mkPipelineDecision jump = Stateful $ \kis -> do
        let jumpInfo = case jump of
              Jumping.JumpTo ji -> ji
              Jumping.JumpToGoodPoint ji -> ji
            dynamoTipPt = castPoint $ AF.headPoint $ jTheirFragment jumpInfo
        traceWith tracer $ TraceOfferJump dynamoTipPt
        return $
            SendMsgFindIntersect [dynamoTipPt] $
            ClientPipelinedStIntersect {
              recvMsgIntersectFound = \pt theirTip ->
                  if
                    | pt == dynamoTipPt -> do
                      Jumping.jgProcessJumpResult jumping $ Jumping.AcceptedJump jump
                      traceWith tracer $ TraceJumpResult $ Jumping.AcceptedJump jump
                      let kis' = case jump of
                            -- Since the updated kis is needed to validate headers,
                            -- we only update it if we are becoming a Dynamo or
                            -- an objector
                            Jumping.JumpToGoodPoint{} -> combineJumpInfo kis jumpInfo
                            _ -> kis
                      continueWithState kis' $ nextStep mkPipelineDecision Zero (Their theirTip)
                    | otherwise         -> throwIO InvalidJumpResponse
            ,
              recvMsgIntersectNotFound = \theirTip -> do
                Jumping.jgProcessJumpResult jumping $ Jumping.RejectedJump jump
                traceWith tracer $ TraceJumpResult $ Jumping.RejectedJump jump
                continueWithState kis $ nextStep mkPipelineDecision Zero (Their theirTip)
            }
        where
          combineJumpInfo ::
               KnownIntersectionState blk
            -> JumpInfo blk
            -> KnownIntersectionState blk
          combineJumpInfo kis ji =
            let mRewoundHistory =
                  HeaderStateHistory.rewind
                    (AF.castPoint $ AF.headPoint $ jTheirFragment ji)
                    (jTheirHeaderStateHistory ji)
                -- We assume the history is always possible to rewind. The case
                -- where this wouldn't be true is if the original candidate
                -- fragment provided by the dynamo contained headers that have
                -- no corresponding header state.
                rewoundHistory =
                  fromMaybe (error "offerJump: cannot rewind history") mRewoundHistory
                -- If the tip of jTheirFragment does not match the tip of
                -- jTheirHeaderStateHistory, then the history needs rewinding.
                historyNeedsRewinding =
                     (/= AF.headPoint (jTheirFragment ji)) $
                     castPoint $
                     either headerStatePoint headerStatePoint $
                     AF.head $
                     HeaderStateHistory.unHeaderStateHistory $
                     jTheirHeaderStateHistory ji
                -- Recompute the intersection only if a suffix of the candidate
                -- fragment was trimmed.
                intersection
                  | historyNeedsRewinding = case AF.intersectionPoint (jOurFragment ji) (jTheirFragment ji) of
                      Just po -> castPoint po
                      Nothing -> error "offerJump: the jumpInfo should have a valid intersection"
                  | otherwise = jMostRecentIntersection ji
             in KnownIntersectionState
                  { mostRecentIntersection = intersection
                  , ourFrag = jOurFragment ji
                  , theirFrag = jTheirFragment ji
                  , theirHeaderStateHistory = rewoundHistory
                  , kBestBlockNo = max (fromWithOrigin 0 $ AF.headBlockNo $ jTheirFragment ji) (kBestBlockNo kis)
                  }

    requestNext ::
        KnownIntersectionState blk
     -> MkPipelineDecision
     -> Nat n
     -> Their (Tip blk)
     -> WithOrigin BlockNo
     -> Consensus (ClientPipelinedStIdle n) blk m
    requestNext kis mkPipelineDecision n theirTip candTipBlockNo =
        let theirTipBlockNo = getTipBlockNo (unTheir theirTip)
            decision        =
                runPipelineDecision
                    mkPipelineDecision
                    n
                    candTipBlockNo
                    theirTipBlockNo
            onMsgAwaitReply =
              idlingStart idling >>
              lbPause loPBucket >>
              Jumping.jgOnAwaitReply jumping
        in
        case (n, decision) of
          (Zero, (Request, mkPipelineDecision')) ->
              SendMsgRequestNext
                  onMsgAwaitReply
                  (handleNext kis mkPipelineDecision' Zero)

          (_, (Pipeline, mkPipelineDecision')) ->
              SendMsgRequestNextPipelined
                onMsgAwaitReply
            $ requestNext
                  kis
                  mkPipelineDecision'
                  (Succ n)
                  theirTip
                  candTipBlockNo

          (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
              CollectResponse
                  (   Just
                    $ pure
                    $ SendMsgRequestNextPipelined
                        onMsgAwaitReply
                    $ requestNext
                          kis
                          mkPipelineDecision'
                          (Succ n)
                          theirTip
                          candTipBlockNo
                  )
                  (handleNext kis mkPipelineDecision' n')

          (Succ n', (Collect, mkPipelineDecision')) ->
              CollectResponse
                  Nothing
                  (handleNext kis mkPipelineDecision' n')

    handleNext ::
        KnownIntersectionState blk
     -> MkPipelineDecision
     -> Nat n
     -> Consensus (ClientStNext n) blk m
    handleNext kis mkPipelineDecision n =
      -- Unconditionally restart the leaky LoP bucket when receiving any
      -- message.
      ClientStNext {
        recvMsgRollForward = \hdr theirTip -> do
            (idlingStop idling >> lbResume loPBucket)
            traceWith tracer $ TraceDownloadedHeader hdr
            continueWithState kis $
                rollForward
                    mkPipelineDecision
                    n
                    hdr
                    (Their theirTip)
      ,
        recvMsgRollBackward = \intersection theirTip -> do
            (idlingStop idling >> lbResume loPBucket)
            let intersection' :: Point blk
                intersection' = castPoint intersection
            traceWith tracer $ TraceRolledBack intersection'
            continueWithState kis $
                rollBackward
                    mkPipelineDecision
                    n
                    intersection'
                    (Their theirTip)
      }

    rollForward ::
        MkPipelineDecision
     -> Nat n
     -> Header blk
     -> Their (Tip blk)
     -> Stateful m blk
            (KnownIntersectionState blk)
            (ClientPipelinedStIdle n)
    rollForward mkPipelineDecision n hdr theirTip =
        Stateful $ \kis -> traceException $ do
            arrival     <- recordHeaderArrival hdr
            arrivalTime <- getMonotonicTime

            let slotNo = blockSlot hdr

            checkKnownInvalid cfgEnv dynEnv intEnv hdr

            Jumping.jgOnRollForward jumping (blockPoint hdr)
            atomically (setLatestSlot dynEnv (NotOrigin slotNo))

            checkTime cfgEnv dynEnv intEnv kis arrival slotNo >>= \case
                NoLongerIntersects ->
                    continueWithState ()
                  $ drainThePipe n
                  $ findIntersectionTop
                        cfgEnv
                        dynEnv
                        intEnv
                        (kBestBlockNo kis)
                        NoMoreIntersection

                StillIntersects ledgerView kis' -> do
                    kis'' <-
                        checkValid cfgEnv intEnv hdr theirTip kis' ledgerView
                    kis''' <- checkLoP cfgEnv dynEnv hdr kis''

                    atomically $ do
                      updateJumpInfoSTM jumping kis'''
                      setCandidate (theirFrag kis''')
                    atomically
                      $ traceWith headerMetricsTracer (slotNo, arrivalTime)

                    continueWithState kis'''
                      $ nextStep mkPipelineDecision n theirTip

    rollBackward ::
        MkPipelineDecision
     -> Nat n
     -> Point blk
     -> Their (Tip blk)
     -> Stateful m blk
          (KnownIntersectionState blk)
          (ClientPipelinedStIdle n)
    rollBackward mkPipelineDecision n rollBackPoint theirTip =
        Stateful $ \kis ->
            traceException
          $ let KnownIntersectionState {
                    mostRecentIntersection
                  , ourFrag
                  , theirFrag
                  , theirHeaderStateHistory
                  , kBestBlockNo
                  } = kis
            in
            case attemptRollback
                     rollBackPoint
                     (theirFrag, theirHeaderStateHistory)
              of
                Nothing ->
                    -- Remember that we use our current chain fragment as the
                    -- starting point for the candidate's chain. Our fragment
                    -- contained @k@ headers. At this point, the candidate
                    -- fragment might have grown to more than @k@ or rolled
                    -- back to less than @k@ headers.
                    --
                    -- But now, it rolled back to some point that is not on the
                    -- fragment, which means that it tried to roll back to some
                    -- point before one of the last @k@ headers we initially
                    -- started from. We could never switch to this fork anyway,
                    -- so just disconnect. Furthermore, our current chain might
                    -- have advanced in the meantime, so the point we would
                    -- have to roll back to might have been much further back
                    -- than @k@ blocks (> @k@ + the number of blocks we have
                    -- advanced since starting syncing).
                    --
                    -- INVARIANT: a candidate fragment contains @>=k@ headers
                    -- (unless near genesis, in which case we mean the total
                    -- number of blocks in the fragment) minus @r@ headers
                    -- where @r <= k@. This ghost variable @r@ indicates the
                    -- number of headers we temporarily rolled back. Such a
                    -- rollback must always be followed by rolling forward @s@
                    -- new headers where @s >= r@.
                    --
                    -- Thus, @k - r + s >= k@.
                    terminateAfterDrain n
                  $ RolledBackPastIntersection
                        rollBackPoint
                        (ourTipFromChain ourFrag)
                        theirTip

                Just (theirFrag', theirHeaderStateHistory') -> do
                  -- We just rolled back to @rollBackPoint@, either our most
                  -- recent intersection was after or at @rollBackPoint@, in
                  -- which case @rollBackPoint@ becomes the new most recent
                  -- intersection.
                  --
                  -- But if the most recent intersection was /before/
                  -- @rollBackPoint@, then the most recent intersection doesn't
                  -- change.
                  let mostRecentIntersection' =
                          if   AF.withinFragmentBounds
                                   (castPoint rollBackPoint)
                                   ourFrag
                          then rollBackPoint
                          else mostRecentIntersection

                      kis' =
                          assertKnownIntersectionInvariants
                              (configConsensus cfg)
                        $ KnownIntersectionState {
                              mostRecentIntersection  = mostRecentIntersection'
                            , ourFrag                 = ourFrag
                            , theirFrag               = theirFrag'
                            , theirHeaderStateHistory = theirHeaderStateHistory'
                            , kBestBlockNo
                            }
                  atomically $ do
                    updateJumpInfoSTM jumping kis'
                    setCandidate theirFrag'
                    setLatestSlot dynEnv (pointSlot rollBackPoint)

                  Jumping.jgOnRollBackward jumping (pointSlot rollBackPoint)

                  continueWithState kis' $
                      nextStep mkPipelineDecision n theirTip

-- | Let ChainSync jumping know about an update to the 'KnownIntersectionState'.
updateJumpInfoSTM ::
     Jumping.Jumping m blk
  -> KnownIntersectionState blk
  -> STM m ()
updateJumpInfoSTM jumping kis@KnownIntersectionState{ourFrag} =
    Jumping.jgUpdateJumpInfo jumping JumpInfo
      { jMostRecentIntersection = mostRecentIntersection kis
      , jOurFragment = ourFrag
      , jTheirFragment = theirFrag kis
      , jTheirHeaderStateHistory = theirHeaderStateHistory kis
      }

{-------------------------------------------------------------------------------
  Header checks
-------------------------------------------------------------------------------}

-- | Check whether 'getIsInvalidBlock' indicates that the peer's most recent
-- header indicates they are either adversarial or buggy
--
-- If the peer is sending headers quickly, the 'invalidBlockRejector' might
-- miss one. So this call is a lightweight supplement. Note that neither check
-- /must/ be 100% reliable.
checkKnownInvalid ::
  forall m blk arrival judgment.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> InternalEnv m blk arrival judgment
  -> Header blk
  -> m ()
checkKnownInvalid cfgEnv dynEnv intEnv hdr = case scrutinee of
    GenesisHash    -> return ()
    BlockHash hash -> do
        isInvalidBlock <- atomically $ forgetFingerprint <$> getIsInvalidBlock
        whenJust (isInvalidBlock hash) $ \reason ->
            disconnect $ InvalidBlock (headerPoint hdr) hash reason
  where
    ConfigEnv {
        chainDbView
      } = cfgEnv

    ChainDbView {
        getIsInvalidBlock
      } = chainDbView

    DynamicEnv {
        version
      } = dynEnv

    InternalEnv {
        disconnect
      } = intEnv

    -- When pipelining, the tip of the candidate is forgiven for being an
    -- invalid block, but not if it extends any invalid blocks.
    scrutinee = case isPipeliningEnabled version of
        NotReceivingTentativeBlocks -> BlockHash (headerHash hdr)
        -- Disconnect if the parent block of `hdr` is known to be invalid.
        ReceivingTentativeBlocks    -> headerPrevHash hdr

-- | Manage the relationships between the header's slot, arrival time, and
-- intersection with the local selection
--
-- The first step is to determine the timestamp of the slot's onset. If the
-- intersection with local selection is much older than the header, then this
-- may not be possible. The client will block until that is no longer true.
-- However, it will stop blocking and 'exitEarly' as soon as
-- 'NoLongerIntersects' arises.
--
-- If the slot is from the far-future, the peer is buggy, so disconnect. If
-- it's from the near-future, follow the Ouroboros Chronos rule and ignore this
-- peer until this header is no longer from the future.
--
-- Finally, the client will block on the intersection a second time, if
-- necessary, since it's possible for a ledger state to determine the slot's
-- onset's timestamp without also determining the slot's 'LedgerView'. During
-- this pause, the LoP bucket is paused.
checkTime ::
  forall m blk arrival judgment.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> InternalEnv m blk arrival judgment
  -> KnownIntersectionState blk
  -> arrival
  -> SlotNo
  -> m (UpdatedIntersectionState blk (LedgerView (BlockProtocol blk)))
checkTime cfgEnv dynEnv intEnv =
    \kis arrival slotNo -> pauseBucket $ castEarlyExitIntersects $ do
        Intersects kis2 lst        <- checkArrivalTime kis arrival
        Intersects kis3 ledgerView <- case projectLedgerView slotNo lst of
            Just ledgerView -> pure $ Intersects kis2 ledgerView
            Nothing         -> do
              EarlyExit.lift $
                  traceWith (tracer cfgEnv)
                $ TraceWaitingBeyondForecastHorizon slotNo
              res <- readLedgerState kis2 (projectLedgerView slotNo)
              EarlyExit.lift $
                  traceWith (tracer cfgEnv)
                $ TraceAccessingForecastHorizon slotNo
              pure res
        pure $ Intersects kis3 ledgerView
  where
    ConfigEnv {
        cfg
      , chainDbView
      } = cfgEnv

    ChainDbView {
        getPastLedger
      } = chainDbView

    InternalEnv {
        disconnect
      , headerInFutureCheck
      , intersectsWithCurrentChain
      } = intEnv

    InFutureCheck.HeaderInFutureCheck {
        handleHeaderArrival
      , judgeHeaderArrival
      } = headerInFutureCheck

    -- Determine whether the header is from the future, and handle that fact if
    -- so. Also return the ledger state used for the determination.
    --
    -- Relies on 'readLedgerState'.
    checkArrivalTime ::
         KnownIntersectionState blk
      -> arrival
      -> WithEarlyExit m (Intersects blk (LedgerState blk))
    checkArrivalTime kis arrival = do
        Intersects kis' (lst, judgment) <- do
            readLedgerState kis $ \lst ->
                case   runExcept
                     $ judgeHeaderArrival (configLedger cfg) lst arrival
                  of
                    Left PastHorizon{} -> Nothing
                    Right judgment     -> Just (lst, judgment)

        -- For example, throw an exception if the header is from the far
        -- future.
        EarlyExit.lift $ handleHeaderArrival judgment >>= \case
            Just exn -> disconnect (InFutureHeaderExceedsClockSkew exn)
            Nothing  -> return $ Intersects kis' lst

    -- Block until the the ledger state at the intersection with the local
    -- selection returns 'Just'.
    --
    -- Exits early if the intersection no longer exists.
    readLedgerState ::
      forall a.
         KnownIntersectionState blk
      -> (LedgerState blk -> Maybe a)
      -> WithEarlyExit m (Intersects blk a)
    readLedgerState kis prj = castM $ readLedgerStateHelper kis prj

    readLedgerStateHelper ::
      forall a.
         KnownIntersectionState blk
      -> (LedgerState blk -> Maybe a)
      -> m (WithEarlyExit m (Intersects blk a))
    readLedgerStateHelper kis prj = atomically $ do
        -- We must first find the most recent intersection with the current
        -- chain. Note that this is cheap when the chain and candidate haven't
        -- changed.
        intersectsWithCurrentChain kis >>= \case
            NoLongerIntersects      -> return exitEarly
            StillIntersects () kis' -> do
                let KnownIntersectionState {
                        mostRecentIntersection
                      } = kis'
                lst <-
                    fmap
                      (maybe
                           (error $
                                 "intersection not within last k blocks: "
                              <> show mostRecentIntersection
                           )
                           ledgerState
                      )
                  $ getPastLedger mostRecentIntersection
                case prj lst of
                    Nothing         -> retry
                    Just ledgerView ->
                        return $ return $ Intersects kis' ledgerView

    -- Returns 'Nothing' if the ledger state cannot forecast the ledger view
    -- that far into the future.
    projectLedgerView ::
         SlotNo
      -> LedgerState blk
      -> Maybe (LedgerView (BlockProtocol blk))
    projectLedgerView slot lst =
        let forecast = ledgerViewForecastAt (configLedger cfg) lst
              -- TODO cache this in the KnownIntersectionState? Or even in the
              -- LedgerDB?
        in
        case runExcept $ forecastFor forecast slot of
            Right ledgerView            -> Just ledgerView
            Left OutsideForecastRange{} ->
                -- The header is too far ahead of the intersection point with
                -- our current chain. We have to wait until our chain and the
                -- intersection have advanced far enough. This will wait on
                -- changes to the current chain via the call to
                -- 'intersectsWithCurrentChain' before it.
                Nothing

    -- Pause the LoP bucket for the entire duration of 'checkTime'. It will
    -- either execute very fast, or it will block on the time translation or
    -- forecast horizon, waiting for our selection to advance. During this
    -- period, we should not leak tokens as our peer is not responsible for this
    -- waiting time.
    pauseBucket :: m a -> m a
    pauseBucket =
        bracket_
          (lbPause (loPBucket dynEnv))
          (lbResume (loPBucket dynEnv))

-- | Update the 'KnownIntersectionState' according to the header, if it's valid
--
-- Crucially: disconnects if it isn't.
checkValid ::
  forall m blk arrival judgment.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => ConfigEnv m blk
  -> InternalEnv m blk arrival judgment
  -> Header blk
  -> Their (Tip blk)
  -> KnownIntersectionState blk
  -> LedgerView (BlockProtocol blk)
  -> m (KnownIntersectionState blk)
checkValid cfgEnv intEnv hdr theirTip kis ledgerView = do
    let KnownIntersectionState {
            mostRecentIntersection
          , ourFrag
          , theirFrag
          , theirHeaderStateHistory
          , kBestBlockNo
          } = kis

    let hdrPoint = headerPoint hdr

    -- Validate header
    theirHeaderStateHistory' <-
        case   runExcept
             $ validateHeader cfg ledgerView hdr theirHeaderStateHistory
          of
            Right theirHeaderStateHistory' -> return theirHeaderStateHistory'
            Left  vErr ->
                disconnect
              $ HeaderError hdrPoint vErr (ourTipFromChain ourFrag) theirTip

    let theirFrag' = theirFrag :> hdr
        -- Advance the most recent intersection if we have the same
        -- header on our fragment too. This is cheaper than recomputing
        -- the intersection from scratch.
        mostRecentIntersection'
          | Just ourSuccessor <-
                AF.successorBlock (castPoint mostRecentIntersection) ourFrag
          , headerHash ourSuccessor == headerHash hdr
          = headerPoint hdr
          | otherwise
          = mostRecentIntersection

    traceWith (tracer cfgEnv) $ TraceValidatedHeader hdr

    pure
      $ assertKnownIntersectionInvariants (configConsensus cfg)
      $ KnownIntersectionState {
            mostRecentIntersection  = mostRecentIntersection'
          , ourFrag                 = ourFrag
          , theirFrag               = theirFrag'
          , theirHeaderStateHistory = theirHeaderStateHistory'
          , kBestBlockNo
          }
  where
    ConfigEnv {
        cfg
      } = cfgEnv

    InternalEnv {
        disconnect
      } = intEnv

-- | Check the limit on patience. If the block number of the new header is
-- better than anything (valid) we have seen from this peer so far, we add a
-- token to their leaky bucket and we remember this new record. Has to happen
-- only after validation of the block.
checkLoP ::
  forall m blk.
   ( IOLike m
   , HasHeader (Header blk) )
  => ConfigEnv m blk
  -> DynamicEnv m blk
  -> Header blk
  -> KnownIntersectionState blk
  -> m (KnownIntersectionState blk)
checkLoP ConfigEnv{tracer} DynamicEnv{loPBucket} hdr kis@KnownIntersectionState{kBestBlockNo} =
  if blockNo hdr > kBestBlockNo
    then do lbGrantToken loPBucket
            traceWith tracer $ TraceGaveLoPToken True hdr kBestBlockNo
            pure $ kis{kBestBlockNo = blockNo hdr}
    else do traceWith tracer $ TraceGaveLoPToken False hdr kBestBlockNo
            pure kis

{-------------------------------------------------------------------------------
  Utilities used in the *top functions
-------------------------------------------------------------------------------}

data UpdatedIntersectionState blk a =
    NoLongerIntersects
    -- ^ The local selection has changed such that 'ourFrag' no longer
    -- intersects 'theirFrag'
    --
    -- (In general, the intersection could also be lost because of messages
    -- they sent, but that's handled elsewhere, not involving this data type.)
  |
    StillIntersects a !(KnownIntersectionState blk)

data Intersects blk a =
    Intersects
        (KnownIntersectionState blk)
        a

castEarlyExitIntersects ::
     Monad m
  => WithEarlyExit m (Intersects blk a)
  -> m (UpdatedIntersectionState blk a)
castEarlyExitIntersects =
    fmap cnv . EarlyExit.withEarlyExit
  where
    cnv = \case
        Nothing                 -> NoLongerIntersects
        Just (Intersects kis a) -> StillIntersects a kis

-- | Recent offsets
--
-- These offsets are used to find an intersection point between our chain
-- and the upstream node's. We use the fibonacci sequence to try blocks
-- closer to our tip, and fewer blocks further down the chain. It is
-- important that this sequence constains at least a point @k@ back: if no
-- intersection can be found at most @k@ back, then this is not a peer
-- that we can sync with (since we will never roll back more than @k).
--
-- For @k = 2160@, this evaluates to
--
-- > [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2160]
--
-- For @k = 5@ (during testing), this evaluates to
--
-- > [0,1,2,3,5]
--
-- In case the fragment contains less than @k@ blocks, we use the length
-- of the fragment as @k@. This ensures that the oldest rollback point is
-- selected.
mkOffsets :: SecurityParam -> Word64 -> [Word64]
mkOffsets (SecurityParam k) maxOffset =
    [0] ++ takeWhile (< l) [fib n | n <- [2..]] ++ [l]
  where
    l = k `min` maxOffset

ourTipFromChain ::
     HasHeader (Header blk)
  => AnchoredFragment (Header blk)
  -> Our (Tip blk)
ourTipFromChain = Our . AF.anchorToTip . AF.headAnchor

-- | A type-legos auxillary function used in 'readLedgerState'.
castM :: Monad m => m (WithEarlyExit m x) -> WithEarlyExit m x
castM = join . EarlyExit.lift

attemptRollback ::
     ( BlockSupportsProtocol blk
     , HasAnnTip blk
     )
  => Point blk
  ->       (AnchoredFragment (Header blk), HeaderStateHistory blk)
  -> Maybe (AnchoredFragment (Header blk), HeaderStateHistory blk)
attemptRollback rollBackPoint (frag, state) = do
    frag'  <- AF.rollback (castPoint rollBackPoint) frag
    state' <- HeaderStateHistory.rewind rollBackPoint state
    return (frag', state')

{-------------------------------------------------------------------------------
   Looking for newly-recognized trap headers on the existing candidate
-------------------------------------------------------------------------------}

-- | Watch the invalid block checker function for changes (using its
-- fingerprint). Whenever it changes, i.e., a new invalid block is detected,
-- check whether the current candidate fragment contains any header that is
-- invalid, if so, disconnect by throwing an 'InvalidBlock' exception.
--
-- Note that it is possible, yet unlikely, that the candidate fragment
-- contains a header that corresponds to an invalid block, but before we have
-- discovered this (after downloading and validating the block), the upstream
-- node could have rolled back such that its candidate chain no longer
-- contains the invalid block, in which case we do not disconnect from it.
--
-- The cost of this check is \( O(cand * check) \) where /cand/ is the size of
-- the candidate fragment and /check/ is the cost of checking whether a block
-- is invalid (typically \( O(\log(invalid)) \) where /invalid/ is the number
-- of invalid blocks).
invalidBlockRejector ::
  forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     )
  => Tracer m (TraceChainSyncClientEvent blk)
  -> NodeToNodeVersion
  -> STM m (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
     -- ^ Get the invalid block checker
  -> STM m (AnchoredFragment (Header blk))
     -- ^ Get the candidate
  -> Watcher m
         (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
         Fingerprint
invalidBlockRejector tracer version getIsInvalidBlock getCandidate =
    Watcher {
        wFingerprint = getFingerprint
      , wInitial     = Nothing
      , wNotify      = checkInvalid . forgetFingerprint
      , wReader      = getIsInvalidBlock
      }
  where
    checkInvalid :: (HeaderHash blk -> Maybe (InvalidBlockReason blk)) -> m ()
    checkInvalid isInvalidBlock = do
        theirFrag <- atomically getCandidate
        -- The invalid block is likely to be a more recent block, so check from
        -- newest to oldest.
        --
        -- As of block diffusion pipelining, their tip header might be
        -- tentative. Since they do not yet have a way to explicitly say
        -- whether it is tentative, we assume it is and therefore skip their
        -- tip here. TODO once it's explicit, only skip it if it's annotated as
        -- tentative
        mapM_ (uncurry disconnect)
          $ firstJust
                (\hdr -> (hdr,) <$> isInvalidBlock (headerHash hdr))
          $ (   case isPipeliningEnabled version of
                    ReceivingTentativeBlocks    -> drop 1
                    NotReceivingTentativeBlocks -> id
            )
          $ AF.toNewestFirst theirFrag

    disconnect :: Header blk -> InvalidBlockReason blk -> m ()
    disconnect invalidHeader reason = do
        let ex =
                InvalidBlock
                  (headerPoint invalidHeader)
                  (headerHash invalidHeader)
                  reason
        traceWith tracer $ TraceException ex
        throwIO ex

{-------------------------------------------------------------------------------
  Explicit state
-------------------------------------------------------------------------------}

-- | Make the state maintained by the chain sync client explicit
--
-- The chain sync client contains of a bunch of functions that basically look
-- like "do some network stuff, compute some stuff, and then continue with
-- such-and-such a new state". We want to make sure to keep that state in NF
-- at all times, but since we don't use a TVar to store it, we cannot reuse
-- the existing infrastructure for checking TVars for NF. Instead, we make
-- the state explicit in the types and do the check in 'continueWithState'.
newtype Stateful m blk s st = Stateful (s -> m (Consensus st blk m))

continueWithState ::
     NoThunks s
  => s
  -> Stateful m blk s st
  -> m (Consensus st blk m)
continueWithState !s (Stateful f) =
    checkInvariant (show <$> unsafeNoThunks s) $ f s

{-------------------------------------------------------------------------------
  Return value
-------------------------------------------------------------------------------}

-- | The Chain sync client only _gracefully_ terminates when the upstream
-- node's chain is not interesting (e.g., forked off too far in the past). By
-- gracefully terminating, the network layer can keep the other mini-protocols
-- connect to the same upstream node running.
--
-- For example, a relay node will often receive connections from nodes syncing
-- from scratch or an old chain. Since these nodes have a chain that is shorter
-- than the relay node's chain, it's useless for the relay node to run the
-- client-side of the chain sync protocol. However, the other direction of the
-- protocol, and, e.g., the transaction submission protocol, should keep
-- running.
data ChainSyncClientResult =
    forall blk. BlockSupportsProtocol blk =>
    ForkTooDeep
        (Point blk)  -- ^ Intersection
        (Our   (Tip blk))
        (Their (Tip blk))
    -- ^ The server we're connecting to forked more than @k@ blocks ago.
  |
    forall blk. BlockSupportsProtocol blk =>
    NoMoreIntersection
        (Our   (Tip blk))
        (Their (Tip blk))
    -- ^ Our chain changed such that it no longer intersects with the
    -- candidate's fragment, and asking for a new intersection did not yield
    -- one.
  |
    forall blk. BlockSupportsProtocol blk =>
    RolledBackPastIntersection
        (Point blk)  -- ^ Point asked to roll back to
        (Our   (Tip blk))
        (Their (Tip blk))
    -- ^ We were asked to roll back past the anchor point of the candidate's
    -- fragment. This means the candidate chain no longer forks off within @k@,
    -- making it impossible to switch to.
  |
    AskedToTerminate
    -- ^ We were asked to terminate via the 'ControlMessageSTM'

deriving instance Show ChainSyncClientResult

instance Eq ChainSyncClientResult where
    (==)
        (ForkTooDeep (a  :: Point blk)  b  c )
        (ForkTooDeep (a' :: Point blk') b' c')
      | Just Refl <- eqT @blk @blk'
      = (a, b, c) == (a', b', c')

    (==)
        (NoMoreIntersection (a  :: Our (Tip blk )) b )
        (NoMoreIntersection (a' :: Our (Tip blk')) b')
      | Just Refl <- eqT @blk @blk'
      = (a, b) == (a', b')

    (==)
        (RolledBackPastIntersection (a  :: Point blk ) b  c )
        (RolledBackPastIntersection (a' :: Point blk') b' c')
      | Just Refl <- eqT @blk @blk'
      = (a, b, c) == (a', b', c')

    AskedToTerminate == AskedToTerminate = True

    ForkTooDeep{}                == _ = False
    NoMoreIntersection{}         == _ = False
    RolledBackPastIntersection{} == _ = False
    AskedToTerminate             == _ = False

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | When the upstream node violates the protocol or exhibits malicious
-- behaviour, e.g., serving an invalid header or a header corresponding to a
-- known invalid block, we throw an exception to disconnect. This will bring
-- down all miniprotocols in both directions with that node.
data ChainSyncClientException =
    forall blk. (BlockSupportsProtocol blk, ValidateEnvelope blk) =>
    HeaderError
        (Point blk)  -- ^ Invalid header
        (HeaderError blk)
        (Our   (Tip blk))
        (Their (Tip blk))
    -- ^ Header validation threw an error.
  |
    forall blk. BlockSupportsProtocol blk =>
    InvalidIntersection
        (Point blk)  -- ^ Intersection
        (Our   (Tip blk))
        (Their (Tip blk))
    -- ^ We send the upstream node a bunch of points from a chain fragment and
    -- the upstream node responded with an intersection point that is not on
    -- our chain fragment, and thus not among the points we sent.
    --
    -- We store the intersection point the upstream node sent us.
  |
    forall blk. LedgerSupportsProtocol blk =>
    InvalidBlock
        (Point blk)
        -- ^ Block that triggered the validity check.
        (HeaderHash blk)
        -- ^ Invalid block. If pipelining was negotiated, this can be
        -- different from the previous argument.
        (InvalidBlockReason blk)
    -- ^ The upstream node's chain contained a block that we know is invalid.
  |
    InFutureHeaderExceedsClockSkew !InFutureCheck.HeaderArrivalException
    -- ^ A header arrived from the far future.
  |
    EmptyBucket
    -- ^ The peer lost its race against the bucket.
  |
    InvalidJumpResponse
    -- ^ When the peer responded incorrectly to a jump request.
  | DensityTooLow
    -- ^ The peer has been deemed unworthy by the GDD

deriving instance Show ChainSyncClientException

instance Eq ChainSyncClientException where
    (==)
        (HeaderError (a  :: Point blk ) b  c  d )
        (HeaderError (a' :: Point blk') b' c' d')
      | Just Refl <- eqT @blk @blk'
      = (a, b, c, d) == (a', b', c', d')

    (==)
        (InvalidIntersection (a  :: Point blk ) b  c )
        (InvalidIntersection (a' :: Point blk') b' c')
      | Just Refl <- eqT @blk @blk'
      = (a, b, c) == (a', b', c')

    (==)
        (InvalidBlock (a  :: Point blk)  b  c )
        (InvalidBlock (a' :: Point blk') b' c')
      | Just Refl <- eqT @blk @blk'
      = (a, b, c) == (a', b', c')

    (==)
        (InFutureHeaderExceedsClockSkew a )
        (InFutureHeaderExceedsClockSkew a')
      = a == a'
    (==)
        EmptyBucket EmptyBucket
      = True
    (==)
        InvalidJumpResponse InvalidJumpResponse
      = True
    (==)
        DensityTooLow DensityTooLow
      = True

    HeaderError{}                    == _ = False
    InvalidIntersection{}            == _ = False
    InvalidBlock{}                   == _ = False
    InFutureHeaderExceedsClockSkew{} == _ = False
    EmptyBucket                      == _ = False
    InvalidJumpResponse              == _ = False
    DensityTooLow                    == _ = False

instance Exception ChainSyncClientException

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Client.
data TraceChainSyncClientEvent blk =
    TraceDownloadedHeader (Header blk)
    -- ^ While following a candidate chain, we rolled forward by downloading a
    -- header.
  |
    TraceRolledBack (Point blk)
    -- ^ While following a candidate chain, we rolled back to the given point.
  |
    TraceFoundIntersection (Point blk) (Our (Tip blk)) (Their (Tip blk))
    -- ^ We found an intersection between our chain fragment and the
    -- candidate's chain.
  |
    TraceException ChainSyncClientException
    -- ^ An exception was thrown by the Chain Sync Client.
  |
    TraceTermination ChainSyncClientResult
    -- ^ The client has terminated.
  |
    TraceValidatedHeader (Header blk)
    -- ^ We have validated the given header.
  |
    TraceWaitingBeyondForecastHorizon SlotNo
    -- ^ The 'SlotNo' is beyond the forecast horizon, the ChainSync client
    -- cannot yet validate a header in this slot and therefore is waiting.
  |
    TraceAccessingForecastHorizon SlotNo
    -- ^ The 'SlotNo', which was previously beyond the forecast horizon, has now
    -- entered it, and we can resume processing.
  |
    TraceGaveLoPToken Bool (Header blk) BlockNo
    -- ^ Whether we added a token to the LoP bucket of the peer. Also carries
    -- the considered header and the best block number known prior to this
    -- header.
  |
    TraceOfferJump (Point blk)
    -- ^ ChainSync Jumping offering a point to jump to.
  |
    TraceJumpResult (Jumping.JumpResult blk)
    -- ^ ChainSync Jumping -- reply.
  |
    TraceJumpingWaitingForNextInstruction
    -- ^ ChainSync Jumping -- the ChainSync client is requesting the next
    -- instruction.
  |
    TraceJumpingInstructionIs (Jumping.Instruction blk)
    -- ^ ChainSync Jumping -- the ChainSync client got its next instruction.

deriving instance
  ( BlockSupportsProtocol blk
  , Eq (Header blk)
  )
  => Eq (TraceChainSyncClientEvent blk)

deriving instance
  ( BlockSupportsProtocol blk
  , Show (Header blk)
  )
  => Show (TraceChainSyncClientEvent blk)
