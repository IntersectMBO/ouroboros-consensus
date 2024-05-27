{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides a watcher of the invariants that are specific to the
-- ChainSync jumping (CSJ) implementation. Those invariants are typically
-- documented in the codebase but are not checked in any way, yet they are
-- crucial for CSJ to work properly. This watcher monitors the ChainSync
-- handlers and throws a 'Violation' exception when an invariant stops holding.
-- It is intended for testing purposes.
module Test.Consensus.PeerSimulator.CSJInvariants (
    Violation
  , watcher
  ) where

import           Control.Monad (forM_, when)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block (Point, StandardHash, castPoint)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State as CSState
import           Ouroboros.Consensus.Util.IOLike (Exception, MonadSTM (STM),
                     MonadThrow (throwIO), readTVar)
import           Ouroboros.Consensus.Util.STM (Watcher (..))

--------------------------------------------------------------------------------
-- Idealised view of the ChainSync client's state
--------------------------------------------------------------------------------

-- | Our idealised view of the ChainSync client's state with respect to
-- ChainSync jumping in particular.
type View peer blk = Map peer (State blk)

-- | Idealised version of 'ChainSyncJumpingState'.
data State blk
  = Dynamo
  | Objector
      -- | The point where the objector dissented with the dynamo when it was a
      -- jumper.
      !(Point blk)
  | Disengaged
  | Jumper !(JumperState blk)
  deriving (Show, Eq)

-- | Idealised version of 'ChainSyncJumpingJumperState'.
data JumperState blk
  = Happy
      -- | Latest accepted jump, if there is one
      !(Maybe (Point blk))
  | LookingForIntersection
      -- | Latest accepted jump
      !(Point blk)
      -- | Earliest rejected jump
      !(Point blk)
  | FoundIntersection
      -- | Latest accepted jump
      !(Point blk)
      -- | Earliest rejected jump
      !(Point blk)
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Invariants on views
--------------------------------------------------------------------------------

allInvariants :: [Invariant peer blk]
allInvariants =
  [ thereIsAlwaysOneDynamoUnlessDisengaged,
    thereIsAlwaysAtMostOneObjector
  ]

thereIsAlwaysOneDynamoUnlessDisengaged :: Invariant peer blk
thereIsAlwaysOneDynamoUnlessDisengaged =
  Invariant
    { name = "There is always one dynamo, unless all are disengaged",
      check = \view ->
        null (filter (not . isDisengaged) $ Map.elems view)
          || length (filter isDynamo $ Map.elems view) == 1
    }

thereIsAlwaysAtMostOneObjector :: Invariant peer blk
thereIsAlwaysAtMostOneObjector =
  Invariant
    { name = "There is always at most one objector",
      check = \view ->
        length (filter isObjector $ Map.elems view) <= 1
    }

--------------------------------------------------------------------------------
-- Helpers for the invariants
--------------------------------------------------------------------------------

isDynamo :: State blk -> Bool
isDynamo (Dynamo {}) = True
isDynamo _           = False

isObjector :: State blk -> Bool
isObjector (Objector {}) = True
isObjector _             = False

isDisengaged :: State blk -> Bool
isDisengaged (Disengaged {}) = True
isDisengaged _               = False

--------------------------------------------------------------------------------
-- Invariant enforcement implementation
--------------------------------------------------------------------------------

readAndView ::
  forall m peer blk.
  ( MonadSTM m
  ) =>
  STM m (Map peer (CSState.ChainSyncClientHandle m blk)) ->
  STM m (View peer blk)
readAndView readHandles =
  traverse (fmap idealiseState . readTVar . CSState.cschJumping) =<< readHandles
  where
    -- Idealise the state of a ChainSync peer with respect to ChainSync jumping.
    -- In particular, we get rid of non-comparable information such as the TVars
    -- it may contain.
    idealiseState :: CSState.ChainSyncJumpingState m blk -> State blk
    idealiseState (CSState.Dynamo {}) = Dynamo
    idealiseState (CSState.Objector _ point _) = Objector $ idealiseJumpInfo point
    idealiseState (CSState.Disengaged _) = Disengaged
    idealiseState (CSState.Jumper _ state) = Jumper $ idealiseJumperState state
    -- Idealise the jumper state by stripping away everything that is more of a
    -- technical necessity and not actually relevant for the invariants.
    idealiseJumperState :: CSState.ChainSyncJumpingJumperState blk -> JumperState blk
    idealiseJumperState (CSState.Happy _ lastAccepted) = Happy $ idealiseJumpInfo <$> lastAccepted
    idealiseJumperState (CSState.LookingForIntersection lastAccepted firstRejected) =
      LookingForIntersection (idealiseJumpInfo lastAccepted) (idealiseJumpInfo firstRejected)
    idealiseJumperState (CSState.FoundIntersection _ lastAccepted firstRejected) =
      FoundIntersection (idealiseJumpInfo lastAccepted) (castPoint firstRejected)
    -- Jumpers actually carry a lot of information regarding the jump. From our
    -- idealised point of view, we only care about the points where the jumpers
    -- agree or disagree with the dynamo.
    idealiseJumpInfo :: CSState.JumpInfo blk -> Point blk
    idealiseJumpInfo = CSState.jMostRecentIntersection

-- | The type of an invariant. Basically a glorified pair of a name and a check
-- function.
data Invariant peer blk = Invariant
  { name  :: !String,
    check :: !(View peer blk -> Bool)
  }

-- | An exception that is thrown when an invariant is violated. It carries the
-- name of the invariant and the view of the state that triggered the invariant
-- violation.
data Violation peer blk = Violation !String !(View peer blk)
  deriving (Eq, Show)

instance
  ( Typeable blk,
    StandardHash blk,
    Eq peer,
    Show peer,
    Typeable peer
  ) =>
  Exception (Violation peer blk)

-- | The watcher of ChainSync jumping invariants. It receives the ChainSync
-- handles and monitors them for changes. When a change is detected, it runs all
-- the invariants and throws 'Violation' if any of the invariants is violated.
watcher ::
  ( MonadSTM m,
    MonadThrow m,
    Eq peer,
    Show peer,
    Typeable peer,
    Typeable blk,
    StandardHash blk
  ) =>
  STM m (Map peer (CSState.ChainSyncClientHandle m blk)) ->
  Watcher m (View peer blk) (View peer blk)
watcher handles =
  Watcher
    { wFingerprint = id,
      wInitial = Nothing,
      wReader = readAndView handles,
      wNotify =
        forM_ allInvariants . \view Invariant {name, check} ->
          when (not $ check view) $ throwIO $ Violation name view
    }
