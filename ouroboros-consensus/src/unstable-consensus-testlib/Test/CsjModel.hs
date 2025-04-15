{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | An executable specification of the centralized CSJ logic
--
-- TODO Can this simply be the CSJ implementation? The biggest challenge seems
-- to be confirming that the 'CsjState' won't become desynchronized from
-- reality due to the ChainSync clients not actually executing each
-- 'CsjReaction' immediately. That doesn't seem insurmountable to resolve, but
-- might add some complexity to this specification.
--
-- TODO Another, smaller challenge is that 'candidate' is assumed to reach back
-- to 'Origin' instead of back to the ImmDB tip. That's fine if this model is
-- only used in tests, but would theoretically prevent it from being used as
-- the implementation. I say "theoretically" because as long as all the
-- necessary intersections are found before the end of the list, it might Just
-- Work after a few tweaks.
module Test.CsjModel (
    -- * ChainSync Jumping (CSJ)
    ChainSyncReply (..),
    CsjClientState (..),
    CsjEnv (..),
    CsjReaction (..),
    CsjState (..),
    CsjStimulus (..),
    csjLoeConstraint,
    csjReactions,
    initialCsjState,
    -- * A non-empty sequence, catered to CSJ
    NonEmptySeq (..),
    nonEmptySeq,
    toSeq,
    -- * 'Perm'
    Perm (..),
    deletePerm,
    indexPerm,
    snocPerm,
  ) where

import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Applicative ((<|>))
import qualified Control.Applicative as Alt (empty)
import           Control.Arrow (first)
import           Control.Monad (guard)
import qualified Control.Monad.State.Strict as State
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as L (Maybe (Just, Nothing), isJust, maybe)
import           Data.Monoid (Alt (Alt, getAlt))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Strict.Classes (toLazy)
import           Data.Strict.Maybe (Maybe (Just, Nothing), fromMaybe,
                     maybe)
import           Data.Word (Word64)
import           Prelude hiding (Maybe (Just, Nothing), maybe)
import           Test.CsjModel.NonEmptySeq
import           Test.CsjModel.Perm

{-------------------------------------------------------------------------------
  An orphan instance for testing
-------------------------------------------------------------------------------}

deriving instance Read p => Read (WithOrigin p)

{-------------------------------------------------------------------------------
  CSJ specification
-------------------------------------------------------------------------------}

-- | CSJ-specific state for a peer
--
-- INVARIANT: For a Jumper, the 'comm' equals the tip of 'candidate'.
data CsjClientState p = CsjClientState {
    -- | points that were the youngest point rejected during some bisection
    -- attempt
    --
    -- See 'trimAnticomm'.
    --
    -- This cannot contain more points than the number of jumps this peer has
    -- ever handled without 'comm'itting to a younger point. And that's almost
    -- entirely bounded by how many adversarial peers became Dynamo without an
    -- honest peer becoming Dynamo. So this seems sufficiently bounded to never
    -- be a resource leak---also: each @p@ is tiny on the heap.
    anticomm :: !(Set p)
  ,
    -- | the candidate fetched from the peer so far
    --
    -- Ascending, and reaches all the way to 'Origin' in this model.
    candidate :: !(Seq p)
  ,
    -- | the greatest point the peer has accepted as part of a jump
    --
    -- If 'latestJump' is 'Origin', then this is the immutable tip as of when
    -- the peer connected (and so is the Dynamo's, which means the Dynamo'S
    -- 'comm' cannot be any younger than this peer's 'comm').
    comm :: !(WithOrigin p)
  }
  deriving (Read, Show)

-- | The (exact!) intersection of a peer's 'comm' and the latest jump request
-- they've handled
--
-- This could be a calculation intead of being tracked in the state, but in any
-- implementation it will definitely be cached, so it's nice for the
-- specification to also explain how to cache it.
--
-- Jumpers that are still bisecting do not have a 'Class', since their exact
-- intersection with 'latestJump' is unknown.
--
-- While 'latestJump' is 'Origin', all peers that are not bisecting have
-- 'Origin' as their 'Class' --- even the Dynamo.
--
-- INVARIANT: Peers serving the same chain will have the same 'Class' once they
-- finish bisecting.
newtype Class p = Class (WithOrigin p)
  deriving (Eq, Ord, Read, Show)

-- | 'anticomm' can be garbage-collected when 'comm' advances
trimAnticomm :: Ord p => WithOrigin p -> Set p -> Set p
trimAnticomm p ps = case p of
    Origin -> ps
    At p'  -> snd $ Set.split p' ps

-- | @MsgRollForward@
--
-- Disengages if it violates 'anticomm'. Updates 'candidate'.
forward :: Ord p => p -> CsjClientState p -> L.Maybe (CsjClientState p)
forward p y = do
    guard $ p `Set.notMember` anticomm y
    pure CsjClientState {
        anticomm  = anticomm y
      ,
        candidate = candidate y Seq.|> p
      ,
        comm      = comm y
      }

-- | @MsgRollBackward@
--
-- Disengages if it violates 'comm'. Updates 'candidate'.
--
-- NB the ChainSync server sends 'MsgRollBackward' in response to the first
-- @MsgRequestNext@ after having sent 'MsgIntersectionFound'. So Jumpers will
-- not receive 'MsgRollBackward' until after the 'Continue' command.
backward ::
     Ord p
  => WithOrigin p
  -> CsjClientState p
  -> L.Maybe (CsjClientState p)
backward wp y = do
    guard $ comm y <= wp
    candidate' <- case wp of
        Origin -> pure Seq.empty
        At p   -> do
            ip <- leftmostInAscSeq (candidate y) p
            pure $ Seq.take (ip + 1) (candidate y)
    pure CsjClientState {
        anticomm  = anticomm y
      ,
        candidate = candidate'
      ,
        comm      = comm y
      }

-- | Reset 'candidate' to 'comm' when demoting the Dynamo
trimCandidate :: Ord p => CsjClientState p -> CsjClientState p
trimCandidate y =
    CsjClientState {
        anticomm  = anticomm y  
      ,
        candidate =
            case comm y of
                Origin -> Seq.empty
                At p   -> case leftmostInAscSeq (candidate y) p of
                    L.Nothing -> error "impossible!"
                    L.Just i  -> Seq.take (i + 1) (candidate y)
      ,
        comm      = comm y  
      }

-- | A 'MsgFindIntersect' for 'comm'
--
-- minor optimization opportunity: if the Jumpers latest response was
-- @MsgIntersectionFound@ then this could be 'NoReset'.
promotionMessage :: CsjClientState p -> CsjReaction p
promotionMessage = MsgFindIntersect . comm

-- | 'MsgIntersectionFound' after promoting a Jumper
--
-- This disengages if the new intersection isn't 'comm'.
finishPromotion ::
     Ord p
  => WithOrigin p
  -> CsjClientState p
  -> L.Maybe (CsjClientState p)
finishPromotion p y = y <$ guard (p == comm y)

-----

-- | A jump requested by the Dynamo
--
-- The payload is the Dynamo's 'Class' and 'candidate' immediately before it
-- requests the jump. Morever, in the real implementation, this data structure
-- includes enough information for a Jumper to update its ChainSync client
-- state each time it accepts a point in a jump.
--
-- NON-invariant: It is very tempting to conclude that the jump points after
-- the Dynamo's 'Class' have never been offered as part of any previous jump by
-- any previous Dynamo. However, the following sequence is a counterexample.
--
-- - A peer X offers a jump as Dynamo.
--
-- - X stops being the Dynamo (disconnect or demote).
--
-- - Another peer Y offers a different jump as Dynamo, overwriting 'latestJump'.
--
-- - In either order: Y stops being the Dynamo and another peer Z /joins/.
--
-- - Z becomes Dynamo and serves the same chain as X.
data JumpRequest p = JumpRequest (Class p) (NonEmptySeq p)
  deriving (Read, Show)

-- | Additional CSJ state maintained by a Jumper
data Bisecting p = Bisecting {
    -- | points that might still be the exact intersection of this peer and the
    -- jump request
    --
    -- This sequence is ascending like 'candidate', but it doesn't necessarily
    -- reach all the way back to 'Origin'.
    --
    -- INVARIANT: The oldest point in this non-empty sequence is the jump
    -- request's successor of this peer's 'comm' (which equals the tip of its
    -- 'candidate' since this peer is a Jumper).
    --
    -- INVARIANT: If @'rejected' = Just p@, then @p@ is the successor of the
    -- youngest point in this non-empty sequence. If @'rejected' = Nothing@,
    -- then this list is the entire jump offer.
    notYetDetermined :: !(NonEmptySeq p)
  ,
    -- | The youngest point rejected so far as part of this jump
    --
    -- It is not yet included in 'anticomm', since that only contains the
    -- points that were the youngest point rejected by some bisection attempt.
    --
    -- INVARIANT: This is 'Just' if and only if the peer has sent
    -- 'MsgIntersectionNotFound' as part of /this/ jump.
    rejected :: !(Maybe p)
  }
  deriving (Read, Show)

-- | @'nextMsgFindIntersect' = 'MsgFindIntersect' . 'nextMsgFindIntersect_'@
nextMsgFindIntersect :: Bisecting p -> CsjReaction p
nextMsgFindIntersect = MsgFindIntersect . At . nextMsgFindIntersect2

-- | A bisecting Jumper is stuck until it receives a response to the
-- @MsgFindIntersect@ with this payload
nextMsgFindIntersect2 :: Bisecting p -> p
nextMsgFindIntersect2 bi =
    case rejected bi of
        Nothing -> neLast nyd
            -- It would be sound and more uniform to just always bisect.
            -- However, the first request per jump is instead for the entire
            -- jump.
            --
            -- This is because /most/ jumps will be offered by an honest
            -- Dynamo, and so by offering the whole jump in the first message,
            -- load on honest Jumpers is minimized: one message per jump.
        Just{}  -> neMid  nyd
            -- TODO At least for the reference Haskell Cardano node, each
            -- additional point in the 'MsgFindIntersect' message is most
            -- negligible unless they're in different ImmDB chunk files. So
            -- this logic here could include more points (perhaps once all of
            -- @nyd@ is in the same chunk file) in order to ensure the
            -- bisection requires fewer round trips while only increasing the
            -- upstream per-message cost only negligibly.
  where
    nyd = notYetDetermined bi

-- | @MsgIntersectionNotFound@
intersectionNotFound ::
     Ord p
  => CsjClientState p
  -> Bisecting p
  -> L.Maybe (CsjClientState p, Either (Class p) (Bisecting p))
intersectionNotFound y bi = pure $ bisectionStep y bi False

-- | @MsgIntersectionFound@
intersectionFound ::
     Ord p
  => WithOrigin p
  -> CsjClientState p
  -> Bisecting p
  -> L.Maybe (CsjClientState p, Either (Class p) (Bisecting p))
intersectionFound p y bi = do
    guard $ p == At (nextMsgFindIntersect2 bi)
    pure $ bisectionStep y bi True

-- | Update 'Bisecting' based on the response to 'nextMsgFindIntersect'.
--
-- Recall that 'newJumpRequest2' used 'comm' and 'anticomm' to constrain
-- 'notYetDetermined' when initializing it. Thus 'bisectionStep' does not even
-- need to check them and so cannot fail.
bisectionStep ::
     Ord p
  => CsjClientState p
  -> Bisecting p
  -> Bool
     -- ^ whether the peer found 'nextMsgFindIntersect2' on its chain
  -> (CsjClientState p, Either (Class p) (Bisecting p))
bisectionStep y bi found =
    p `seq` (y', eBi')
  where
    nyd = notYetDetermined      bi
    p   = nextMsgFindIntersect2 bi
    
    rejected' = if found then rejected bi else Just p

    -- the points that are accepted by implication and the points that remain
    -- undetermined
    --
    -- NB @implied@ excludes @p@ even when @p@ is accepted. This is so the
    -- definition of 'candidate' below can explicitly include @p@ in parallel
    -- with 'comm', in order to make the relevant invariant for Jumpers
    -- obviously true.
    (implied, nyd') = case (rejected bi, found) of
        (Nothing, False) -> (Seq.empty, neInit nyd)
        (Nothing, True ) -> (neInit (notYetDetermined bi), Seq.empty)
        (Just{},  False) -> (Seq.empty, neBeforeMid nyd)
        (Just{},  True ) -> neInit `first` neHalves nyd

    y' = CsjClientState {
        anticomm  =
            (if Seq.null nyd' then maybe id Set.insert rejected' else id)
          $ (if not found then id else trimAnticomm (At p))
          $ anticomm y
      ,
        candidate =
            (if not found then id else (\cand -> cand <> implied Seq.|> p))
          $ candidate y
      ,
        comm      = if not found then comm y else At p
      }

    eBi' = case nonEmptySeq nyd' of
        L.Nothing     -> Left (Class (At p))
        L.Just neNyd' -> Right Bisecting {
            notYetDetermined = neNyd'
          ,
            rejected         = rejected'
          }

-- | For 'jumpings' that just processed their latest response
--
-- They were otherwise about to either become 'jumpeds' or send another request
-- and stay in 'jumpings'.
newJumpRequest ::
     Ord p
  => JumpRequest p
  -> (CsjClientState p, Either (Class p) (Bisecting p))
  -> (CsjClientState p, Either (Class p) (Bisecting p))
newJumpRequest req (y, eBi) =
    (y', newJumpRequest2 req y' mbClss)
  where
    (y', mbClss) = case eBi of
        Left clss -> (y, Just clss)
        Right bi  -> (
            -- Its previous bisection is being interrupted by this new jump
            -- request, so integrate 'rejected' into 'anticomm'.
            CsjClientState {
                anticomm  = maybe id Set.insert (rejected bi) (anticomm y)
              ,
                candidate = candidate y
              ,
                comm      = comm y
              }
          ,
            Nothing
          )

-- | For possibly restarting 'jumpeds' and for pivoting 'jumpings' that just
-- processed their latest response
--
-- Note that the resulting 'notYetDetermined' has already been constrained by
-- 'comm' and 'anticomm'; therefore, a Jumper /cannot/ violate them.
newJumpRequest2 ::
     Ord p
  => JumpRequest p
  -> CsjClientState p
  -> Maybe (Class p)
  -> Either (Class p) (Bisecting p)
newJumpRequest2 (JumpRequest dynamoClss ps0) y mbClss
  | Just clss <- mbClss, clss /= dynamoClss = Left (min clss dynamoClss)
  | isect /= comm y                         = Left (Class isect)
  | otherwise                               =
    case nonEmptySeq nyd' of
        L.Nothing     ->
            -- The Jumper already knows its exact intersection with this jump.
            --
            -- It seems impossible to reach this branch except as part of a
            -- counterexample to the NON-invariant documented on 'JumpRequest'.
            Left (Class isect)
        L.Just neNyd' -> Right Bisecting {
            notYetDetermined = neNyd'
          ,
            rejected         =
                if Seq.length suffix == Seq.length nyd' then Nothing else
                -- If this peer has already rejected part of this jump request,
                -- then don't give it a chance to accept the rest in one
                -- 'JumpFindIntersect': the Dynamo and Jumper are not serving
                -- the same chain, and so we can't be in the steady-state where
                -- they're both honest, which is the only motivation for the
                -- all-in-one @MsgFindIntersect@.
                Just $ suffix `Seq.index` Seq.length nyd'
          }
  where
    onCandidate = L.isJust . leftmostInAscSeq (candidate y)

    -- the jump request's intersection with the Jumper's 'candidate' and the
    -- jump request's suffix after that intersection
    (isect, suffix) = case neBinarySearch (not . onCandidate) ps0 of
        LastFalseFirstTrue i j -> (At $ ps0 `neIndex` i, neDrop j ps0)
        Uniformly False        ->
            -- TODO Is this reachable? How could the /entire/ 'JumpRequest'
            -- already be on the Jumper's 'candidate'?
            (At $ neLast ps0, Seq.empty)
        Uniformly True         -> (Origin, toSeq ps0)

    -- the prefix of @suffix@ before any 'anticomm'
    --
    -- Yet again, consider counterexamples to the NON-invariant documented on
    -- 'JumpRequest' for how @nyd'@ might not equal @suffix@.
    --
    -- Note that membership in 'anticomm' is a non-monotonic property and so
    -- this must be a linear search.
    nyd' = Seq.takeWhileL (`Set.notMember` anticomm y) suffix

-----

-- | State for the CSJ logic
--
-- Disengaged peers are not present here, but it is assumed that they still
-- participate in the GDD contests and calculation of the Limit on Eagerness
-- (LoE).
--
-- See 'backfill' for the invariants.
data CsjState pid p = CsjState {
    -- | The peer that will request the next jump
    --
    -- See 'issueNextJump' for the conditions that will trigger that next
    -- jump.
    --
    -- The extra @p@ is the header the ChainSync client has just received,
    -- before having processed it. The reception of a header beyond the
    -- forecast range can still trigger a jump because the CSJ logic can
    -- observe this piece of state (via 'MsgRollForwardSTART').
    --
    -- Excluding transient states, there will only not be a Dynamo if all peers
    -- are disengaged. See 'backfill' for more details.
    dynamo :: !(Maybe (Dynamo pid p))
  ,
    -- | All peers that are not the Dynamo
    --
    -- See 'NonDynamo'.
    nonDynamos :: !(Map pid (NonDynamo p))
  ,
    -- | Only used to handle the 'Connect' event
    --
    -- It is only 'Origin' before any Dynamo has ever sent a jump request.
    -- Thereafter, it will always be 'At'.
    latestJump :: !(WithOrigin (JumpRequest p))
  ,
    -- | The queue for becoming Dynamo
    --
    -- It would be safe for an implementation to break ties randomly instead of
    -- via this queue, but it's nice to not need randomness in the
    -- specification and the queue might reduce volatility in the "fairness".
    queue :: !(Perm pid)
  }
  deriving (Read, Show)

initialCsjState :: CsjState pid p
initialCsjState = CsjState Nothing Map.empty Origin (UnsafePerm Seq.empty)

-- | The Limit on Eagerness (LoE) must never extend this point
--
-- This additional constraint on the LoE prevents the following awkward corner
-- case.
--
-- - The first peer joins. Its 'comm' and 'candidate' are initialized to the
--   ImmDB tip and it becomes the Dynamo.
--
-- - It syncs some headers and blocks, such that the ImmDB advances. (Recall
--   that in this counterfactual, the LoE is not constrained by
--   'csjLoeContribution'.)
--
-- - A second peer joins.
--
-- - The first peer is demoted before processing enough headers to offer a
--   jump.
--
-- - The second peer is promoted, but is also demoted before processing enough
--   headers to offer a jump.
--
-- - As the first peer is promoted to Dynamo again, its 'promotionMessage' is
--   for a point that is older than local node's ImmDB tip. That's because its
--   'comm' still has its original value, since only jump requests advance the
--   'comm'. The ChainSync client can't accept this intersection; it will throw
--   'Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InvalidIntersection'
--   instead.
--
-- It's technically possible that the first peer was the only peer that
-- satisfies the Honest Availability Assumption (HAA) and the starvation was
-- just a temporary hiccup (which is why 'Starvation' doesn't even disengage
-- the peer). So that risk of disconnection is unacceptable.
--
-- This constraint eliminates that risk by preventing the LoE from advancing
-- while the first peer is the Dynamo.
--
-- /Remark/. The PreSyncing state of the Genesis State Machine (GSM) overrides
-- the LoE to 'Origin' until the Diffusion Layer indicates the node almost
-- certainly has a peer that satisfies the HAA. That makes the above
-- counterfactual /extremely/ unlikely even without this additional constraint
-- under normal circumstances. But this additional constraint instead simply
-- makes the counterfactual always impossible.
csjLoeConstraint :: CsjState pid p -> L.Maybe (WithOrigin p)
csjLoeConstraint x =
    case dynamo x of
        Nothing                         -> L.Nothing
        Just (Dynamo _pid _clss y _mbQ) -> L.Just $ comm y

data Dynamo pid p =
    Dynamo !pid !(Class p) !(CsjClientState p) !(Maybe p)
  deriving (Read, Show)

data NonDynamo p =
    -- | Jumpers that are done bisecting the latest jump request
    Jumped !(Class p) !(CsjClientState p)
  |
    -- | Jumpers that are still bisecting
    --
    -- The 'Bisecting' tracks their progress in determined the peer's
    -- intersection with the jump request.
    --
    -- The 'JumpRequest' is present if a new request arose.
    Jumping !(CsjClientState p) !(Bisecting p) !(Maybe (JumpRequest p))
  |
    -- | Peers that are currently challenging the Dynamo
    --
    -- The extra @p@ is the same as for 'dynamo'.
    Objector !(Class p) !(CsjClientState p) !(Maybe p)
  deriving (Read, Show)


objectorClasses :: Ord p => CsjState pid p -> Set (Class p)
objectorClasses x =
    Map.foldl snoc Set.empty (nonDynamos x)
  where
    snoc acc = \case
        Jumped{}              -> acc
        Jumping{}             -> acc
        Objector clss _y _mbQ -> Set.insert clss acc
-----

-- | Events the CSJ logic needs to react to
data CsjStimulus p =
    -- | The peer responded to a ChainSync request
    ChainSyncReply !(ChainSyncReply p)
  |
    -- | The peer just connected
    --
    -- The argument is the contemporary ImmDB chain, which is used to
    -- initialize 'CsjClientState' in reasonable way.
    Connect !(Seq p)
  |
    -- | The peer disconnected, including LoP, GDD, etc
    Disconnect
  |
    -- | The peer starved ChainSel
    Starvation
  deriving (Read, Show)

data ChainSyncReply p =
    MsgAwaitReply
  |
    MsgIntersectionFound !(WithOrigin p)
  |
    MsgIntersectionNotFound
  |
    MsgRollBackward !(WithOrigin p)
  |
    -- | After the ChainSync client processes the header
    MsgRollForwardDONE !p
  |
    -- | As soon as the ChainSync client recieves the header
    --
    -- In particular: before it might block on forecasting the ledger view.
    MsgRollForwardSTART !p
  deriving (Read, Show)

-- | Actions the ChainSync clients should execute as part of the CSJ's logics
-- reaction to some 'CsjStimulus'
data CsjReaction p =
    -- | the ChainSync client should run as normal (ie pipelining
    -- @MsgRequestNext@s) until 'Stop' is given
    Continue
  |
    -- | ChainSync should hereafter run as normal; CSJ will never give it
    -- another command
    Disengage
  |
    -- | the ChainSync client should send a @MsgFindIntersect@ with only this
    -- point
    --
    -- The point cannot be older than the ImmDB tip, since it's younger than
    -- the peer's 'comm' and the LoE cannot be younger than any 'comm'.
    MsgFindIntersect !(WithOrigin p)
  |
    -- | used to demote the Dynamo to a Jumper upon 'Starvation'
    --
    -- Implies a @drainThePipe@ call and even suppression of an outstanding
    -- 'MsgRollForwardDONE' if there is one.
    Stop
  deriving (Read, Show)

-- | Update the state and emit new 'CsjReaction's in response to the arrival of
-- 'ChainSyncReply's
--
-- It only returns 'Nothing' if the given @pid@ is a disengaged or the given
-- 'ChainSyncReply' implies a mini protocol violation.
csjReactions ::
     (Ord p, Ord pid)
  => CsjEnv p
  -> CsjState pid p
  -> pid
  -> CsjStimulus p
  -> L.Maybe (CsjState pid p, [(pid, CsjReaction p)])
csjReactions env x pid = fmap (issueNextJump env . backfill) . \case

    Connect immCh -> pure $
        let y = CsjClientState {
                anticomm  = Set.empty
              ,
                candidate = immCh
              ,
                comm      = case immCh of
                    Seq.Empty   -> Origin
                    _ Seq.:|> p -> At p
              }

            (nonDynamo, mbMsg) = case latestJump x of
                Origin                          ->
                    (Jumped (Class Origin) y, Nothing)
                At (JumpRequest _dynamoClss ps) ->
                    let bi = Bisecting {
                            notYetDetermined = ps
                          ,
                            rejected         = Nothing
                          }
                    in
                    (Jumping y bi Nothing, Just (nextMsgFindIntersect bi))
        in (
            CsjState {
                dynamo     = dynamo x
              ,
                latestJump = latestJump x
              ,
                nonDynamos = Map.insert pid nonDynamo (nonDynamos x)
              ,
                -- Prefer older peers so that the peer that satisfies
                -- the Honest Availability Assumption will become
                -- Dynamo before any replacement peers do.
                --
                -- It would also be sound for an implementation to
                -- instead insert at a random position.
                queue      = snocPerm pid (queue x)
              }
          ,
            maybe [] ((:[]) . (,) pid) mbMsg
          )

    ChainSyncReply reply -> case reply of

        MsgAwaitReply -> onActive (const L.Nothing)
            -- TODO should the Dynamo immediately issue a jump regardless of
            -- 'minJumpSlots'? Otherwise every peer will have to fetch the
            -- recent headers, and there could be thousands of them.

        MsgIntersectionFound p ->
                onJumping (intersectionFound p)
            <|>
                onActive (finishPromotion p)
        MsgIntersectionNotFound ->
                onJumping intersectionNotFound
            <|>
                -- The Jumper failed its promotion.
                onActive (const L.Nothing)

        MsgRollBackward     p -> onActive (backward p)
        MsgRollForwardDONE  p -> onActive (forward  p)
        MsgRollForwardSTART p ->
            fmap (flip (,) [])
          $ preDynamo p <|> preObjector p

    Disconnect -> pure disconnect

    Starvation -> do
        Dynamo pid' clss y _mbQ <- toLazy $ dynamo x
        let shouldNotDemote =
                pid' /= pid
             ||
                -- Don't demote the Dynamo if we'll have to immediately
                -- repromote them.
                Map.null (nonDynamos x)
        pure $
            if shouldNotDemote then (x, []) else (
                CsjState {
                    dynamo     = Nothing
                  ,
                    latestJump = latestJump x
                  ,
                    nonDynamos =
                        Map.insert
                            pid
                            (Jumped clss (trimCandidate y))
                            (nonDynamos x)
                  ,
                    queue      = snocPerm pid (queue x)
                  }
              ,
                [(pid, Stop)]
              )

  where
    disengage  = forget False
    disconnect = forget True

    forget disconnecting = (
        CsjState {
            dynamo     =
                case dynamo x of
                    Just (Dynamo pid' _clss _y _mbQ) | pid /= pid' -> dynamo x
                    _                                              -> Nothing
          ,
            latestJump = latestJump x
          ,
            nonDynamos = Map.delete pid (nonDynamos x)
          ,
            queue      = deletePerm pid (queue x)
          }
        ,
          if disconnecting then [] else [(pid, Disengage)]
        )

    onActive f = onDynamo f <|> onObjector f

    onDynamo f = do
        Dynamo pid' clss y _mbQ <- toLazy $ dynamo x
        guard $ pid' == pid
        pure $ case f y of
            L.Nothing -> (
                CsjState {
                    dynamo     = Nothing
                  ,
                    latestJump = latestJump x
                  ,
                    nonDynamos = nonDynamos x
                  ,
                    queue      = queue x
                  }
              ,
                [(pid, Disengage)]
              )
            L.Just y' -> (
                CsjState {
                    dynamo     = Just $ Dynamo pid clss y' Nothing
                  ,
                    latestJump = latestJump x
                  ,
                    nonDynamos = nonDynamos x
                  ,
                    queue      = queue x
                  }
              ,
                [(pid, Continue)]
              )

    onObjector f = do
        Objector clss y _mbQ <- Map.lookup pid (nonDynamos x)
        pure $ case f y of
            L.Nothing -> disengage
            L.Just y' -> (
                CsjState {
                    dynamo     = dynamo x
                  ,
                    latestJump = latestJump x
                  ,
                    nonDynamos =
                        Map.insert
                            pid
                            (Objector clss y' Nothing)
                            (nonDynamos x)
                  ,
                    queue      = queue x
                  }
              ,
                [(pid, Continue)]
              )

    onJumping f = do
        Jumping y bi mbNext <- Map.lookup pid (nonDynamos x)
        pure $ case maybe id newJumpRequest mbNext <$> f y bi of
            L.Nothing         -> disengage
            L.Just (y', eBi') -> case eBi' of
                Left clss -> (
                    CsjState {
                        dynamo     = dynamo x
                      ,
                        latestJump = latestJump x
                      ,
                        nonDynamos =
                            Map.insert
                                pid
                                (Jumped clss y')
                                (nonDynamos x)
                      ,
                        queue      = queue x
                      }
                  ,
                    []
                  )
                Right bi' -> (
                    CsjState {
                        dynamo     = dynamo x
                      ,
                        latestJump = latestJump x
                      ,
                        nonDynamos =
                            Map.insert
                                pid
                                (Jumping y' bi' Nothing)
                                (nonDynamos x)
                      ,
                        queue      = queue x
                      }
                  ,
                    [(pid, nextMsgFindIntersect bi')]
                  )

    preDynamo p = do
        Dynamo pid' clss y Nothing <- toLazy $ dynamo x
        guard $ pid' == pid
        pure CsjState {
            dynamo     = Just $ Dynamo pid' clss y (Just p)
          ,
            latestJump = latestJump x
          ,
            nonDynamos = nonDynamos x
          ,
            queue      = queue x
          }

    preObjector p = do
        Objector clss y Nothing <- Map.lookup pid (nonDynamos x)
        pure CsjState {
            dynamo     = dynamo x
          ,
            latestJump = latestJump x
          ,
            nonDynamos =
                Map.insert pid (Objector clss y (Just p)) (nonDynamos x)
          ,
            queue      = queue x
          }

-----

-- | Promote Jumpers or Objectors to the Dynamo and Jumpers to Objectors as
-- necessary
--
-- @'backfill' = 'fillObjectors' . 'backfillDynamo'@
--
-- There are only the following invariants to maintain.
--
-- - There is a Dynamo unless all peers are disengaged or are Jumpers that are
--   not yet done bisecting.
--
-- - No two peers among the Dynamo and the Objectors are serving the same
--   chain. This is ensured by only promoting a Jumper if it is done bisecting
--   and has a different 'Class' than the Dynamo and every Objector. The
--   'Class' of a Jumper that's done bisecting is its exact intersection with
--   the bisected jump; the 'comm' before its done bisecting is merely a lower
--   bound on its 'Class'.
--
-- - A Jumper that is not yet done bisecting must not be promoted. The 'Class'
--   of the Dynamo or an Objector must be its exact intersection with
--   'latestJump', since if it's not, a Jumper with a younger 'Class' might
--   actually be serving the same chain as the Dynamo or an Objector.
--
-- - The oldest 'Class' among the Jumpers that are done bisecting is equal to
--   the 'Class' of the Dynamo or some Objector. This should be restored by
--   promoting such a Jumper.
--
-- Beyond those invariants, it doesn't matter which Jumper or Objector
-- backfills as the Dynamo nor which Jumper backfills as the Objector for the
-- oldest 'comm', as long as ties are broken in a way that ensures a peer that
-- satisifes the Honest Availability Assumption will eventually become Dynamo.
-- The 'queue' mechanism achieves that without overreacting to 'Starvation'
-- events (which are inevitable due to the imperfection of the public Internet
-- infrastructure).
--
-- Once this function promotes a Jumper to Dynamo or Objector, that peer must
-- never subsequently be demoted to a Jumper. Doing so risks fetching headers
-- from honest peers more than once, which CSJ should avoid. The one exception
-- is if the Dynamo starves ChainSel. It is excusable to demote that Dynamo
-- since it will only happen at most once to each honest peer until a peer that
-- satisfies the Honest Availability Assumption becomes the Dynamo, which will
-- remain the Dynamo until the end of the sync.
--
-- The prohibition on demoting Objectors prevents the CSJ design from doing
-- both of the following.
--
-- - Promoting a Jumper to Objector before all Jumpers have finished bisecting.
--
-- - Never having multiple Objectors.
--
-- The design in this file favors having multiple Objectors, since every Jumper
-- that is ready to be Objector will eventually need to have a chance to
-- contest the Dynamo---might as well start immediately. But the alternative
-- design that waits for all Jumpers to finish bisecting before promoting any
-- to Objectors also seems plausible.
backfill ::
     (Ord p, Ord pid)
  => (CsjState pid p, [(pid, CsjReaction p)])
  -> (CsjState pid p, [(pid, CsjReaction p)])
backfill = fillObjectors . backfillDynamo

-- | See 'backfill'
--
-- Promotes the leftmost @pid@ in 'queue' that is either an Objector or a
-- 'Jumped' in a 'Class' that has no Objectors.
backfillDynamo ::
     (Ord p, Ord pid)
  => (CsjState pid p, [(pid, CsjReaction p)])
  -> (CsjState pid p, [(pid, CsjReaction p)])
backfillDynamo (x, msgs) =
    L.maybe (x, msgs) promote
  $ case dynamo x of
        Just{}  -> Alt.empty
        Nothing -> getAlt $ foldMap try (queue x)
  where
    try pid = Alt $ case Map.lookup pid (nonDynamos x) of
        L.Nothing        -> error "impossible!"
        L.Just nonDynamo -> case nonDynamo of
            Jumped clss y       -> do
                guard $ clss `Set.notMember` objectorClasses x
                pure (Dynamo pid clss y Nothing, Just (promotionMessage y))
            Jumping{}           -> Alt.empty
            Objector clss y mbQ -> pure (Dynamo pid clss y mbQ, Nothing)

    promote (Dynamo pid clss y mbQ, mbMsg) = (
        CsjState {
            dynamo     = Just $ Dynamo pid clss y mbQ
          ,
            latestJump = latestJump x
          ,
            nonDynamos = Map.delete pid (nonDynamos x)
          ,
            queue      = deletePerm pid (queue x)
          }
      ,
          maybe id (:) ((,) pid <$> mbMsg) msgs
      )

-- | See 'backfill'
--
-- If there is a 'dynamo' and the oldest 'Class' has only 'Jumped's, then
-- promotes the leftmost according to 'queue'.
--
-- A correctly implementation could also promote 'Jumped's from any 'Class'
-- that contains no 'Objector' nor 'dynamo'.
fillObjectors ::
     (Ord p, Ord pid)
  => (CsjState pid p, [(pid, CsjReaction p)])
  -> (CsjState pid p, [(pid, CsjReaction p)])
fillObjectors (x, msgs) =
    fromMaybe (x, msgs)
  $ case dynamo x of
        Nothing                         -> Nothing
        Just (Dynamo _pid clss _y _mbQ) ->
            uncurry promote
          $ Map.foldlWithKey snoc (clss, AlreadyOccupied) (nonDynamos x)
  where
    promote clss = \case
        AlreadyOccupied -> Nothing
        Filler _i pid y -> Just (
            CsjState {
                dynamo     = dynamo x
              ,
                latestJump = latestJump x
              ,
                nonDynamos =
                    Map.insert
                        pid
                        (Objector clss y Nothing)
                        (nonDynamos x)
              ,
                queue      = queue x
              }
          ,
            (pid, promotionMessage y) : msgs
          )

    snoc acc pid nonDynamo = case nonDynamo of
        Jumped clss y         ->
            snoc2 acc clss
          $ Filler (indexPerm pid (queue x)) pid y
        Jumping{}             -> acc
        Objector clss _y _mbQ -> snoc2 acc clss AlreadyOccupied

    snoc2 acc clss filler =
        if newBest acc clss filler then (clss, filler) else acc

    -- whether the new nonDynamo is the best filler so far
    newBest (clss, filler) clss' filler' =
        case compare clss' clss of
            LT -> True
            EQ -> case fillerRank filler of
                Nothing -> False
                Just i  -> case fillerRank filler' of
                    Nothing -> True
                    Just j  -> j < i
            GT -> False

data FillAcc pid p =
    -- | The Dynamo or some Objector already inhabits this 'Class'
    AlreadyOccupied
  |
    -- | A peer with the given position in 'queue' inhabits this class
    Filler !Int !pid !(CsjClientState p)

-- | 'fillObjectors' breaks ties between two 'Filler's by choosing the lowest
-- rank
fillerRank :: FillAcc pid p -> Maybe Int
fillerRank = \case
    AlreadyOccupied  -> Nothing
    Filler i _pid _y -> Just i

-----

data CsjEnv p = CsjEnv {
    -- | The minimum jump size
    --
    -- PREREQUISITE: not greater than the Genesis window
    --
    -- From the Genesis window, this also inherits the forecast range as an
    -- upper bound. If 'minJumpSlots' is the full forecast range and the
    -- ChainSync client were restricted to only forecast from the immutable
    -- tip, then there cannot be a next jump until everyone has finished
    -- bisecting the current jump.
    --
    -- - We tend to think of 'minJumpSlots' as the full forecast range. But
    --   that's not required at all. The larger it is, however, the less load
    --   on the honest network.
    --
    -- - ChainSync currently forecasts from the candidate's intersection with
    --   the local selection. This could prevent the ChainSync pipeline from
    --   starving when the selection is along the Dynamo's chain. It also makes
    --   ChainSync possible for chains that don't satisfy Chain Growth (as long
    --   as they're uncontested). However, it's not necessary (for chains that
    --   satisfy Chain Growth) and we do suspect some correctness arguments
    --   would be simpler if forecasts only originated from the immutable tip.
    --
    -- Because the jump size might not necessarily be maxed and because
    -- ChainSync currently uses opportunistic forecast ranges, CSJ can let the
    -- Dynamo request another jump before all peers have resolved the current
    -- jump. It would be possible to simply wait for all bisections to finish
    -- before allowing the next jump, but the logic for interrupting them isn't
    -- too complicated, which might reduce the adversary's opportunity to delay
    -- (since 'newJumpRequest2' leverages the 'comm' and 'rejected' of the
    -- interrupted jump to trim the new jump's 'notYetDetermined').
    minJumpSlots :: Word64
  ,
    realPointSlot :: p -> Word64
  }

-- | Whether the Dynamo has received enough headers to issue a new jump
bigEnoughJump :: CsjEnv p -> WithOrigin p -> p -> Maybe p -> Bool
bigEnoughJump env anchor p mbQ =
    realPointSlot env p + 1 >= firstSlotAfterJumpWindow
 ||
    maybe False (\q -> realPointSlot env q >= firstSlotAfterJumpWindow) mbQ
  where
    firstSlotAfterJumpWindow = minJumpSlots env + case anchor of
        Origin -> 0
        At q   -> 1 + realPointSlot env q

-- | Possibly issue a new jump to the non-Dynamos
--
-- A jump is triggered once all of the following are true.
--
-- - The jump request includes a point that the previous jump request did not.
--
-- - The Dynamo received a header more than 'bigEnoughJump' past its 'comm'.
--   The 'MsgRollForwardSTART' lets this conjunct be satisfied even if the
--   ChainSync client is not yet able to forecast the ledger view necessary to
--   validate the header. This logic is careful to exclude such an unvalidated
--   point from the jump request itself, since in the real implementation every
--   point in the jump request needs to be mapped to the corresponding
--   ChainSync client state, which isn't available for an unvalidated header.
issueNextJump ::
     Ord p
  => CsjEnv p
  -> (CsjState pid p, [(pid, CsjReaction p)])
  -> (CsjState pid p, [(pid, CsjReaction p)])
issueNextJump env (x, msgs) =
    case dynamo x of
        Just (Dynamo dynamoPid dynamoClss dynamoY dynamoMbQ)
          | L.Just neCandidate <- nonEmptySeq (candidate dynamoY)
          , let p = neLast neCandidate
          , bigEnoughJump env (comm dynamoY) p dynamoMbQ
              -- It is crucial to use 'comm' as the anchor instead of
              -- @dynamoClss@. At steady-state, they'll be equivalent, but
              -- while 'latestJump' is 'Origin', @dynamoClss@ will be 'Origin'
              -- the immutable tip is well beyond that. In contrast, 'comm' is
              -- initialized to an immutable tip.
          , dynamoClss /= Class (At p)
              -- Suppress empty jumps.
              --
              -- If the Dynamo could never satisfy this conjunct because its
              -- forecast range is empty it will be disconnected, since the
              -- ChainSync client kills any peer with an empty forecast range.
         ->
            let req = JumpRequest dynamoClss neCandidate

                (nonDynamos', msgs') =
                    flip State.runState msgs
                  $ Map.traverseWithKey instructNewJump (nonDynamos x)

                instructNewJump = \pid nonDynamo ->
                    State.state $ instructNewJump2 pid `flip` nonDynamo

                instructNewJump2 pid acc = \case
                    Jumped clss y ->
                        case newJumpRequest2 req y (Just clss) of
                            Left clss' -> (Jumped clss' y, acc)
                            Right bi   -> (
                                Jumping y bi Nothing
                              ,
                                (pid, nextMsgFindIntersect bi) : acc
                              )
                    Jumping y bi _next ->
                        (Jumping y bi (Just req), [])
                    Objector clss y mbQ ->
                        -- This 'min' ensures the Objector's class is a point
                        -- in the new jump.
                        --
                        -- If an Objector's class might not be some point in
                        -- the latest jump request, then Jumpers that are
                        -- serving the same chain as the Objector could end up
                        -- with a inequal class after bisecting the new jump.
                        (Objector (min dynamoClss clss) y mbQ, [])
          in
            (
              CsjState {
                  -- The Dynamo effectively instantly accepts the entire jump.
                  dynamo     =
                      Just
                    $ Dynamo
                          dynamoPid
                          (Class (At p))
                          CsjClientState {
                              -- 'Set.empty' would suffice here if it weren't
                              -- for counterexamples to the NON-invariant
                              -- documented at 'JumpRequest'.
                              anticomm  =
                                  trimAnticomm (At p) $ anticomm dynamoY
                            ,
                              candidate = candidate dynamoY
                            ,
                              comm      = At p
                            }
                          dynamoMbQ
                ,
                  latestJump = At req
                ,
                  nonDynamos = nonDynamos'
                ,
                  queue      = queue x
                }
            ,
                msgs'
            )
        _ -> (x, msgs)
