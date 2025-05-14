{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.CsjModel.StateTypes (module Test.CsjModel.StateTypes) where

import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Data.Functor ((<&>))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as L
-- TODO use AnchoredSeq?
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Strict.Either (Either (Left, Right), either)
import           Data.Strict.Maybe (Maybe (Just, Nothing), maybe)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Prelude hiding (Either (Left, Right), Maybe (Just, Nothing), either, maybe)
import           Test.CsjModel.NonEmptySeq
import           Test.CsjModel.Perm

{-------------------------------------------------------------------------------
  Helper pair type for clarity
-------------------------------------------------------------------------------}

-- | A point paired with whatever data (from the Dynamo) a Jumper needs in
-- order to correctly set its ChainSync client candidate (TODO not even
-- necessarily the rest of the ChainSync state) when the peer replies with
-- 'Test.CsjModel.MsgIntersectFound' for that point
--
-- Note that the payload argument is intentionally not strict.
data WithPayload p a = WP !p a
  deriving stock    (Functor, Generic, Read, Show)
  deriving anyclass (NoThunks)

mkWithPayload :: p -> a -> WithPayload p a
mkWithPayload = WP

wpPoint :: WithPayload p a -> p
wpPoint (WP p _x) = p

wpPayload :: WithPayload p a -> a
wpPayload (WP _p x) = x

wpAt :: WithPayload p a -> WithPayload (WithOrigin p) a
wpAt (WP p x) = WP (At p) x

findPoint :: Ord p => Seq (WithPayload p a) -> p -> L.Maybe Int
findPoint = leftmostInAscSeq wpPoint

neFindPoint :: Ord p => NonEmptySeq (WithPayload p a) -> p -> L.Maybe Int
neFindPoint = leftmostInAscSeq wpPoint . toSeq

{-------------------------------------------------------------------------------
  State for each engaged ChainSync client
-------------------------------------------------------------------------------}

-- | CSJ-specific state for a peer
data CsjClientState p a = CsjClientState {
    -- | points that were the youngest point rejected while bisecting a single
    -- 'JumpRequest'
    --
    -- See 'trimAnticomm'.
    --
    -- This cannot contain more points than the number of jumps this peer has
    -- ever handled without accepting more points. And that's almost
    -- entirely bounded by how many adversarial peers became Dynamo without an
    -- honest peer becoming Dynamo. So this seems sufficiently bounded to never
    -- be a resource leak---also: each @p@ is tiny on the heap.
    anticomm  :: !(Set p)
  ,
    -- | the candidate fetched from the peer so far
    --
    -- Strictly ascending, and begins with a recent immutable tip. It's only
    -- empty if the candidate (and recent immutable tip) is 'Origin'.
    candidate :: !(Seq (WithPayload p a))
  }
  deriving stock (Functor, Generic, Read, Show)
  deriving anyclass (NoThunks)

candidateTip :: CsjClientState p a -> WithOrigin p
candidateTip y = case candidate y of
    Seq.Empty       -> Origin
    _wps Seq.:|> wp -> At $ wpPoint wp

-- | 'anticomm' can be garbage-collected when more points are accepted
trimAnticomm :: Ord p => WithOrigin p -> Set p -> Set p
trimAnticomm newcomm ps = case newcomm of
    Origin -> ps
    At p   -> snd $ Set.split p ps

-- | Drop all but one immutable point from 'candidate'
trimCandidate ::
     Ord p
  => CsjClientState p a
  -> WithOrigin p
     -- ^ current imm tip
  -> CsjClientState p a
trimCandidate y = \case
    Origin -> y
    At p   -> case findPoint (candidate y) p of
        L.Nothing ->
            -- TODO debug why this is happening in the tests
            y

            -- The LoE prevents this, since 'candidate' includes at least
            -- one point that was previously the immutable tip.
--            error "impossible!"
        L.Just i  ->
            CsjClientState {
                anticomm  = anticomm y
              ,
                candidate = Seq.drop i (candidate y)
              }

{-------------------------------------------------------------------------------
  The state of the CSJ governor
-------------------------------------------------------------------------------}

-- | State for the CSJ logic
--
-- Disengaged peers are not present here, but it is assumed that they still
-- participate in the GDD contests and calculation of the Limit on Eagerness
-- (LoE).
--
-- See 'Test.CsjModel.backfill' for the invariants about where a @pid@ is
-- located within this data structure.
data CsjState pid p a = CsjState {
    disengaged :: !(Set pid)
  ,
    -- | The peer that will request the next jump, unless it stops being Dynamo
    -- before then
    --
    -- See 'Test.CsjModel.issueNextJumpIfReady' for the conditions that will
    -- trigger that next jump.
    --
    -- See 'Test.CsjModel.backfill' for details about how the 'dynamo' is
    -- replaced when lost (eg disconnects, disengages, or Devoted BlockFetch
    -- demotion).
    dynamo     :: !(Maybe (Dynamo pid p a))
  ,
    -- | Peers that are not the 'dynamo' and are not waiting in 'latestJump'
    --
    -- See 'NonDynamo'.
    --
    -- TODO need a different name here, since each peer in 'latestJump' is not
    -- disengaged, not the Dynamo, and not in this map.
    nonDynamos :: !(Map pid (NonDynamo p a))
  ,
    -- | Only used to handle the 'Test.CsjModel.Connect' event
    --
    -- It's initialized as @'Left' 'Map.empty'@. Whenever a Dynamo requests a
    -- jump, it becomes 'Right'.
    --
    -- If a new peer 'Test.CsjModel.Connect's while this is 'Left', then they
    -- are added to the 'Map'; they'll become a Jumper as soon as this value
    -- becomes a 'Right'. If a new peer 'Test.CsjModel.Connect's while this is
    -- 'Right', then either the imm tip is a point on this jump or it isn't (a
    -- surprising corner case that must be supported). If it is, then the peer
    -- is initialized as a Jumper with that 'JumpRequest'. If the imm tip isn't
    -- present, this value reverts to 'Left' with the peer as its singleton
    -- argument.
    --
    -- The peers in the @'Left' 'Map'@ have no 'Class' and their 'candidate' is
    -- set to the contemporary imm tip. Note that as soon as any peer has the
    -- imm tip as their candidate the LoE prevents the imm tip from changing.
    --
    -- It's important that there will necessarily eventually be a next
    -- 'JumpRequest' while this value is 'Left'.
    --
    -- - 'Test.CsjModel.minJumpSlots' is no greater than the forecast range.
    --
    -- - 'Test.CsjModel.issueNextJumpIfReady' measures its
    -- 'Test.CsjModel.minJumpSlots' starting from the 'Class' of the Dynamo.
    --
    -- - This value is 'Left' only when the imm tip is younger than every point
    --   on the most recent 'JumpRequest', which includes the Dynamo's 'Class'.
    --   Thus the Dynamo's forecast range starts ahead of it's 'Class', and so
    --   the Dynamo will either reach 'Test.CsjModel.minJumpSlots' or die
    --   trying (and be backfilled).
    --
    -- - 'Test.CsjModel.backfill' will even promote a Dynamo from this set of
    --   peers as a last resort.
    latestJump :: !(Either (Map pid (CsjClientState p a)) (JumpRequest p a))
  ,
    -- | The queue for becoming Dynamo; it contains the @pid@ of every peer
    -- that is not disengaged
    --
    -- It would be safe for an implementation to break ties randomly instead of
    -- via this queue, but it's nice to not need randomness in the
    -- specification and the queue might reduce volatility in the "fairness".
    queue      :: !(Perm pid)
  }
  deriving stock (Generic, Show)

deriving instance (Ord p, Ord pid, Read (WithOrigin p), Read pid, Read p, Read a) => Read (CsjState pid p a)
deriving instance (forall x y. (NoThunks x, NoThunks y) => NoThunks (Either x y), forall x. NoThunks x => NoThunks (Maybe x), NoThunks pid, NoThunks p, NoThunks a) => NoThunks (CsjState pid p a)

-- the deriver can't handle the 'Left' case
instance Functor (CsjState pid p) where
    fmap f x = CsjState {
        disengaged = disengaged x
      ,
        dynamo     = maybe Nothing (Just . fmap f) $ dynamo x
      ,
        nonDynamos = Map.map (fmap f) $ nonDynamos x
      ,
        latestJump = either (Left . Map.map (fmap f)) (Right . fmap f) $ latestJump x
      ,
        queue      = queue x
      }

initialCsjState :: CsjState pid p a
initialCsjState = CsjState {
    disengaged = Set.empty
  ,
    dynamo     = Nothing
  ,
    nonDynamos = Map.empty
  ,
    latestJump = Left Map.empty
  ,
    queue      = UnsafePerm Seq.empty
  }

-----

-- | The (exact!) intersection of a peer's 'cand' and the latest jump request
-- they've handled
--
-- This could be a calculation intead of being tracked in the state, but in any
-- implementation it will definitely be cached, so it's nice for the
-- specification to also explain how to maintain it directly.
--
-- Jumpers that are still bisecting do not have a 'Class', since their exact
-- intersection with 'latestJump' is unknown.
--
-- INVARIANT: Peers serving the same chain will have the same 'Class' once they
-- finish bisecting.
--
-- Since it's possible for the 'latestJump' to be so stale that the imm tip is
-- a proper extension of some point on it, the 'Class' might also be older than
-- the imm tip. However, 'cand' is an extension of the 'Class', so the imm tip
-- in that case must also be an extension of the 'Class'.
--
-- WARNING. The 'Class' of an 'Objector' cannot increase, for two reasons.
--
-- - The CSJ governor doesn't even check if the 'candidate' of an 'Objector'
--   overlaps with a new 'JumpRequest'---it merely combines their 'Class'es.
--
-- - Even if the governor did check, the 'Objector' couldn't have a 'Class'
--   until they send enough headers (either one that disagrees or the tip of
--   the latest 'JumpRequest'), since the header they haven't sent yet might
--   also be on the 'JumpRequest'. Thus adding that logic would either require
--   more synchronization (eg delaying the next 'JumpRequest', or maybe a
--   @Objecting@ comparable to 'Jumping', etc) or else risk assigning the
--   'Objector' to a 'Class' that is older than its actual intersection.
--
-- Because the 'Class' of an 'Objector' cannot increase, it also must not
-- decrease! Suppose it decreased and then it should have increased but could
-- not (see above). Thus its 'Class' is again older than its actual
-- intersection with the 'Dynamo' that sent the most recent 'JumpRequest'.
--
-- It's unacceptable for its 'Class' to be different than it's actual
-- intersection, since that would unjustly prevent a 'Jumped' with that same
-- older 'Class' from being promoted. And that threatens liveness, since the
-- 'candidate' of that 'Jumped' might be restraining the LoE, and so the imm
-- tip, and so the forecast range of the 'Dynamo' and all 'Objector's, and so
-- preventing a GDD disconnection.
--
-- Thus, the 'Dynamo' must never have a 'Class' older than the 'Class' of any
-- 'Objector', since such a 'Dynamo' would eventually send out a 'JumpRequest'
-- that would decrease the 'Class' of some 'Objector's. A peer with an older
-- 'Class' could eventually become 'Dynamo', but only by outliving all the
-- younger 'Class' members /as an 'Objector'/.
newtype Class p = Class (WithOrigin p)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NoThunks)

deriving instance (Read (WithOrigin p), Read p) => Read (Class p)

data Dynamo pid p a =
    -- | The @'Maybe' p@ is the header the ChainSync client has just received,
    -- /before/ having processed it. The reception of a header beyond the
    -- forecast range can still trigger a jump because the CSJ logic can
    -- observe this piece of state (via 'Test.CsjModel.MsgRollForwardSTART')---the peer has
    -- informed the syncing node that the slots between this header and the
    -- previous are empty.
    Dynamo !pid !(Class p) !(CsjClientState p a) !(Maybe p)
  deriving stock (Functor, Generic, Show)

deriving instance (Ord p, Read (WithOrigin p), Read pid, Read p, Read a) => Read (Dynamo pid p a)
deriving instance (forall x. NoThunks x => NoThunks (Maybe x), NoThunks pid, NoThunks p, NoThunks a) => NoThunks (Dynamo pid p a)

-- | Peers that are not the Dynamo and are not waiting in 'latestJump'
data NonDynamo p a =
    -- | Jumpers that are done bisecting the latest jump request
    --
    -- It's easy to forget that 'cand' might be a proper extension of 'Class':
    -- the 'Class' must be part of the previous 'JumpRequest', so if a Jumper
    -- had already committed to a different chain (due to a prior
    -- 'JumpRequest'), its 'Class' will be the intersection of 'cand' and the
    -- 'latestJump'.
    Jumped !(Class p) !(CsjClientState p a)
  |
    -- | Jumpers that are still bisecting
    Jumping !(CsjClientState p a) !(Bisecting p a) !(SentStatus p a)
  |
    -- | Peers that are currently challenging the Dynamo
    --
    -- The @'Maybe' p@ is the same as for 'dynamo'.
    --
    -- They must be serving a different chain than the Dynamo and all
    -- Objectors; see 'Test.CsjModel.backfill' for details.
    Objector !(Class p) !(CsjClientState p a) !(Maybe p)
  deriving stock (Functor, Generic, Show)

deriving instance (Ord p, Read (WithOrigin p), Read p, Read a) => Read (NonDynamo p a)
deriving instance (forall x. NoThunks x => NoThunks (Maybe x), NoThunks p, NoThunks a) => NoThunks (NonDynamo p a)

-----

-- | An argument to the 'Jumping' constructor that tracks the progress of
-- determining the peer's intersection with some 'JumpRequest' (not necessarily
-- the latest!)
data Bisecting p a = Bisecting {
    -- | points that might still be the exact intersection of this peer and the
    -- jump request
    --
    -- INVARIANT: The oldest point in this non-empty sequence is the jump
    -- request's successor of this peer's 'cand'.
    --
    -- INVARIANT: If @'rejected' = Just p@, then @p@ is the successor of the
    -- youngest point in this sequence. If @'rejected' = Nothing@, then this
    -- list is the entire jump offer.
    notYetDetermined :: !(NonEmptySeq (WithPayload p a))
  ,
    -- | The youngest point rejected so far as part of this jump
    --
    -- It is not yet included in 'anticomm', since that only contains the
    -- points that were the youngest of all points rejected while bisecting a
    -- single 'JumpRequest'.
    --
    -- INVARIANT: This is 'Just' if and only if the peer has sent
    -- 'Test.CsjModel.MsgIntersectNotFound' as part of /this/ bisection.
    rejected         :: !(Maybe p)
  }
  deriving stock (Functor, Generic, Read, Show)

deriving instance (forall x. NoThunks x => NoThunks (Maybe x), NoThunks p, NoThunks a) => NoThunks (Bisecting p a)

-- | A bisecting Jumper is stuck until it receives a response to the
-- 'Test.CsjModel.MsgFindIntersect' with this payload
nextPivot :: Bisecting p a -> WithPayload p a
nextPivot bi =
    case rejected bi of
        Nothing -> neLast nyd
            -- It would be sound and more uniform to just always bisect.
            -- However, the first request per jump is instead for the entire
            -- jump.
            --
            -- This is because /most/ jumps will be offered by an honest
            -- Dynamo, and so by offering the whole jump in the first message,
            -- load on honest Jumpers is minimized: one round-trip per jump.
        Just{}  -> neMid nyd
            -- TODO At least for the reference Haskell Cardano node, the cost
            -- of each additional point in the 'Test.CsjModel.MsgFindIntersect'
            -- message is mostly negligible unless they're in different ImmDB
            -- chunk files. So this logic here could include more points
            -- (perhaps once all of @nyd@ is in the same chunk file) in order
            -- to ensure the bisection requires fewer round trips while
            -- increasing the upstream per-message cost only negligibly.
  where
    nyd = notYetDetermined bi

-- | An argument to the 'Jumping' constructor that tracks whether the expected
-- 'Test.CsjModel.Offered', 'Test.CsjModel.MsgIntersectFound', or
-- 'Test.CsjModel.MsgIntersectNotFound stimulus has arrived.
data SentStatus p a =
    -- | The CSJ governor is waiting for the ChainSync client to report the
    -- peer's reply to the 'Test.CsjModel.MsgFindIntersect' that was sent
    --
    -- The CSJ governor will not emit more 'Test.CsjModel.MsgFindIntersect' in
    -- this state.
    --
    -- The argument is 'Just' if a new 'JumpRequest' arose before the CSJ
    -- governor was informed of the peer's reply to the latest jump; we wait to
    -- update the Jumper for the new request until the in-flight reply has been
    -- processed. When the reply arrives, the peer either continues bisecting
    -- with 'NotYetSet' (pivoting if there's a new jump request) or it finishes
    -- bisecting with 'Jumped' (and perhaps is immediately promoted). (Or it
    -- might disengage).
    --
    -- Clarification: the actual semantics here is that a Jumper's 'SentStatus'
    -- should become 'AlreadySent' as soon as the ChainSync client's
    -- continuation involves sending the message (ie after some
    -- 'Control.Monad.STM.atomically' block reads the latest
    -- 'Test.CsjModel.MsgFindIntersect' 'Test.CsjModel.CsjReaction'), even
    -- before the message is actually on the wire. IE, it should be
    -- 'AlreadySent' as soon as the CSJ governor should stop emitting more
    -- 'Test.CsjModel.MsgFindIntersect' 'Test.CsjModel.CsjReaction's for this
    -- peer and synchronized such that no emitted
    -- 'Test.CsjModel.MsgFindIntersect' is lost.
    AlreadySent !(Maybe (JumpRequest p a))
  |
    -- | The CSJ governor has emitted at least one
    -- 'Test.CsjModel.MsgFindIntersect' to this 'Jumping' peer and is waiting
    -- for the ChainSync client to indicate that it has sent the most recently
    -- emitted 'Test.CsjModel.MsgFindIntersect' by signaling
    -- 'Test.CsjModel.Offered'
    --
    -- The CSJ governor might emit more 'Test.CsjModel.MsgFindIntersect' before
    -- that 'Test.CsjModel.Offered' happens.
    --
    -- The most interesting motivation for 'NotYetSet' is if the Dynamo was
    -- demoted while blocked on the forecast range. It will be stuck until the
    -- forecast horizon advances far enough, and during that time the CSJ
    -- governor will consider its state to be 'NotYetSent', without needing any
    -- details beyond that.
    NotYetSent
  deriving stock (Functor, Generic, Show)

deriving instance (Read (WithOrigin p), Read p, Read a) => Read (SentStatus p a)
deriving instance (forall x. NoThunks x => NoThunks (Maybe x), NoThunks p, NoThunks a) => NoThunks (SentStatus p a)

-----

-- | A jump requested by the Dynamo
--
-- The arguments are the Dynamo's 'Class' and 'candidate' immediately before it
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
data JumpRequest p a =
    JumpRequest !(Class p) !(NonEmptySeq (WithPayload p a))
  deriving stock (Functor, Generic, Show)
  deriving anyclass (NoThunks)

deriving instance (Read (WithOrigin p), Read p, Read a) => Read (JumpRequest p a)

-----

class FullTrim a where fullTrim :: a -> a

instance FullTrim (CsjState pid p a) where
    fullTrim x = CsjState {
        disengaged = disengaged x
      ,
        dynamo     = dynamo x <&> \(Dynamo pid clss y mbQ) -> Dynamo pid clss (fullTrim y) mbQ
      ,
        nonDynamos = nonDynamos x <&> \case
            Jumped clss y -> Jumped clss (fullTrim y)
            Jumping y bi sent -> Jumping (fullTrim y) (fullTrim bi) (fullTrim sent)
            Objector clss y mbQ -> Objector clss (fullTrim y) mbQ
      ,
        latestJump = case latestJump x of
            Left  waiting -> Left $ fmap fullTrim waiting
            Right jreq    -> Right $ fullTrim jreq
      ,
        queue      = queue x
      }

instance FullTrim (CsjClientState p a) where
    fullTrim y = CsjClientState {
        anticomm  = anticomm y
      ,
        candidate = fullTrim $ candidate y
      }

instance FullTrim (Bisecting p a) where
    fullTrim bi = Bisecting {
        notYetDetermined = fullTrim $ notYetDetermined bi
      ,
        rejected = rejected bi
      }

instance FullTrim (SentStatus p a) where
    fullTrim = \case
        AlreadySent mbReq -> AlreadySent (fmap fullTrim mbReq)
        NotYetSent        -> NotYetSent

instance FullTrim (JumpRequest p a) where
    fullTrim (JumpRequest clss ps) = JumpRequest clss (fullTrim ps)

instance FullTrim (NonEmptySeq (WithPayload p a)) where
    fullTrim = neTrim

instance FullTrim (Seq (WithPayload p a)) where
    fullTrim xs = case xs of
        Seq.Empty                 -> Seq.Empty
        _ Seq.:<| Seq.Empty       -> xs
        x0 Seq.:<| (_ Seq.:|> xN) -> Seq.Empty Seq.|> x0 Seq.|> xN
