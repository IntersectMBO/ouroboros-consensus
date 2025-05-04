{-# LANGUAGE LambdaCase #-}

-- | An executable specification of the centralized CSJ logic
--
-- This module contains the expected interface. The @Test.CsjModel.*@ modules
-- export the rest of the definitions, but should be considered internal and
-- unstable.
module Test.CsjModel (
    ChainSyncReply (..)
  , CsjEnv (..)
  , CsjIntersectPurpose (..)
  , CsjReaction (..)
  , CsjState
  , CsjStimulus (..)
  , WithPayload
  , csjReactions
  , initialCsjState
  , mkWithPayload
  , wpPayload
  , wpPoint
  ) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo), WithOrigin (At, Origin))
import           Control.Applicative ((<|>), liftA2)
import qualified Control.Applicative as Alt (empty)
import           Control.Arrow (first)
import           Control.Exception (assert)
import           Control.Monad (guard)
import qualified Control.Monad.State.Strict as State
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as L (Maybe (Just, Nothing), isJust, maybe)
import           Data.Monoid (Alt (Alt, getAlt))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Strict.Classes (toLazy)
import           Data.Strict.Either (Either (Left, Right), either)
import           Data.Strict.Maybe (Maybe (Just, Nothing), fromMaybe, maybe)
import           Data.Word (Word64)
import           Prelude hiding (Either (Left, Right), Maybe (Just, Nothing), either, maybe)
import           Test.CsjModel.NonEmptySeq
import           Test.CsjModel.Perm
import           Test.CsjModel.StateTypes

{-------------------------------------------------------------------------------
  Stimuli to and reactions of the CSJ governor
-------------------------------------------------------------------------------}

-- | Events the CSJ logic needs to react to
--
-- TODO copy <https://github.com/IntersectMBO/ouroboros-consensus/pull/1492>'s
-- explanation that a @Reengage@ stimulus is unnecessary even when the GSM
-- transitions out of 'Ouroboros.Consensus.Node.GsmState.CaughtUp'
data CsjStimulus p a =
    -- | The peer responded to a ChainSync message
    ChainSyncReply !(ChainSyncReply p a)
  |
    -- | An upstream peer just connected
    --
    -- Note that this should simply not be called when the Genesis State
    -- Machine (GSM) is in the 'Ouroboros.Consensus.Node.GsmState.CaughtUp'
    -- state.
    --
    -- The argument is the payload for the current immutable tip.
    Connect !a
  |
    -- | The peer disconnected, including LoP, GDD, etc
    Disconnect
  |
    -- | The ChainSync client sent the CSJ's latest 'MsgFindIntersect'
    -- to the peer
    Offered
  |
    -- | The peer starved ChainSel
    Starvation
  deriving (Read, Show)

data ChainSyncReply p a =
    MsgAwaitReply
  |
    -- |
    -- 'Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InvalidJumpResponse'
    -- ensures that response from the peer is the same point we offered
    MsgIntersectFound
  |
    MsgIntersectNotFound
  |
    MsgRollBackward !(WithOrigin p)
  |
    -- | After the ChainSync client processes the header
    MsgRollForwardDONE !(WithPayload p a)
  |
    -- | As soon as the ChainSync client receives the header
    --
    -- In particular: before it might block on forecasting the ledger view.
    MsgRollForwardSTART !p
  deriving (Read, Show)

-- | What a particular ChainSync client should do as part of the CSJ logic's
-- reaction to some 'CsjStimulus'
data CsjReaction p a =
    -- | the ChainSync client should run as normal (ie pipelining
    -- @MsgRequestNext@s) until 'Stop' is given
    --
    -- This message is only sent to the Dynamo or an Objector, never to a Jumper.
    Continue
  |
    -- | ChainSync should hereafter run as normal; CSJ will never give it
    -- another command
    --
    -- This message is only sent to the Dynamo or an Objector, never to a Jumper.
    Disengage
  |
    -- | the ChainSync client should send a @MsgFindIntersect@ with only this
    -- point
    --
    -- The point cannot be older than the ImmDB tip, since it's younger than
    -- the peer's 'comm' and the LoE cannot be younger than any 'comm'.
    --
    -- ChainSync client's don't necessarily have to handle all of these (eg
    -- while they're blocked on the ledger view forecast), but should handle
    -- the latest as soon as they're able to.
    --
    -- This message is only sent to a Jumper, never the Dynamo or an Objector.
    MsgFindIntersect !CsjIntersectPurpose !(WithPayload (WithOrigin p) a)
  |
    -- | used to demote the Dynamo to a Jumper upon 'Starvation'
    --
    -- Implies a @drainThePipe@ call and also suppression of an imminent
    -- 'MsgRollForwardDONE' if there is one.
    --
    -- This message is only sent to the Dynamo, never an Objector nor a Jumper.
    Stop
  deriving (Read, Show)

-- | Why this 'MsgFindIntersect' is being sent
data CsjIntersectPurpose =
    -- | To determine which points of a 'JumpRequest' the peer has
    ToBisect
  |
    -- | To reset the mini protocol state so that the peer can definitely send
    -- headers starting from the expected point
    --
    -- If the peer replies with 'MsgIntersectNotFound', they will be
    -- disengaged.
    ToPromote
  deriving (Read, Show)

-- | @'nextMsgFindIntersect' = 'MsgFindIntersect' . 'wpAt' . 'nextPivot'@
nextMsgFindIntersect :: Bisecting p a -> CsjReaction p a
nextMsgFindIntersect = MsgFindIntersect ToBisect . wpAt . nextPivot

{-------------------------------------------------------------------------------
  Promotion and demotion
-------------------------------------------------------------------------------}

-- | A 'MsgFindIntersect' for 'comm'
--
-- This message is always sent when a peer is promoted to the Dynamo or an
-- Objector.
--
-- Minor optimization opportunity: if the Jumper's latest response was
-- @MsgIntersectFound@ then this could be skipped. There are only few cases
-- where this 'promotionMessage' currently must not be skipped.
--
-- - The first peer to 'Connect' will immediately be promoted to Dynamo despite
--   never having sent any messages to its peer.
--
-- - Similarly, if a peer waiting in 'latestJump' is promoted to Dynamo, it has
--   never sent any messages to its peer.
--
-- - If a peer was demoted from Dynamo, the last message it sent to its peer
--   most likely would have been @MsgRequestNext@. But when it was demoted,
--   'truncCandidate' most likely discarded some of its 'candidate' and it also
--   probably had to @drainThePipe@. So it's out of sync with its mini protocol
--   peer.
promotionMessage :: CsjClientState p a -> CsjReaction p a
promotionMessage = MsgFindIntersect ToPromote . comm

-- | Reset 'candidate' to 'comm' when demoting the Dynamo in response to
-- 'Starvation'
truncCandidate :: Ord p => CsjClientState p a -> CsjClientState p a
truncCandidate y =
    CsjClientState {
        anticomm  = anticomm y
      ,
        candidate =
            case wpPoint (comm y) of
                Origin -> Seq.empty
                At p   -> case findPoint (candidate y) p of
                    L.Just i  -> Seq.take (i + 1) (candidate y)
                    L.Nothing ->
                        -- 'candidate' must be an extension of 'comm'
                        error "impossible!"
      ,
        comm      = comm y
      }

{-------------------------------------------------------------------------------
  Updates due to 'MsgRollForwardDONE' and 'MsgRollBackward'
-------------------------------------------------------------------------------}

-- | @MsgRollForward@
--
-- Disengages if it violates 'anticomm'. Extends 'candidate'.
forward ::
     Ord p
  => WithPayload p a
  -> CsjClientState p a
  -> L.Maybe (CsjClientState p a)
forward p y = do
    guard $ wpPoint p `Set.notMember` anticomm y
    pure CsjClientState {
        anticomm  = anticomm y
      ,
        candidate = candidate y Seq.|> p
      ,
        comm      = comm y
      }

-- | @MsgRollBackward@
--
-- Disengages if it violates 'comm'. Truncates 'candidate'.
--
-- NB the ChainSync server sends 'MsgRollBackward' in response to the first
-- @MsgRequestNext@ after having sent 'MsgIntersectFound'. So Jumpers will not
-- receive 'MsgRollBackward' until after the 'Continue' command.
backward ::
     Ord p
  => WithOrigin p
  -> CsjClientState p a
  -> L.Maybe (CsjClientState p a)
backward wp y = do
    guard $ wpPoint (comm y) <= wp
    candidate' <- case wp of
        Origin -> pure Seq.empty
        At p   -> do
            ip <- findPoint (candidate y) p
            pure $ Seq.take (ip + 1) (candidate y)
    pure CsjClientState {
        anticomm  = anticomm y
      ,
        candidate = candidate'
      ,
        comm      = comm y
      }

{-------------------------------------------------------------------------------
  Updates due to 'MsgIntersectFound' and 'MsgIntersectNotFound'
-------------------------------------------------------------------------------}

-- | Update a 'Bisecting' based on the response to its 'nextMsgFindIntersect'.
--
-- Recall that 'newJumpRequest' uses 'comm' and 'anticomm' to constrain
-- 'notYetDetermined' when initializing it. Thus 'bisectionStep' does not even
-- need to check them and so cannot fail.
bisectionStep ::
     Ord p
  => Bool
     -- ^ 'True' for 'MsgIntersectFound', 'False' for 'MsgIntersectNotFound'
  -> CsjClientState p a
  -> Bisecting p a
  -> (CsjClientState p a, Either (Class p) (Bisecting p a))
bisectionStep found y bi =
    (y', eBi')
  where
    nyd = notYetDetermined bi
    wp  = nextPivot bi
    p   = wpPoint wp

    rejected' = if found then rejected bi else Just p

    comm' = if not found then comm y else wpAt wp

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
            (if not found then id else (\c -> c <> implied Seq.|> wp))
          $ candidate y
      ,
        comm      = comm'
      }

    eBi' = case nonEmptySeq nyd' of
        L.Nothing     -> Left $ Class $ wpPoint comm'
        L.Just neNyd' -> Right Bisecting {
            notYetDetermined = neNyd'
          ,
            rejected         = rejected'
          }

{-------------------------------------------------------------------------------
  CSJ reactions to a single stimulus
-------------------------------------------------------------------------------}

-- | Update the state and emit new 'CsjReaction's in response to the arrival of
-- 'CsjStimulus'es.
--
-- It only returns 'Nothing' if the given @pid@ is disengaged or the given
-- 'ChainSyncReply' implies a mini protocol violation.
csjReactions ::
     (Ord p, Ord pid)
  => CsjEnv p
  -> WithOrigin p
     -- ^ the current immutable tip
  -> CsjState pid p a
  -> pid
  -> CsjStimulus p a
  -> L.Maybe (CsjState pid p a, [(pid, CsjReaction p a)])
csjReactions env imm x pid = fmap fixup . \case

    Connect z -> pure $ connect z

    ChainSyncReply reply -> case reply of

        MsgAwaitReply -> onActive (const L.Nothing)
            -- TODO should the Dynamo immediately issue a jump regardless of
            -- 'minJumpSlots'? Otherwise every peer will have to fetch the
            -- recent headers, and there could be several thousand of them.

        MsgIntersectFound ->
                onAlreadySent (bisectionStep True)
            <|>
                onPromotee L.Just
        MsgIntersectNotFound ->
                onAlreadySent (bisectionStep False)
            <|>
                onPromotee (const L.Nothing)

        MsgRollBackward     p -> onActive (backward  p)
        MsgRollForwardDONE wp -> onActive (forward  wp)
        MsgRollForwardSTART p ->
            fmap (flip (,) [])
          $ preDynamo p <|> preObjector p

    Disconnect -> pure disconnect

    Offered -> onNotYetSent

    Starvation -> starvation

  where
    fixup = issueNextJumpIfReady env imm . backfill

    disengage  = forget False
    disconnect = forget True

    forget disconnecting = (
        CsjState {
            dynamo     =
                case dynamo x of
                    Just (Dynamo pid' _clss _y _mbQ) | pid /= pid' -> dynamo x
                    _                                              -> Nothing
          ,
            latestJump = either (Left . Map.delete pid) Right $ latestJump x
          ,
            nonDynamos = Map.delete pid (nonDynamos x)
          ,
            queue      = deletePerm pid (queue x)
          }
        ,
          if disconnecting then [] else [(pid, Disengage)]
        )

    connect z =
        let -- TODO currently, ChainSync clients send the "standard Fibonacci
            -- points" before issuing 'Connect'. That's ultimately harmless,
            -- but does seem wasteful. The immediate concern, however, is that
            -- it means this 'comm' assignment is out of sync with the actual
            -- ChainSync client.
            --
            -- TODO ChainSync client initialization should immediately set the
            -- candidate to the contemporary imm tip and force the LoE to
            -- update accordingly? Is that a harmless race condition?
            y = CsjClientState {
                anticomm  = Set.empty
              ,
                candidate = case imm of
                    Origin -> Seq.empty
                    At p   -> Seq.singleton $ WP p z
              ,
                comm      = WP imm z
              }

            -- the imm tip is not on 'latestJump'
            cornerCase waitings =
                (Left $ Map.insert pid y waitings, Nothing, Nothing)

            (latestJump', mbNonDynamo, mbMsg) = case latestJump x of
                Left waitings -> cornerCase waitings
                Right req     ->
                    case firstJumpRequest imm req of
                        Nothing          -> cornerCase Map.empty
                        Just (Left clss) -> (
                            latestJump x
                          ,
                            Just $ Jumped clss y
                          ,
                            Nothing
                          )
                        Just (Right bi)  -> (
                            latestJump x
                          ,
                            Just $ Jumping y bi NotYetSent
                          ,
                            Just $ nextMsgFindIntersect bi
                          )
        in
        (
            CsjState {
                dynamo     = dynamo x
              ,
                latestJump = latestJump'
              ,
                nonDynamos =
                    maybe id (Map.insert pid) mbNonDynamo (nonDynamos x)
              ,
                -- Prefer older peers so that the peer that satisfies the
                -- Honest Availability Assumption will become Dynamo before any
                -- more recent peers do.
                --
                -- It would also be sound for an implementation to
                -- instead insert at a random position.
                queue      = snocPerm pid (queue x)
              }
          ,
            maybe [] ((:[]) . (,) pid) mbMsg
          )

    -- A Dynamo or Objector has responded to its 'promotionMessage'.
    onPromotee = onActive

    onActive f = onDynamo f <|> onObjector f

    onDynamo f = do
        Dynamo pid' clss y _mbQ <- toLazy $ dynamo x
        guard $ pid' == pid
        pure $ case f y of
            L.Nothing -> disengage
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

    onNotYetSent = do
        Jumping y bi sent <- Map.lookup pid (nonDynamos x)
        case sent of
            AlreadySent _mbNext -> L.Nothing
            NotYetSent          -> pure ()
        pure (
            CsjState {
                dynamo     = dynamo x
              ,
                latestJump = latestJump x
              ,
                nonDynamos =
                    Map.insert
                        pid
                        (Jumping y bi (AlreadySent Nothing))
                        (nonDynamos x)
              ,
                queue      = queue x
              }
          ,
            []
          )

    onAlreadySent f = do
        Jumping y bi sent <- Map.lookup pid (nonDynamos x)
        k <- case sent of
            AlreadySent mbNext ->
                pure $ maybe id (uncurry . newJumpRequest) mbNext
            NotYetSent         -> L.Nothing
        pure $
            let (y', eBi') = k $ f y bi
            in
            case eBi' of
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
                                (Jumping y' bi' NotYetSent)
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
                Map.insert
                    pid
                    (Objector clss y (Just p))
                    (nonDynamos x)
          ,
            queue      = queue x
          }

    starvation = do
        Dynamo pid' clss y _mbQ <- toLazy $ dynamo x
        let shouldDemote =
                pid' == pid
             &&
                -- Don't demote the Dynamo if we'll have to immediately
                -- repromote the same peer.
                any eligible (nonDynamos x)
            eligible = \case
                Jumped{}   -> True
                Jumping{}  -> False
                Objector{} -> True
        pure $
            if not shouldDemote then (x, []) else (
                CsjState {
                    dynamo     = Nothing
                  ,
                    latestJump = latestJump x
                  ,
                    nonDynamos =
                        Map.insert
                            pid
                            (Jumped clss (truncCandidate y))
                            (nonDynamos x)
                  ,
                    queue      = queue x
                  }
              ,
                [(pid, Stop)]
              )

{-------------------------------------------------------------------------------
  Promoting Objectors and the Dynamo
-------------------------------------------------------------------------------}

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
-- oldest 'Class', as long as ties are broken in a way that ensures a peer that
-- satisifes the Honest Availability Assumption will eventually become Dynamo.
-- The 'queue' mechanism achieves that without overreacting to 'Starvation'
-- events (which are inevitable due to the imperfection of the public Internet
-- infrastructure).
--
-- Once this function promotes a Jumper to Dynamo or Objector, that peer must
-- never subsequently be demoted to a Jumper. Doing so risks fetching headers
-- from honest peers more than once, which CSJ should avoid (if this risk could
-- be avoided in some simple way, then the following design decision could be
-- revisited). The one exception is if the Dynamo starves ChainSel. It is
-- excusable to demote that Dynamo since it will only happen at most once to
-- each honest peer until a peer that satisfies the Honest Availability
-- Assumption becomes the Dynamo, which will remain the Dynamo until the end of
-- the sync (or the next networking "hiccup"). It is also noteworthy that the
-- idealized honest Dynamo will not be demoted during tests, and so CSJ /in/
-- /tests/ will actually /never/ refetch a historical header from an honest
-- peer, so that aspect of the CSJ correctness condition can be quite simple.
--
-- The prohibition on demoting Objectors is incompatible with a specification
-- that does both of the following simultaneously.
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
  => (CsjState pid p a, [(pid, CsjReaction p a)])
  -> (CsjState pid p a, [(pid, CsjReaction p a)])
backfill = fillObjectors . backfillDynamo

-- | See 'backfill'
--
-- Promotes the leftmost @pid@ in 'queue' that is either an Objector or a
-- 'Jumped' in a 'Class' that has no Objectors. Otherwise, if /all/ peers are
-- waiting for the first jump offer, it promotes the leftmost of those.
backfillDynamo ::
     (Ord p, Ord pid)
  => (CsjState pid p a, [(pid, CsjReaction p a)])
  -> (CsjState pid p a, [(pid, CsjReaction p a)])
backfillDynamo (x, msgs) =
    L.maybe (x, msgs) promote
  $ case dynamo x of
        Just{}  -> Alt.empty
        Nothing -> getAlt $ foldMap (Alt . try) (queue x)
  where
    try pid = case Map.lookup pid (nonDynamos x) of
        L.Just nonDynamo -> case nonDynamo of
            Jumped clss y       -> do
                guard $ clss `Set.notMember` objectorClasses x
                pure (
                    Dynamo pid clss y Nothing
                  ,
                    Just (promotionMessage y)
                  ,
                    latestJump x
                  )
            Jumping{}           -> Alt.empty
            Objector clss y mbQ ->
                pure (Dynamo pid clss y mbQ, Nothing, latestJump x)
        L.Nothing        ->
            let waitings = either id (\_req -> Map.empty) $ latestJump x
            in
            case Map.lookup pid waitings of
                L.Just y  ->
                    if not $ Map.null $ nonDynamos x then Alt.empty else
                    let clss = Class $ wpPoint $ comm y
                    in
                    pure (Dynamo pid clss y Nothing, Nothing, latestJump x)
                L.Nothing ->
                    -- Each pid in 'queue' must be in either 'nonDynamos' or
                    -- 'latestJump'.
                    error "impossible!"

    promote (Dynamo pid clss y mbQ, mbMsg, latestJump') = (
        CsjState {
            dynamo     = Just $ Dynamo pid clss y mbQ
          ,
            latestJump = latestJump'
          ,
            nonDynamos = Map.delete pid (nonDynamos x)
          ,
            queue      =
                -- This peer just got picked, so send them to the end.
                snocPerm pid $ deletePerm pid $ queue x
          }
      ,
          maybe id (:) ((,) pid <$> mbMsg) msgs
      )

-- | See 'backfill'
--
-- If there is a 'dynamo' and the oldest 'Class' has only 'Jumped's, then
-- promotes the leftmost according to 'queue'.
--
-- A correct implementation could additinoally promote 'Jumped's from /any/
-- 'Class' that contains no 'Objector' nor 'dynamo'.
fillObjectors ::
     (Ord p, Ord pid)
  => (CsjState pid p a, [(pid, CsjReaction p a)])
  -> (CsjState pid p a, [(pid, CsjReaction p a)])
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

data FillAcc pid p a =
    -- | The Dynamo or some Objector already inhabits this 'Class'
    AlreadyOccupied
  |
    -- | A peer with the given position in 'queue' inhabits this class
    Filler !Int !pid !(CsjClientState p a)

-- | 'fillObjectors' breaks ties between two 'Filler's by choosing the lowest
-- rank
fillerRank :: FillAcc pid p a -> Maybe Int
fillerRank = \case
    AlreadyOccupied  -> Nothing
    Filler i _pid _y -> Just i

{-------------------------------------------------------------------------------
  Triggering and constructing jump requests
-------------------------------------------------------------------------------}

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
    --   that's not required. The larger it is, however, the less load on the
    --   honest network.
    --
    -- - ChainSync currently forecasts from the candidate's intersection with
    --   the local selection. This could help prevent the ChainSync pipeline
    --   from starving when the selection is along the Dynamo's chain. It also
    --   makes ChainSync possible for chains that don't satisfy Chain Growth
    --   (as long as they're uncontested). However, it's not necessary (for
    --   chains that satisfy Chain Growth) and we do suspect some correctness
    --   arguments would be simpler if forecasts only originated from the
    --   immutable tip.
    --
    -- Because the jump size might not necessarily be maxed and because
    -- ChainSync currently uses opportunistic forecast ranges, CSJ can let the
    -- Dynamo request another jump before all peers have resolved the current
    -- jump. It would be possible to simply wait for all bisections to finish
    -- before allowing the next jump, but the logic for interrupting them isn't
    -- too complicated, which might reduce the adversary's opportunity to delay
    -- (since 'newJumpRequest' leverages the 'comm' and 'rejected' of the
    -- interrupted jump to trim&truncate the new jump's 'notYetDetermined').
    minJumpSlots  :: Word64
  ,
    realPointSlot :: p -> SlotNo
  }

-- | Whether the Dynamo has received enough headers to issue a new jump
bigEnoughJump :: CsjEnv p -> WithOrigin p -> p -> Maybe p -> Bool
bigEnoughJump env clss p mbQ =
    slotOf p + 1 >= firstSlotAfterJumpWindow
 ||
    case mbQ of
        Nothing -> False
        Just q  -> slotOf q >= firstSlotAfterJumpWindow
  where
    slotOf = unSlotNo . realPointSlot env

    firstSlotAfterJumpWindow = minJumpSlots env + case clss of
        Origin -> 0
        At q   -> 1 + slotOf q

-- | Possibly issue a new jump to the non-Dynamos
--
-- A jump is triggered once all of the following are true.
--
-- - The jump request includes a point that the previous jump request did not.
--
-- - The Dynamo received a header more than 'minPastSlots' past its 'Class'.
--   The 'MsgRollForwardSTART' stimulus lets this conjunct be satisfied even if
--   the ChainSync client is not yet able to forecast the ledger view necessary
--   to validate the header. This logic is careful to exclude such an
--   unvalidated point from the jump request itself, since in the real
--   implementation every point in the jump request needs to be mapped to the
--   corresponding ChainSync client state, which isn't available for an
--   unvalidated header.
issueNextJumpIfReady ::
     (Ord pid, Ord p)
  => CsjEnv p
  -> WithOrigin p
  -> (CsjState pid p a, [(pid, CsjReaction p a)])
  -> (CsjState pid p a, [(pid, CsjReaction p a)])
issueNextJumpIfReady env imm (x, msgs) =
    case dynamo x of

        Nothing -> (x, msgs)

        Just dyn@(Dynamo _dynamoPid dynamoClss dynamoY dynamoMbQ)
          | L.Just neCandidate <- nonEmptySeq (candidate dynamoY)
          , let wp = neLast neCandidate
                p  = wpPoint wp
          , bigEnoughJump env (let Class q = dynamoClss in q) p dynamoMbQ
              -- It's crucial to use the 'Class' here rather than their 'comm',
              -- since---after they're promoted but before they request their
              -- first jump---their 'comm' might be ahead of their 'Class' .
          , dynamoClss /= Class (At p)
              -- Suppress empty jumps.
              --
              -- If the Dynamo could never satisfy this conjunct because its
              -- forecast range is empty it will be disconnected, since the
              -- ChainSync client kills any peer with an empty forecast range.
         -> issueNextJump imm (:) (x, msgs) dyn neCandidate
          | otherwise -> (x, msgs)

-- | Called by 'issueNextJumpIfReady' when there actually is a new jump
issueNextJump ::
     (Ord pid, Ord p)
  => WithOrigin p
  -> ((pid, CsjReaction p a) -> msgs -> msgs)
     -- ^ The parametricity here ensures this function doesn't scrutinize or
     -- alter (eg accidentally drop!) the incoming messages.
  -> (CsjState pid p a, msgs)
  -> Dynamo pid p a
  -> NonEmptySeq (WithPayload p a)
  -> (CsjState pid p a, msgs)
issueNextJump imm cons (x, msgs) dyn neCandidate =
  (
    CsjState {
        -- The Dynamo effectively instantly accepts the entire jump.
        dynamo     =
            Just
          $ Dynamo
                dynamoPid
                dynamoClss
                (trimC CsjClientState {
                    -- 'Set.empty' would suffice here if it weren't
                    -- for counterexamples to the NON-invariant
                    -- documented at 'JumpRequest'.
                    anticomm  = trimAnticomm (At p) $ anticomm dynamoY
                  ,
                    candidate = candidate dynamoY
                  ,
                    comm      = wpAt wp
                  })
                dynamoMbQ
      ,
        latestJump = Right req
      ,
        nonDynamos = nonDynamos'
      ,
        queue      = queue x
      }
  ,
      msgs'
  )
  where
    -- 'issueNextJump' necessarily traverses all peers and must happen
    -- infinitely often, so it's a reasonble place to call 'trimCandidate' on
    -- all peers
    trimC = trimCandidate imm

    Dynamo dynamoPid dynamoPrevClss dynamoY dynamoMbQ = dyn

    dynamoClss = Class $ At p

    -- Note that this jump request includes the imm tip because
    -- 'candidate' does.
    req = JumpRequest dynamoPrevClss neCandidate

    wp = neLast neCandidate
    p  = wpPoint wp

    (nonDynamos', msgs') =
        flip State.runState msgs
      $ liftA2
            Map.union
            (Map.traverseWithKey instructNewJump (nonDynamos x))
      $ case latestJump x of
           Left waiting -> Map.traverseWithKey doneWaiting waiting
           Right{}      -> pure Map.empty

    instructNewJump pid nonDynamo =
        State.state $ instructNewJump2 pid `flip` nonDynamo

    instructNewJump2 pid acc = \case
        Jumped clss y ->
            let (y', eBi') = newJumpRequest req (trimC y) (Left clss)
            in
            case eBi' of
                Left clss' -> (Jumped clss' y', acc)
                Right bi   -> (
                    Jumping y' bi NotYetSent
                  ,
                    (pid, nextMsgFindIntersect bi) `cons` acc
                  )
        Jumping y bi sent -> case sent of
            AlreadySent _mbNext ->
                (Jumping (trimC y) bi (AlreadySent (Just req)), acc)
            NotYetSent          ->
                case newJumpRequest req (trimC y) (Right bi) of
                    (y', Left clss') -> (Jumped clss' y', acc)
                    (y', Right bi')  -> (
                        Jumping y' bi' NotYetSent
                      ,
                        (pid, nextMsgFindIntersect bi') `cons` acc
                      )
        Objector clss y mbQ ->
            -- This 'min' ensures the Objector's class is always a point in the
            -- new jump.
            --
            -- If an Objector's class instead weren't updated after their
            -- promotion, then Jumpers that are serving the same chain as the
            -- Objector might end up with a inequal class after bisecting the
            -- new jump.
            (Objector (min dynamoPrevClss clss) y mbQ, acc)

    doneWaiting pid y = State.state $ doneWaiting2 pid y

    doneWaiting2 pid y acc =
        case firstJumpRequest imm req of
            Nothing           ->
                -- 'firstJumpRequest' is only called on waiting peers in 'Left'
                -- of 'latestJump'. Those peers are only put their by the
                -- 'Connect' event, and when they are put there, their
                -- 'candidate' and 'comm' are set to the current imm tip. The
                -- LoE is therefore that same point, which means the imm tip
                -- cannot have advanced. So the current imm tip must be the
                -- same as the waiting peer's 'comm'. Also, 'req' must include
                -- that same imm tip, since it's the Dynamo's 'candidate',
                -- which reaches back to current imm tip (and perhaps further).
                error "impossible!"
            Just (Left clss') -> (Jumped clss' y, acc)
            Just (Right bi)   -> (
                Jumping y bi NotYetSent
              ,
                (pid, nextMsgFindIntersect bi) `cons` acc
              )

-- | The first jump request for a peer since they joined
--
-- Returns 'Nothing' if and only if the imm tip is not on the given
-- 'JumpRequest'. See 'latestJump' for an explanation of how this corner case
-- could arise.
firstJumpRequest ::
     Ord p
  => WithOrigin p
     -- ^ current immutable tip
  -> JumpRequest p a
  -> Maybe (Either (Class p) (Bisecting p a))
firstJumpRequest imm req =
    case imm of
        Origin ->
            -- If the imm tip is still 'Origin', then 'Origin' is
            -- definitely the anchor of @ps@.
            Just $ Right $ mkBi ps
        At p   -> case neFindPoint ps p of
            L.Just i  -> Just $ case nonEmptySeq $ neDrop (i + 1) ps of
                L.Just neNyd -> Right $ mkBi neNyd
                L.Nothing    ->
                    -- TODO how could this branch be reachable?
                    Left $ Class imm
            L.Nothing ->
                -- the imm tip is not on the jump request
                Nothing
  where
    JumpRequest _dynamoPrevClss ps = req

    mkBi neNyd =
        Bisecting {
            notYetDetermined = neNyd
          ,
            rejected         = Nothing
          }

-- | For Jumpers that have processed the latest reply from their peer
--
-- If they were 'Jumping's, they were otherwise about to either become
-- 'Jumped's or send another offer and remain a 'Jumping'.
--
-- Note that the resulting 'notYetDetermined' (if any) will have been
-- constrained by the incoming 'comm' and 'anticomm'; therefore, a Jumper
-- /cannot/ violate them.
newJumpRequest ::
     Ord p
  => JumpRequest p a
  -> CsjClientState p a
  -> Either (Class p) (Bisecting p a)
  -> (CsjClientState p a, Either (Class p) (Bisecting p a))
newJumpRequest req y eBi =
    case eBi of
        Left clss ->
            if clss == dynamoPrevClss then go else
            -- This 'min' is the same as for Objectors, which makes sense: this
            -- 'Jumped' is merely waiting its turn to be promoted to Objector.
            -- See the relevant case in 'issueNextJump' for more comments.
            (y, Left $ min clss dynamoPrevClss)
        Right bi  ->
            -- If 'rejected' is on the new jump (TODO how?), then this Jumping
            -- doesn't need to change its state at all.
            case rejected bi of
                Nothing -> go
                Just p  ->
                    case neFindPoint ps p of
                        L.Nothing -> goWithRejected p
                        L.Just _  -> (y, eBi)
  where
    JumpRequest dynamoPrevClss ps = req

    go               = (y, eBi')
    goWithRejected p = (integrateRejected p, eBi')

    eBi' = newJumpRequest2 y ps
        -- This result does not depend on the other changes to @y@.

    -- This peer's ongoing bisection is being cancelled in favor of this new
    -- jump request, so integrate 'rejected' into 'anticomm' now.
    integrateRejected p =
        CsjClientState {
            anticomm  = Set.insert p $ anticomm y
          ,
            candidate = candidate y
          ,
            comm      = comm y
          }

-- | Helper for 'newJumpRequest'
--
-- Discards however much of the new 'JumpRequest' the Jumper has already
-- accepted via 'comm'.
newJumpRequest2 ::
     Ord p
  => CsjClientState p a
  -> NonEmptySeq (WithPayload p a)
  -> Either (Class p) (Bisecting p a)
newJumpRequest2 y = \jump ->
    case neBinarySearch (\p -> At (wpPoint p) >= commp) jump of
        LastFalseFirstTrue _i j ->
            if commp == get jump j then go $ neDrop (j + 1) jump else
            Left $ Class $ isect $ neTake j jump
        Uniformly False         ->
            Left $ Class $ isect $ toSeq jump
        Uniformly True          ->
            if commp == prj (neLast jump) then go Seq.empty else
            -- @jump@ and 'candidate' both include a recent immutable tip. So
            -- this is impossible unless 'comm' is 'Origin'.
                assert (Origin == commp)
              $ go $ toSeq jump
  where
    commp = wpPoint $ comm y

    get ps = prj . neIndex ps
    prj    = At . wpPoint

    go nyd = case newJumpRequest3 (anticomm y) nyd of
        Nothing -> Left $ Class commp
        Just bi -> Right bi

    -- find the intersection of @jump@ and 'candidate', when 'comm' is not on
    -- @jump@
    --
    -- The argument is the prefix of @jump@ that is older than 'comm', since
    -- those are the points that might 'comm' might be an extension of.
    isect jumpPrefix =
        case nonEmptySeq jumpPrefix of
            L.Nothing        -> Origin
            L.Just ne ->
                let onCandidate = L.isJust . findPoint (candidate y)
                in
                case neBinarySearch (not . onCandidate . wpPoint) ne of
                    LastFalseFirstTrue i _j -> get ne i
                    Uniformly False         -> Origin
                    Uniformly True          -> prj $ neLast ne

-- | Helper for 'newJumpRequest2'
--
-- Discards however much of the new 'JumpRequest' the Jumper has already
-- rejected via 'anticomm' and then builds the final 'Bisecting' if any jump
-- points remain.
newJumpRequest3 ::
     Ord p
  => Set p
     -- ^ 'anticomm'
  -> Seq (WithPayload p a)
  -> Maybe (Bisecting p a)
newJumpRequest3 anticommps nyd =
    case nonEmptySeq nyd' of
        L.Nothing     ->
            -- The Jumper already knows its exact intersection with this
            -- jump.
            --
            -- It seems this branch can only be reached by a counterexample to
            -- the NON-invariant documented on 'JumpRequest'.
            Nothing
        L.Just neNyd' ->
            Just Bisecting {
                notYetDetermined = neNyd'
              ,
                rejected         =
                    if Seq.length nyd == Seq.length nyd' then Nothing else
                    -- If this peer has already rejected part of this jump
                    -- request, then don't give it a chance to accept the rest
                    -- in one 'JumpFindIntersect': the Dynamo and Jumper are
                    -- not serving the same chain, and so we can't be in the
                    -- steady-state where they're both honest, which is the
                    -- only motivation for the all-in-one @MsgFindIntersect@.
                    Just $ wpPoint $ nyd `Seq.index` Seq.length nyd'
              }
  where
    -- possible optimization: if @nyd@ is smaller than @anticommps@, a
    -- 'Seq.takeWhileL' would be faster, but that should be rare
    nyd' = foldl' snoc nyd anticommps

    snoc acc anticommp = case findPoint acc anticommp of
        L.Nothing -> acc
        L.Just i  -> Seq.take i acc
