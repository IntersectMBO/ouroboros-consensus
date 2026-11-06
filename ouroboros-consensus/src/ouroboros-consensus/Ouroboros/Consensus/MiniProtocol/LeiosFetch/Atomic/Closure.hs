{-# LANGUAGE NamedFieldPuns #-}

-- | Logical closure rescue, separate from immutable wire requests and their
-- reservations. Rationale: @docs/fetch-policy/cancellation-contract.md#8-checkpoint-deadlines-and-policy-reactions@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Closure
  ( ClosureState
  , ClosureRole (..)
  , ClosureChange (..)
  , ClosureError (..)
  , ClosureSnapshot (..)
  , emptyClosureState
  , ensureClosureAcquisition
  , refreshClosure
  , admitClosureAttempt
  , expireClosureAttempt
  , rescueBlockedClosure
  , closureRescuePlans
  , closureRescueCandidates
  , closureTailPlans
  , abandonClosureAttempt
  , noteClosureService
  , finishClosureOriginal
  , finishClosureCancel
  , unwithdrawnClosureAttempts
  , recordClosureWithdrawal
  , pendingClosureCancels
  , recordClosureCancel
  , retireClosureAcquisition
  , removeClosurePeer
  , closureRequestPoint
  , closureFailures
  , closureSnapshots
  ) where

import Control.Monad (unless, when)
import Control.Monad.Class.MonadTime.SI (Time)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity

-- | An ordinary owner, or the single additional batch rescuing that owner.
data ClosureRole peer = ClosurePrimary | ClosureRescue !(RequestKey peer)
  deriving (Eq, Show)

-- | Once-only service failures. FIFO blockage and fulfillment are not failures.
data ClosureChange = NoClosureChange | ClosurePrimaryStalled | ClosureRescueFailed
  deriving (Eq, Show)

-- | Local policy transition failure, before publishing an admitted request.
data ClosureError = UnknownClosure | StaleClosure | InvalidClosureAttempt | RescueSlotOccupied
  deriving (Eq, Show)

-- | Per-EB eligibility and the FIFO of stalled ordinary obligations.
data Entry peer = Entry
  { generation :: !AcquisitionGeneration
  -- ^ Exact relevance generation; callbacks cannot resurrect a retired EB.
  , missing :: !IntSet.IntSet
  -- ^ Shared handoff's current missing set, excluding verified pending writes.
  , queue :: !(Seq.Seq (RequestKey peer))
  -- ^ Distinct stalled primaries, rotated after each rescue opportunity.
  , rescue :: !(Maybe (RequestKey peer))
  -- ^ At most one wanted additional batch, independently of draining copies.
  }
  deriving (Eq, Show)

-- | One immutable original plus its independent cancellation lifecycle.
data Attempt peer request = Attempt
  { descriptor :: !BatchDescriptor
  -- ^ Original whole-TX membership, sizes and acquisition generation.
  , payload :: !request
  -- ^ Client-owned original context used to issue a whole-batch cancel.
  , expected :: !(IntMap.IntMap ExpectedTx)
  -- ^ Indexed immutable membership; a TX callback performs one logarithmic lookup.
  , attemptMembers :: !IntSet.IntSet
  -- ^ Cached immutable offsets, shared by possession and copy-limit checks.
  , role :: !(ClosureRole peer)
  -- ^ Policy role, without changing the underlying wire identity.
  , wanted :: !Bool
  -- ^ Logical interest; clearing this releases no wire reservation.
  , withdrawn :: !Bool
  -- ^ Timing withdrawal has been published once to the client.
  , originalDone :: !Bool
  -- ^ Original terminal response has actually been consumed.
  , cancel :: !(Maybe (RequestKey peer))
  -- ^ Separately admitted cancellation exchange, if any.
  , cancelDone :: !Bool
  -- ^ That exchange's ACK has actually been consumed.
  , stalled :: !Bool
  -- ^ Primary is eligible for rescue, including blocked queued work.
  , charged :: !Bool
  -- ^ Its own ordinary service expiry has already charged a failure.
  , service :: !(Map.Map (LeiosPeerLifetimeId peer) (Word64, Time))
  -- ^ Unique useful rescue bytes and first service time, for productive handoff.
  }
  deriving (Eq, Show)

-- | Node-owned logical policy. Heavy attempt metadata is retained only for
-- wire debt, an active rescue, or a still-actionable productive tail handoff.
data ClosureState peer request = ClosureState
  { entries :: !(Map.Map LeiosPoint (Entry peer))
  -- ^ Live acquisitions, bounded by shared acquisition admission.
  , attempts :: !(Map.Map (RequestKey peer) (Attempt peer request))
  -- ^ Original descriptors pinned independently of current ordinary ownership.
  , cancels :: !(Map.Map (RequestKey peer) (RequestKey peer))
  -- ^ ACK identities mapped to their original, never to a replacement.
  , failures :: !(Map.Map (LeiosPeerLifetimeId peer) Word32)
  -- ^ Saturating cumulative closure service failures for live registrations.
  }
  deriving (Eq, Show)

-- | Bounded evidence for logical-copy and physical-drain assertions.
data ClosureSnapshot peer = ClosureSnapshot
  { closureGeneration :: !AcquisitionGeneration
  -- ^ Current relevance generation.
  , closureRescue :: !(Maybe (RequestKey peer))
  -- ^ Single wanted extra batch, if any.
  , closureStalled :: ![RequestKey peer]
  -- ^ FIFO order of still-needed stalled ordinary obligations.
  , closureCopies :: !(IntMap.IntMap Int)
  -- ^ Wanted new-policy copies per TX, never more than two.
  , closureDraining :: !Int
  -- ^ Unwanted originals whose original or issued ACK has not retired.
  }
  deriving (Eq, Show)

emptyClosureState :: ClosureState peer request
emptyClosureState = ClosureState Map.empty Map.empty Map.empty Map.empty

-- | Install a generation once, without replacing a live generation's state.
ensureClosureAcquisition ::
  LeiosPoint -> AcquisitionGeneration -> ClosureState peer request -> ClosureState peer request
ensureClosureAcquisition point gen s =
  s
    { entries =
        Map.insertWith (\_ old -> old) point (Entry gen IntSet.empty Seq.empty Nothing) (entries s)
    }

members :: Attempt peer request -> IntSet.IntSet
members = attemptMembers

currentEntry :: Attempt peer request -> ClosureState peer request -> Maybe (Entry peer)
currentEntry a s = do
  e <- Map.lookup (batchPoint (descriptor a)) (entries s)
  if generation e == batchGeneration (descriptor a) then Just e else Nothing

needed :: Attempt peer request -> ClosureState peer request -> IntSet.IntSet
needed a s = maybe IntSet.empty (IntSet.intersection (members a) . missing) (currentEntry a s)

-- | Reconcile verified possession before expiry or allocation. A partly needed
-- original stays immutable; a wholly redundant one becomes cancelable.
refreshClosure ::
  Ord peer =>
  LeiosPoint ->
  AcquisitionGeneration ->
  IntSet.IntSet ->
  ClosureState peer request ->
  ClosureState peer request
refreshClosure point gen remaining s = case Map.lookup point (entries s) of
  Just e | generation e == gen -> cleanup $ refreshed{attempts = Map.map update (attempts refreshed)}
   where
    refreshed = s{entries = Map.insert point e{missing = remaining} (entries s)}
    update a
      | batchPoint (descriptor a) == point
      , batchGeneration (descriptor a) == gen
      , IntSet.null (needed a refreshed) =
          a{wanted = False}
    update a = a
  _ -> s

-- | Add a successfully reserved original. Rescue membership must be a still
-- needed subset of its primary and cannot create a third wanted TX copy.
admitClosureAttempt ::
  Ord peer =>
  ClosureRole peer ->
  RequestKey peer ->
  request ->
  BatchDescriptor ->
  ClosureState peer request ->
  Either ClosureError (ClosureState peer request)
admitClosureAttempt kind key request batch s = do
  e <- maybe (Left UnknownClosure) Right (Map.lookup (batchPoint batch) (entries s))
  unless (generation e == batchGeneration batch) (Left StaleClosure)
  let offsets = IntSet.fromList (map expectedOffset (NE.toList (batchExpected batch)))
      copies = copyCounts (batchPoint batch) e s
  when
    ( Map.member key (attempts s)
        || not (offsets `IntSet.isSubsetOf` missing e)
        || any (\off -> IntMap.findWithDefault 0 off copies >= 2) (IntSet.toList offsets)
    )
    (Left InvalidClosureAttempt)
  e' <- case kind of
    ClosurePrimary -> pure e
    ClosureRescue parent -> do
      unless (isNothing (rescue e)) (Left RescueSlotOccupied)
      a <- maybe (Left InvalidClosureAttempt) Right (Map.lookup parent (attempts s))
      unless
        ( role a == ClosurePrimary
            && wanted a
            && stalled a
            && offsets `IntSet.isSubsetOf` needed a s
            && lifetime key /= lifetime parent
            && lifetime key `elem` closureRescueCandidates parent [lifetime key] s
        )
        (Left InvalidClosureAttempt)
      pure e{rescue = Just key}
  pure
    s
      { entries = Map.insert (batchPoint batch) e' (entries s)
      , attempts =
          Map.insert
            key
            ( Attempt
                batch
                request
                (IntMap.fromDistinctAscList (zip [0 ..] (NE.toList (batchExpected batch))))
                offsets
                kind
                True
                False
                False
                Nothing
                False
                False
                False
                Map.empty
            )
            (attempts s)
      }

-- | Retain an expired primary; rotate an expired rescue. Only its own wanted
-- data-head expiry charges service failure, once per immutable request.
expireClosureAttempt ::
  Ord peer =>
  RequestKey peer -> Bool -> ClosureState peer request -> (ClosureChange, ClosureState peer request)
expireClosureAttempt key charge s = case Map.lookup key (attempts s) of
  Just a
    | wanted a
    , charge
    , not (charged a)
    , not (IntSet.null (needed a s)) ->
        let failed =
              s
                { failures = Map.alter (Just . increment . maybe 0 id) (lifetime key) (failures s)
                , attempts = Map.insert key a{charged = True} (attempts s)
                }
         in case role a of
              ClosurePrimary -> (ClosurePrimaryStalled, markStalled key failed)
              ClosureRescue _ -> (ClosureRescueFailed, abandonClosureAttempt key failed)
  _ -> (NoClosureChange, s)

-- | A blocked data/ACK head makes queued work rescuable without starting its
-- timer or charging a service failure. An unserved rescue releases its slot.
rescueBlockedClosure ::
  Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
rescueBlockedClosure key s = case Map.lookup key (attempts s) of
  Just a | wanted a -> case role a of
    ClosurePrimary -> markStalled key s
    ClosureRescue _ -> abandonClosureAttempt key s
  _ -> s

markStalled :: Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
markStalled key s = case Map.lookup key (attempts s) of
  Just a
    | Just e <- currentEntry a s ->
        s
          { attempts = Map.insert key a{stalled = True} (attempts s)
          , entries =
              Map.insert
                (batchPoint (descriptor a))
                e{queue = if key `elem` queue e then queue e else queue e Seq.|> key}
                (entries s)
          }
  _ -> s

-- | FIFO obligations, skipping no-alternative obligations at the caller. At
-- most one admission from each EB's list can succeed before its slot is full.
closureRescuePlans ::
  Ord peer =>
  ClosureState peer request -> [(RequestKey peer, LeiosPoint, AcquisitionGeneration, IntSet.IntSet)]
closureRescuePlans s =
  [ (key, point, generation e, eligible a)
  | (point, e) <- Map.toAscList (entries s)
  , isNothing (rescue e)
  , key <- toList (queue e)
  , Just a <- [Map.lookup key (attempts s)]
  , wanted a
  , not (IntSet.null (eligible a))
  ]
 where
  eligible a =
    let counts = maybe IntMap.empty (\e -> copyCounts (batchPoint (descriptor a)) e s) (currentEntry a s)
     in IntSet.filter (\off -> IntMap.findWithDefault 0 off counts < 2) (needed a s)

-- | Failure count, then arrived offer order, then stable peer identity. An
-- unresolved same-EB original/ACK excludes reuse of that registration.
closureRescueCandidates ::
  Ord peer =>
  RequestKey peer ->
  [LeiosPeerLifetimeId peer] ->
  ClosureState peer request ->
  [LeiosPeerLifetimeId peer]
closureRescueCandidates parent offered s = case Map.lookup parent (attempts s) of
  Nothing -> []
  Just a ->
    map snd $
      sortOn
        (\(order, p) -> (closureFailures p s, order, p))
        [(order, p) | (order, p) <- zip [0 :: Int ..] offered, p /= lifetime parent, not (busy p a)]
 where
  busy p original =
    any
      ( \(key, a) ->
          lifetime key == p
            && batchPoint (descriptor a) == batchPoint (descriptor original)
            && batchGeneration (descriptor a) == batchGeneration (descriptor original)
            && not (retired a)
      )
      (Map.toList (attempts s))

-- | Productive rescue suppliers can receive still-needed later batches from
-- that exact original pipeline. Rank actual unique bytes, then first service.
closureTailPlans ::
  Ord peer =>
  ClosureState peer request -> [(RequestKey peer, LeiosPeerLifetimeId peer, IntSet.IntSet)]
closureTailPlans s =
  [ (tailKey, p, needed tailAttempt s)
  | (headKey, headAttempt) <- Map.toAscList (attempts s)
  , role headAttempt == ClosurePrimary
  , stalled headAttempt
  , IntSet.null (needed headAttempt s)
  , (p, _) <-
      sortOn
        (\(peer, (bytes, at)) -> (negate (toInteger bytes), at, peer))
        (Map.toList (service headAttempt))
  , (tailKey, tailAttempt) <- tails headKey headAttempt s
  ]

tails ::
  Ord peer =>
  RequestKey peer ->
  Attempt peer request ->
  ClosureState peer request ->
  [(RequestKey peer, Attempt peer request)]
tails key a s =
  [ (k, b)
  | (k, b) <- Map.toAscList (attempts s)
  , samePipeline key k
  , k > key
  , role b == ClosurePrimary
  , wanted b
  , batchPoint (descriptor b) == batchPoint (descriptor a)
  , batchGeneration (descriptor b) == batchGeneration (descriptor a)
  , not (IntSet.null (needed b s))
  ]

-- | Abandon a whole original. Its descriptor, unfinished-head timer and wire
-- reservations are retained; only ordinary scheduling ownership may move.
abandonClosureAttempt ::
  Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
abandonClosureAttempt key s = cleanup s{attempts = Map.adjust (\a -> a{wanted = False}) key (attempts s)}

-- | Credit only the caller's uniquely useful verified TX acceptance. Duplicate,
-- reclaimed and initially held data must never call this function.
noteClosureService ::
  Ord peer =>
  RequestKey peer -> Word32 -> Time -> ClosureState peer request -> ClosureState peer request
noteClosureService key index at s = case Map.lookup key (attempts s) of
  Just a
    | ClosureRescue parent <- role a
    , Just tx <- IntMap.lookup (fromIntegral index) (expected a) ->
        s
          { attempts =
              Map.adjust
                ( \original ->
                    original
                      { service =
                          Map.insertWith
                            (\(bytes, t) (old, first) -> (old + bytes, min t first))
                            (lifetime key)
                            (fromIntegral (expectedSize tx), at)
                            (service original)
                      }
                )
                parent
                (attempts s)
          }
  _ -> s

-- | Original and cancel terminal observations retire independently.
finishClosureOriginal ::
  Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
finishClosureOriginal key s = cleanup s{attempts = Map.adjust (\a -> a{originalDone = True, wanted = False}) key (attempts s)}

-- | Settle only the original named by this immutable cancel identity.
finishClosureCancel ::
  Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
finishClosureCancel key s = cleanup $ case Map.lookup key (cancels s) of
  Nothing -> s
  Just original ->
    s
      { attempts = Map.adjust (\a -> a{cancelDone = True}) original (attempts s)
      , cancels = Map.delete key (cancels s)
      }

unwithdrawnClosureAttempts :: ClosureState peer request -> [RequestKey peer]
unwithdrawnClosureAttempts s =
  [key | (key, a) <- Map.toList (attempts s), not (wanted a), not (withdrawn a), not (originalDone a)]

recordClosureWithdrawal ::
  Ord peer => RequestKey peer -> ClosureState peer request -> ClosureState peer request
recordClosureWithdrawal key s = s{attempts = Map.adjust (\a -> a{withdrawn = True}) key (attempts s)}

pendingClosureCancels :: ClosureState peer request -> [(RequestKey peer, request)]
pendingClosureCancels s =
  [ (key, payload a)
  | (key, a) <- Map.toList (attempts s)
  , not (wanted a)
  , not (originalDone a)
  , isNothing (cancel a)
  ]

recordClosureCancel ::
  Ord peer =>
  RequestKey peer ->
  RequestKey peer ->
  ClosureState peer request ->
  Either ClosureError (ClosureState peer request)
recordClosureCancel original ack s = case Map.lookup original (attempts s) of
  Just a
    | not (originalDone a)
    , isNothing (cancel a) ->
        Right
          s
            { attempts = Map.insert original a{wanted = False, cancel = Just ack} (attempts s)
            , cancels = Map.insert ack original (cancels s)
            }
  _ -> Left InvalidClosureAttempt

retireClosureAcquisition ::
  Ord peer =>
  LeiosPoint -> AcquisitionGeneration -> ClosureState peer request -> ClosureState peer request
retireClosureAcquisition point gen s = case Map.lookup point (entries s) of
  Just e
    | generation e == gen ->
        cleanup
          s
            { entries = Map.delete point (entries s)
            , attempts =
                Map.map
                  ( \a ->
                      if batchPoint (descriptor a) == point && batchGeneration (descriptor a) == gen
                        then a{wanted = False}
                        else a
                  )
                  (attempts s)
            }
  _ -> s

removeClosurePeer ::
  Ord peer => LeiosPeerLifetimeId peer -> ClosureState peer request -> ClosureState peer request
removeClosurePeer p s =
  cleanup
    s
      { attempts =
          Map.map
            (\a -> a{service = Map.delete p (service a)})
            (Map.filterWithKey (\key _ -> lifetime key /= p) (attempts s))
      , cancels =
          Map.filterWithKey (\key original -> lifetime key /= p && lifetime original /= p) (cancels s)
      , failures = Map.delete p (failures s)
      }

closureRequestPoint ::
  Ord peer =>
  RequestKey peer -> ClosureState peer request -> Maybe (LeiosPoint, AcquisitionGeneration)
closureRequestPoint key s =
  (\a -> (batchPoint (descriptor a), batchGeneration (descriptor a))) <$> Map.lookup key (attempts s)

closureFailures :: Ord peer => LeiosPeerLifetimeId peer -> ClosureState peer request -> Word32
closureFailures p = Map.findWithDefault 0 p . failures

closureSnapshots :: ClosureState peer request -> Map.Map LeiosPoint (ClosureSnapshot peer)
closureSnapshots s =
  Map.mapWithKey
    ( \point e ->
        ClosureSnapshot
          (generation e)
          (rescue e)
          (toList (queue e))
          (copyCounts point e s)
          ( length
              [ ()
              | a <- Map.elems (attempts s)
              , batchPoint (descriptor a) == point
              , batchGeneration (descriptor a) == generation e
              , not (wanted a)
              , not (retired a)
              ]
          )
    )
    (entries s)

copyCounts :: LeiosPoint -> Entry peer -> ClosureState peer request -> IntMap.IntMap Int
copyCounts point e s =
  IntMap.fromListWith
    (+)
    [ (off, 1)
    | a <- Map.elems (attempts s)
    , wanted a
    , batchPoint (descriptor a) == point
    , batchGeneration (descriptor a) == generation e
    , off <- IntSet.toList (members a `IntSet.intersection` missing e)
    ]

retired :: Attempt peer request -> Bool
retired a = originalDone a && (isNothing (cancel a) || cancelDone a)

cleanup :: Ord peer => ClosureState peer request -> ClosureState peer request
cleanup s = s{entries = Map.map update (entries s), attempts = Map.filterWithKey retain (attempts s)}
 where
  live key = maybe False (\a -> wanted a && not (IntSet.null (needed a s))) (Map.lookup key (attempts s))
  update e = e{rescue = rescue e >>= (\key -> if live key then Just key else Nothing), queue = rotated e}
  rotated e = case rescue e >>= (`Map.lookup` attempts s) of
    Just a
      | not (wanted a)
      , ClosureRescue parent <- role a ->
          Seq.filter (\key -> key /= parent && live key) (queue e)
            Seq.>< (if live parent then Seq.singleton parent else Seq.empty)
    _ -> Seq.filter live (queue e)
  retain key a =
    not (retired a)
      || any (\b -> role b == ClosureRescue key && not (retired b)) (Map.elems (attempts s))
      || ( role a == ClosurePrimary
             && not (Map.null (service a))
             && IntSet.null (needed a s)
             && not (null (tails key a s))
         )

lifetime :: RequestKey peer -> LeiosPeerLifetimeId peer
lifetime (RequestKey p _ _) = p

samePipeline :: Eq peer => RequestKey peer -> RequestKey peer -> Bool
samePipeline (RequestKey a s _) (RequestKey b t _) = a == b && s == t

increment :: Word32 -> Word32
increment n = if n == maxBound then n else n + 1
