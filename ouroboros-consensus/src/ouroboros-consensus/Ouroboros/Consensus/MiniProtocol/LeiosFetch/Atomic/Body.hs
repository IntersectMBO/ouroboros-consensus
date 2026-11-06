{-# LANGUAGE NamedFieldPuns #-}

-- | Pure whole-body primary/hedge ownership. Logical abandonment leaves the
-- original and optional cancel acknowledgment represented until both retire.
-- Rationale and code-to-contract map: @docs/fetch-policy/cancellation-contract.md#2-separate-policy-obligations-from-wire-attempts@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Body
  ( BodyState
  , BodyRole (..)
  , BodyChange (..)
  , BodySnapshot (..)
  , BodyError (..)
  , emptyBodyState
  , ensureBodyAcquisition
  , registerBodyOffers
  , nextBodyRole
  , bodyCandidates
  , admitBodyAttempt
  , expireBodyAttempt
  , rescueBlockedBody
  , fulfillBody
  , finishBodyOriginal
  , finishBodyCancel
  , pendingBodyCancels
  , unwithdrawnBodyAttempts
  , recordBodyWithdrawal
  , recordBodyCancel
  , retireBodyAcquisition
  , removeBodyPeer
  , bodySnapshot
  , bodySnapshots
  , bodyRequestPoint
  , bodyAttemptPayload
  ) where

import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Data.Word (Word32)
import Ouroboros.Consensus.Leios.Types (LeiosEbBounds, LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity

-- | Body-only logical role, independent of wire/session admission records.
data BodyRole
  = -- | Keep receiving after its one trigger deadline.
    BodyPrimary
  | -- | Abandon on its own expiry and rotate eligible alternatives.
    BodyHedge
  deriving (Eq, Show)

-- | One newly charged policy event; duplicate/withdrawn/ACK expiry is NoChange.
data BodyChange = NoBodyChange | BodyPrimaryTriggered | BodyHedgeFailed deriving (Eq, Show)

-- | One acquisition's bounded policy state. The offer queue contains each live
-- peer registration once; the complete wire identity is retained separately.
data Entry peer = Entry
  { entryGeneration :: !AcquisitionGeneration
  -- ^ Exact relevance generation; stale callbacks cannot recreate it.
  , entryBounds :: !(Maybe LeiosEbBounds)
  -- ^ Trusted forecast fallback for offers lacking their own ledger bounds.
  , entryPrimary :: !(Maybe (RequestKey peer))
  -- ^ Persistent wanted primary, until fulfillment or source removal.
  , entryHedge :: !(Maybe (RequestKey peer))
  -- ^ At most one wanted hedge; older canceled copies stay in the attempt map.
  , entryTriggered :: !Bool
  -- ^ Sticky primary trigger; independent of hedge failures.
  , entryFulfilled :: !Bool
  -- ^ A verified whole body is known, including a legacy/storage winner.
  , entryWinner :: !(Maybe (RequestKey peer))
  -- ^ First verified new-version winner, protected through metadata retirement.
  , entryFailures :: !Word32
  -- ^ Saturating cumulative hedge expiries for this EB, across alternative peers.
  , entryOffers :: !(Seq.Seq (LeiosPeerLifetimeId peer))
  -- ^ Round robin of currently live registrations in first-offer order.
  }
  deriving (Eq, Show)

-- | One admitted whole-body original, including any cancellation debt.
data Attempt peer request = Attempt
  { attemptPoint :: !LeiosPoint
  -- ^ EB point of this immutable request.
  , attemptGeneration :: !AcquisitionGeneration
  -- ^ Acquisition generation captured at admission.
  , attemptRole :: !BodyRole
  -- ^ Policy role; promotion retains the request's original timer snapshot.
  , attemptPayload :: !request
  -- ^ Opaque original request context; never rewritten for a replacement.
  , attemptWanted :: !Bool
  -- ^ Logical interest, independent of wire retirement.
  , attemptWithdrawn :: !Bool
  -- ^ Logical withdrawal was published to the client timing ledger once.
  , attemptOriginalDone :: !Bool
  -- ^ Its own typed original terminal has been consumed.
  , attemptCancel :: !(Maybe (RequestKey peer))
  -- ^ Separately admitted cancel exchange, if any.
  , attemptCancelDone :: !Bool
  -- ^ That cancel's own ACK has been consumed.
  }
  deriving (Eq, Show)

-- | Node-owned body-only state. Attempts are bounded by live admitted wire
-- records; fulfilled entries are retired with acquisition metadata. No attempt
-- history or closure backoff/cooldown is retained here.
data BodyState peer request = BodyState
  { entries :: !(Map.Map LeiosPoint (Entry peer))
  -- ^ Live acquisition policy, with independent primary/hedge slots per EB.
  , attempts :: !(Map.Map (RequestKey peer) (Attempt peer request))
  -- ^ Wanted originals plus unresolved canceled/fulfilled originals.
  , cancelTargets :: !(Map.Map (RequestKey peer) (RequestKey peer))
  -- ^ Original context for each independently outstanding cancel ACK.
  }
  deriving (Eq, Show)

-- | Read-only policy evidence, including debt kept after logical release.
data BodySnapshot peer = BodySnapshot
  { bodyGeneration :: !AcquisitionGeneration
  -- ^ Current relevance generation.
  , bodyBounds :: !(Maybe LeiosEbBounds)
  -- ^ Trusted forecast fallback, separate from peer-announced timer bytes.
  , bodyPrimary :: !(Maybe (RequestKey peer))
  -- ^ Current wanted persistent original.
  , bodyHedge :: !(Maybe (RequestKey peer))
  -- ^ Current wanted hedge original.
  , bodyTriggered :: !Bool
  -- ^ Whether hedging has been enabled by primary expiry.
  , bodyFulfilled :: !Bool
  -- ^ Whether a complete verified body has won.
  , bodyFailures :: !Word32
  -- ^ Cumulative hedge failures only; no progress-based decay.
  , bodyWanted :: !Int
  -- ^ Wanted body copies; at most two for this point and generation.
  , bodyDraining :: !Int
  -- ^ Logically abandoned originals still awaiting wire retirement.
  }
  deriving (Eq, Show)

-- | Rejected local ownership transitions leave the input state unchanged.
data BodyError
  = UnknownBody
  | StaleBody
  | BodySlotOccupied
  | BodyPeerBusy
  | UnknownBodyAttempt
  | DuplicateBodyKey
  deriving (Eq, Show)

-- | No live acquisitions or wire-body originals.
emptyBodyState :: BodyState peer request
emptyBodyState = BodyState Map.empty Map.empty Map.empty

-- | Bind metadata once to the live generation; fill only an absent forecast.
ensureBodyAcquisition ::
  LeiosPoint ->
  AcquisitionGeneration ->
  Maybe LeiosEbBounds ->
  BodyState peer request ->
  BodyState peer request
ensureBodyAcquisition point generation bounds state = state{entries = Map.alter ensure point (entries state)}
 where
  ensure Nothing = Just (Entry generation bounds Nothing Nothing False False Nothing 0 Seq.empty)
  ensure old@(Just e)
    | entryGeneration e /= generation = old
    | Just _ <- entryBounds e = old
    | otherwise = Just e{entryBounds = bounds}

-- | Append each registration once, without preempting the active hedge.
registerBodyOffers ::
  Ord peer =>
  LeiosPoint -> [LeiosPeerLifetimeId peer] -> BodyState peer request -> BodyState peer request
registerBodyOffers point offered state = state{entries = Map.adjust append point (entries state)}
 where
  append e = e{entryOffers = foldl' add (entryOffers e) offered}
  add queue peer
    | peer `elem` queue = queue
    | otherwise = queue Seq.|> peer

-- | Which logical slot can be filled next, without allocating wire identity.
nextBodyRole :: LeiosPoint -> BodyState peer request -> Maybe BodyRole
nextBodyRole point state = do
  e <- Map.lookup point (entries state)
  if entryFulfilled e
    then Nothing
    else case (entryPrimary e, entryTriggered e, entryHedge e) of
      (Nothing, _, _) -> Just BodyPrimary
      (_, True, Nothing) -> Just BodyHedge
      _ -> Nothing

-- | Offer order with every same-EB unresolved original/ACK pair excluded.
bodyCandidates :: Ord peer => LeiosPoint -> BodyState peer request -> [LeiosPeerLifetimeId peer]
bodyCandidates point state = case Map.lookup point (entries state) of
  Nothing -> []
  Just e -> [peer | peer <- toList (entryOffers e), not (busy peer)]
 where
  busy peer = any (\(key, a) -> attemptPoint a == point && lifetime key == peer) (Map.toList (attempts state))

-- | Publish one admitted original into its free logical slot.
admitBodyAttempt ::
  Ord peer =>
  LeiosPoint ->
  AcquisitionGeneration ->
  BodyRole ->
  RequestKey peer ->
  request ->
  BodyState peer request ->
  Either BodyError (BodyState peer request)
admitBodyAttempt point generation role key payload state = do
  e <- maybe (Left UnknownBody) Right (Map.lookup point (entries state))
  if entryGeneration e /= generation then Left StaleBody else pure ()
  if nextBodyRole point state /= Just role then Left BodySlotOccupied else pure ()
  if Map.member key (attempts state) then Left DuplicateBodyKey else pure ()
  if lifetime key `notElem` bodyCandidates point state then Left BodyPeerBusy else pure ()
  let e' = case role of BodyPrimary -> e{entryPrimary = Just key}; BodyHedge -> e{entryHedge = Just key}
  pure
    state
      { entries = Map.insert point e' (entries state)
      , attempts =
          Map.insert
            key
            (Attempt point generation role payload True False False Nothing False)
            (attempts state)
      }

-- | Charge a still-wanted head once; primary persistence and hedge rotation differ.
expireBodyAttempt ::
  Ord peer =>
  RequestKey peer -> Bool -> BodyState peer request -> (BodyChange, BodyState peer request)
expireBodyAttempt key charge state
  | not charge = (NoBodyChange, state)
  | otherwise = case Map.lookup key (attempts state) of
      Just a
        | attemptWanted a
        , Just e <- Map.lookup (attemptPoint a) (entries state)
        , entryGeneration e == attemptGeneration a
        , not (entryFulfilled e) ->
            case attemptRole a of
              BodyPrimary | not (entryTriggered e) -> (BodyPrimaryTriggered, put e{entryTriggered = True})
              BodyHedge
                | entryHedge e == Just key ->
                    let queue = Seq.filter (/= lifetime key) (entryOffers e) Seq.|> lifetime key
                        e' = e{entryHedge = Nothing, entryFailures = increment (entryFailures e), entryOffers = queue}
                     in ( BodyHedgeFailed
                        , (put e'){attempts = Map.adjust (\old -> old{attemptWanted = False}) key (attempts state)}
                        )
              _ -> (NoBodyChange, state)
       where
        put e = state{entries = Map.insert (attemptPoint a) e (entries state)}
      _ -> (NoBodyChange, state)

-- | A verified full winner, possibly from an already abandoned original. A
-- source key excludes its own response from loser cancellation. Storage/legacy
-- fulfillment supplies Nothing. Earlier recorded failures remain recorded.
fulfillBody ::
  Ord peer =>
  LeiosPoint ->
  AcquisitionGeneration ->
  Maybe (RequestKey peer) ->
  BodyState peer request ->
  BodyState peer request
fulfillBody point generation winner state = case Map.lookup point (entries state) of
  Just e
    | entryGeneration e == generation, entryFulfilled e -> state
    | entryGeneration e == generation ->
        state
          { entries =
              Map.insert
                point
                e{entryFulfilled = True, entryWinner = winner, entryPrimary = Nothing, entryHedge = Nothing}
                (entries state)
          , attempts =
              Map.mapWithKey
                ( \key a ->
                    if attemptPoint a == point && attemptGeneration a == generation && Just key /= winner
                      then a{attemptWanted = False}
                      else a
                )
                (attempts state)
          }
  _ -> state

-- | Consume only this original terminal; an outstanding ACK still pins the peer.
finishBodyOriginal ::
  Ord peer => RequestKey peer -> BodyState peer request -> BodyState peer request
finishBodyOriginal key =
  cleanup
    . ( \state ->
          state
            { attempts =
                Map.adjust (\a -> a{attemptOriginalDone = True, attemptWanted = False}) key (attempts state)
            }
      )

-- | Consume only this cancel ACK; its original can still be draining.
finishBodyCancel :: Ord peer => RequestKey peer -> BodyState peer request -> BodyState peer request
finishBodyCancel key state = cleanup $ case Map.lookup key (cancelTargets state) of
  Nothing -> state
  Just target ->
    state
      { attempts = Map.adjust (\a -> a{attemptCancelDone = True}) target (attempts state)
      , cancelTargets = Map.delete key (cancelTargets state)
      }

-- | Abandoned unresolved originals whose cancel has not yet been admitted.
pendingBodyCancels :: BodyState peer request -> [(RequestKey peer, request)]
pendingBodyCancels state =
  [ (key, attemptPayload a)
  | (key, a) <- Map.toList (attempts state)
  , not (attemptWanted a)
  , not (attemptOriginalDone a)
  , isNothing (attemptCancel a)
  ]

-- | Bind the separately admitted cancel key without releasing original debt.
recordBodyCancel ::
  Ord peer =>
  RequestKey peer ->
  RequestKey peer ->
  BodyState peer request ->
  Either BodyError (BodyState peer request)
recordBodyCancel original cancel state = case Map.lookup original (attempts state) of
  Just a
    | not (attemptOriginalDone a)
    , isNothing (attemptCancel a) ->
        Right
          state
            { attempts =
                Map.insert original a{attemptWanted = False, attemptCancel = Just cancel} (attempts state)
            , cancelTargets = Map.insert cancel original (cancelTargets state)
            }
  _ -> Left UnknownBodyAttempt

-- | Drop policy metadata while retaining abandoned originals for cancellation/drain.
retireBodyAcquisition ::
  Ord peer => LeiosPoint -> AcquisitionGeneration -> BodyState peer request -> BodyState peer request
retireBodyAcquisition point generation state = case Map.lookup point (entries state) of
  Just e
    | entryGeneration e == generation ->
        (fulfillBody point generation (entryWinner e) state){entries = Map.delete point (entries state)}
  _ -> state

-- | Remove exact registration debt. Promote a surviving wanted hedge to the
-- persistent role without renewing its original timing or forgetting backoff.
removeBodyPeer ::
  Ord peer => LeiosPeerLifetimeId peer -> BodyState peer request -> BodyState peer request
removeBodyPeer peer state =
  cleanup
    state
      { attempts = promoted
      , cancelTargets =
          Map.filterWithKey
            (\key original -> lifetime key /= peer && lifetime original /= peer)
            (cancelTargets state)
      , entries = Map.map update (entries state)
      }
 where
  surviving = Map.filterWithKey (\key _ -> lifetime key /= peer) (attempts state)
  promoted = Map.mapWithKey promote surviving
  promote key a = case Map.lookup (attemptPoint a) (entries state) of
    Just e | gone (entryPrimary e), entryHedge e == Just key -> a{attemptRole = BodyPrimary}
    _ -> a
  gone = maybe False ((== peer) . lifetime)
  update e = e{entryPrimary = primary, entryHedge = hedge, entryOffers = Seq.filter (/= peer) (entryOffers e)}
   where
    primary
      | gone (entryPrimary e) = if gone (entryHedge e) then Nothing else entryHedge e
      | otherwise = entryPrimary e
    hedge
      | gone (entryPrimary e) || gone (entryHedge e) = Nothing
      | otherwise = entryHedge e

-- | Inspect one current generation without exposing mutable wire context.
bodySnapshot :: LeiosPoint -> BodyState peer request -> Maybe (BodySnapshot peer)
bodySnapshot point state = do
  e <- Map.lookup point (entries state)
  let current =
        [ a
        | a <- Map.elems (attempts state)
        , attemptPoint a == point
        , attemptGeneration a == entryGeneration e
        ]
  pure
    ( BodySnapshot
        (entryGeneration e)
        (entryBounds e)
        (entryPrimary e)
        (entryHedge e)
        (entryTriggered e)
        (entryFulfilled e)
        (entryFailures e)
        (length (filter attemptWanted current))
        (length (filter (not . attemptWanted) current))
    )

-- | Inspect current per-EB logical limits, not an attempt history.
bodySnapshots :: BodyState peer request -> Map.Map LeiosPoint (BodySnapshot peer)
bodySnapshots state = Map.mapMaybeWithKey (\point _ -> bodySnapshot point state) (entries state)

-- | Recover the original point/generation for a pinned completion callback.
bodyRequestPoint ::
  Ord peer => RequestKey peer -> BodyState peer request -> Maybe (LeiosPoint, AcquisitionGeneration)
bodyRequestPoint key state = (\a -> (attemptPoint a, attemptGeneration a)) <$> Map.lookup key (attempts state)

-- | Recover immutable request context even after logical abandonment.
bodyAttemptPayload :: Ord peer => RequestKey peer -> BodyState peer request -> Maybe request
bodyAttemptPayload key state = attemptPayload <$> Map.lookup key (attempts state)

cleanup :: BodyState peer request -> BodyState peer request
cleanup state =
  state
    { attempts =
        Map.filter
          (\a -> not (attemptOriginalDone a && (isNothing (attemptCancel a) || attemptCancelDone a)))
          (attempts state)
    }

lifetime :: RequestKey peer -> LeiosPeerLifetimeId peer
lifetime (RequestKey peer _ _) = peer

increment :: Word32 -> Word32
increment n = if n == maxBound then n else n + 1

-- | Abandoned originals whose logical timing disposition still needs publishing.
unwithdrawnBodyAttempts :: BodyState peer request -> [RequestKey peer]
unwithdrawnBodyAttempts state =
  [ key
  | (key, a) <- Map.toList (attempts state)
  , not (attemptWanted a)
  , not (attemptWithdrawn a)
  , not (attemptOriginalDone a)
  ]

-- | Remember one successful publication to the independent timing ledger.
recordBodyWithdrawal ::
  Ord peer => RequestKey peer -> BodyState peer request -> BodyState peer request
recordBodyWithdrawal key state = state{attempts = Map.adjust (\a -> a{attemptWithdrawn = True}) key (attempts state)}

-- | A fired data/ACK head can strand wanted bodies queued behind it. Enable
-- their primary rescue, or rotate an unserved hedge, without pretending that
-- their own service timer started or adding a hedge-failure penalty.
rescueBlockedBody :: Ord peer => RequestKey peer -> BodyState peer request -> BodyState peer request
rescueBlockedBody key state = case Map.lookup key (attempts state) of
  Just a
    | attemptWanted a
    , Just e <- Map.lookup (attemptPoint a) (entries state)
    , entryGeneration e == attemptGeneration a
    , not (entryFulfilled e) ->
        case attemptRole a of
          BodyPrimary -> put e{entryTriggered = True}
          BodyHedge ->
            ( put
                e
                  { entryHedge = Nothing
                  , entryOffers = Seq.filter (/= lifetime key) (entryOffers e) Seq.|> lifetime key
                  }
            )
              { attempts = Map.adjust (\old -> old{attemptWanted = False}) key (attempts state)
              }
   where
    put e = state{entries = Map.insert (attemptPoint a) e (entries state)}
  _ -> state
