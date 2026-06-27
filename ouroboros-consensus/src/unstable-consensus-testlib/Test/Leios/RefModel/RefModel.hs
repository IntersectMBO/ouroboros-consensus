module RefModel (module RefModel) where

import           EbHashMap (EbHash (..), Election (..), electionSlot, Slot (..))
import           EbHashMap (EbHashMap)
import qualified EbHashMap as EM
import           Data.Foldable (foldl', toList)
import           Data.List (maximumBy, minimumBy, sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Ord (comparing)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe, isJust)
import           Data.Semigroup (Any (..))
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.These (These (..))
import           Data.Word (Word16, Word64)

newtype ByteCount  = ByteCount  Word64 deriving (Eq, Num, Ord, Show)
newtype Peer       = Peer       Word64 deriving (Eq, Ord, Show)
newtype HeaderHash = HeaderHash Word64 deriving (Eq, Ord, Show)
newtype TxHash     = TxHash     Word64 deriving (Eq, Ord, Show)
newtype JobId      = JobId      Word64 deriving (Eq, Ord, Show)
newtype Time       = Time       Word64 deriving (Eq, Ord, Show)

data Class = StakeSampled | PeerSharingSampled  -- BEH-PeerClass
  deriving (Eq, Ord, Show)

data Phase = Active | WindingDown               -- §2 LstPeerPresent
  deriving (Eq, Ord, Show)

data EbAnn = EbAnn                               -- §Conventions announcement
  { annEbHash :: EbHash
  , annBodySize :: ByteCount
  , annClosureSize :: ByteCount
  }
  deriving (Eq, Ord, Show)

data RbHeader = RbHeader                         -- §Conventions · §3 LevRollForward
  { rbHeaderHash :: HeaderHash
  , rbElection :: Election
  , rbAnnounce :: Maybe EbAnn
  , rbHasLeiosCert :: Bool
  , rbValid :: Bool
  }
  deriving (Eq, Ord, Show)

election :: RbHeader -> Election                 -- §Conventions election(header)
election = rbElection

data TxRef = TxRef { txRefHash :: TxHash, txRefSize :: ByteCount }  -- §Conventions tx (hash + size)
  deriving (Eq, Ord, Show)

data Body = Body                                 -- §Conventions closure / txseq
  { bodyEbHash :: EbHash
  , bodyTxlist :: [TxRef]
  , bodyActualSize :: ByteCount
  }
  deriving (Eq, Show)

data Tx = Tx { txHash :: TxHash, txSize :: ByteCount }
  deriving (Eq, Ord, Show)

data AnnouncementTriple = AnnouncementTriple     -- §3 LevRollForward / LevCertValidated (announced-EB identity)
  { atElection :: Election
  , atHeaderHash :: HeaderHash
  , atEbHash :: EbHash
  }
  deriving (Eq, Ord, Show)

data AnnState = AnnOne RbHeader | AnnTwo RbHeader RbHeader   -- §2 LstFirstAnnouncements
  deriving (Eq, Show)

data AnnSeen = SeenOne RbHeader | SeenTwo RbHeader  -- §2 LstPeerFirstAnnouncements
  deriving (Eq, Show)

seenFirst :: AnnSeen -> RbHeader
seenFirst (SeenOne h)  = h
seenFirst (SeenTwo h1) = h1

annEbHashOf :: RbHeader -> Maybe EbHash
annEbHashOf = fmap annEbHash . rbAnnounce

data OfferLevel = OfferBody | OfferBodyAndClosure  -- §2 LstPeerOfferings / OfferLevel
  deriving (Eq, Ord, Show)

newtype LeiosNotifySide = LeiosNotifySide OfferLevel  -- §2 LstPeerOfferings / LeiosNotifySide
  deriving (Eq, Ord, Show)

instance Semigroup LeiosNotifySide where           -- §2 LstPeerOfferings
  (<>) = max

data ChainSyncSide = ChainSyncSide HeaderHash      -- §2 LstPeerOfferings / ChainSyncSide
  deriving (Eq, Show)

data Job = Job (NonEmpty TxHash) ByteCount               -- §2 Job (ordered txs + byte size)
  deriving (Eq, Ord, Show)

jobTxs :: Job -> NonEmpty TxHash
jobTxs (Job txs _) = txs

jobByteSize :: Job -> ByteCount
jobByteSize (Job _ n) = n

newtype OutstandingDiskWritesLessOne = OutstandingDiskWritesLessOne Word16  -- §2 AwaitingTxs: one less than the closure's outstanding DiskWrites
  deriving (Eq, Ord, Show)

data WantState                                    -- §2 LstWanted / WantState (per-EB; keyed by EbHash in the EbHashMap)
  = AwaitingBody ByteCount ByteCount
  | AwaitingTxs (These (NEMap JobId Job) OutstandingDiskWritesLessOne)
  deriving (Eq, Show)

instance Semigroup WantState where                -- §2 LstWanted: a shared EB keeps its existing fetch progress
  a <> _ = a

mkTxsState :: Map JobId Job -> Word16 -> Maybe (These (NEMap JobId Job) OutstandingDiskWritesLessOne)  -- §2 AwaitingTxs payload; Nothing ⇒ closure fully fetched and persisted
mkTxsState jobs writes = case (NEMap.nonEmptyMap jobs, writes) of
  (Nothing, 0) -> Nothing
  (Just j,  0) -> Just (This j)
  (Nothing, n) -> Just (That (OutstandingDiskWritesLessOne (n - 1)))
  (Just j,  n) -> Just (These j (OutstandingDiskWritesLessOne (n - 1)))

txsFetch :: These (NEMap JobId Job) OutstandingDiskWritesLessOne -> Map JobId Job  -- §2 AwaitingTxs jobs still awaiting a response
txsFetch (This j)    = NEMap.toMap j
txsFetch (That _)    = Map.empty
txsFetch (These j _) = NEMap.toMap j

txsWrites :: These (NEMap JobId Job) OutstandingDiskWritesLessOne -> Word16  -- §2 AwaitingTxs outstanding DiskWrites
txsWrites (This _)                                   = 0
txsWrites (That (OutstandingDiskWritesLessOne n))    = n + 1
txsWrites (These _ (OutstandingDiskWritesLessOne n)) = n + 1

decTxsState :: These (NEMap JobId Job) OutstandingDiskWritesLessOne -> Maybe (These (NEMap JobId Job) OutstandingDiskWritesLessOne)  -- §3 LevDiskDone: one DiskWrite finished
decTxsState t = case txsWrites t of
  0 -> error "decTxsState: a LevDiskDone arrived with no outstanding write"
  n -> mkTxsState (txsFetch t) (n - 1)

setTxs :: EbHash -> Map JobId Job -> Word16 -> St -> St  -- §2 LstWanted AwaitingTxs (callers always pass ≥ 1 outstanding write)
setTxs eh jobs writes st = case mkTxsState jobs writes of
  Just t  -> updateWant eh (\_ -> Just (AwaitingTxs t)) st
  Nothing -> error "setTxs: no outstanding writes (completion is handled only by decWrite)"

data Req                                           -- §2 LstPeerInflight / Req
  = ReqBody EbHash ByteCount ByteCount             -- bodySize and closureSize, for validation
  | ReqJob EbHash JobId Job                        -- the job itself, for validation
  deriving (Eq, Ord, Show)

data PeerInfo = PeerInfo { peerClass :: Class, peerPhase :: Phase }  -- §2 LstPeerPresent
  deriving (Eq, Show)

data Notification                                  -- §2 LstPeerNotifyQueue
  = NotifyAnnouncement RbHeader
  | NotifyEquivProof (Maybe RbHeader) RbHeader
  | NotifyBlockOffer Election EbHash               -- MsgLeiosBlockOffer
  | NotifyBlockTxsOffer Election EbHash            -- MsgLeiosBlockTxsOffer
  deriving (Eq, Ord, Show)

data St = St
  { stFirstAnnouncements :: Map Election AnnState                        -- §2 LstFirstAnnouncements
  , stPeerFirstAnnouncements :: Map Peer (Map Election AnnSeen)          -- §2 LstPeerFirstAnnouncements
  , stPeerOfferings :: Map Peer (EbHashMap (Maybe LeiosNotifySide, Any) (Maybe ChainSyncSide)) -- §2 LstPeerOfferings
  , stPeerOfferGates :: Map Peer (EbHashMap () ())                      -- §2 LstPeerOfferGates
  , stPeerNotifyQueue :: Map Peer (Set Notification, Int)                -- §2 LstPeerNotifyQueue
  , stWanted :: EbHashMap WantState ()                                  -- §2 LstWanted
  , stVolatileBody :: EbHashMap () ()                                   -- §2 LstVolatileBody
  , stVolatileClosure :: EbHashMap () ()                                -- §2 LstVolatileClosure (INVARIANT: subset of stVolatileBody)
  , stCertified :: Map Election (HeaderHash, EbHash)                     -- §2 LstCertified
  , stPeerInflight :: Map Peer (Seq Req)                                 -- §2 LstPeerInflight
  , stPeerPresent :: Map Peer PeerInfo                                   -- §2 LstPeerPresent
  }
  deriving (Eq, Show)

emptySt :: St                                      -- BEH-Startup
emptySt = St Map.empty Map.empty Map.empty Map.empty Map.empty EM.empty EM.empty EM.empty Map.empty Map.empty Map.empty

data Env = Env
  { envStakeMaxActiveEbs :: Int                    -- §2 stakeMaxActiveEbs
  , envPeerSharingMaxActiveEbs :: Int              -- §2 peerSharingMaxActiveEbs
  , envPeerSharingClosureByteLimit :: ByteCount          -- §2 peerSharingClosureByteLimit
  , envJobSize :: ByteCount                              -- §2 jobSize
  , envRequestTimeout :: Word64                    -- §2 requestTimeout
  , envNotifyMaxCapacity :: Int                    -- §2 notifyMaxCapacity
  , envNotifyStaleHorizon :: Word64                -- §2 notifyStaleHorizon (slots; ≈ 10 min)
  , envImmutableTip :: Slot                         -- ChainDB-owned, harness-updated
  }
  deriving (Eq, Show)

maxActiveEbs :: Env -> Class -> Int                -- §3 active-EB cap
maxActiveEbs env StakeSampled       = envStakeMaxActiveEbs env
maxActiveEbs env PeerSharingSampled = envPeerSharingMaxActiveEbs env

belowTip :: Env -> Election -> Bool                -- BEH-ImmTipAdvance
belowTip env el = electionSlot el < envImmutableTip env

data DbKey = DbBody EbHash | DbClosureTx EbHash TxHash  -- §2 dbQueryPresent keys
  deriving (Eq, Ord, Show)

data LeiosDb m = LeiosDb                            -- §2 disk store interface
  { dbQueryPresent :: Set DbKey -> m (Set DbKey)    -- BEH-Completion / BEH-FetchServe
  , dbReadBody :: EbHash -> m (Maybe Body)          -- BEH-Completion
  , dbReadClosureTxs :: EbHash -> [TxHash] -> m [Tx]  -- BEH-FetchServe
  }

data TxCache m = TxCache                            -- §7 TxCache hooks
  { txCacheNoteAnnouncement :: EbHash -> m ()       -- BEH-Wanting
  , txCacheOnBody :: EbHash -> Set TxHash -> m [Tx]  -- BEH-ChunkJobs
  , txCacheOnAcquire :: [Tx] -> m ()                -- BEH-Responses
  }

data Mempool m = Mempool                            -- §2 Reads of un-owned state
  { mempoolQueryPresent :: Set TxHash -> m [Tx] }    -- BEH-ChunkJobs

data Ifaces m = Ifaces { ifDb :: LeiosDb m, ifTxc :: TxCache m, ifMem :: Mempool m }

data WireMsg                                        -- §2 Wire messages
  = MsgLeiosNotificationRequestNext
  | MsgLeiosBlockAnnouncement RbHeader
  | MsgLeiosBlockEquivocationProof (Maybe RbHeader) RbHeader
  | MsgLeiosBlockOffer EbHash
  | MsgLeiosBlockTxsOffer EbHash
  | MsgLeiosBlockRequest EbHash
  | MsgLeiosBlock EbHash Body
  | MsgLeiosBlockTxsRequest EbHash (NonEmpty TxHash)
  | MsgLeiosBlockTxs EbHash [Tx]
  deriving (Eq, Show)

data Stimulus                                       -- §2 Stimuli
  = LevWiredMsg Peer WireMsg
  | LevRollForward Peer RbHeader (Maybe AnnouncementTriple)  -- BEH-Offers / BEH-Wanting
  | LevCertValidated AnnouncementTriple ByteCount ByteCount  -- BEH-Wanting / BEH-FetchPriority (bodySize, closureSize)
  | LevPeerAdd Peer Class                           -- BEH-PeerChurn
  | LevPeerWindDown Peer                            -- BEH-PeerChurn
  | LevPeerRemove Peer                              -- BEH-PeerChurn
  | LevTimer Time Peer Req                          -- BEH-Timeout
  | LevDiskDone DiskWrite                           -- §3 LevDiskDone (only writes signal completion)
  | LevImmTipAdvanced                               -- BEH-ImmTipAdvance (slot read from envImmutableTip)
  | LevGarbageCollect Slot                          -- BEH-ImmTipAdvance
  | LevSelfIssued RbHeader Body                     -- BEH-SelfIssued
  | LevNotifyDequeue Peer                           -- BEH-NotifyServe
  deriving (Eq, Show)

data DiskWrite                                      -- §2 disk store interface (writes; each yields one LevDiskDone)
  = WriteBody Body
  | WriteClosure EbHash [Tx]
  deriving (Eq, Show)

data DiskOp                                         -- §2 disk store interface (scheduled actions)
  = Write DiskWrite
  | GarbageCollect Slot
  | Promote Slot
  deriving (Eq, Show)

data Offence                                        -- §3 disconnect reasons (carried by Disconnect)
  = NotAnAnnouncement       -- a LeiosNotify announcement that announces no EB
  | InvalidHeader           -- header failed validation
  | AnnouncementBound       -- re-announced, or exceeded the per-peer two-first-announcements bound
  | BogusEquivocationProof  -- did not prove a genuine equivocation
  | UnannouncedOffer        -- offered an EB the peer never first-announced
  | RedundantOffer          -- an offer that raised no EB's LeiosNotify level
  | CertConflict            -- cert for a HeaderHash conflicting with our validated cert
  | BodyMismatch            -- body disagrees with the request (hash / size / closure size)
  | TxsMismatch             -- closure txs disagree with the requested job (content or order)
  | UnsolicitedResponse     -- a response matching no front in-flight request
  | RequestedAbsentData     -- requested body/txs we do not hold
  | RequestTimeout          -- an in-flight request went overdue
  | ExcessNotifyCredits     -- extended more notify credits than notifyMaxCapacity allows
  | StaleNotification       -- a LeiosNotify message whose slot is > notifyStaleHorizon below the immutable tip
  deriving (Eq, Ord, Show)

data Effect                                         -- §2 Actions
  = Send Peer WireMsg
  | Disconnect Peer Offence
  | SubmitDisk DiskOp
  | SetTimer Peer Req Time
  | NotifyVotingAndChainSel Election EbHash         -- BEH-Completion (persisted closure → voting + ChainSel)
  | NotifyEnqueue Peer                              -- BEH-NotifyServe
  deriving (Eq, Show)

classOf :: St -> Peer -> Maybe (Class, Phase)
classOf st peer = (\pi_ -> (peerClass pi_, peerPhase pi_)) <$> Map.lookup peer (stPeerPresent st)

activePeers :: St -> [Peer]  -- NEEDS-TO-BE-INCREMENTAL: maintained set of Active peers
activePeers st = [ peer | (peer, pi_) <- Map.toList (stPeerPresent st), peerPhase pi_ == Active ]

inflightOf :: St -> Peer -> Seq Req
inflightOf st peer = fromMaybe Seq.empty (Map.lookup peer (stPeerInflight st))

inflightHas :: St -> Peer -> Req -> Bool  -- NEEDS-TO-BE-INCREMENTAL: per-peer requested-Req set
inflightHas st peer r = r `elem` inflightOf st peer

activeEbs :: St -> Peer -> Set EbHash             -- §2 activeEbs · NEEDS-TO-BE-INCREMENTAL: per-peer active-EB set
activeEbs st peer = Set.fromList (map reqEb (toList (inflightOf st peer)))

reqEb :: Req -> EbHash
reqEb (ReqBody eh _ _) = eh
reqEb (ReqJob eh _ _)  = eh

jobInflightPeers :: St -> EbHash -> JobId -> Int  -- §2 jobInflightPeers · NEEDS-TO-BE-INCREMENTAL: (EbHash,JobId)↦in-flight-peer-count index
jobInflightPeers st eh j =
  length [ () | (_, sq) <- Map.toList (stPeerInflight st)
              , ReqJob eh' j' _ <- toList sq
              , eh' == eh, j' == j ]

peerSharingInFlightBytes :: St -> Peer -> EbHash -> ByteCount  -- §2 peerSharingInFlightBytes · NEEDS-TO-BE-INCREMENTAL: per-(peer,EbHash) in-flight byte total
peerSharingInFlightBytes st peer eh =
  sum [ jobByteSize job
      | ReqJob eh' _ job <- toList (inflightOf st peer), eh' == eh ]

jobBytes :: St -> EbHash -> JobId -> ByteCount
jobBytes st eh j =
  case wantStateOf st eh of
    Just (AwaitingTxs t) -> maybe 0 jobByteSize (Map.lookup j (txsFetch t))
    _ -> 0

wantStateOf :: St -> EbHash -> Maybe WantState  -- §2 LstWanted
wantStateOf st eh = (\(EM.RefCounts _ _ ws) -> ws) <$> EM.lookupEb eh (stWanted st)

wantsEh :: St -> EbHash -> Bool  -- §2 LstWanted
wantsEh st eh = isJust (EM.lookupEb eh (stWanted st))

wantedEbs :: St -> [EbHash]  -- §2 LstWanted · NEEDS-TO-BE-INCREMENTAL: maintained wanted-EB set
wantedEbs st = let EM.EbHashMap ebs _ = stWanted st in Map.keys (Map.filter (\(EM.RefCounts act _ _) -> 0 < act) ebs)

electionsNaming :: EbHash -> St -> [Election]  -- §2 LstWanted reverse EbHash↦Elections · NEEDS-TO-BE-INCREMENTAL: maintained reverse index
electionsNaming eh st =
  let EM.EbHashMap _ els = stWanted st
  in [ el | (el, EM.Refs _ act _) <- Map.toList els, act == eh ]

firstAnnouncedEb :: St -> Peer -> Election -> Maybe EbHash  -- §2 LstPeerFirstAnnouncements
firstAnnouncedEb st peer el = do
  perPeer <- Map.lookup peer (stPeerFirstAnnouncements st)
  seen    <- Map.lookup el perPeer
  annEbHashOf (seenFirst seen)

electionsFirstAnnouncing :: St -> Peer -> EbHash -> [Election]  -- §2 LstPeerFirstAnnouncements reverse · NEEDS-TO-BE-INCREMENTAL: per-peer reverse EbHash↦Elections index
electionsFirstAnnouncing st peer eh =
  [ el
  | (el, seen) <- Map.toList (fromMaybe Map.empty (Map.lookup peer (stPeerFirstAnnouncements st)))
  , annEbHashOf (seenFirst seen) == Just eh ]

youngestAnnouncedSlot :: St -> Peer -> EbHash -> Maybe Slot  -- BEH-NotifyServe staleness
youngestAnnouncedSlot st peer eh = case map electionSlot (electionsFirstAnnouncing st peer eh) of
  [] -> Nothing
  ss -> Just (maximum ss)

offerersBody :: St -> EbHash -> Set Peer          -- §2 offerersBody · BEH-BodyFetch
offerersBody st eh = offerersAtLeast st eh OfferBody

offerersClosure :: St -> EbHash -> Set Peer       -- §2 offerersClosure · BEH-ClosureFetch
offerersClosure st eh = offerersAtLeast st eh OfferBodyAndClosure

offerersAtLeast :: St -> EbHash -> OfferLevel -> Set Peer  -- NEEDS-TO-BE-INCREMENTAL: reverse EbHash↦offering-peers index
offerersAtLeast st eh need = Set.fromList
  [ peer
  | (peer, m) <- Map.toList (stPeerOfferings st)
  , effectiveOffer m eh >= Just need ]

effectiveOffer :: EbHashMap (Maybe LeiosNotifySide, Any) (Maybe ChainSyncSide) -> EbHash -> Maybe OfferLevel  -- §2 LstPeerOfferings effective offer
effectiveOffer m eh = case EM.lookupEb eh m of
  Just (EM.RefCounts a _ (notify, Any chain)) | a > 0 -> max (unwrap <$> notify) (if chain then Just OfferBodyAndClosure else Nothing)
  _                                                   -> Nothing
  where
    unwrap (LeiosNotifySide lvl) = lvl

offeredWanted :: St -> Peer -> [EbHash]  -- NEEDS-TO-BE-INCREMENTAL: per-peer offered∩wanted set
offeredWanted st peer =
  [ eh
  | eh <- wantedEbs st
  , peer `Set.member` offerersBody st eh || peer `Set.member` offerersClosure st eh ]

fetchPriorityOrder :: St -> [EbHash] -> [EbHash]  -- BEH-FetchPriority · NEEDS-TO-BE-INCREMENTAL: maintained priority order, not a per-decision re-sort
fetchPriorityOrder st ebs = sortOn (priorityKey st) ebs

newtype NotificationPriority = NotificationPriority Word64
  deriving (Eq, Num, Ord, Show)

priorityKey :: St -> EbHash -> (NotificationPriority, Slot)
priorityKey st eh =
  case electionsNaming eh st of
    []  -> (2, Slot 0)
    els -> case [ electionSlot el | el <- els, isCertifiedEb st el eh ] of
             (s : _) -> (0, s)
             []      -> (1, invertSlot (maximum (map electionSlot els)))

isCertifiedEb :: St -> Election -> EbHash -> Bool
isCertifiedEb st el eh = case Map.lookup el (stCertified st) of
  Just (_, ceb) -> ceb == eh
  Nothing       -> False

invertSlot :: Slot -> Slot
invertSlot (Slot s) = Slot (maxBound - s)

decide :: Env -> St -> Peer -> [Req]               -- §3 Decision · BEH-BodyFetch · BEH-ClosureFetch · BEH-FetchPriority
decide env st peer =
  case classOf st peer of
    Just (cls, Active) -> concatMap (decideForEb env st peer cls) (admittedEbs env st peer cls)
    _                  -> []

admittedEbs :: Env -> St -> Peer -> Class -> [EbHash]  -- §3 active-EB cap · BEH-FetchPriority
admittedEbs env st peer cls =
  let already = activeEbs st peer
      cap = maxActiveEbs env cls
      grow _ [] = []
      grow seen (eh : ebs)
        | eh `Set.member` already   = eh : grow seen ebs
        | Set.size seen < cap       = eh : grow (Set.insert eh seen) ebs
        | otherwise                 = grow seen ebs
  in grow already (fetchPriorityOrder st (offeredWanted st peer))

decideForEb :: Env -> St -> Peer -> Class -> EbHash -> [Req]  -- §3 Body · Stake closure · PeerShare closure
decideForEb env st peer cls eh =
  case wantStateOf st eh of
    Just (AwaitingBody bs cs) | peer `Set.member` offerersBody st eh ->
      [ ReqBody eh bs cs | not (inflightHas st peer (ReqBody eh bs cs)) ]
    Just (AwaitingTxs t) | peer `Set.member` offerersClosure st eh ->
      case cls of
        StakeSampled ->
          [ ReqJob eh j job
          | (j, job) <- Map.toList (txsFetch t)
          , not (inflightHas st peer (ReqJob eh j job)) ]
        PeerSharingSampled -> peerShareJobs env st peer eh (txsFetch t)
    _ -> []

peerShareJobs :: Env -> St -> Peer -> EbHash -> Map JobId Job -> [Req]  -- §3 PeerShare closure · BEH-ClosureFetch
peerShareJobs env st peer eh jobs =
  let budget = envPeerSharingClosureByteLimit env - peerSharingInFlightBytes st peer eh
      candidates = sortOn (\(j, _) -> (jobInflightPeers st eh j, frontSkewKey j))
                     [ (j, job) | (j, job) <- Map.toList jobs
                                , not (inflightHas st peer (ReqJob eh j job)) ]
  in takeWhileBudget budget [ (jobByteSize job, ReqJob eh j job) | (j, job) <- candidates ]

frontSkewKey :: JobId -> Word64                    -- §3 Job ordering (frontSkew)
frontSkewKey (JobId j) = j

takeWhileBudget :: ByteCount -> [(ByteCount, Req)] -> [Req]
takeWhileBudget _ [] = []
takeWhileBudget budget ((b, r) : rest)
  | b <= budget = r : takeWhileBudget (budget - b) rest
  | otherwise   = []

chunk :: Env -> [TxRef] -> Map JobId Job            -- BEH-ChunkJobs · §2 Job (≈ jobSize batches)
chunk env trs =
  Map.fromList (zip (map JobId [0 ..]) (map mkJob (batchBySize (envJobSize env) trs)))
  where mkJob batch = Job (NE.fromList (map txRefHash batch)) (sum (map txRefSize batch))

batchBySize :: ByteCount -> [TxRef] -> [[TxRef]]          -- §2 jobSize
batchBySize _ [] = []
batchBySize limit (x : xs) = go [x] (txRefSize x) xs
  where
    go acc _ [] = [reverse acc]
    go acc sz (t : ts)
      | sz + txRefSize t > limit && not (null acc) = reverse acc : go [t] (txRefSize t) ts
      | otherwise                                  = go (t : acc) (sz + txRefSize t) ts

hashes :: [Tx] -> Set TxHash
hashes = Set.fromList . map txHash

-----

step :: Monad m => Ifaces m -> Env -> Time -> Stimulus -> St -> m (St, [Effect])  -- §3
step ifs env now stim st = case stim of
  LevWiredMsg peer msg     -> stepWired ifs env now peer msg st
  LevRollForward peer h pe -> hRollForward ifs env now peer h pe st
  LevCertValidated at bs cs -> hCertValidated ifs env now at bs cs st
  LevPeerAdd peer cls      -> hPeerAdd peer cls st
  LevPeerWindDown peer     -> hPeerWindDown env now peer st
  LevPeerRemove peer       -> hPeerRemove env now peer st
  LevTimer _ peer req      -> hTimer peer req st
  LevDiskDone w            -> hDiskDone ifs env now w st
  LevImmTipAdvanced        -> hImmTipAdvanced env st
  LevGarbageCollect s      -> hGarbageCollect s st
  LevSelfIssued h body     -> hSelfIssued ifs env now h body st
  LevNotifyDequeue peer    -> hNotifyDequeue env peer st

stepWired :: Monad m => Ifaces m -> Env -> Time -> Peer -> WireMsg -> St -> m (St, [Effect])
stepWired ifs env now peer msg st
  | Just s <- notifyMsgSlot st peer msg, notifyStale env s = pure (st, [Disconnect peer StaleNotification])
  | otherwise = case msg of
      MsgLeiosNotificationRequestNext       -> hRequestNext env peer st
      MsgLeiosBlockAnnouncement h           -> hAnnouncement ifs env now peer h st
      MsgLeiosBlockEquivocationProof m1 h2  -> hEquivProof env peer m1 h2 st
      MsgLeiosBlockOffer eh                 -> hOffer env now peer eh (LeiosNotifySide OfferBody) st
      MsgLeiosBlockTxsOffer eh              -> hOffer env now peer eh (LeiosNotifySide OfferBodyAndClosure) st
      MsgLeiosBlockRequest eh               -> hServeBody ifs env now peer eh st
      MsgLeiosBlock eh body                 -> hBlock ifs env now peer eh body st
      MsgLeiosBlockTxsRequest eh txs        -> hServeTxs ifs env now peer eh txs st
      MsgLeiosBlockTxs eh txs               -> hBlockTxs ifs env now peer eh txs st

hAnnouncement :: Monad m => Ifaces m -> Env -> Time -> Peer -> RbHeader -> St -> m (St, [Effect])  -- BEH-Wanting · §3 LevBlockAnnouncement
hAnnouncement ifs env now peer h st = case rbAnnounce h of
  Nothing  -> pure (st, [Disconnect peer NotAnAnnouncement])
  Just ann
    | not (rbValid h) -> pure (st, [Disconnect peer InvalidHeader])
    | otherwise ->
        case advancePeerAnn peer (rbElection h) h st of
          Nothing  -> pure (st, [Disconnect peer AnnouncementBound])
          Just st1 -> do
            txCacheNoteAnnouncement (ifTxc ifs) (annEbHash ann)
            centralAnnounce ifs env now h (copyAnnIntoOfferings peer (rbElection h) st1)

hEquivProof :: Monad m => Env -> Peer -> Maybe RbHeader -> RbHeader -> St -> m (St, [Effect])  -- BEH-Wanting · §3 LevBlockEquivocationProof
hEquivProof env peer mh1 h2 st =
  case maybe (recordedFirst st peer (rbElection h2)) Just mh1 of
    Nothing -> pure (st, [Disconnect peer BogusEquivocationProof])
    Just h1
      | not (genuineEquiv h1 h2) -> pure (st, [Disconnect peer BogusEquivocationProof])
      | otherwise -> case advancePeerToTwo peer (rbElection h2) h1 st of
          Nothing  -> pure (st, [Disconnect peer BogusEquivocationProof])
          Just st1 -> let (st2, fx) = centralEquiv env (rbElection h2) h1 h2 st1
                      in pure (st2, fx)

recordedFirst :: St -> Peer -> Election -> Maybe RbHeader  -- §2 LstPeerFirstAnnouncements
recordedFirst st peer el = do
  perPeer <- Map.lookup peer (stPeerFirstAnnouncements st)
  seenFirst <$> Map.lookup el perPeer

genuineEquiv :: RbHeader -> RbHeader -> Bool        -- §3 LevBlockEquivocationProof validation
genuineEquiv h1 h2 =
     rbValid h1 && rbValid h2
  && isJust (rbAnnounce h1) && isJust (rbAnnounce h2)
  && rbElection h1 == rbElection h2
  && rbHeaderHash h1 /= rbHeaderHash h2

advancePeerToTwo :: Peer -> Election -> RbHeader -> St -> Maybe St  -- §3 LstPeerFirstAnnouncements -> Two
advancePeerToTwo peer el h1 st =
  let perPeer = fromMaybe Map.empty (Map.lookup peer (stPeerFirstAnnouncements st))
      putTwo  = st { stPeerFirstAnnouncements =
                       Map.insert peer (Map.insert el (SeenTwo h1) perPeer) (stPeerFirstAnnouncements st) }
  in case Map.lookup el perPeer of
       Nothing                                              -> Just putTwo
       Just (SeenOne f) | rbHeaderHash f == rbHeaderHash h1 -> Just putTwo
                        | otherwise                         -> Nothing
       Just (SeenTwo{})                                     -> Nothing

centralEquiv :: Env -> Election -> RbHeader -> RbHeader -> St -> (St, [Effect])  -- §3 LevBlockEquivocationProof central branch
centralEquiv env el h1 h2 st =
  case Map.lookup el (stFirstAnnouncements st) of
    Just (AnnTwo{}) -> (st, [])
    prev ->
      let st1 = st { stFirstAnnouncements = Map.insert el (AnnTwo h1 h2) (stFirstAnnouncements st) }
          st2 = case prev of
                  Nothing | not (belowTip env el) -> ensureWantedBodyAnn st1 el h1
                  _                               -> st1
      in enqueueToAll (NotifyEquivProof (Just h1) h2) st2

ensureWantedBodyAnn :: St -> Election -> RbHeader -> St
ensureWantedBodyAnn st el h = case rbAnnounce h of
  Just a  -> wantBody el (annEbHash a) (annBodySize a) (annClosureSize a) st
  Nothing -> st

hOffer :: Monad m => Env -> Time -> Peer -> EbHash -> LeiosNotifySide -> St -> m (St, [Effect])  -- BEH-Offers · §3 LevBlockOffer / LevBlockTxsOffer
hOffer env now peer eh lvl st = case electionsFirstAnnouncing st peer eh of
  []  -> pure (st, [Disconnect peer UnannouncedOffer])
  els -> case notifyOf (raiseOffer els st) of
           Nothing  -> pure (st, [Disconnect peer RedundantOffer])
           Just st' -> considerFetchAfter env now st' []
  where
    raiseOffer els0 s = foldl' (\acc el -> anchorOffer peer el eh acc) s els0
    notifyOf s =
      let perPeer = fromMaybe EM.empty (Map.lookup peer (stPeerOfferings s))
          bump (notify, chain) | notify >= Just lvl = Nothing
                               | otherwise          = Just (notify <> Just lvl, chain)
      in (\m -> s { stPeerOfferings = Map.insert peer m (stPeerOfferings s) }) <$> EM.updateEb eh bump perPeer

hRollForward :: Monad m => Ifaces m -> Env -> Time -> Peer -> RbHeader -> Maybe AnnouncementTriple -> St -> m (St, [Effect])  -- BEH-Offers / BEH-Wanting · §3 LevRollForward
hRollForward ifs env now peer h pe st = do
  (st1, fx1) <- case rbAnnounce h of
                  Nothing -> pure (st, [])
                  Just _  -> centralAnnounce ifs env now h st
  let (st2, fx2) = case (rbHasLeiosCert h, pe) of
                     (True, Just at) -> rollForwardCert peer at st1
                     _               -> (st1, [])
  considerFetchAfter env now st2 (fx1 ++ fx2)

rollForwardCert :: Peer -> AnnouncementTriple -> St -> (St, [Effect])  -- §3 LevRollForward cert bit
rollForwardCert peer at st =
  let el = atElection at
      conflict = case Map.lookup el (stCertified st) of
                   Just (hh, _) -> hh /= atHeaderHash at
                   Nothing      -> False
  in if conflict
       then (st, [Disconnect peer CertConflict])
       else (recordChainSyncSide peer el (atHeaderHash at) (atEbHash at) st, [])

recordChainSyncSide :: Peer -> Election -> HeaderHash -> EbHash -> St -> St  -- §3 LevRollForward cert bit
recordChainSyncSide peer el hh eh st =
  case EM.lookupElection el perPeer of
    Just (EM.Refs EM.NoInactiveRef _ _)   -> st
    Just (EM.Refs (EM.InactiveRef _) _ _) -> st
    _ -> st { stPeerOfferings = Map.insert peer (EM.supersede el eh (Nothing, Any True) (Just (ChainSyncSide hh)) perPeer) (stPeerOfferings st) }
  where
    perPeer = fromMaybe EM.empty (Map.lookup peer (stPeerOfferings st))

hRequestNext :: Monad m => Env -> Peer -> St -> m (St, [Effect])  -- BEH-NotifyServe · §3 LevNotificationRequestNext
hRequestNext env peer st =
  let (s, cap) = fromMaybe (Set.empty, 0) (Map.lookup peer (stPeerNotifyQueue st))
  in if cap >= envNotifyMaxCapacity env
       then pure (st, [Disconnect peer ExcessNotifyCredits])
       else pure (setQueue peer (s, cap + 1) st, [])

hNotifyDequeue :: Monad m => Env -> Peer -> St -> m (St, [Effect])  -- BEH-NotifyServe · §3 LevNotifyDequeue
hNotifyDequeue env peer st =
  let (s, cap) = fromMaybe (Set.empty, 0) (Map.lookup peer (stPeerNotifyQueue st))
  in case pickMax s of
       Nothing      -> error "hNotifyDequeue: LevNotifyDequeue on an empty queue"
       Just (n, s')
         | belowTip env (notifyElection n) -> pure (setQueue peer (s', cap) st, [])
         | otherwise                       -> pure (sendNotification peer n (setQueue peer (s', cap - 1) st))

setQueue :: Peer -> (Set Notification, Int) -> St -> St
setQueue peer v st = st { stPeerNotifyQueue = Map.insert peer v (stPeerNotifyQueue st) }

enqueue :: Notification -> (Set Notification, Int) -> (Set Notification, Int)  -- BEH-NotifyServe
enqueue n (q, cap) =
  let q' = Set.insert n q
  in if Set.size q' > cap
       then (Set.delete (minimumBy (comparing notifyRank) (Set.toList q')) q', cap)
       else (q', cap)

enqueueTo :: Peer -> Notification -> St -> (St, [Effect])  -- BEH-NotifyServe
enqueueTo peer n st =
  let (q, cap) = fromMaybe (Set.empty, 0) (Map.lookup peer (stPeerNotifyQueue st))
      (q', _)  = enqueue n (q, cap)
  in (setQueue peer (q', cap) st, [ NotifyEnqueue peer | Set.size q' > Set.size q ])

enqueueToAll :: Notification -> St -> (St, [Effect])  -- BEH-NotifyServe relay
enqueueToAll n st = foldl' (\(s, fx) peer -> let (s', fx') = enqueueTo peer n s in (s', fx ++ fx')) (st, []) (Map.keys (stPeerPresent st))

pickMax :: Set Notification -> Maybe (Notification, Set Notification)  -- §2 notifyPriority · NEEDS-TO-BE-INCREMENTAL: two slot-ordered queues (announcements; non-announcements)
pickMax s
  | Set.null s = Nothing
  | otherwise  = let n = maximumBy (comparing notifyRank) (Set.toList s) in Just (n, Set.delete n s)

notifyRank :: Notification -> (NotificationPriority, Word64)         -- §2 notifyPriority (simplified; L_hdr tiers TODO)
notifyRank (NotifyAnnouncement h)     = (3, slotW (rbElection h))
notifyRank (NotifyEquivProof _ h2)    = (2, slotW (rbElection h2))
notifyRank (NotifyBlockTxsOffer el _) = (1, slotW el)
notifyRank (NotifyBlockOffer el _)    = (1, slotW el)

slotW :: Election -> Word64
slotW (Election (Slot s) _) = s

notifyElection :: Notification -> Election             -- §2 notification's election (for BEH-ImmTipAdvance staleness)
notifyElection (NotifyAnnouncement h)     = rbElection h
notifyElection (NotifyEquivProof _ h2)    = rbElection h2
notifyElection (NotifyBlockOffer el _)    = el
notifyElection (NotifyBlockTxsOffer el _) = el

notifyMsgSlot :: St -> Peer -> WireMsg -> Maybe Slot   -- BEH-NotifyServe staleness: a LeiosNotify message's slot (youngest, for a multi-election EbHash offer)
notifyMsgSlot _  _    (MsgLeiosBlockAnnouncement h)         = Just (electionSlot (rbElection h))
notifyMsgSlot _  _    (MsgLeiosBlockEquivocationProof _ h2) = Just (electionSlot (rbElection h2))
notifyMsgSlot st peer (MsgLeiosBlockOffer eh)             = youngestAnnouncedSlot st peer eh
notifyMsgSlot st peer (MsgLeiosBlockTxsOffer eh)          = youngestAnnouncedSlot st peer eh
notifyMsgSlot _  _    _                                     = Nothing

notifyStale :: Env -> Slot -> Bool                     -- BEH-NotifyServe staleness: slot-difference (as a duration) exceeds notifyStaleHorizon
notifyStale env (Slot s) = case envImmutableTip env of Slot tip -> tip > s + envNotifyStaleHorizon env

sendNotification :: Peer -> Notification -> St -> (St, [Effect])  -- BEH-NotifyServe
sendNotification peer n st = case n of
  NotifyAnnouncement h ->
    let el = rbElection h
        eh = maybe (EbHash 0) annEbHash (rbAnnounce h)
    in (openGate peer el eh st, [Send peer (MsgLeiosBlockAnnouncement h)])
  NotifyEquivProof m1 h2   -> (st, [Send peer (MsgLeiosBlockEquivocationProof m1 h2)])
  NotifyBlockOffer _ eh    -> (st, [Send peer (MsgLeiosBlockOffer eh)])
  NotifyBlockTxsOffer _ eh -> (st, [Send peer (MsgLeiosBlockTxsOffer eh)])

openGate :: Peer -> Election -> EbHash -> St -> St  -- §2 LstPeerOfferGates
openGate peer el eh st =
  st { stPeerOfferGates = Map.insert peer (EM.upsert eh el () () perPeer) (stPeerOfferGates st) }
  where perPeer = fromMaybe EM.empty (Map.lookup peer (stPeerOfferGates st))

hBlock :: Monad m => Ifaces m -> Env -> Time -> Peer -> EbHash -> Body -> St -> m (St, [Effect])  -- BEH-Responses · BEH-ChunkJobs · §3 LevBlock
hBlock ifs env now peer eh body st = case frontReq st peer of
  Just (ReqBody eh' bs cs) | eh' == eh ->
    let st1 = popFront peer st
        closureBytes = sum (map txRefSize (bodyTxlist body))
    in if bodyEbHash body /= eh || bodyActualSize body /= bs || closureBytes /= cs
         then pure (st1, [Disconnect peer BodyMismatch])
         else if not (wantsEh st eh)
           then considerFetchAfter env now st1 []
           else do
             let txrefs = bodyTxlist body
                 txhs   = Set.fromList (map txRefHash txrefs)
             hits  <- txCacheOnBody (ifTxc ifs) eh txhs
             memHs <- mempoolQueryPresent (ifMem ifs) (txhs `Set.difference` hashes hits)
             txCacheOnAcquire (ifTxc ifs) memHs
             let onHand  = hashes hits `Set.union` hashes memHs
                 toFetch = [ tr | tr <- toList txrefs, not (txRefHash tr `Set.member` onHand) ]
                 jobsMap = chunk env toFetch
                 copied  = hits ++ memHs
                 writes  = SubmitDisk (Write (WriteBody body))
                         : [ SubmitDisk (Write (WriteClosure eh copied)) | not (null copied) ]
                 st2     = setTxs eh jobsMap (fromIntegral (length writes)) st1
             considerFetchAfter env now st2 writes
  _ -> pure (st, [Disconnect peer UnsolicitedResponse])

hBlockTxs :: Monad m => Ifaces m -> Env -> Time -> Peer -> EbHash -> [Tx] -> St -> m (St, [Effect])  -- BEH-Responses · §3 LevBlockTxs
hBlockTxs ifs env now peer eh txs st = case frontReq st peer of
  Just (ReqJob eh' j job) | eh' == eh ->
    let st1 = popFront peer st
    in if map txHash txs /= NE.toList (jobTxs job)
         then pure (st1, [Disconnect peer TxsMismatch])
         else do
           txCacheOnAcquire (ifTxc ifs) txs
           let writes = [SubmitDisk (Write (WriteClosure eh txs))]
           case wantStateOf st1 eh of
             Just (AwaitingTxs t) ->
               let jobs' = Map.delete j (txsFetch t)
                   st2   = setTxs eh jobs' (txsWrites t + 1) st1
               in considerFetchAfter env now st2 writes
             _ -> considerFetchAfter env now st1 writes
  _ -> pure (st, [Disconnect peer UnsolicitedResponse])

hServeBody :: Monad m => Ifaces m -> Env -> Time -> Peer -> EbHash -> St -> m (St, [Effect])  -- BEH-FetchServe · §3 LevBlockRequest
hServeBody ifs _env _now peer eh st = do
  mbBody <- dbReadBody (ifDb ifs) eh
  case mbBody of
    Just body -> pure (st, [Send peer (MsgLeiosBlock eh body)])
    _         -> pure (st, [Disconnect peer RequestedAbsentData])

hServeTxs :: Monad m => Ifaces m -> Env -> Time -> Peer -> EbHash -> NonEmpty TxHash -> St -> m (St, [Effect])  -- BEH-FetchServe · §3 LevBlockTxsRequest
hServeTxs ifs _env _now peer eh txs st = do
  served <- dbReadClosureTxs (ifDb ifs) eh (NE.toList txs)
  if length served == NE.length txs
    then pure (st, [Send peer (MsgLeiosBlockTxs eh served)])
    else pure (st, [Disconnect peer RequestedAbsentData])

hCertValidated :: Monad m => Ifaces m -> Env -> Time -> AnnouncementTriple -> ByteCount -> ByteCount -> St -> m (St, [Effect])  -- BEH-Wanting · BEH-FetchPriority · §3 LevCertValidated
hCertValidated ifs env now (AnnouncementTriple el hh eh) bs cs st = do
  let st1 = setCertified el (hh, eh) st
  done <- isComplete ifs eh
  let st2 | belowTip env el = st1
          | done            = removeWantEl el st1
          | otherwise       = supersedeWant el eh bs cs st1
  considerFetchAfter env now st2 []

hPeerAdd :: Monad m => Peer -> Class -> St -> m (St, [Effect])  -- BEH-PeerChurn · §3 LevPeerAdd
hPeerAdd peer cls st = pure
  ( st { stPeerPresent = Map.insert peer (PeerInfo cls Active) (stPeerPresent st)
       , stPeerOfferings = Map.insert peer EM.empty (stPeerOfferings st) }
  , [] )

hPeerWindDown :: Monad m => Env -> Time -> Peer -> St -> m (St, [Effect])  -- BEH-PeerChurn · §3 LevPeerWindDown
hPeerWindDown env now peer st =
  let st1 = st { stPeerPresent = Map.adjust (\pi_ -> pi_ { peerPhase = WindingDown }) peer (stPeerPresent st) }
  in considerFetchAfter env now st1 []

hPeerRemove :: Monad m => Env -> Time -> Peer -> St -> m (St, [Effect])  -- BEH-PeerChurn · §3 LevPeerRemove
hPeerRemove env now peer st =
  let st1 = st { stPeerPresent = Map.delete peer (stPeerPresent st)
               , stPeerFirstAnnouncements = Map.delete peer (stPeerFirstAnnouncements st)
               , stPeerOfferings = Map.delete peer (stPeerOfferings st)
               , stPeerOfferGates = Map.delete peer (stPeerOfferGates st)
               , stPeerNotifyQueue = Map.delete peer (stPeerNotifyQueue st)
               , stPeerInflight = Map.delete peer (stPeerInflight st) }
  in considerFetchAfter env now st1 []

hTimer :: Monad m => Peer -> Req -> St -> m (St, [Effect])  -- BEH-Timeout · §3 LevTimer
hTimer peer req st = pure (st, [Disconnect peer RequestTimeout | inflightHas st peer req])

hDiskDone :: Monad m => Ifaces m -> Env -> Time -> DiskWrite -> St -> m (St, [Effect])  -- §3 LevDiskDone · BEH-Completion
hDiskDone _ifs _env _now w st = case w of
  WriteBody body ->
    let eh  = bodyEbHash body
        els = electionsNaming eh st
        st0 = st { stVolatileBody = addVolatile eh els (stVolatileBody st) }
        (st1, fx1) = case youngestElection els of
                       Just el -> enqueueBodyOffers el eh st0
                       Nothing -> (st0, [])
        (st2, fx2) = decWrite eh st1
    in pure (st2, fx1 ++ fx2)
  WriteClosure eh _ -> pure (decWrite eh st)

decWrite :: EbHash -> St -> (St, [Effect])  -- §3 LevDiskDone persist-before-expose (one DiskWrite finished)
decWrite eh st = case wantStateOf st eh of
  Just (AwaitingTxs t) -> case decTxsState t of
    Just t' -> (updateWant eh (\_ -> Just (AwaitingTxs t')) st, [])
    Nothing -> completeEb eh st
  _ -> (st, [])

completeEb :: EbHash -> St -> (St, [Effect])  -- BEH-Completion (per-EbHash; fan out to every interested election)
completeEb eh st =
  let els = electionsNaming eh st
      st0 = st { stVolatileClosure = addVolatile eh els (stVolatileClosure st) }
      (st1, fx) = case youngestElection els of
                    Just el -> enqueueClosureOffers el eh (removeWantEb eh st0)
                    Nothing -> (removeWantEb eh st0, [])
  in (st1, [ NotifyVotingAndChainSel el eh | el <- els ] ++ fx)

hImmTipAdvanced :: Monad m => Env -> St -> m (St, [Effect])  -- BEH-ImmTipAdvance · §3 LevImmTipAdvanced
hImmTipAdvanced env st =
  let s = envImmutableTip env
      st1 = pruneBelow s st
  in pure (st1, [SubmitDisk (Promote s)])

hGarbageCollect :: Monad m => Slot -> St -> m (St, [Effect])  -- BEH-ImmTipAdvance · §3 LevGarbageCollect
hGarbageCollect s st = pure (st, [SubmitDisk (GarbageCollect s)])

hSelfIssued :: Monad m => Ifaces m -> Env -> Time -> RbHeader -> Body -> St -> m (St, [Effect])  -- BEH-SelfIssued · §3 LevSelfIssued
hSelfIssued ifs env now h body st = case rbAnnounce h of
  Nothing  -> pure (st, [])
  Just ann -> do
    (st1, fx) <- centralAnnounce ifs env now h st
    let eh = annEbHash ann
        txrefs = bodyTxlist body
        txhs = Set.fromList (map txRefHash txrefs)
    memHs <- mempoolQueryPresent (ifMem ifs) txhs
    txCacheOnAcquire (ifTxc ifs) memHs
    let onHand  = hashes memHs
        toFetch = [ tr | tr <- toList txrefs, not (txRefHash tr `Set.member` onHand) ]
        jobsMap = chunk env toFetch
        copied  = memHs
        writes  = SubmitDisk (Write (WriteBody body))
                : [ SubmitDisk (Write (WriteClosure eh copied)) | not (null copied) ]
        st2     = setTxs eh jobsMap (fromIntegral (length writes)) st1
    pure (st2, fx ++ writes)

considerFetching :: Monad m => Env -> Time -> St -> m (St, [Effect])  -- §3 consider-fetching · NEEDS-TO-BE-INCREMENTAL: reconsider only the affected peer(s)/EB, not every Active peer
considerFetching env now st = pure (foldl' go (st, []) (activePeers st))
  where
    go (s, fx) peer =
      let new = decide env s peer
          s'  = foldl' (issue peer) s new
          fx' = fx ++ concatMap (sendReq s now peer) new
      in (s', fx')

considerFetchAfter :: Monad m => Env -> Time -> St -> [Effect] -> m (St, [Effect])
considerFetchAfter env now st fx = do
  (st', fx') <- considerFetching env now st
  pure (st', fx ++ fx')

issue :: Peer -> St -> Req -> St
issue peer st r =
  st { stPeerInflight = Map.insertWith (flip (<>)) peer (Seq.singleton r) (stPeerInflight st) }

sendReq :: St -> Time -> Peer -> Req -> [Effect]
sendReq st now peer r = [Send peer (reqWire st r), SetTimer peer r now]

reqWire :: St -> Req -> WireMsg
reqWire _ (ReqBody eh _ _)   = MsgLeiosBlockRequest eh
reqWire _ (ReqJob eh _ job) = MsgLeiosBlockTxsRequest eh (jobTxs job)

frontReq :: St -> Peer -> Maybe Req
frontReq st peer = case inflightOf st peer of
  r :<| _ -> Just r
  _       -> Nothing

popFront :: Peer -> St -> St
popFront peer st = st { stPeerInflight = Map.adjust dropFront peer (stPeerInflight st) }
  where dropFront sq = case sq of _ :<| rest -> rest; Empty -> Empty

wantBody :: Election -> EbHash -> ByteCount -> ByteCount -> St -> St  -- §2 LstWanted (announcement; keep existing progress if already wanted)
wantBody el eh bs cs st = st { stWanted = EM.upsert eh el (AwaitingBody bs cs) () (stWanted st) }

supersedeWant :: Election -> EbHash -> ByteCount -> ByteCount -> St -> St  -- §2 LstWanted (cert validated; once per election)
supersedeWant el eh bs cs st = st { stWanted = EM.supersede el eh (AwaitingBody bs cs) () (stWanted st) }

updateWant :: EbHash -> (WantState -> Maybe WantState) -> St -> St  -- §2 LstWanted (in-place per-EB payload update; no-op if absent)
updateWant eh f st = st { stWanted = fromMaybe (stWanted st) (EM.updateEb eh f (stWanted st)) }

removeWantEl :: Election -> St -> St  -- §2 LstWanted (drop an election's references)
removeWantEl el st = st { stWanted = EM.deleteElection el (stWanted st) }

removeWantEb :: EbHash -> St -> St  -- §2 LstWanted (drop every election actively wanting eh)
removeWantEb eh st = st { stWanted = foldl' (flip EM.deleteElection) (stWanted st) (electionsNaming eh st) }

isComplete :: Monad m => Ifaces m -> EbHash -> m Bool  -- BEH-Completion (restart-time check only, in LevCertValidated)
isComplete ifs eh = do
  mb <- dbReadBody (ifDb ifs) eh
  case mb of
    Nothing   -> pure False
    Just body -> do
      let need = Set.insert (DbBody eh) (Set.fromList (map (DbClosureTx eh . txRefHash) (bodyTxlist body)))
      have <- dbQueryPresent (ifDb ifs) need
      pure (need `Set.isSubsetOf` have)

setCertified :: Election -> (HeaderHash, EbHash) -> St -> St
setCertified el v st = st { stCertified = Map.insert el v (stCertified st) }

enqueueOffer :: Notification -> EbHash -> St -> (St, [Effect])  -- BEH-NotifyServe · BEH-Completion · NEEDS-TO-BE-INCREMENTAL: EbHash↦gated-downstream-peers index
enqueueOffer notif eh st =
  foldl' (\(s, fx) peer -> let (s', fx') = enqueueTo peer notif s in (s', fx ++ fx')) (st, []) gated
  where
    gated = [ peer | (peer, gates) <- Map.toList (stPeerOfferGates st), isJust (EM.lookupEb eh gates) ]

enqueueBodyOffers :: Election -> EbHash -> St -> (St, [Effect])  -- BEH-NotifyServe · BEH-Completion
enqueueBodyOffers el eh = enqueueOffer (NotifyBlockOffer el eh) eh

enqueueClosureOffers :: Election -> EbHash -> St -> (St, [Effect])  -- BEH-NotifyServe · BEH-Completion
enqueueClosureOffers el eh = enqueueOffer (NotifyBlockTxsOffer el eh) eh

addVolatile :: EbHash -> [Election] -> EbHashMap () () -> EbHashMap () ()  -- §2 LstVolatileBody/LstVolatileClosure
addVolatile eh els m = foldl' (\acc el -> EM.upsert eh el () () acc) m els

youngestElection :: [Election] -> Maybe Election  -- §3 offer representative (youngest naming election)
youngestElection [] = Nothing
youngestElection els = Just (maximumBy (comparing electionSlot) els)

heldIn :: EbHash -> EbHashMap a b -> Bool  -- §2 LstVolatileBody/LstVolatileClosure membership
heldIn eh m = isJust (EM.lookupEb eh m)

pruneBelow :: Slot -> St -> St                     -- BEH-ImmTipAdvance range-delete
pruneBelow s st = st
  { stFirstAnnouncements = pruneElectionMap s (stFirstAnnouncements st)
  , stWanted = EM.pruneElections (\el -> electionSlot el < s) (stWanted st)
  , stVolatileBody = EM.pruneElections (\el -> electionSlot el < s) (stVolatileBody st)
  , stVolatileClosure = EM.pruneElections (\el -> electionSlot el < s) (stVolatileClosure st)
  , stCertified = pruneElectionMap s (stCertified st)
  , stPeerFirstAnnouncements = Map.map (pruneElectionMap s) (stPeerFirstAnnouncements st)
  , stPeerOfferings = Map.map (EM.pruneElections (\el -> electionSlot el < s)) (stPeerOfferings st)
  , stPeerOfferGates = Map.map (EM.pruneElections (\el -> electionSlot el < s)) (stPeerOfferGates st)
  }

pruneElectionMap :: Slot -> Map Election a -> Map Election a
pruneElectionMap s = Map.filterWithKey (\el _ -> not (electionSlot el < s))

advancePeerAnn :: Peer -> Election -> RbHeader -> St -> Maybe St  -- §3 LevBlockAnnouncement per-peer check
advancePeerAnn peer el h st =
  let perPeer = fromMaybe Map.empty (Map.lookup peer (stPeerFirstAnnouncements st))
  in case Map.lookup el perPeer of
       Nothing          -> Just (putSeen (SeenOne h))
       Just (SeenOne h1)
         | rbHeaderHash h1 == rbHeaderHash h -> Nothing
         | otherwise                         -> Just (putSeen (SeenTwo h1))
       Just (SeenTwo{}) -> Nothing
  where
    putSeen seen = st
      { stPeerFirstAnnouncements =
          Map.insert peer
            (Map.insert el seen (fromMaybe Map.empty (Map.lookup peer (stPeerFirstAnnouncements st))))
            (stPeerFirstAnnouncements st) }

centralAnnounce :: Monad m => Ifaces m -> Env -> Time -> RbHeader -> St -> m (St, [Effect])  -- §3 LevBlockAnnouncement central branch
centralAnnounce _ifs env _now h st = case rbAnnounce h of
  Nothing  -> pure (st, [])
  Just ann ->
    let el = rbElection h
        eh = annEbHash ann
    in case Map.lookup el (stFirstAnnouncements st) of
         Nothing ->
           let st1 = st { stFirstAnnouncements = Map.insert el (AnnOne h) (stFirstAnnouncements st) }
               st2 = anchorVolatile el eh st1
               st3 | belowTip env el    = st2
                   | heldIn eh (stVolatileClosure st2) = st2
                   | otherwise          = wantBody el eh (annBodySize ann) (annClosureSize ann) st2
               (st4, fx)  = enqueueToAll (NotifyAnnouncement h) st3
               (st5, fx') = enqueueHeldOffers el eh st4
           in pure (st5, fx ++ fx')
         Just (AnnOne h1)
           | rbHeaderHash h1 == rbHeaderHash h -> pure (st, [])
           | otherwise ->
               let st1 = st { stFirstAnnouncements = Map.insert el (AnnTwo h1 h) (stFirstAnnouncements st) }
               in pure (enqueueToAll (NotifyEquivProof (Just h1) h) st1)
         Just (AnnTwo{}) -> pure (st, [])

anchorVolatile :: Election -> EbHash -> St -> St  -- §2 LstVolatileBody/Closure: a fresh announcement extends a held EB's elections
anchorVolatile el eh st = st
  { stVolatileBody    = if heldIn eh (stVolatileBody st)    then addVolatile eh [el] (stVolatileBody st)    else stVolatileBody st
  , stVolatileClosure = if heldIn eh (stVolatileClosure st) then addVolatile eh [el] (stVolatileClosure st) else stVolatileClosure st }

enqueueHeldOffers :: Election -> EbHash -> St -> (St, [Effect])  -- BEH-Offers · BEH-Completion: offer a held EB to peers we are announcing it to
enqueueHeldOffers el eh st
  | heldIn eh (stVolatileClosure st) = enqueueToAll (NotifyBlockTxsOffer el eh) st
  | heldIn eh (stVolatileBody st)    = enqueueToAll (NotifyBlockOffer el eh) st
  | otherwise                        = (st, [])

copyAnnIntoOfferings :: Peer -> Election -> St -> St  -- BEH-Offers · §2 LstPeerOfferings
copyAnnIntoOfferings peer el st = case firstAnnouncedEb st peer el of
  Just eh | offered eh -> anchorOffer peer el eh st
  _                    -> st
  where
    offered eh = isJust (EM.lookupEb eh (fromMaybe EM.empty (Map.lookup peer (stPeerOfferings st))))

anchorOffer :: Peer -> Election -> EbHash -> St -> St  -- §2 LstPeerOfferings
anchorOffer peer el eh st =
  case EM.activeRef <$> EM.lookupElection el perPeer of
    Just act | act /= eh -> st
    _                    -> st { stPeerOfferings = Map.insert peer (EM.upsert eh el (Nothing, Any False) Nothing perPeer) (stPeerOfferings st) }
  where
    perPeer = fromMaybe EM.empty (Map.lookup peer (stPeerOfferings st))

nullIfaces :: Applicative m => Ifaces m
nullIfaces = Ifaces
  { ifDb  = LeiosDb { dbQueryPresent = const (pure Set.empty)
                    , dbReadBody = const (pure Nothing)
                    , dbReadClosureTxs = \_ _ -> pure [] }
  , ifTxc = TxCache { txCacheNoteAnnouncement = const (pure ())
                    , txCacheOnBody = \_ _ -> pure []
                    , txCacheOnAcquire = const (pure ()) }
  , ifMem = Mempool { mempoolQueryPresent = const (pure []) }
  }

prop_wantAnnouncementGated :: St -> Bool          -- §4 Want is announcement-gated
prop_wantAnnouncementGated st =
  let EM.EbHashMap _ els = stWanted st
  in Map.keysSet els
       `Set.isSubsetOf` (Map.keysSet (stFirstAnnouncements st) `Set.union` Map.keysSet (stCertified st))

prop_perPeerActiveEbCap :: Env -> St -> Bool      -- §4 Per-peer active-EB cap
prop_perPeerActiveEbCap env st =
  all (\(peer, pinfo) -> Set.size (activeEbs st peer) <= maxActiveEbs env (peerClass pinfo))
      (Map.toList (stPeerPresent st))

prop_offersAnnouncedOrCertified :: St -> Bool     -- §4 Offers are announced or certified
prop_offersAnnouncedOrCertified st = and
  [ case inact of
      EM.NoInactiveRefYet -> isJust (firstAnnouncedEb st peer el)
      _                   -> True
  | (peer, m)  <- Map.toList (stPeerOfferings st)
  , let EM.EbHashMap _ els = m
  , (el, EM.Refs inact _ _) <- Map.toList els ]

prop_inflightOffered :: St -> Bool                -- §4 Client soundness (offer half)
prop_inflightOffered st = and
  [ case r of
      ReqBody eh _ _  -> peer `Set.member` offerersBody st eh
      ReqJob eh _ _ -> peer `Set.member` offerersClosure st eh
  | (peer, sq) <- Map.toList (stPeerInflight st)
  , r <- toList sq ]
