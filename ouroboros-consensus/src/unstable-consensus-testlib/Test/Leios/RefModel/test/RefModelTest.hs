module Main (main) where

import           Control.Monad (foldM)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.IORef (modifyIORef', newIORef, readIORef)
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid (Sum (..))
import qualified Data.Sequence as Seq

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           EbHashMap
import           RefModel

main :: IO ()
main = defaultMain tests

env :: Env
env = Env 5 2 1000000 65536 16384 30 300 600 21600 (Slot 0)

el100 :: Election
el100 = Election (Slot 5) (PoolId 1)

hdr100, hdr101, hdr200 :: RbHeader
hdr100 = RbHeader (HeaderHash 10) el100 (Just (EbAnn (EbHash 100) 200 300)) False True
hdr101 = RbHeader (HeaderHash 11) el100 (Just (EbAnn (EbHash 101) 200 300)) False True
hdr200 = RbHeader (HeaderHash 12) (Election (Slot 6) (PoolId 1)) (Just (EbAnn (EbHash 200) 200 300)) False True

credit :: Peer -> Stimulus
credit p = LevWiredMsg p MsgLeiosNotificationRequestNext

dequeue :: Peer -> Stimulus
dequeue = LevNotifyDequeue

body100 :: Body
body100 = Body (EbHash 100) [TxRef (TxHash 1) 150, TxRef (TxHash 2) 150] 200

ann :: Peer -> RbHeader -> Stimulus
ann p h = LevWiredMsg p (MsgLeiosBlockAnnouncement h)

offer :: Peer -> EbHash -> Stimulus
offer p eh = LevWiredMsg p (MsgLeiosBlockOffer (electionSlot el100) eh)

txsOffer :: Peer -> EbHash -> Stimulus
txsOffer p eh = LevWiredMsg p (MsgLeiosBlockTxsOffer (electionSlot el100) eh)

runWith :: Ifaces Identity -> [Stimulus] -> (St, [Effect])
runWith ifs = foldl' go (emptySt, [])
  where go (st, fx) s = let (st', fx') = runIdentity (step ifs env (Time 0) s st) in (st', fx ++ fx')

run :: [Stimulus] -> (St, [Effect])
run = runWith nullIfaces

foldEnv :: Env -> [Stimulus] -> (St, [Effect])
foldEnv e = foldl' go (emptySt, [])
  where go (st, fx) s = let (st', fx') = runIdentity (step nullIfaces e (Time 0) s st) in (st', fx ++ fx')

servingIfaces :: Ifaces Identity
servingIfaces = nullIfaces
  { ifDb = (ifDb nullIfaces)
      { dbQueryPresent   = pure
      , dbReadClosureTxs = \_ hs -> pure [ Tx h 1 | h <- hs ] } }


isReq :: Effect -> Bool
isReq (Send _ (MsgLeiosBlockRequest _))      = True
isReq (Send _ (MsgLeiosBlockTxsRequest _ _)) = True
isReq _                                      = False

isDisconnect :: Effect -> Bool
isDisconnect (Disconnect _ _) = True
isDisconnect _              = False

tests :: TestTree
tests = testGroup "Leios RefModel — Spec.md main spec"
  [ testCase "BEH-PeerChurn: LevPeerAdd registers an Active peer and primes the notify client" $ do
      let (st, fx) = run [LevPeerAdd (Peer 1) StakeSampled]
      fx @?= replicate (envNotifyMaxCapacity env) (Send (Peer 1) MsgLeiosNotificationRequestNext)
      fmap peerPhase (Map.lookup (Peer 1) (stPeerPresent st)) @?= Just Active

  , testCase "BEH-NotifyServe (client): a consumed notification refills one RequestNext" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100 ]
          reqNexts = length [ () | Send _ MsgLeiosNotificationRequestNext <- fx ]
      reqNexts @?= envNotifyMaxCapacity env + 1

  , testCase "BEH-NotifyServe (client): no refill for a WindingDown peer" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerWindDown (Peer 1), ann (Peer 1) hdr100 ]
          reqNexts = length [ () | Send _ MsgLeiosNotificationRequestNext <- fx ]
      reqNexts @?= envNotifyMaxCapacity env

  , testCase "BEH-Wanting: an announcement gates the want (AwaitingBody), no fetch yet" $ do
      let (st, fx) = run [LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100]
      wantStateOf st (EbHash 100) @?= Just (AwaitingBody 200 300)
      assertBool "no request before an offer" (not (any isReq fx))

  , testCase "BEH-Offers + BEH-BodyFetch: an offer for the first-announced EB issues ReqBody" $ do
      let (_, fx) = run [LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)]
      assertBool "sends MsgLeiosBlockRequest" (Send (Peer 1) (MsgLeiosBlockRequest (EbHash 100)) `elem` fx)

  , testCase "BEH-ChunkJobs + BEH-ClosureFetch: body then closure-offer requests the chunked job's txs" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                         , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) (EbHash 100) ]
      [ txs | Send _ (MsgLeiosBlockTxsRequest _ txs) <- fx ] @?= [NE.fromList [TxHash 1, TxHash 2]]
      assertBool "want advanced to AwaitingTxs"
        (case wantStateOf st (EbHash 100) of Just AwaitingTxs{} -> True; _ -> False)

  , testCase "BEH-Responses: body and txs responses use separate per-mini-protocol FIFOs" $ do
      let body200  = Body (EbHash 200) [TxRef (TxHash 9) 300] 200
          offer200 = LevWiredMsg (Peer 1) (MsgLeiosBlockTxsOffer (Slot 6) (EbHash 200))
          (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled
                         , ann (Peer 1) hdr200, offer200
                         , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 200) body200)   -- eh200 -> AwaitingTxs: a ReqJob is now in flight
                         , ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)            -- a ReqBody for eh100 is now in flight
                         , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100) ] -- the body response must not be blocked behind the ReqJob
      assertBool "a body response is not blocked behind an in-flight txs request" (not (any isDisconnect fx))
      assertBool "eh100 advanced to AwaitingTxs" (case wantStateOf st (EbHash 100) of Just AwaitingTxs{} -> True; _ -> False)

  , testCase "BEH-Responses: a second body for an already-fetched EB is ignored (no double write)" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) StakeSampled
                        , ann (Peer 1) hdr100, ann (Peer 2) hdr100
                        , offer (Peer 1) (EbHash 100), offer (Peer 2) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100)
                        , LevWiredMsg (Peer 2) (MsgLeiosBlock (EbHash 100) body100) ]
          bodyWrites = length [ () | SubmitDisk (Write (WriteBody _)) <- fx ]
      bodyWrites @?= 1

  , testCase "Job: jobBytes is the real summed tx size" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100) ]
      jobBytes st (EbHash 100) (JobId 0) @?= 300

  , testCase "BEH-NotifyServe: a dequeue ships the buffered announcement and opens the gate" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                         , credit (Peer 2), ann (Peer 1) hdr100, dequeue (Peer 2) ]
      assertBool "relays the announcement to Peer 2" (Send (Peer 2) (MsgLeiosBlockAnnouncement hdr100) `elem` fx)
      (activeRef <$> (Map.lookup (Peer 2) (stPeerOfferGates st) >>= lookupElection el100)) @?= Just (EbHash 100)

  , testCase "BEH-NotifyServe: an enqueue that grows the queue emits NotifyEnqueue" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                        , credit (Peer 2), ann (Peer 1) hdr100 ]
      length [ () | NotifyEnqueue (Peer 2) <- fx ] @?= 1

  , testCase "BEH-NotifyServe back-pressure: with no credit nothing is buffered and no NotifyEnqueue fires" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled, ann (Peer 1) hdr100 ]
          q = maybe Seq.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
      assertBool "queue empty without a credit" (Seq.null q)
      assertBool "no NotifyEnqueue for Peer 2"  (null [ () | NotifyEnqueue (Peer 2) <- fx ])

  , testCase "BEH-NotifyServe back-pressure: a full FIFO drops the overflowing (newest) message, keeping the first" $ do
      let setup ps  = run ([ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled, credit (Peer 2) ] ++ ps)
          qOf (st, _)   = maybe Seq.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
          enqOf (_, fx) = length [ () | NotifyEnqueue (Peer 2) <- fx ]
          stalerFirst  = setup [ ann (Peer 1) hdr100, ann (Peer 1) hdr200 ]
          fresherFirst = setup [ ann (Peer 1) hdr200, ann (Peer 1) hdr100 ]
      qOf stalerFirst    @?= Seq.fromList [NotifyAnnouncement hdr100]
      qOf fresherFirst   @?= Seq.fromList [NotifyAnnouncement hdr200]
      enqOf stalerFirst  @?= 1
      enqOf fresherFirst @?= 1

  , testCase "BEH-NotifyServe back-pressure: credits up to notifyMaxCapacity raise capacity, no disconnect" $ do
      let envCap2   = env { envNotifyMaxCapacity = 2 }
          (st, fx)  = foldEnv envCap2 (LevPeerAdd (Peer 2) PeerSharingSampled : replicate 2 (credit (Peer 2)))
      (snd <$> Map.lookup (Peer 2) (stPeerNotifyQueue st)) @?= Just 2
      assertBool "no disconnect within the bound" (not (any isDisconnect fx))

  , testCase "BEH-NotifyServe back-pressure: over-crediting past notifyMaxCapacity disconnects" $ do
      let envCap2   = env { envNotifyMaxCapacity = 2 }
          (st, fx)  = foldEnv envCap2 (LevPeerAdd (Peer 2) PeerSharingSampled : replicate 3 (credit (Peer 2)))
      assertBool "disconnects on the excess credit" (Disconnect (Peer 2) ExcessNotifyCredits `elem` fx)
      (snd <$> Map.lookup (Peer 2) (stPeerNotifyQueue st)) @?= Just 2

  , testCase "BEH-NotifyServe: an inbound notification far below the immutable tip disconnects (stale)" $ do
      let envStale = env { envImmutableTip = Slot 700 }   -- hdr100 is slot 5, i.e. 695 > notifyStaleHorizon (600)
          (_, fx)  = foldEnv envStale [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100 ]
      assertBool "disconnects on the stale notification" (Disconnect (Peer 1) StaleNotification `elem` fx)

  , testCase "BEH-NotifyServe: an inbound notification within the staleness horizon is not a disconnect" $ do
      let envNear = env { envImmutableTip = Slot 100 }    -- hdr100 is slot 5, only 95 < notifyStaleHorizon (600)
          (_, fx) = foldEnv envNear [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100 ]
      assertBool "no stale disconnect within the horizon" (not (any isDisconnect fx))

  , testCase "BEH-NotifyServe: a dequeue discards a notification older than the immutable tip" $ do
      let envT      = env { envImmutableTip = Slot 6 }
          (st0, _)  = foldEnv envT [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                                   , credit (Peer 2), ann (Peer 1) hdr100 ]
          (st1, fx) = runIdentity (step nullIfaces envT (Time 0) (dequeue (Peer 2)) st0)
      assertBool "stale notification not sent" (null [ () | Send (Peer 2) _ <- fx ])
      assertBool "stale notification discarded" (maybe True (Seq.null . fst) (Map.lookup (Peer 2) (stPeerNotifyQueue st1)))

  , testCase "BEH-ImmTipAdvance: promote is prompt, GC is deferred" $ do
      let envT    = env { envImmutableTip = Slot 7 }
          (_, fx) = runIdentity (step nullIfaces envT (Time 0) LevImmTipAdvanced emptySt)
      assertBool "promotes slot 7"  (SubmitDisk (Promote (Slot 7)) `elem` fx)
      assertBool "no GC at advance" (null [ () | SubmitDisk (GarbageCollect _) <- fx ])

  , testCase "BEH-ImmTipAdvance: a deferred LevGarbageCollect submits the disk GC" $ do
      let (_, fx) = runIdentity (step nullIfaces env (Time 0) (LevGarbageCollect (Slot 7)) emptySt)
      fx @?= [SubmitDisk (GarbageCollect (Slot 7))]

  , testCase "BEH-Wanting: a genuine equivocation is accepted; first EB stays wanted, equivocating never" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                         , LevWiredMsg (Peer 1) (MsgLeiosBlockEquivocationProof Nothing hdr101) ]
      assertBool "no disconnect" (not (any isDisconnect fx))
      wantStateOf st (EbHash 100) @?= Just (AwaitingBody 200 300)

  , testCase "BEH-Offers: an offer for an equivocating (non-first) EB disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                        , LevWiredMsg (Peer 1) (MsgLeiosBlockEquivocationProof Nothing hdr101)
                        , offer (Peer 1) (EbHash 101) ]
      assertBool "disconnects (unannounced offer)" (Disconnect (Peer 1) UnannouncedOffer `elem` fx)

  , testCase "BEH-Offers: two different certified HeaderHashes for one election disconnects; a repeat does not" $ do
      let certRf hh eb = LevRollForward (Peer 1) (RbHeader (HeaderHash hh) el100 Nothing True True)
                                        (Just (AnnouncementTriple el100 (HeaderHash hh) (EbHash eb)))
          (_, fxConflict) = run [ LevPeerAdd (Peer 1) StakeSampled, certRf 10 100, certRf 11 100 ]
          (_, fxRepeat)   = run [ LevPeerAdd (Peer 1) StakeSampled, certRf 10 100, certRf 10 100 ]
      assertBool "disconnects with CertConflict" (Disconnect (Peer 1) CertConflict `elem` fxConflict)
      assertBool "an identical certified claim is idempotent" (not (any isDisconnect fxRepeat))

  , testCase "BEH-Wanting: TxCache noted once per election (first-announced); a superseding cert evicts" $ do
      ref <- newIORef ([] :: [(String, Election, EbHash)])
      let recIfaces :: Ifaces IO
          recIfaces = nullIfaces
            { ifTxc = (ifTxc nullIfaces)
                { txCacheNoteAnnouncement  = \el eh -> modifyIORef' ref (("note",  el, eh) :)
                , txCacheEvictForValidCert = \el eh -> modifyIORef' ref (("evict", el, eh) :) } }
          stimuli = [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) StakeSampled
                    , ann (Peer 1) hdr100, ann (Peer 2) hdr101
                    , LevCertValidated (AnnouncementTriple el100 (HeaderHash 11) (EbHash 101)) 200 300 ]
      _ <- foldM (\st s -> fst <$> step recIfaces env (Time 0) s st) emptySt stimuli
      evs <- reverse <$> readIORef ref
      [ (el, eh) | ("note", el, eh) <- evs ] @?= [ (el100, EbHash 100) ]
      assertBool "a superseding cert evicts the election" (("evict", el100, EbHash 101) `elem` evs)

  , testCase "BEH-Responses: a body whose closure size mismatches the announcement disconnects" $ do
      let badBody = Body (EbHash 100) [TxRef (TxHash 1) 1] 200
          (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) badBody) ]
      assertBool "disconnects with BodyMismatch" (Disconnect (Peer 1) BodyMismatch `elem` fx)

  , testCase "BEH-Responses: a body containing an oversized tx is rejected (BodyMismatch)" $ do
      let bigHdr  = RbHeader (HeaderHash 50) el100 (Just (EbAnn (EbHash 100) 200 20000)) False True
          bigBody = Body (EbHash 100) [TxRef (TxHash 1) 20000] 200
          (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) bigHdr, offer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) bigBody) ]
      assertBool "disconnects with BodyMismatch (tx exceeds envMaxTxSize)" (Disconnect (Peer 1) BodyMismatch `elem` fx)

  , testCase "BEH-Timeout: a peer's timer disconnects (RequestTimeout) while requests are outstanding" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                        , LevTimer (Time 0) (Peer 1) ]
      assertBool "disconnects with RequestTimeout" (Disconnect (Peer 1) RequestTimeout `elem` fx)

  , testCase "BEH-Timeout: a peer's timer with no outstanding requests does not disconnect" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevTimer (Time 0) (Peer 1) ]
      assertBool "no disconnect when nothing is outstanding" (not (any isDisconnect fx))

  , testCase "BEH-Responses: an unsolicited body (no matching in-flight request) disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100) ]
      assertBool "disconnects with UnsolicitedResponse" (Disconnect (Peer 1) UnsolicitedResponse `elem` fx)

  , testCase "BEH-Responses: a tx-closure reply in the wrong order disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlockTxs (EbHash 100) [Tx (TxHash 2) 150, Tx (TxHash 1) 150]) ]
      assertBool "disconnects with TxsMismatch" (Disconnect (Peer 1) TxsMismatch `elem` fx)

  , testCase "BEH-FetchServe: a tx request is served with the requested txs in order" $ do
      let (_, fx) = runWith servingIfaces
                      [ LevPeerAdd (Peer 2) PeerSharingSampled
                      , LevWiredMsg (Peer 2) (MsgLeiosBlockTxsRequest (EbHash 100) (NE.fromList [TxHash 1, TxHash 2])) ]
      [ ts | Send _ (MsgLeiosBlockTxs _ ts) <- fx ] @?= [[Tx (TxHash 1) 1, Tx (TxHash 2) 1]]

  , testCase "BEH-Completion: a cert for an already-complete EB still fans voting + ChainSel" $ do
      let ifs = nullIfaces { ifDb = (ifDb nullIfaces)
                  { dbReadBody = \e -> pure (if e == EbHash 100 then Just body100 else Nothing)
                  , dbQueryPresent = pure } }
          el2 = Election (Slot 7) (PoolId 2)
          (_, fx) = runWith ifs [ LevCertValidated (AnnouncementTriple el2 (HeaderHash 99) (EbHash 100)) 200 300 ]
      assertBool "fans NotifyVotingAndChainSel for the (re)certified EB"
        (NotifyVotingAndChainSel (EbHash 100) [] `elem` fx)

  , testCase "BEH-Wanting: a duplicate LevCertValidated is idempotent (no crash)" $ do
      let at = AnnouncementTriple el100 (HeaderHash 10) (EbHash 100)
          (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                        , LevCertValidated at 200 300, LevCertValidated at 200 300 ]
      isCertifiedEb st el100 (EbHash 100) @?= True

  , testCase "BEH-Completion: announcing an already-available closure under a new election fans voting + ChainSel" $ do
      let ifs = nullIfaces { ifDb = (ifDb nullIfaces)
                  { dbReadBody = \e -> pure (if e == EbHash 100 then Just body100 else Nothing)
                  , dbQueryPresent = pure } }
          e2    = Election (Slot 7) (PoolId 2)
          e3    = Election (Slot 8) (PoolId 3)
          hdrE3 = RbHeader (HeaderHash 78) e3 (Just (EbAnn (EbHash 100) 200 300)) False True
          (_, fx) = runWith ifs [ LevCertValidated (AnnouncementTriple e2 (HeaderHash 99) (EbHash 100)) 200 300
                                , LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdrE3 ]
      assertBool "fans NotifyVotingAndChainSel carrying the new announcing RB's HeaderHash"
        (NotifyVotingAndChainSel (EbHash 100) [HeaderHash 78] `elem` fx)

  , testCase "BEH-Completion: finishing a body write enqueues a body offer" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                        , credit (Peer 2), ann (Peer 1) hdr100, dequeue (Peer 2), credit (Peer 2)
                        , LevDiskDone (WriteBody body100) ]
          q = maybe Seq.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
      assertBool "body offer enqueued" (NotifyBlockOffer (EbHash 100) `elem` q)

  , testCase "BEH-Completion: voting + ChainSel are notified only when the last write lands (persist-gated)" $ do
      let txs    = [Tx (TxHash 1) 150, Tx (TxHash 2) 150]
          setup  = [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                   , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) (EbHash 100)
                   , LevWiredMsg (Peer 1) (MsgLeiosBlockTxs (EbHash 100) txs) ]
          exposed fx = NotifyVotingAndChainSel (EbHash 100) [HeaderHash 10] `elem` fx
          (_, fx1)   = run (setup ++ [ LevDiskDone (WriteBody body100) ])
          (st2, fx2) = run (setup ++ [ LevDiskDone (WriteBody body100)
                                     , LevDiskDone (WriteClosure (EbHash 100) txs) ])
      assertBool "fetch-complete + first write expose nothing" (not (exposed fx1))
      assertBool "last write notifies voting and ChainSel"     (exposed fx2)
      assertBool "and the want is cleared"                     (isNothing (lookupElection el100 (stWanted st2)))

  , testCase "BEH-Wanting: a validated cert (no prior want) creates an AwaitingBody with the carried sizes" $ do
      let (st, _) = run [ LevCertValidated (AnnouncementTriple el100 (HeaderHash 10) (EbHash 100)) 200 300 ]
      wantStateOf st (EbHash 100) @?= Just (AwaitingBody 200 300)

  , testCase "§4 safety properties hold on the closure-fetch scenario" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) (EbHash 100) ]
      assertBool "want announcement-gated"   (prop_wantAnnouncementGated st)
      assertBool "per-peer active-EB cap"     (prop_perPeerActiveEbCap env st)
      assertBool "offers announced/certified" (prop_offersAnnouncedOrCertified st)
      assertBool "in-flight offered"          (prop_inflightOffered st)

  , testProperty "EbHashMap: invariant + op postconditions hold across random op sequences"
      (withMaxSuccess 2000 prop_ebHashMap)

  , testCase "EbHashMap: per-EB payload combines via <> across references" $ do
      let el1 = Election (Slot 1) (PoolId 0)
          el2 = Election (Slot 2) (PoolId 0)
          el3 = Election (Slot 3) (PoolId 0)
          el4 = Election (Slot 4) (PoolId 0)
          ehA = EbHash 10
          ehB = EbHash 11
          ehC = EbHash 12
          ehD = EbHash 13
          m0 = empty :: EbHashMap [Int]
          m1 = upsert ehA el1 [1] m0
          m2 = upsert ehA el2 [2] m1   -- second election names ehA: incRef combines
          m3 = upsert ehA el1 [3] m2   -- re-upsert same active: adjust combines
          m4 = supersede el1 ehB [9] m3 -- el1 switches to ehB; ehA untouched, now el1's inactive
          m5 = fromMaybe m4 (updateEb ehA (Just . (<> [4])) m4)
          mC = supersede el3 ehC [6] (upsert ehC el3 [5] m5) -- supersede with active==eh combines
          m  = supersede el4 ehD [7] mC                          -- supersede before any upsert
      lookupEb ehA m @?= Just (RefCounts 1 1 [1, 2, 3, 4])  -- el2 active, el1 now inactive
      lookupEb ehB m @?= Just (RefCounts 1 0 [9])
      lookupEb ehC m @?= Just (RefCounts 1 0 [5, 6])
      lookupEb ehD m @?= Just (RefCounts 1 0 [7])
      assertBool "invariant holds" (invariant m)
  ]

-- EbHashMap property test --------------------------------------------------

-- | A random operation over a small key space (so collisions, supersessions,
-- and shared EbHashes across elections are common).
data Op
  = OpInsert    Election EbHash Int
  | OpSupersede Election EbHash Int
  | OpDelete    Election
  | OpPrune     Slot
  deriving Show

genElection :: Gen Election
genElection = Election <$> (Slot . fromIntegral <$> chooseInt (0, 5))
                       <*> (PoolId . fromIntegral <$> chooseInt (0, 2))

genEbHash :: Gen EbHash
genEbHash = EbHash . fromIntegral <$> chooseInt (0, 4)

instance Arbitrary Op where
  arbitrary = oneof
    [ OpInsert    <$> genElection <*> genEbHash <*> arbitrary
    , OpSupersede <$> genElection <*> genEbHash <*> arbitrary
    , OpDelete    <$> genElection
    , OpPrune . Slot . fromIntegral <$> chooseInt (0, 6)
    ]

activeOf :: Election -> EbHashMap a -> Maybe EbHash
activeOf el m = activeRef <$> lookupElection el m

-- | The ops error on misuse; only apply each when its precondition holds.
insertValid :: Election -> EbHash -> EbHashMap a -> Bool
insertValid el eh m = maybe True (== eh) (activeOf el m)

supersedeValid :: Election -> EbHash -> EbHashMap a -> Bool
supersedeValid el _ m = case lookupElection el m of
  Nothing                        -> True   -- cert before announcement
  Just (Refs NoInactiveRefYet _) -> True   -- first cert for this election
  Just (Refs NoInactiveRef   _)  -> False  -- already superseded
  Just (Refs (InactiveRef _) _)  -> False  -- already superseded

apply :: Op -> EbHashMap (Sum Int) -> EbHashMap (Sum Int)
apply op m = case op of
  OpInsert el eh a    -> if insertValid el eh m    then upsert eh el (Sum a) m    else m
  OpSupersede el eh a -> if supersedeValid el eh m then supersede el eh (Sum a) m else m
  OpDelete el         -> deleteElection el m
  OpPrune s           -> pruneElections (\e -> electionSlot e < s) m

electionsOf :: EbHashMap a -> [Election]
electionsOf (EbHashMap _ els) = Map.keys els

postcond :: Op -> EbHashMap (Sum Int) -> EbHashMap (Sum Int) -> Bool
postcond op pre post = case op of
  OpInsert el eh _    -> case lookupElection el pre of   -- a fresh announce sets the active
                           Nothing -> activeOf el post == Just eh
                           _       -> True
  OpSupersede el eh _ -> not (supersedeValid el eh pre) || activeOf el post == Just eh
  OpDelete el         -> isNothing (lookupElection el post)
  OpPrune s           -> all (\e -> not (electionSlot e < s)) (electionsOf post)

prop_ebHashMap :: [Op] -> Property
prop_ebHashMap = go empty
  where
    go _ []         = property True
    go m (op : ops) =
      let m' = apply op m
      in counterexample
           (show op <> "\n  before: " <> show m <> "\n  after:  " <> show m')
           (invariant m' && postcond op m m')
         .&&. go m' ops
