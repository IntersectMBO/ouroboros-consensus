module Main (main) where

import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Test.Tasty
import           Test.Tasty.HUnit

import           RefModel

main :: IO ()
main = defaultMain tests

env :: Env
env = Env 5 2 1000000 65536 30 300 600 (Slot 0)

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

offer :: Peer -> Election -> EbHash -> Stimulus
offer p el eh = LevWiredMsg p (MsgLeiosBlockOffer el eh)

txsOffer :: Peer -> Election -> EbHash -> Stimulus
txsOffer p el eh = LevWiredMsg p (MsgLeiosBlockTxsOffer el eh)

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
  [ testCase "BEH-PeerChurn: LevPeerAdd registers an Active peer, no effects" $ do
      let (st, fx) = run [LevPeerAdd (Peer 1) StakeSampled]
      fx @?= []
      fmap peerPhase (Map.lookup (Peer 1) (stPeerPresent st)) @?= Just Active

  , testCase "BEH-Wanting: an announcement gates the want (AwaitingBody), no fetch yet" $ do
      let (st, fx) = run [LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100]
      Map.lookup el100 (stWanted st) @?= Just (AwaitingBody (EbHash 100) 200 300)
      assertBool "no request before an offer" (not (any isReq fx))

  , testCase "BEH-Offers + BEH-BodyFetch: an offer for the first-announced EB issues ReqBody" $ do
      let (_, fx) = run [LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)]
      assertBool "sends MsgLeiosBlockRequest" (Send (Peer 1) (MsgLeiosBlockRequest (EbHash 100)) `elem` fx)

  , testCase "BEH-ChunkJobs + BEH-ClosureFetch: body then closure-offer requests the chunked job's txs" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                         , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) el100 (EbHash 100) ]
      [ txs | Send _ (MsgLeiosBlockTxsRequest _ txs) <- fx ] @?= [NE.fromList [TxHash 1, TxHash 2]]
      assertBool "want advanced to AwaitingTxs"
        (case Map.lookup el100 (stWanted st) of Just AwaitingTxs{} -> True; _ -> False)

  , testCase "Job: jobBytes is the real summed tx size" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100) ]
      jobBytes st el100 (EbHash 100) (JobId 0) @?= 300

  , testCase "BEH-NotifyServe: a dequeue ships the buffered announcement and opens the gate" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                         , credit (Peer 2), ann (Peer 1) hdr100, dequeue (Peer 2) ]
      assertBool "relays the announcement to Peer 2" (Send (Peer 2) (MsgLeiosBlockAnnouncement hdr100) `elem` fx)
      (Map.lookup (Peer 2) (stPeerOfferGates st) >>= Map.lookup el100) @?= Just (EbHash 100)

  , testCase "BEH-NotifyServe: an enqueue that grows the queue emits NotifyEnqueue" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                        , credit (Peer 2), ann (Peer 1) hdr100 ]
      length [ () | NotifyEnqueue (Peer 2) <- fx ] @?= 1

  , testCase "BEH-NotifyServe back-pressure: with no credit nothing is buffered and no NotifyEnqueue fires" $ do
      let (st, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled, ann (Peer 1) hdr100 ]
          q = maybe Set.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
      assertBool "queue empty without a credit" (Set.null q)
      assertBool "no NotifyEnqueue for Peer 2"  (null [ () | NotifyEnqueue (Peer 2) <- fx ])

  , testCase "BEH-NotifyServe back-pressure: a full queue evicts the notifyPriority-min, and only the enqueue with an unoccupied credit emits NotifyEnqueue" $ do
      let setup ps  = run ([ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled, credit (Peer 2) ] ++ ps)
          qOf (st, _)   = maybe Set.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
          enqOf (_, fx) = length [ () | NotifyEnqueue (Peer 2) <- fx ]
          stalerFirst  = setup [ ann (Peer 1) hdr100, ann (Peer 1) hdr200 ]
          fresherFirst = setup [ ann (Peer 1) hdr200, ann (Peer 1) hdr100 ]
      qOf stalerFirst    @?= Set.singleton (NotifyAnnouncement hdr200)
      qOf fresherFirst   @?= Set.singleton (NotifyAnnouncement hdr200)
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
      assertBool "stale notification discarded" (maybe True (Set.null . fst) (Map.lookup (Peer 2) (stPeerNotifyQueue st1)))

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
      Map.lookup el100 (stWanted st) @?= Just (AwaitingBody (EbHash 100) 200 300)

  , testCase "BEH-Offers: an offer for an equivocating (non-first) EB disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                        , LevWiredMsg (Peer 1) (MsgLeiosBlockEquivocationProof Nothing hdr101)
                        , offer (Peer 1) el100 (EbHash 101) ]
      assertBool "disconnects (unannounced offer)" (Disconnect (Peer 1) UnannouncedOffer `elem` fx)

  , testCase "BEH-Responses: a body whose closure size mismatches the announcement disconnects" $ do
      let badBody = Body (EbHash 100) [TxRef (TxHash 1) 1] 200
          (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) badBody) ]
      assertBool "disconnects with BodyMismatch" (Disconnect (Peer 1) BodyMismatch `elem` fx)

  , testCase "BEH-Responses: an unsolicited body (no matching in-flight request) disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100) ]
      assertBool "disconnects with UnsolicitedResponse" (Disconnect (Peer 1) UnsolicitedResponse `elem` fx)

  , testCase "BEH-Responses: a tx-closure reply in the wrong order disconnects" $ do
      let (_, fx) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) el100 (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlockTxs (EbHash 100) [Tx (TxHash 2) 150, Tx (TxHash 1) 150]) ]
      assertBool "disconnects with TxsMismatch" (Disconnect (Peer 1) TxsMismatch `elem` fx)

  , testCase "BEH-FetchServe: a tx request is served with the requested txs in order" $ do
      let (_, fx) = runWith servingIfaces
                      [ LevPeerAdd (Peer 2) PeerSharingSampled
                      , LevWiredMsg (Peer 2) (MsgLeiosBlockTxsRequest (EbHash 100) (NE.fromList [TxHash 1, TxHash 2])) ]
      [ ts | Send _ (MsgLeiosBlockTxs _ ts) <- fx ] @?= [[Tx (TxHash 1) 1, Tx (TxHash 2) 1]]

  , testCase "BEH-Completion: finishing a body write enqueues a body offer to gated peers" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, LevPeerAdd (Peer 2) PeerSharingSampled
                        , credit (Peer 2), ann (Peer 1) hdr100, dequeue (Peer 2), credit (Peer 2)
                        , LevDiskDone (WriteBody el100 body100) ]
          q = maybe Set.empty fst (Map.lookup (Peer 2) (stPeerNotifyQueue st))
      assertBool "body offer enqueued" (Set.member (NotifyBlockOffer el100 (EbHash 100)) q)

  , testCase "BEH-Completion: voting + ChainSel are notified only when the last write lands (persist-gated)" $ do
      let txs    = [Tx (TxHash 1) 150, Tx (TxHash 2) 150]
          setup  = [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                   , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) el100 (EbHash 100)
                   , LevWiredMsg (Peer 1) (MsgLeiosBlockTxs (EbHash 100) txs) ]
          exposed fx = NotifyVotingAndChainSel el100 (EbHash 100) `elem` fx
          (_, fx1)   = run (setup ++ [ LevDiskDone (WriteBody el100 body100) ])
          (st2, fx2) = run (setup ++ [ LevDiskDone (WriteBody el100 body100)
                                     , LevDiskDone (WriteClosure el100 (EbHash 100) txs) ])
      assertBool "fetch-complete + first write expose nothing" (not (exposed fx1))
      assertBool "last write notifies voting and ChainSel"     (exposed fx2)
      assertBool "and the want is cleared"                     (not (Map.member el100 (stWanted st2)))

  , testCase "BEH-Wanting: a validated cert (no prior want) creates an AwaitingBody with the carried sizes" $ do
      let (st, _) = run [ LevCertValidated (AnnouncementTriple el100 (HeaderHash 10) (EbHash 100)) 200 300 ]
      Map.lookup el100 (stWanted st) @?= Just (AwaitingBody (EbHash 100) 200 300)

  , testCase "§4 safety properties hold on the closure-fetch scenario" $ do
      let (st, _) = run [ LevPeerAdd (Peer 1) StakeSampled, ann (Peer 1) hdr100, offer (Peer 1) el100 (EbHash 100)
                        , LevWiredMsg (Peer 1) (MsgLeiosBlock (EbHash 100) body100), txsOffer (Peer 1) el100 (EbHash 100) ]
      assertBool "want announcement-gated"   (prop_wantAnnouncementGated st)
      assertBool "per-peer active-EB cap"     (prop_perPeerActiveEbCap env st)
      assertBool "offers announced/certified" (prop_offersAnnouncedOrCertified st)
      assertBool "in-flight offered"          (prop_inflightOffered st)
  ]
