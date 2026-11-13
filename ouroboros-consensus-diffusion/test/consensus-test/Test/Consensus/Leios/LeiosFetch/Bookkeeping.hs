{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Generated valid FIFO scripts against the actual planner/runtime. The
-- oracle reconstructs debt and job multiplicities independently from exchanges.
module Test.Consensus.Leios.LeiosFetch.Bookkeeping (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (BlockIssuer))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM, forM_, void)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Foldable as F
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.NonEmpty as NEIntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Util.IOLike
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()

type Peer = Int

fixtureElectee :: KeyHash BlockIssuer
fixtureElectee = KeyHash (UnsafeHash (SBS.pack (replicate 28 0)))

point :: Word -> Char -> LeiosPoint
point slot tag =
  MkLeiosPoint
    (SlotNo (fromIntegral slot))
    (UnsafeHash (SBS.pack (replicate 32 (fromIntegral (fromEnum tag)))))

txHashOf :: Int -> TxHash
txHashOf n = hashLeiosTx (MkLeiosTx (BS.singleton (fromIntegral n)))

priorityWindow :: Word64
priorityWindow = 10

tests :: TestTree
tests =
  testProperty
    "generated FIFO/receipt/disconnect/prune scripts preserve independent accounting"
    prop_bookkeeping

prop_bookkeeping :: Property
prop_bookkeeping = forAll genSetup $ \setup -> forAll (vectorOf 100 genStep) $ \steps ->
  let (failures, bodies, txs, late) = runSimOrThrow $ do
        ready <- atomically W.newReadiness
        rt <- R.newRuntime propEnv ready (pure False)
        forM_ propPeers $ \peer -> do
          queue <- uncheckedNewTVarM Seq.empty
          void (atomically (R.openClient rt peer (peer == 2) queue (pure Nothing)))
        let go _ _ bad b t l [] = pure (reverse bad, b, t, l)
            go index sim bad b t l (step : rest) = do
              next <- simulate rt (Time (fromIntegral index)) setup sim step
              facts <- atomically (readTVar (R.runtimeFacts rt))
              let errors = violations next facts
                  settled = simSettled next
                  bodyCount = length [() | (_, _, LeiosBlockRequest{}) <- settled]
                  txCount = length settled - bodyCount
              go
                (index + 1)
                next
                (if null errors then bad else (index, errors) : bad)
                (b + bodyCount)
                (t + txCount)
                (l + case step of LateReply{} | not (null settled) -> 1; _ -> 0)
                rest
        go (0 :: Int) (initialSim setup) [] (0 :: Int) (0 :: Int) (0 :: Int) steps
   in checkCoverage $
        cover 95 (bodies > 0) "body service" $
          cover 80 (txs > 0) "TX service" $
            cover 50 (late > 0) "late receipts" $
              tabulate "body completions" [bucket bodies] $
                tabulate "TX completions" [bucket txs] $
                  tabulate "late completions" [bucket late] $
                    counterexample (show (take 1 failures)) (null failures)
 where
  bucket n | n == 0 = "none" | n < 10 = "1..9" | otherwise = "10+"

data Setup = MkSetup
  { setupSeed :: !Int
  , setupOffers :: !(Map.Map Peer (Map.Map LeiosPoint PeerOffer))
  , setupMisses :: !(Map.Map LeiosPoint (IntMap.IntMap (TxHash, BytesSize)))
  -- ^ The transactions each point lacks once its body is held
  }
  deriving Show

-- | One step of the decision loop or of what settles its requests.
data Step
  = -- | Decide, and send the requests.
    Decide
  | -- | One of the body requests in flight is answered, and its write settles.
    BodyReply Int
  | -- | One of the txs requests in flight is answered, and its write settles.
    TxsReply Int
  | -- | A peer disconnects.
    Disconnect Int
  | -- | A request of a disconnected peer settles.
    LateReply Int
  | -- | The immutable tip reaches this slot.
    Prune Word64
  deriving Show

data Sim = MkSim
  { simOutstanding :: !(LeiosOutstanding Peer)
  , simOffers :: !(Map.Map Peer (Map.Map LeiosPoint PeerOffer))
  -- ^ The offers of each connected peer
  , simInFlight :: !(Map.Map Peer [(L.AttemptKey Peer, LeiosFetchRequest)])
  -- ^ The requests in flight from each connected peer
  , simLate :: ![(Peer, L.AttemptKey Peer, LeiosFetchRequest)]
  -- ^ The requests that were in flight when their peer disconnected
  , simSettled :: ![(Peer, L.AttemptKey Peer, LeiosFetchRequest)]
  -- ^ What the last step settled
  }

-- | Small enough that the per-peer budgets and the job and request sizes bind.
propEnv :: LeiosFetchStaticEnv
propEnv =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 32768
    , maxRequestedBytesSizePerBigLedgerPeer = 65536
    , maxJobBytesSize = 1024
    , maxBodiesPerPeer = 4
    , minimumServiceRate = 300000
    , maxJobTxCount = 4
    }

propPeers :: [Peer]
propPeers = [0, 1, 2]

-- | Each in its own slot, so each is its own election's.
propPoints :: [LeiosPoint]
propPoints = zipWith point [1 ..] "abcdef"

genSetup :: Gen Setup
genSetup =
  MkSetup
    <$> arbitrary
    <*> (Map.fromList <$> traverse (\peer -> (,) peer <$> genOffers) propPeers)
    <*> (Map.fromList <$> traverse (\p -> (,) p <$> genMisses) propPoints)
 where
  genOffers = do
    points <- sublistOf propPoints
    Map.fromList <$> traverse (\p -> (,) p . MkPeerOffer SNothing <$> genHalves) points
  genHalves =
    frequency
      [ (1, OfferedBody <$> choose (100, 2000))
      , (1, pure OfferedClosure)
      , (3, OfferedBodyAndClosure <$> choose (100, 2000))
      ]
  genMisses = do
    n <- choose (1, 12)
    sizes <- vectorOf n (choose (2000, 20000))
    pure $ IntMap.fromList [(off, (txHashOf off, sz)) | (off, sz) <- zip [0 ..] sizes]

genStep :: Gen Step
genStep =
  frequency
    [ (10, pure Decide)
    , (8, BodyReply <$> arbitrary)
    , (8, TxsReply <$> arbitrary)
    , (1, Disconnect <$> arbitrary)
    , (1, LateReply <$> arbitrary)
    , -- Never past the last two points.
      (1, Prune <$> choose (0, 5))
    ]

-- | Every point announced, each focusing its own election.
initialSim :: Setup -> Sim
initialSim setup =
  MkSim
    { simOutstanding =
        F.foldl' (flip announce) (emptyLeiosOutstanding (mkStdGen (setupSeed setup)) (SlotNo 0)) propPoints
    , simOffers = setupOffers setup
    , simInFlight = Map.empty
    , simLate = []
    , simSettled = []
    }
 where
  announce p@(MkLeiosPoint slot ebHash) =
    focusElectionIfUnfocused (ElectionId slot fixtureElectee) ebHash
      . recordReferencedPoint p SNothing

simulate :: R.Runtime (IOSim s) Peer -> Time -> Setup -> Sim -> Step -> IOSim s Sim
simulate rt now setup original step = case step of
  Decide -> do
    let out = simOutstanding sim
    atomically (modifyTVar (R.runtimeFacts rt) (R.expireHeads now (`workStillNeeded` out)))
    updated <-
      assignRequests
        rt
        now
        (const (Right maxBound))
        (Just (SlotNo 0, priorityWindow))
        Map.empty
        (simOffers sim)
        out
    sessions <- atomically (R.sessions <$> readTVar (R.runtimeFacts rt))
    sent <- foldM (flush rt) Map.empty (Map.toList sessions)
    pure sim{simOutstanding = updated, simInFlight = Map.unionWith (<>) (simInFlight sim) sent}
  BodyReply index -> reply index True
  TxsReply index -> reply index False
  Disconnect index -> case pickIndex index (Map.keys (simOffers sim)) of
    Nothing -> pure sim
    Just (peer, _) -> do
      atomically (R.closePeer rt peer)
      pure
        sim
          { simOutstanding = removePeerFromOutstanding peer (simOutstanding sim)
          , simOffers = Map.delete peer (simOffers sim)
          , simInFlight = Map.delete peer (simInFlight sim)
          , simLate =
              [(peer, key, req) | (key, req) <- Map.findWithDefault [] peer (simInFlight sim)] ++ simLate sim
          }
  LateReply index -> case pickIndex index (simLate sim) of
    Nothing -> pure sim
    Just ((peer, key, req), rest) -> pure (settle peer key req sim{simLate = rest})
  Prune slot -> pure sim{simOutstanding = pruneOutstandingToImmTip (SlotNo slot) (simOutstanding sim)}
 where
  sim = original{simSettled = []}
  flush runtime accumulated (peer, session) = do
    let client = R.Client runtime peer (R.invocation session)
        go results =
          atomically (R.takeRequest client) >>= \case
            Nothing -> pure (Map.insert peer (reverse results) accumulated)
            Just (key, req) -> do
              atomically $ R.commitRequest client key >>= check >> R.observeSend client now
              go ((key, req) : results)
    go []
  reply index body = case pickIndex
    index
    [ (peer, key, req, rest)
    | (peer, (key, req) : rest) <- Map.toList (simInFlight sim)
    , isBodyRequest req == body
    ] of
    Nothing -> pure sim
    Just ((peer, key, req, rest), _) -> do
      facts <- atomically (readTVar (R.runtimeFacts rt))
      case Map.lookup peer (R.sessions facts) of
        Nothing -> error "in-flight request without session"
        Just session -> do
          let client = R.Client rt peer (R.invocation session)
          atomically $ do
            R.installReceiver client key
            R.completeWire client key now
            whenNeeded <- R.claimUseful rt key
            if whenNeeded then R.acceptReply rt key else pure ()
            R.finishHandler client key now
            R.settleReceipt rt key
          pure (settle peer key req sim{simInFlight = Map.insert peer rest (simInFlight sim)})
  settle peer key req state =
    state
      { simOutstanding = case req of
          LeiosBlockRequest body ->
            acquireEbBody
              (lbrPoint body)
              (newLeiosJobPool propEnv (Map.findWithDefault IntMap.empty (lbrPoint body) (setupMisses setup)))
              (simOutstanding state)
          LeiosBlockTxsRequest tx@(MkLeiosBlockTxsRequest _ jobs) ->
            settleTxRequest
              peer
              tx
              (foldMap (\(Jobs.MkLeiosJob offsets _ _) -> offsets) jobs)
              (simOutstanding state)
      , simSettled = [(peer, key, req)]
      }

isBodyRequest :: LeiosFetchRequest -> Bool
isBodyRequest LeiosBlockRequest{} = True
isBodyRequest _ = False

requestBytes :: LeiosFetchRequest -> Word64
requestBytes = fromIntegral . R.bytesOf

pickIndex :: Int -> [a] -> Maybe (a, [a])
pickIndex index values = go (index `mod` max 1 (length values)) values
 where
  go _ [] = Nothing
  go 0 (value : rest) = Just (value, rest)
  go n (value : rest) = fmap (value :) <$> go (n - 1) rest

violations :: Sim -> R.RuntimeState (IOSim s) Peer -> [String]
violations sim facts = concatMap peerErrors propPeers ++ concatMap poolErrors (Map.toList (ebState out))
 where
  out = simOutstanding sim
  inFlight peer = Map.findWithDefault [] peer (simInFlight sim)
  held peer = Map.map NEIntSet.toSet (Map.findWithDefault Map.empty peer (requestedJobsPerPeer out))
  peerErrors peer =
    let requests = map snd (inFlight peer)
        expected =
          L.Usage
            (sum [requestBytes req | req@LeiosBlockTxsRequest{} <- requests])
            (fromIntegral (length [() | LeiosBlockRequest{} <- requests]))
        used = L.usage peer (R.ledger facts)
        cap =
          if peer == 2
            then maxRequestedBytesSizePerBigLedgerPeer propEnv
            else maxRequestedBytesSizePerPeer propEnv
        jobs =
          Map.fromListWith
            IntSet.union
            [ (p, NEIntSet.toSet (NEIntMap.keysSet js))
            | LeiosBlockTxsRequest (MkLeiosBlockTxsRequest p js) <- requests
            ]
     in ["physical debt disagrees with exchanges at " ++ show peer | used /= expected]
          ++ [ "credit overshoot at " ++ show peer
             | L.transactionBytes used > fromIntegral cap || L.bodyRequests used > maxBodiesPerPeer propEnv
             ]
          ++ ["logical job ownership disagrees with exchanges at " ++ show peer | held peer /= jobs]
          ++ [ "attempt key missing physical lease"
             | (key, _) <- inFlight peer
             , Map.notMember key (L.leases (R.ledger facts))
             ]
  poolErrors (p, MkEbState _ state) = case state of
    BodyAcquired pool ->
      ["inexact job reverse index" | not (Jobs.hasExactReverseIndex pool)]
        ++ [ "wrong multiplicity " ++ show (p, jid, multiplicity)
           | (jid, multiplicity) <- IntMap.toList (Jobs.jobMultiplicities pool)
           , multiplicity
               /= length [() | peer <- propPeers, maybe False (IntSet.member jid) (Map.lookup p (held peer))]
           ]
    _ -> []
