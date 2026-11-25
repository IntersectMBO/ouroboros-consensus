{-# LANGUAGE TypeApplications #-}

-- | Tests of how the ChainDB and the LeiosDB interact: which endorser blocks the
-- ChainDB has the LeiosDB keep, once the blocks that announce and certify them
-- are immutable, and when ChainSel may select a block certifying one.
module Test.Consensus.Leios.ChainDB (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios (VerificationError (..))
import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (forM_, unless, void)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Block
  ( Point
  , Proxy (..)
  , SlotNo
  , WithOrigin (..)
  , blockHash
  , blockPoint
  , blockSlot
  , castPoint
  )
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..), ledgerState)
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisition (..)
  , LeiosAcquisitions (..)
  )
import Ouroboros.Consensus.Leios.Cert (LeiosCertRejection (..), LeiosClaim (..))
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( LeiosFetchClientContext (..)
  , ReplyOwnership (..)
  , processLeiosBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( EbFetchState (..)
  , EbState (..)
  , ebState
  , isFocusedPoint
  , recordReferencedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel (..)
  , LeiosPeerLifetimeId (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request (LeiosBlockRequest (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork as Owned
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  )
import Ouroboros.Consensus.Util.IOLike
  ( check
  , finally
  , modifyMVar_
  , poll
  , readMVar
  , wait
  , withAsync
  )
import Ouroboros.Consensus.Util.STM (forgetFingerprint)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Fixtures
import Test.Consensus.Leios.Committee (certOf)
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "ChainDB and LeiosDB"
    [ testCase "ChainSel rejects a block whose certificate a minority made" $
        runSimOrThrow
          (whyInvalid (\node -> addBlocks node [announcingA, minorityCertifyingA]) minorityCertifyingA)
          @?= Just (ExtValidationErrorLeiosCert (LeiosCertInvalid (InsufficientWeight (1 / 4))))
    , testCase "ChainSel does not verify a certificate again for a claim it established" $
        runSimOrThrow (whyInvalid addSibling minoritySibling) @?= Nothing
    , testCase "a restart forgets a certifying block off the selection, until it arrives again" $
        runSimOrThrow (heldAcrossRestart BodyOfA) @?= (True, False, True)
    , testCase "a restart keeps a certifying block on the selection" $
        runSimOrThrow (heldAcrossRestart ClosureOfA) @?= (True, True, True)
    , testCase "the GC keeps an endorser block an immutable block certifies" $
        fst (runSimOrThrow bodiesAfterGc)
          @?= Just (serialiseEndorserBlock ebA)
    , testCase "the GC evicts an endorser block no block certifies" $
        snd (runSimOrThrow bodiesAfterGc) @?= Nothing
    , testCase "ChainSel parks a certifying block until the closure arrives" $
        runSimOrThrow tipsAroundClosure
          @?= (blockPoint announcingA, blockPoint certifyingA)
    , testCase "ChainSel selects a certifying block whose closure the LeiosDB held at start" $
        runSimOrThrow tipWithClosureBeforeStart @?= blockPoint certifyingA
    , testCase
        "after a crash, the ImmutableDB drops a block certifying an endorser block the LeiosDB lost"
        $ runSimOrThrow (afterCrash immutableChain pointA LostA) @?= (blockPoint announcingA, False)
    , testCase
        "after a crash, the ImmutableDB keeps a block certifying an endorser block the LeiosDB holds"
        $ runSimOrThrow (afterCrash immutableChain pointA KeptA)
          @?= (blockPoint (immutableChain !! 5), True)
    , testCase
        "after a crash, the ImmutableDB drops a block starting a chunk that certifies an endorser block the LeiosDB lost"
        $ runSimOrThrow (afterCrash chunkStartChain chunkStartPointA LostA)
          @?= (blockPoint chunkStartAnnouncingA, False)
    , testCase
        "after a crash, the ImmutableDB keeps a block starting a chunk that certifies an endorser block the LeiosDB holds"
        $ runSimOrThrow (afterCrash chunkStartChain chunkStartPointA KeptA)
          @?= (blockPoint (chunkStartChain !! 13), True)
    , testCase
        "after losing an endorser block's row, the LeiosDB drops the later ones and takes it again"
        $ runSimOrThrow lostRowBeforeAnother @?= (blockPoint announcingA, False, True)
    , testGroup "verified claim publication"
        [ testCase "new conflicting claims publish in order and an exact earlier claim does not refocus" $
            runSimOrThrow certifiedPublicationOrder @?= True
        , testCase "pruning and processed retired block arrivals publish no replacement event" $
            runSimOrThrow certifiedPublicationPrune @?= True
        , testCase "retrying the real callback publishes nothing before its transaction commits" $
            runSimOrThrow certifiedPublicationRollback @?= True
        , testCase "two blocked body writers do not block certificate publication or refocusing" $
            runSimOrThrow certifiedPublicationPressure @?= True
        ]
    , testCase "a verified certificate has LeiosFetch fetch the endorser block it certifies" $
        runSimOrThrow certifiedIsFetched @?= ([(blockSlot announcingA, pointEbHash pointA)], True)
    ]

-- | The node adds 'announcingA' and 'certifyingA', whose certificate ChainSel
-- verifies. For each new valid claim the ChainDB then passes on, its slot and
-- the endorser block its announcement names; and whether LeiosFetch is
-- fetching A.
certifiedIsFetched :: IOSim s ([(SlotNo, EndorserBlockHash)], Bool)
certifiedIsFetched = withNode $ \node -> do
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  addBlocks node [announcingA, certifyingA]
  threadDelay 1
  validClaims <- atomically $ nodeNewLeiosValidClaims node
  outstanding <- readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nodeKernel node))))
  pure
    ( [ (leiosClaimSlot claim, announcementEbHash ann)
      | (claim, ann) <- Map.toList validClaims
      ]
    , isFocusedPoint pointA outstanding
    )

-- Observe committed callback invocations independently of the sole focuser.
-- Both claims in this scenario are genuinely validated by ChainSel, using
-- equivocal test-committee signatures for the same election on two branches.
certifiedPublicationOrder :: IOSim s Bool
certifiedPublicationOrder = do
  events <- S.newTVarIO []
  let observe claim ann = S.modifyTVar events (++ [(claim, ann)])
      rival = setAnnouncement (pointEbHash pointB) (encodeEndorserBlockSize ebB) (forkLeiosBlock announcingA)
      certifiedRival = setLeiosCert (certOf (rbHashOf rival) [0, 1, 2]) (successorLeiosBlock rival)
      rivalPoint = MkLeiosPoint (blockSlot rival) (pointEbHash pointB)
      claimA = LeiosClaim (blockSlot announcingA) (rbHashOf announcingA)
      claimB = LeiosClaim (blockSlot rival) (rbHashOf rival)
  withNodeObservingClaims observe $ \node -> do
    threadDelay 20
    addBlocks node [announcingA, certifyingA]
    first <- SI.timeout 1 (awaitNodeFocus node pointA)
    addBlocks node [rival, certifiedRival]
    second <- SI.timeout 1 (awaitNodeFocus node rivalPoint)
    -- Another certifying RB makes exactly A's claim after B was established.
    addBlocksAfter node (NotOrigin announcingA) [minoritySibling]
    threadDelay 0.001
    published <- S.readTVarIO events
    out <- readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nodeKernel node))))
    isInvalid <- atomically $ forgetFingerprint <$> ChainDB.getIsInvalidBlock (nodeChainDB node)
    pure
      ( first == Just () && second == Just ()
          && map fst published == [claimA, claimB]
          && map (announcementEbHash . snd) published == [pointEbHash pointA, pointEbHash rivalPoint]
          && isFocusedPoint rivalPoint out
          && isNothing (isInvalid (blockHash minoritySibling))
      )

-- Poll only the state predicate. Consuming the planner's coalesced readiness
-- signal here would steal one of its wakeups and would change the scenario.
awaitNodeFocus :: NodeUnderTest (IOSim s) -> LeiosPoint -> IOSim s ()
awaitNodeFocus node point = do
  out <- readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nodeKernel node))))
  unless (isFocusedPoint point out) $ threadDelay 0.001 >> awaitNodeFocus node point

certifiedPublicationPrune :: IOSim s Bool
certifiedPublicationPrune = do
  events <- S.newTVarIO []
  db <- newLeiosDBInMemory
  withWriter db $ \writer ->
    void $ await =<< writeEbBody writer pointA ebA mempty (Map.singleton 0 txA)
  withNodeOverObservingClaims db (\claim ann -> S.modifyTVar events (++ [(claim, ann)])) $ \node -> do
    threadDelay 30
    addBlocks node [announcingA, certifyingA]
    addBlocksAfter node (NotOrigin announcingA) [minoritySibling]
    before <- S.readTVarIO events
    addBlocks node immutableChain
    -- The real background copier advances and prunes the claim store.
    threadDelay 100
    immutableSlot <- atomically $ getTipSlot . ledgerState <$> ChainDB.getImmutableLedger (nodeChainDB node)
    -- A distinct old block is processed after the fence advanced. This does
    -- not claim it reaches insertion: pure store tests check the insertion guard.
    let retired = setLeiosCert (certOf (rbHashOf announcingA) [0, 1, 2]) (forkLeiosBlock (forkLeiosBlock certifyingA))
    addBlocksAfter node (NotOrigin announcingA) [retired]
    afterPruning <- S.readTVarIO events
    pure
      ( map fst before == [LeiosClaim (blockSlot announcingA) (rbHashOf announcingA)]
          && before == afterPruning
          && immutableSlot > NotOrigin (blockSlot announcingA)
      )

-- The fixture observer is after the real queue/map writes in the very same
-- callback transaction. Retrying rolls them back, without a duplicate test
-- implementation of ChainSel's insert-and-publish transaction.
certifiedPublicationRollback :: IOSim s Bool
certifiedPublicationRollback = do
  released <- S.newTVarIO False
  events <- S.newTVarIO []
  let observe claim ann = do
        S.modifyTVar events (++ [(claim, ann)])
        S.readTVar released >>= check
  withNodeObservingClaims observe $ \node -> do
    threadDelay 20
    addBlocks node [announcingA]
    withAsync (addBlocksAfter node (NotOrigin announcingA) [certifyingA]) $ \adding -> do
      -- With no future-block wait and a known valid certificate, IOSim runs
      -- the worker to quiescence at the fixture's closed callback gate.
      threadDelay 0.01
      processing <- poll adding
      fetched <- atomically $ ($ blockPoint certifyingA) <$> ChainDB.getIsFetched (nodeChainDB node)
      unpublished <- S.readTVarIO events
      unrecorded <- atomically $ nodeNewLeiosValidClaims node
      atomically $ S.writeTVar released True
      wait adding
      published <- S.readTVarIO events
      pure
        ( isNothing processing && fetched
            && null unpublished && Map.null unrecorded
            && map fst published == [LeiosClaim (blockSlot announcingA) (rbHashOf announcingA)]
        )

certifiedPublicationPressure :: IOSim s Bool
certifiedPublicationPressure = do
  db <- newLeiosDBInMemory
  entered <- S.newTVarIO (0 :: Int)
  release <- S.newEmptyTMVarIO
  events <- S.newTVarIO []
  let wrapWriter writer =
        writer
          { writeEbBody = \point body fills given -> do
              atomically $ S.modifyTVar entered (+ 1)
              atomically $ S.readTMVar release
              writeEbBody writer point body fills given
          }
      wrapped = db{openWriter = wrapWriter <$> openWriter db}
      observe claim ann = S.modifyTVar events (++ [(claim, ann)])
      hash = endorserBlockHash ebA
      first = MkLeiosPoint 50 hash
      second = MkLeiosPoint 51 hash
      announced = setAnnouncement hash (encodeEndorserBlockSize ebA) (firstLeiosBlock 10)
      certified = setLeiosCert (certOf (rbHashOf announced) [0, 1, 2]) (successorLeiosBlock announced)
      target = MkLeiosPoint (blockSlot announced) hash
      laterAnnounced =
        setSlot (blockSlot announced + 1) $
          setAnnouncement hash (encodeEndorserBlockSize ebA) (firstLeiosBlock 11)
      laterCertified =
        setLeiosCert (certOf (rbHashOf laterAnnounced) [0, 1, 2]) (successorLeiosBlock laterAnnounced)
      laterTarget = MkLeiosPoint (blockSlot laterAnnounced) hash
  withNodeOverObservingClaims wrapped observe $ \node -> do
    let kernel = getLeiosFetchKernel (nodeKernel node)
        ctx = lfkContext kernel
        peer = MkLeiosPeerLifetimeId (ConnectionId (PeerAddr (-1)) (PeerAddr (-2))) 0
        receive point =
          processLeiosBlock
            ReplyOwnership
              { claimUsefulReply = pure True
              , claimBodyReply = Fixtures.claimReceivedBodyFrom (lfcRuntime ctx) peer point
              , acceptedReply = \_ -> pure (pure ())
              , bodyWon = const (pure ())
              , settledReceipt = \_ -> pure (pure ())
              , abandonUnsubmitted = pure ()
              , abandonAccepted = pure ()
              }
            ctx peer
            (MkLeiosBlockRequest point (encodeEndorserBlockSize ebA) maxBound)
            (serialiseEndorserBlock ebA)
    modifyMVar_ (lfcOutstanding ctx) $ pure . recordReferencedPoint first SNothing . recordReferencedPoint second SNothing
    receive first
    receive second
    atomically $ S.readTVar entered >>= check . (== 2)
    available <- atomically $ Owned.canSubmitWork (lfcBodyWork ctx) target
    threadDelay 20
    progress <-
      ( SI.timeout 1 $ do
          addBlocks node [announced, certified]
          awaitNodeFocus node target
          -- This claim is published only after the first batch changed focus.
          -- A consumer parked on body-capacity admission cannot focus it.
          addBlocks node [laterAnnounced, laterCertified]
          awaitNodeFocus node laterTarget
          published <- S.readTVarIO events
          out <- readMVar (lfcOutstanding ctx)
          count <- S.readTVarIO entered
          pure
            ( not available && count == 2
                && map fst published
                  == [ LeiosClaim (blockSlot announced) (rbHashOf announced)
                     , LeiosClaim (blockSlot laterAnnounced) (rbHashOf laterAnnounced)
                     ]
                && all
                  ( \point -> case Map.lookup point (ebState out) of
                      Just (MkEbState _ NoBody) -> True
                      _ -> False
                  )
                  [target, laterTarget]
            )
      ) `finally` atomically (S.putTMVar release ())
    pure (progress == Just True)

-- | The node holds endorser blocks A and B, with their transactions. The
-- chain's first block announces A, its second certifies A, and its third
-- announces B, which nothing certifies. Once the immutable tip has passed both
-- announcements and the GC has run, these are the bodies of A and B that the
-- LeiosDB still holds.
bodiesAfterGc :: IOSim s (Maybe (Serialised EndorserBlock), Maybe (Serialised EndorserBlock))
bodiesAfterGc = do
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer ->
    forM_ [(pointA, ebA, txA), (pointB, ebB, txB)] $ \(point, eb, tx) -> do
      void $ await =<< writeEbBody writer point eb mempty mempty
      void $ await =<< writeTxs writer point (Map.singleton 0 tx)
  withNodeOver leiosDb $ \node -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    addBlocks node immutableChain
    -- Long enough for the copy to the ImmutableDB and for the delayed GC.
    threadDelay 100
    withReader leiosDb $ \reader ->
      (,)
        <$> lookupEbBody reader pointA
        <*> lookupEbBody reader pointB

-- | Sixteen blocks: as k is 10, the immutable tip is the sixth, past the
-- announcements of A and B, and past the block certifying A.
immutableChain :: [LeiosTestBlock]
immutableChain = take 16 $ announcingA : certifyingA : iterate successorLeiosBlock announcingB

-- | What the LeiosDB holds of A after a crash
data AOnRestart
  = -- | Nothing
    LostA
  | -- | Its promoted closure
    KeptA

-- | The node adds the chain, which certifies A at the given point, over a
-- LeiosDB holding A's closure there, so A is promoted. Then it restarts after a
-- crash over a LeiosDB holding the given part of A. Its ImmutableDB tip after
-- the restart, and whether the LeiosDB then holds A's closure.
afterCrash ::
  [LeiosTestBlock] -> LeiosPoint -> AOnRestart -> IOSim s (Point LeiosTestBlock, Bool)
afterCrash chain point aOnRestart = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  writeClosureOfA leiosDb
  withNodeOverDBs nodeDBs leiosDb $ \node -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 30
    addBlocks node chain
    -- Long enough for the copy to the ImmutableDB and for the delayed GC.
    threadDelay 100
  leiosDb' <- case aOnRestart of
    KeptA -> pure leiosDb
    LostA -> newLeiosDBInMemory
  withNodeOverDBsAfterUncleanShutdown nodeDBs leiosDb' $ \node ->
    (,)
      <$> atomically (castPoint . AF.anchorPoint <$> ChainDB.getCurrentChain (nodeChainDB node))
      <*> withReader leiosDb' (\reader -> isJust <$> lookupEbClosure reader point)
 where
  writeClosureOfA db =
    withWriter db $ \writer -> do
      void $ await =<< writeEbBody writer point ebA mempty mempty
      void $ await =<< writeTxs writer point (Map.singleton 0 txA)

-- | The node adds 'twoCertifiedChain' over a LeiosDB holding the closures of
-- A and B, so both are promoted, and restarts after a crash over a LeiosDB that
-- lost A's row but kept B's promoted one, as from a damaged file. The
-- ImmutableDB tip after the restart, whether the immutable partition still
-- holds B, and whether A's closure lands when fetched again.
lostRowBeforeAnother :: IOSim s (Point LeiosTestBlock, Bool, Bool)
lostRowBeforeAnother = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  writeClosure leiosDb pointA ebA txA
  writeClosure leiosDb pointB ebB txB
  withNodeOverDBs nodeDBs leiosDb $ \node -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 30
    addBlocks node twoCertifiedChain
    -- Long enough for the copy to the ImmutableDB and for the delayed GC.
    threadDelay 100
  damaged <- newLeiosDBInMemory
  writeClosure damaged pointB ebB txB
  leiosDbPromoteToImmutable damaged (blockSlot certifyingB) [pointB]
  withNodeOverDBsAfterUncleanShutdown nodeDBs damaged $ \node -> do
    immTip <- atomically (castPoint . AF.anchorPoint <$> ChainDB.getCurrentChain (nodeChainDB node))
    keptB <- withReader damaged (\reader -> isImmutable reader pointB)
    writeClosure damaged pointA ebA txA
    landedA <- withReader damaged (\reader -> isJust <$> lookupEbClosure reader pointA)
    pure (immTip, keptB, landedA)
 where
  writeClosure db point eb tx =
    withWriter db $ \writer -> do
      void $ await =<< writeEbBody writer point eb mempty mempty
      void $ await =<< writeTxs writer point (Map.singleton 0 tx)

-- | 'immutableChain', but its fourth block certifies B. With sixteen blocks,
-- the immutable tip is past both certificates.
twoCertifiedChain :: [LeiosTestBlock]
twoCertifiedChain =
  [announcingA, certifyingA, announcingB, certifyingB]
    ++ take 12 (drop 1 (iterate successorLeiosBlock certifyingB))

certifyingB :: LeiosTestBlock
certifyingB =
  setLeiosCert (certOf (rbHashOf announcingB) [0, 1, 2]) (successorLeiosBlock announcingB)

-- | 'immutableChain', but announcing A in slot 9 and certifying it in slot 10,
-- where the ImmutableDB's second chunk starts, as its chunks span ten slots.
-- With 24 blocks, the immutable tip is in slot 14.
chunkStartChain :: [LeiosTestBlock]
chunkStartChain =
  prefix
    ++ [chunkStartAnnouncingA, certifying]
    ++ take 14 (drop 1 (iterate successorLeiosBlock certifying))
 where
  prefix = take 8 (iterate successorLeiosBlock (firstLeiosBlock 2))
  certifying =
    setLeiosCert
      (certOf (rbHashOf chunkStartAnnouncingA) [0, 1, 2])
      (successorLeiosBlock chunkStartAnnouncingA)

-- | The block of 'chunkStartChain' announcing A, in slot 9.
chunkStartAnnouncingA :: LeiosTestBlock
chunkStartAnnouncingA =
  setAnnouncement
    (pointEbHash pointA)
    (encodeEndorserBlockSize ebA)
    (iterate successorLeiosBlock (firstLeiosBlock 2) !! 8)

chunkStartPointA :: LeiosPoint
chunkStartPointA = pointA{pointSlotNo = blockSlot chunkStartAnnouncingA}

-- | Add the blocks with the given action, then report why the ChainDB judges
-- the given block invalid, if it does.
whyInvalid ::
  (NodeUnderTest (IOSim s) -> IOSim s ()) ->
  LeiosTestBlock ->
  IOSim s (Maybe (ExtValidationError LeiosTestBlock))
whyInvalid add blk = withNode $ \node -> do
  -- Let the slots of the blocks pass, so none is from the future.
  threadDelay 20
  add node
  threadDelay 1
  isInvalid <- atomically $ forgetFingerprint <$> ChainDB.getIsInvalidBlock (nodeChainDB node)
  pure $ isInvalid (blockHash blk)

-- | The node adds 'announcingA' and 'certifyingA', restarts, and then adds
-- 'certifyingA' again. Whether the ChainDB holds 'certifyingA' before the
-- restart, after it, and after the second add.
--
-- Without A's transactions, 'certifyingA' is parked and so not on the
-- selection.
heldAcrossRestart :: WhatOfA -> IOSim s (Bool, Bool, Bool)
heldAcrossRestart whatOfA = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer -> do
    void $ await =<< writeEbBody writer pointA ebA mempty mempty
    case whatOfA of
      BodyOfA -> pure ()
      ClosureOfA ->
        void $
          await
            =<< writeTxs
              writer
              pointA
              (Map.singleton 0 txA)
  beforeRestart <- withNodeOverDBs nodeDBs leiosDb $ \node -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    addBlocks node [announcingA, certifyingA]
    threadDelay 1
    holds node
  withNodeOverDBs nodeDBs leiosDb $ \node -> do
    afterRestart <- holds node
    addBlocksAfter node (NotOrigin announcingA) [certifyingA]
    threadDelay 1
    afterAddingAgain <- holds node
    pure (beforeRestart, afterRestart, afterAddingAgain)
 where
  holds node = atomically $ ($ blockPoint certifyingA) <$> ChainDB.getIsFetched (nodeChainDB node)

-- | What the LeiosDB holds of A
data WhatOfA
  = -- | Its body, but not its transactions
    BodyOfA
  | -- | Its body and its transactions
    ClosureOfA

-- | 'certifyingA', except a minority made its certificate.
minorityCertifyingA :: LeiosTestBlock
minorityCertifyingA =
  setLeiosCert (certOf (rbHashOf announcingA) [0]) (successorLeiosBlock announcingA)

-- | A sibling of 'certifyingA', making the same claim, with a certificate a
-- minority made.
minoritySibling :: LeiosTestBlock
minoritySibling =
  setLeiosCert (certOf (rbHashOf announcingA) [0]) (forkLeiosBlock certifyingA)

-- | Add 'certifyingA', which establishes its claim, and then 'minoritySibling'.
addSibling :: NodeUnderTest (IOSim s) -> IOSim s ()
addSibling node = do
  addBlocks node [announcingA, certifyingA]
  addBlocksAfter node (NotOrigin announcingA) [minoritySibling]

rbHashOf :: LeiosTestBlock -> RankingBlockHash
rbHashOf = toRankingBlockHash (Proxy @LeiosTestBlock) . blockHash

pointA, pointB :: LeiosPoint
pointA = MkLeiosPoint (blockSlot announcingA) (UnsafeHash (SBS.replicate 32 1))
pointB = MkLeiosPoint (blockSlot announcingB) (UnsafeHash (SBS.replicate 32 2))

-- | The first block, which announces A; the second, which certifies A; and
-- the third, which announces B.
announcingA, certifyingA, announcingB :: LeiosTestBlock
announcingA = setAnnouncement (pointEbHash pointA) (encodeEndorserBlockSize ebA) (firstLeiosBlock 1)
certifyingA =
  setLeiosCert (certOf (rbHashOf announcingA) [0, 1, 2]) (successorLeiosBlock announcingA)
announcingB =
  setAnnouncement (pointEbHash pointB) (encodeEndorserBlockSize ebB) (successorLeiosBlock certifyingA)

-- | A and B each name one transaction of ten bytes.
ebA, ebB :: EndorserBlock
ebA = endorserBlockFromTxBytes (txA :| [])
ebB = endorserBlockFromTxBytes (txB :| [])

txA, txB :: BS.ByteString
txA = BS.replicate 10 1
txB = BS.replicate 10 2

-- | The chain announces A and then certifies it, while the LeiosDB holds A's
-- body but not its transactions. The tip of the selected chain before they
-- arrive and the node emits their acquisition, and after.
tipsAroundClosure :: IOSim s (Point LeiosTestBlock, Point LeiosTestBlock)
tipsAroundClosure = withNode $ \node -> do
  let leiosDb = getLeiosDB (nodeKernel node)
      chainDB = nodeChainDB node
  withWriter leiosDb $ \writer -> do
    void $ await =<< writeEbBody writer pointA ebA mempty mempty
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  addBlocks node [announcingA, certifyingA]
  threadDelay 1
  tipBefore <- atomically $ ChainDB.getTipPoint chainDB
  withWriter leiosDb $ \writer ->
    void $
      await
        =<< writeTxs
          writer
          pointA
          ( Map.fromList
              [ (offset, BS.replicate (fromIntegral size) 0)
              | (offset, (_txHash, size)) <- zip [0 ..] (endorserBlockReferences ebA)
              ]
          )
  atomically $
    emitLeiosAcquisition (nodeLeiosAcquisitions node) (AcquiredEbTxs pointA)
  threadDelay 1
  tipAfter <- atomically $ ChainDB.getTipPoint chainDB
  pure (tipBefore, tipAfter)

-- | As 'tipsAroundClosure', except that the LeiosDB holds A's closure before
-- the node starts. The tip of the selected chain.
tipWithClosureBeforeStart :: IOSim s (Point LeiosTestBlock)
tipWithClosureBeforeStart = do
  leiosDb <- newLeiosDBInMemory
  withWriter leiosDb $ \writer -> do
    void $ await =<< writeEbBody writer pointA ebA mempty mempty
    void $
      await
        =<< writeTxs
          writer
          pointA
          (Map.singleton 0 txA)
  withNodeOver leiosDb $ \node -> do
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    addBlocks node [announcingA, certifyingA]
    threadDelay 1
    atomically $ ChainDB.getTipPoint (nodeChainDB node)
