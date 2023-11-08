{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A minimal, synchronous implementation of the 'ChainDB' effect API that
-- only implements what's strictly necessary for Genesis tests.
module Test.Consensus.PeerSimulator.ChainDB (mockChainDb) where

import           Cardano.Slotting.Slot (at, origin)
import           Control.Tracer (Tracer)
import           Data.Foldable (foldl', toList)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block.Abstract (ChainHash (BlockHash),
                     blockPoint, blockSlot, castHash, castPoint, getHeader,
                     headerHash, headerPrevHash, pointHash)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint,
                     realPointToPoint)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import           Ouroboros.Consensus.HeaderValidation
                     (HeaderState (HeaderState), getAnnTip)
import           Ouroboros.Consensus.Ledger.Extended
                     (ExtLedgerState (ExtLedgerState))
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     AddBlockResult (..), BlockComponent, ChainDB (ChainDB),
                     ChainType, Follower, InvalidBlockReason, Iterator,
                     StreamFrom, StreamTo, UnknownRange)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import           Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
                     (InvalidBlockPunishment)
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
                     (Checkpoint (Checkpoint), LedgerDB (LedgerDB), LedgerDB')
import           Ouroboros.Consensus.Util (eitherToMaybe, firstJust)
import           Ouroboros.Consensus.Util.AnchoredFragment
                     (preferAnchoredCandidate)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Fingerprint (Fingerprint),
                     WithFingerprint (WithFingerprint))
import           Ouroboros.Network.AnchoredFragment (Anchor (AnchorGenesis),
                     AnchoredSeq (Empty, (:>)), anchorNewest, dropNewest,
                     headHash, headPoint, intersect, rollback, toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.Block (HeaderHash, MaxSlotNo (MaxSlotNo),
                     Point (Point), blockHash)
import           Test.Consensus.PeerSimulator.Trace (traceUnitWith)
import           Test.Consensus.PointSchedule (PeerId, TestFragH)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig),
                     Header (..), LedgerState (TestLedger), TestBlock,
                     testInitExtLedger)

-- | The state of the minimal 'ChainDB'.
--
-- Most of it is pure, so each handler can operate in a simple function,
-- except for those that need to access candidates, since they are shared
-- with the other components.
data CDBEnv m =
  CDBEnv {
    -- | Each ChainSync client's candidate fragment, provided externally.
    candidates   :: StrictTVar m (Map PeerId (StrictTVar m TestFragH)),
    -- | The current Limit on Eagerness fragment.
    -- This is updated externally, by the GDD governor, but constructed here.
    --
    -- | Its head always waits at the earliest intersection of the current chain
    -- with another candidate fragment, only advancing when the governor kills
    -- one of the ChainSync clients that anchor at its head.
    --
    -- The selection is only allowed to extend beyond this head by the number of
    -- blocks specified by 'loeLimit'.
    loeFrag      :: TestFragH,
    -- | The Limit on Eagerness limit – the number of blocks that may be selected
    -- beyond the LoE fragment.
    -- Provided externally.
    loeLimit     :: Word64,
    -- | The internal state of blocks that have been added to the DB.
    blocks       :: Map (HeaderHash TestBlock) TestBlock,
    -- | The internal state containing the current selection.
    currentChain :: TestFragH,
    -- | The internal state of snapshots of the selection.
    ledgerDb     :: LedgerDB' TestBlock
  }

----------------------------------------------------------------------------------------------------
-- Chain selection
----------------------------------------------------------------------------------------------------

-- | Insert a newly selected chain into the 'LedgerDB''.
--
-- The ledger db contains snapshots of the last @k@ selections.
-- When the selection is rolled back, the db also needs to be rolled back.
--
-- The snapshots are stored as 'AnchoredFragment's, and for 'TestBlock' the
-- individual entries just contain the current tip.
--
-- We look at the intersection fragments of the new and current chain, to
-- determine the number of rolled back states from the length of the suffix
-- of the current chain and the new tip from the new suffix.
--
-- Finally we truncate the states to the newest @k@ entries.
updateLedgerDb :: TestFragH -> CDBEnv m -> LedgerDB' TestBlock
updateLedgerDb newChain CDBEnv {blocks, currentChain, ledgerDb = LedgerDB oldDb, loeLimit = k} =
  LedgerDB case intersect currentChain newChain of
    Just (_, _, sufCur, sufNew) ->
      update (AnchoredFragment.length sufCur) sufNew
    Nothing ->
      oldDb
  where
    update remove new =
      anchorNewest k (foldl' (:>) (dropNewest remove oldDb) (checkpoint <$> toOldestFirst new))

    checkpoint block =
      Checkpoint (ExtLedgerState (TestLedger point ()) (HeaderState (at (getAnnTip block)) ()))
      where
        point = blockPoint (blocks ! headerHash block)

-- | Find a candidate fragment that contains the given point, and return its
-- prefix that is headed by that point.
candidateContainingPoint ::
  Point TestBlock ->
  Map PeerId TestFragH ->
  Maybe TestFragH
candidateContainingPoint point candidates =
  firstJust (rollback (castPoint point)) (toList candidates)

-- | Examine whether the @block@ can be grafted on the current selection or any of the
-- candidate fragments, and decide whether the new chain with that block at the tip
-- is eligible to replace the current selection.
--
-- If the block is a direct successor of the current chain, accept it unconditionally.
--
-- If the block is an extension of one of the candidates, accept the new fragment only if
-- it is longer than the current chain, indicated by 'preferAnchoredCandidate'.
--
-- Otherwise, return 'Nothing'.
betterChain ::
  TestBlock ->
  Map PeerId TestFragH ->
  CDBEnv m ->
  (String, Maybe TestFragH)
betterChain block candidates env
  | extendsChain
  = ("Extended the current selection with " ++ condense block, Just newChain)

  | Just fork <- candidateContainingPoint (blockPoint block) candidates
  = if preferAnchoredCandidate (TestBlockConfig (NumCoreNodes 1)) chain fork
    then ("Switched to a fork at " ++ condense block, Just fork)
    else ("Block extends a shorter fork: " ++ condense block, Nothing)

  | otherwise
  = ("Block didn't match any fragments", Nothing)
  where
    extendsChain = tipHash == headerPrevHash header

    tipHash = castHash (headHash chain)

    newChain = chain :> header

    chain = currentChain env

    header = getHeader block

-- | Return the number of blocks that the second fragment extends beyond the
-- first fragment plus @limit@ blocks, if it is greater than 0, else return
-- 'Nothing'.
loeExcess :: Word64 -> TestFragH -> TestFragH -> Maybe Int
loeExcess limit loeFrag newChain =
  if diff > 0
  then Just diff
  else Nothing
  where
    diff = fromIntegral (Diff.getRollback (Diff.diff newChain loeFrag)) - fromIntegral limit

-- | Check that the new chain doesn't violate the LoE.
--
-- If the fragment exceeds the limit, return a new status message and 'Nothing', otherwise
-- just pass through the original values from the previous check.
checkLoE ::
  CDBEnv m ->
  String ->
  Maybe TestFragH ->
  (String, Maybe TestFragH)
checkLoE env msg newChain
  | Just new <- newChain
  , let lf = loeFrag env
  , Just excess <- loeExcess (loeLimit env) lf new
  = ("LoE at " ++ condense (headPoint lf) ++ " exceeded by " ++ show excess, Nothing)

  | otherwise
  = (msg, newChain)

-- | Update the 'CDBEnv' with a new current chain selection.
--
-- The current chain is truncated to the last @k@ blocks, emulating the commit to the
-- immutable db.
--
-- The ledger db is updated to store a new snapshot, and possibly rolling back.
selectNewChain ::
  CDBEnv m ->
  TestFragH ->
  CDBEnv m
selectNewChain env newChain =
  env {currentChain = anchorNewest (loeLimit env) newChain, ledgerDb = updateLedgerDb newChain env}

-- | Perform chain selection for a candidate chain.
--
-- There are two conditions:
--
-- - The chain length rule, checked by 'betterChain'
--
-- - The LoE, checked by 'checkLoE'
--
-- If both steps return 'Just' on the right of the pair, we call 'selectNewChain' to update the
-- ledger db.
--
-- The 'String' on the left contains the message that will be traced.
chainSel ::
  TestBlock ->
  Map PeerId TestFragH ->
  CDBEnv m ->
  (String, CDBEnv m)
chainSel block candidates env =
  maybe env (selectNewChain env) <$> uncurry (checkLoE env) (betterChain block candidates env)

----------------------------------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------------------------------

-- | Helper for getting all candidate fragments out of the TVars.
getCandidates ::
  IOLike m =>
  CDBEnv m ->
  STM m (Map PeerId TestFragH)
getCandidates varEnv =
  traverse readTVar =<< readTVar (candidates varEnv)

-- | In the real implementation, this is supposed to return an async computation that
-- adds a block and performs chain selection.
--
-- Since our test execution only executes a single peer at a time, we perform these
-- steps synchronously and return a trivial promise.
--
-- First we add the block to our state, which is simply a 'Map'.
-- Based on whether it was already present, we determine the result value.
--
-- Then we perform chain selection with 'chainSel'.
addBlockAsync ::
  IOLike m =>
  Tracer m String ->
  StrictTVar m (CDBEnv m) ->
  InvalidBlockPunishment m ->
  TestBlock ->
  m (AddBlockPromise m TestBlock)
addBlockAsync tracer varEnv _ block = do
  (result, msg) <- atomically do
    frags <- getCandidates =<< readTVar varEnv
    result <- stateTVar varEnv addToDb
    msg <- stateTVar varEnv (chainSel block frags)
    pure (result, msg)
  trace msg
  pure AddBlockPromise {
    blockWrittenToDisk = pure True,
    blockProcessed = pure result
  }
  where
    addToDb env@CDBEnv {blocks}
      | Map.member hash blocks
      = (FailedToAddBlock "already added", env)
      | otherwise
      = (SuccesfullyAddedBlock (blockPoint block), env {blocks = Map.insert hash block blocks})

    hash = blockHash block

    trace = traceUnitWith tracer "ChainDB"

-- | Return the header at the tip of the current chain.
tipHeader :: CDBEnv m -> Maybe (Header TestBlock)
tipHeader =
  eitherToMaybe . AnchoredFragment.head . currentChain

-- | Return the block at the tip of the current chain.
tipBlock :: CDBEnv m -> Maybe TestBlock
tipBlock env =
  tipHeader env >>= \ header ->
    blocks env !? headerHash header

-- | Return the point at the tip of the current chain, or Genesis.
tipPoint :: CDBEnv m -> Point TestBlock
tipPoint env =
  maybe (Point origin) blockPoint (tipBlock env)

-- | Determine whether a point is present in the 'ChainDB'.
--
-- We simply look it up in our 'Map'.
-- Unclear whether this is appropriate.
getIsFetched ::
  CDBEnv m ->
  Point TestBlock ->
  Bool
getIsFetched env =
  isFetched (blocks env)
  where
    isFetched blks block
      | BlockHash hash <- pointHash block
      = Map.member hash blks
      | otherwise
      = False

-- | Determine whether a point is on any of the candidate fragments, and whether
-- it is valid.
--
-- Since we don't have a notion of validity, we always return 'True' if the block
-- is present, and 'Nothing' otherwise..
getIsValid ::
  IOLike m =>
  CDBEnv m ->
  STM m (RealPoint TestBlock -> Maybe Bool)
getIsValid env =
  getCandidates env <&> \ candidates point ->
    True <$ candidateContainingPoint (realPointToPoint point) candidates

-- | Compute the largest slot of any of the fetched blocks.
--
-- Unclear whether it should also examine the candidates.
getMaxSlotNo :: CDBEnv m -> MaxSlotNo
getMaxSlotNo env = foldMap (MaxSlotNo . blockSlot) (blocks env)

-- | We have no notion of invalid blocks at the moment.
getIsInvalidBlock ::
  IOLike m =>
  STM m (WithFingerprint (HeaderHash TestBlock -> Maybe (InvalidBlockReason TestBlock)))
getIsInvalidBlock = pure (WithFingerprint (const Nothing) (Fingerprint 0))

-- | Update the LoE fragment in the state.
--
-- This is used by the GDD governor to signal that the selection may advance.
setLoEFrag ::
  IOLike m =>
  StrictTVar m (CDBEnv m) ->
  TestFragH ->
  STM m ()
setLoEFrag varEnv loeFrag =
  modifyTVar varEnv \ env -> env {loeFrag}

----------------------------------------------------------------------------------------------------
-- Handlers we're not using
----------------------------------------------------------------------------------------------------

getBlockComponent ::
  StrictTVar m (CDBEnv m) ->
  BlockComponent TestBlock b ->
  RealPoint TestBlock ->
  m (Maybe b)
getBlockComponent _ _ _ =
  error "getBlockComponent not implemented for pure ChainDB"

stream ::
     ResourceRegistry m
  -> BlockComponent TestBlock b
  -> StreamFrom TestBlock
  -> StreamTo TestBlock
  -> m (Either (UnknownRange TestBlock) (Iterator m TestBlock b))
stream = error "stream not implemented for pure ChainDB"

newFollower ::
     ResourceRegistry m
  -> ChainType
  -> BlockComponent TestBlock b
  -> m (Follower m TestBlock b)
newFollower _ _ _ =
  error "newFollower not implemented for pure ChainDB"

----------------------------------------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------------------------------------

-- | Construct 'CDBEnv' with a @k@ and a set of candidate fragment TVars.
cdbEnv ::
  IOLike m =>
  Word64 ->
  StrictTVar m (Map PeerId (StrictTVar m TestFragH)) ->
  m (CDBEnv m)
cdbEnv loeLimit candidates =
  pure CDBEnv {
    candidates,
    blocks = mempty,
    currentChain = Empty AnchorGenesis,
    ledgerDb = LedgerDB (Empty (Checkpoint testInitExtLedger)),
    loeLimit,
    loeFrag = Empty AnchorGenesis
  }

mockChainDbWithEnv ::
  forall m .
  IOLike m =>
  Tracer m String ->
  StrictTVar m (CDBEnv m) ->
  ChainDB m TestBlock
mockChainDbWithEnv tracer env =
  ChainDB {
    addBlockAsync = addBlockAsync tracer env,
    getCurrentChain = stmPure currentChain,
    getLedgerDB = stmPure ledgerDb,
    getTipBlock = atomic tipBlock,
    getTipHeader = atomic tipHeader,
    getTipPoint = stmPure tipPoint,
    getBlockComponent = getBlockComponent env,
    getIsFetched = stmPure getIsFetched,
    getIsValid = stm getIsValid,
    getMaxSlotNo = stmPure getMaxSlotNo,
    stream,
    newFollower,
    getIsInvalidBlock,
    setLoEFrag = setLoEFrag env,
    closeDB = pure (),
    isOpen = pure True
  }
  where
    -- Some helpers to make the implementation functions less noisy.
    stm :: forall a . (CDBEnv m -> STM m a) -> STM m a
    stm f = f =<< readTVar env

    stmPure :: forall a . (CDBEnv m -> a) -> STM m a
    stmPure f = stm (pure . f)

    atomic :: forall a . (CDBEnv m -> a) -> m a
    atomic = atomically . stmPure

-- | Construct a 'ChainDB' that operates on our minimal state, 'CDBEnv'.
--
-- We use @k@ for the LoE limit.
mockChainDb ::
  forall m .
  IOLike m =>
  SecurityParam ->
  Tracer m String ->
  StrictTVar m (Map PeerId (StrictTVar m TestFragH)) ->
  m (ChainDB m TestBlock)
mockChainDb (SecurityParam loeLimit) tracer candidates = do
  env <- cdbEnv loeLimit candidates
  mockChainDbWithEnv tracer <$> uncheckedNewTVarM env
