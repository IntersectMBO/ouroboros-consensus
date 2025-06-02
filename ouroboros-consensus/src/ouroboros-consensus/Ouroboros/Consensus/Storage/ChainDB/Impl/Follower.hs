{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Followers
module Ouroboros.Consensus.Storage.ChainDB.Impl.Follower
  ( closeAllFollowers
  , newFollower
  , switchFork
  ) where

import Codec.CBOR.Write (toLazyByteString)
import Control.Exception (assert)
import Control.Monad (join)
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer (contramap, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Storage.ChainDB.API
  ( BlockComponent (..)
  , ChainDbError (..)
  , ChainType (..)
  , Follower (..)
  , getPoint
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (blockUntilJust)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (ChainUpdate (..))

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

-- | Check if the ChainDB is open. If not, throw a 'ClosedDBError'. Next,
-- check whether the follower with the given 'FollowerKey' still exists. If not,
-- throw a 'ClosedFollowerError'.
--
-- Otherwise, execute the given function on the 'ChainDbEnv'.
getFollower ::
  forall m blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  FollowerKey ->
  (ChainDbEnv m blk -> m r) ->
  m r
getFollower (CDBHandle varState) followerKey f = do
  env <-
    atomically $
      readTVar varState >>= \case
        ChainDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack
        ChainDbOpen env -> do
          followerOpen <- Map.member followerKey <$> readTVar (cdbFollowers env)
          if followerOpen
            then return env
            else throwIO $ ClosedFollowerError @blk
  f env

-- | Variant 'of 'getFollower' for functions taking one argument.
getFollower1 ::
  forall m blk a r.
  (IOLike m, HasHeader blk) =>
  ChainDbHandle m blk ->
  FollowerKey ->
  (ChainDbEnv m blk -> a -> m r) ->
  a ->
  m r
getFollower1 h followerKey f a = getFollower h followerKey (\env -> f env a)

{-------------------------------------------------------------------------------
  Follower
-------------------------------------------------------------------------------}

newFollower ::
  forall m blk b.
  ( IOLike m
  , HasHeader blk
  , GetHeader blk
  , HasNestedContent Header blk
  , EncodeDiskDep (NestedCtxt Header) blk
  ) =>
  ChainDbHandle m blk ->
  ResourceRegistry m ->
  ChainType ->
  BlockComponent blk b ->
  m (Follower m blk b)
newFollower h registry chainType blockComponent = getEnv h $ \CDB{..} -> do
  -- The following operations don't need to be done in a single transaction
  followerKey <- atomically $ stateTVar cdbNextFollowerKey $ \r -> (r, succ r)
  varFollower <- newTVarIO FollowerInit
  let followerHandle = mkFollowerHandle varFollower
  atomically $ modifyTVar cdbFollowers $ Map.insert followerKey followerHandle
  let follower =
        makeNewFollower h followerKey varFollower chainType registry blockComponent
  traceWith cdbTracer $ TraceFollowerEvent NewFollower
  return follower
 where
  mkFollowerHandle :: StrictTVar m (FollowerState m blk b) -> FollowerHandle m blk
  mkFollowerHandle varFollower =
    FollowerHandle
      { fhChainType = chainType
      , fhClose = do
          -- This is only called by 'closeAllFollowers'. We just release the
          -- resources. We don't check whether the Follower is still open.
          -- We don't have to remove the follower from the 'cdbFollowers',
          -- 'closeAllFollowers' will empty that map already.
          followerState <- atomically $ readTVar varFollower
          closeFollowerState followerState
      , fhSwitchFork = \ipoint oldPoints ->
          modifyTVar varFollower $
            switchFork ipoint oldPoints
      }

makeNewFollower ::
  forall m blk b.
  ( IOLike m
  , HasHeader blk
  , GetHeader blk
  , HasNestedContent Header blk
  , EncodeDiskDep (NestedCtxt Header) blk
  ) =>
  ChainDbHandle m blk ->
  FollowerKey ->
  StrictTVar m (FollowerState m blk b) ->
  ChainType ->
  ResourceRegistry m ->
  BlockComponent blk b ->
  Follower m blk b
makeNewFollower h followerKey varFollower chainType registry blockComponent = Follower{..}
 where
  followerInstruction :: m (Maybe (ChainUpdate blk b))
  followerInstruction =
    getFollower h followerKey $
      instructionHelper registry varFollower chainType blockComponent id

  followerInstructionBlocking :: m (ChainUpdate blk b)
  followerInstructionBlocking =
    fmap runIdentity $
      getFollower h followerKey $
        instructionHelper registry varFollower chainType blockComponent (fmap Identity . blockUntilJust)

  followerForward :: [Point blk] -> m (Maybe (Point blk))
  followerForward =
    getFollower1 h followerKey $
      forward registry varFollower blockComponent

  followerClose :: m ()
  followerClose = getEnv h $ close followerKey varFollower

-- | Implementation of 'followerClose'.
--
-- To be called using 'getEnv' to make sure the ChainDB is still open.
--
-- Idempotent: the follower doesn't have to be open.
--
-- Unlike 'closeAllFollowers', this is meant to be called by the user of the
-- ChainDB.Follower.
close ::
  forall m blk b.
  IOLike m =>
  FollowerKey ->
  StrictTVar m (FollowerState m blk b) ->
  ChainDbEnv m blk ->
  m ()
close followerKey varFollower CDB{cdbFollowers} = do
  -- If the FollowerKey is not present in the map, the Follower must have been
  -- closed already.
  atomically $ modifyTVar cdbFollowers $ Map.delete followerKey
  followerState <- atomically $ readTVar varFollower
  closeFollowerState followerState

-- | Close the given 'FollowerState' by closing any 'ImmutableDB.Iterator' it
-- might contain.
closeFollowerState :: MonadCatch m => FollowerState m blk b -> m ()
closeFollowerState = \case
  FollowerInit -> return ()
  FollowerInMem _ -> return ()
  -- IMPORTANT: the main reason we're closing followers: to close this open
  -- iterator, which contains a reference to a file handle.
  FollowerInImmutableDB _ immIt -> ImmutableDB.iteratorClose immIt

-- | Helper for 'followerInstruction' and 'followerInstructionBlocking'.
--
-- The type @f@ will be instantiated to:
--
-- * 'Maybe' in case of 'followerInstruction'.
-- * 'Identity' in case of 'followerInstructionBlocking'.
--
-- The returned 'ChainUpdate' contains a 'b', as defined by 'BlockComponent'.
--
-- When in the 'FollowerInImmutableDB' state, we never have to block, as we can
-- just stream the next block/header from the ImmutableDB.
--
-- When in the 'FollowerInMem' state, we may have to block when we have reached
-- the end of the current chain.
instructionHelper ::
  forall m blk b f.
  ( IOLike m
  , HasHeader blk
  , GetHeader blk
  , HasNestedContent Header blk
  , EncodeDiskDep (NestedCtxt Header) blk
  , Traversable f
  , Applicative f
  ) =>
  ResourceRegistry m ->
  StrictTVar m (FollowerState m blk b) ->
  ChainType ->
  BlockComponent blk b ->
  -- | How to turn a transaction that may or may not result in a new
  -- 'ChainUpdate' in one that returns the right return type: use @fmap
  -- Identity . 'blockUntilJust'@ to block or 'id' to just return the
  -- @Maybe@.
  ( STM m (Maybe (ChainUpdate blk (Header blk))) ->
    STM m (f (ChainUpdate blk (Header blk)))
  ) ->
  ChainDbEnv m blk ->
  m (f (ChainUpdate blk b))
instructionHelper registry varFollower chainType blockComponent fromMaybeSTM CDB{..} = do
  -- In one transaction: check in which state we are, if in the
  -- @FollowerInMem@ state, just call 'instructionSTM', otherwise,
  -- return the contents of the 'FollowerInImmutableDB' state.
  inImmutableDBOrRes <- atomically $ do
    curChain <- getCurrentChainByType
    readTVar varFollower >>= \case
      -- Just return the contents of the state and end the transaction in
      -- these two cases.
      FollowerInit ->
        return $ Left (RollBackTo GenesisPoint, Nothing)
      FollowerInImmutableDB rollState immIt ->
        return $ Left (rollState, Just immIt)
      FollowerInMem rollState
        | AF.withinFragmentBounds
            (castPoint (followerRollStatePoint rollState))
            curChain ->
            -- The point is still in the current chain fragment
            fmap Right $
              fromMaybeSTM $
                instructionSTM
                  rollState
                  curChain
                  (writeTVar varFollower . FollowerInMem)
        | otherwise ->
            -- The point is no longer on the fragment. Blocks must have moved
            -- (off the fragment) to the ImmutableDB. Note that 'switchFork'
            -- will try to keep the point on the fragment in case we switch to
            -- a fork.
            return $ Left (rollState, Nothing)
  case inImmutableDBOrRes of
    -- We were able to obtain the result inside the transaction as we were
    -- in the 'FollowerInMem' state. We only got a header, which we must first
    -- convert to the right block component.
    Right fupdate -> headerUpdateToBlockComponentUpdate fupdate
    -- We were in the 'FollowerInImmutableDB' state or we need to switch to it.
    Left (rollState, mbImmIt) -> do
      immIt <- case mbImmIt of
        Just immIt -> return immIt
        -- We were in the 'FollowerInMem' state but have to switch to the
        -- 'FollowerInImmutableDB' state.
        Nothing -> do
          trace $ FollowerNoLongerInMem rollState
          ImmutableDB.streamAfterKnownPoint
            cdbImmutableDB
            registry
            ((,) <$> getPoint <*> blockComponent)
            (followerRollStatePoint rollState)
      case rollState of
        RollForwardFrom pt -> rollForwardImmutableDB immIt pt
        RollBackTo pt -> do
          let followerState' = FollowerInImmutableDB (RollForwardFrom pt) immIt
          atomically $ writeTVar varFollower followerState'
          return $ pure $ RollBack pt
 where
  trace = traceWith (contramap TraceFollowerEvent cdbTracer)

  getCurrentChainByType = do
    curChain <- icWithoutTime <$> readTVar cdbChain
    case chainType of
      SelectedChain -> pure curChain
      TentativeChain ->
        readTVar cdbTentativeHeader <&> \case
          SJust hdr -> curChain AF.:> hdr
          SNothing -> curChain

  codecConfig :: CodecConfig blk
  codecConfig = configCodec cdbTopLevelConfig

  headerUpdateToBlockComponentUpdate ::
    f (ChainUpdate blk (Header blk)) -> m (f (ChainUpdate blk b))
  headerUpdateToBlockComponentUpdate =
    traverse (traverse (`getBlockComponentFromHeader` blockComponent))

  -- \| We only got the header for the in-memory chain fragment, so depending
  -- on the 'BlockComponent' that's requested, we might have to read the
  -- whole block.
  getBlockComponentFromHeader ::
    forall b'. Header blk -> BlockComponent blk b' -> m b'
  getBlockComponentFromHeader hdr = \case
    GetVerifiedBlock -> getBlockComponent GetVerifiedBlock
    GetBlock -> getBlockComponent GetBlock
    GetRawBlock -> getBlockComponent GetRawBlock
    GetHeader -> return $ hdr
    GetRawHeader -> return $ rawHdr
    GetHash -> return $ headerHash hdr
    GetSlot -> return $ blockSlot hdr
    GetIsEBB -> return $ headerToIsEBB hdr
    GetBlockSize -> getBlockComponent GetBlockSize
    -- We could look up the header size in the index of the VolatileDB,
    -- but getting the serialisation is cheap because we keep the
    -- serialisation in memory as an annotation, and the following way is
    -- less stateful
    GetHeaderSize -> return $ fromIntegral $ Lazy.length rawHdr
    GetNestedCtxt -> return nestedCtxt
    GetPure a -> return a
    GetApply f bc ->
      getBlockComponentFromHeader hdr f
        <*> getBlockComponentFromHeader hdr bc
   where
    -- \| Use the 'ImmutableDB' and 'VolatileDB' to read the 'BlockComponent' from
    -- disk (or memory).
    getBlockComponent :: forall c. BlockComponent blk c -> m c
    getBlockComponent bc =
      Query.getAnyKnownBlockComponent cdbImmutableDB cdbVolatileDB bc (headerRealPoint hdr)

    rawHdr :: Lazy.ByteString
    nestedCtxt :: SomeSecond (NestedCtxt Header) blk
    (nestedCtxt, rawHdr) = case unnest hdr of
      DepPair ctxt h ->
        ( SomeSecond ctxt
        , toLazyByteString $ encodeDiskDep codecConfig ctxt h
        )

  next ::
    ImmutableDB.Iterator m blk (Point blk, b) ->
    m (Maybe (Point blk, b))
  next immIt =
    ImmutableDB.iteratorNext immIt <&> \case
      ImmutableDB.IteratorResult b -> Just b
      ImmutableDB.IteratorExhausted -> Nothing

  rollForwardImmutableDB ::
    ImmutableDB.Iterator m blk (Point blk, b) ->
    Point blk ->
    m (f (ChainUpdate blk b))
  rollForwardImmutableDB immIt pt =
    next immIt >>= \case
      Just (pt', b) -> do
        let followerState' = FollowerInImmutableDB (RollForwardFrom pt') immIt
        atomically $ writeTVar varFollower followerState'
        return $ pure $ AddBlock b
      Nothing -> do
        -- Even though an iterator is automatically closed internally when
        -- exhausted, we close it again (idempotent), but this time to
        -- unregister the associated clean-up action.
        ImmutableDB.iteratorClose immIt
        -- The iterator is exhausted: we've reached the end of the
        -- ImmutableDB, or actually what was the end of the ImmutableDB at the
        -- time of opening the iterator. We must now check whether that is
        -- still the end (blocks might have been added to the ImmutableDB in
        -- the meantime).
        pointAtImmutableDBTip <-
          atomically $ ImmutableDB.getTipPoint cdbImmutableDB
        let slotNoAtImmutableDBTip = pointSlot pointAtImmutableDBTip
        case pointSlot pt `compare` slotNoAtImmutableDBTip of
          -- The ImmutableDB somehow rolled back
          GT -> error "follower streamed beyond tip of the ImmutableDB"
          -- The tip is still the same, so switch to the in-memory chain
          EQ | pt == pointAtImmutableDBTip ->
            do
              trace $ FollowerSwitchToMem pt slotNoAtImmutableDBTip
              fupdate <- atomically $ fromMaybeSTM $ do
                curChain <- getCurrentChainByType
                instructionSTM
                  (RollForwardFrom pt)
                  curChain
                  (writeTVar varFollower . FollowerInMem)
              -- We only got the header, we must first convert it to the right
              -- block component.
              headerUpdateToBlockComponentUpdate fupdate

          -- Two possibilities:
          --
          -- 1. (EQ): the tip changed, but the slot number is the same. This
          --    is only possible when an EBB was at the tip and the regular
          --    block in the same slot was appended to the ImmutableDB.
          --
          -- 2. (LT): the tip of the ImmutableDB has progressed since we
          --    opened the iterator.
          _ -> do
            trace $ FollowerNewImmIterator pt slotNoAtImmutableDBTip
            immIt' <-
              ImmutableDB.streamAfterKnownPoint
                cdbImmutableDB
                registry
                ((,) <$> getPoint <*> blockComponent)
                pt
            -- Try again with the new iterator
            rollForwardImmutableDB immIt' pt

-- | 'followerInstruction' for when the follower is in the 'FollowerInMem' state.
instructionSTM ::
  forall m blk.
  (MonadSTM m, HasHeader (Header blk)) =>
  -- | The current 'FollowerRollState' of the follower
  FollowerRollState blk ->
  -- | The current chain fragment
  AnchoredFragment (Header blk) ->
  -- | How to save the updated 'FollowerRollState'
  (FollowerRollState blk -> STM m ()) ->
  STM m (Maybe (ChainUpdate blk (Header blk)))
instructionSTM rollState curChain saveRollState =
  assert (invariant curChain) $ case rollState of
    RollForwardFrom pt ->
      case AF.successorBlock (castPoint pt) curChain of
        -- There is no successor block because the follower is at the head
        Nothing -> return Nothing
        Just hdr -> do
          saveRollState $ RollForwardFrom $ headerPoint hdr
          return $ Just $ AddBlock hdr
    RollBackTo pt -> do
      saveRollState $ RollForwardFrom pt
      return $ Just $ RollBack pt
 where
  invariant =
    AF.withinFragmentBounds (castPoint (followerRollStatePoint rollState))

forward ::
  forall m blk b.
  ( IOLike m
  , HasCallStack
  , HasHeader blk
  , HasHeader (Header blk)
  ) =>
  ResourceRegistry m ->
  StrictTVar m (FollowerState m blk b) ->
  BlockComponent blk b ->
  ChainDbEnv m blk ->
  [Point blk] ->
  m (Maybe (Point blk))
forward registry varFollower blockComponent CDB{..} =
  findM checkIfPointOnChain
 where
  checkIfPointOnChain :: HasCallStack => Point blk -> m Bool
  checkIfPointOnChain pt = join $ atomically $ do
    -- NOTE: we use 'cdbChain' instead of 'Query.getCurrentChain', which only
    -- returns the last @k@ headers, because we need to also see the headers
    -- that happen to have not yet been copied over to the ImmutableDB.
    curChain <- icWithoutTime <$> readTVar cdbChain
    followerState <- readTVar varFollower
    if
      | AF.withinFragmentBounds (castPoint pt) curChain ->
          do
            -- It's in the in-memory chain fragment.
            action <- updateState $ FollowerInMem $ RollBackTo pt
            return $ True <$ action
      | otherwise ->
          -- Not in the in-memory chain fragment, so older than @k@, hence it
          -- should be in the ImmutableDB. If not, then the point is not on our
          -- chain.
          --
          -- We try to avoid IO (in the ImmutableDB) as much as possible by
          -- checking whether the requested point corresponds to the current
          -- state of the follower.
          return $ case followerState of
            FollowerInit
              | pt == GenesisPoint ->
                  -- The 'FollowerInit' state is equivalent to @'RollBackTo'
                  -- 'genesisPoint'@, so the state doesn't have to change when
                  -- requesting a rollback to genesis.
                  return True
            FollowerInImmutableDB rollState immIt
              | rollState == RollBackTo pt ->
                  -- If we already have to roll back to the given point in the
                  -- ImmutableDB, the state doesn't have to change, saving us from
                  -- checking whether the point is in the ImmutableDB (cached disk
                  -- reads), closing, and opening the same ImmutableDB iterator.
                  return True
              | rollState == RollForwardFrom pt ->
                  -- If we're already rolling forward from the given point in the
                  -- ImmutableDB, we can reuse the open ImmutableDB iterator,
                  -- saving the same costs as in the comment above. We do have to
                  -- update the state from 'RollForwardFrom' to 'RollBackTo'.
                  do
                    atomically $
                      writeTVar varFollower $
                        FollowerInImmutableDB (RollBackTo pt) immIt
                    return True
            _otherwise -> case pt of
              -- Genesis is always "in" the ImmutableDB
              GenesisPoint -> do
                join $ atomically $ updateState FollowerInit
                return True
              BlockPoint{} ->
                ImmutableDB.streamAfterPoint
                  cdbImmutableDB
                  registry
                  ((,) <$> getPoint <*> blockComponent)
                  pt
                  >>= \case
                    Right immIt -> do
                      join $
                        atomically $
                          updateState $
                            FollowerInImmutableDB (RollBackTo pt) immIt
                      return True
                    Left _ ->
                      -- The point is not in the current chain
                      return False

  -- \| Update the state of the follower to the given state. If the current
  -- state is 'FollowerInImmutableDB', close the ImmutableDB iterator to avoid
  -- leaking the file handles.
  updateState :: FollowerState m blk b -> STM m (m ())
  updateState newFollowerState =
    stateTVar varFollower $ \followerState ->
      (,newFollowerState) $ case followerState of
        -- Return a continuation (that we'll 'join') that closes the
        -- previous iterator.
        FollowerInImmutableDB _ immIt -> ImmutableDB.iteratorClose immIt
        FollowerInit -> return ()
        FollowerInMem _ -> return ()

-- | Switches the follower to the new fork, by checking whether the follower is
-- following an old fork, and updating the follower state to rollback to the
-- intersection point if it is.
switchFork ::
  forall m blk b.
  HasHeader blk =>
  -- | Intersection point between the new and the old chain.
  Point blk ->
  -- | Set of points that are in the old chain and not in the
  -- new chain.
  Set (Point blk) ->
  FollowerState m blk b ->
  FollowerState m blk b
switchFork ipoint oldPoints =
  \case
    -- Roll back to the intersection point if and only if the position of the
    -- follower is not in the new chain, but was part of the volatile DB. By the
    -- invariant that the follower state is always in the current chain, it then
    -- should be in `oldPoints`.
    FollowerInMem (RollBackTo pt)
      | pt `Set.member` oldPoints -> FollowerInMem (RollBackTo ipoint)
    FollowerInMem (RollForwardFrom pt)
      | pt `Set.member` oldPoints -> FollowerInMem (RollBackTo ipoint)
    followerState -> followerState

-- | Close all open block and header 'Follower's.
closeAllFollowers ::
  IOLike m =>
  ChainDbEnv m blk ->
  m ()
closeAllFollowers CDB{..} = do
  followerHandles <- atomically $ do
    followerHandles <- Map.elems <$> readTVar cdbFollowers
    writeTVar cdbFollowers Map.empty
    return followerHandles
  mapM_ fhClose followerHandles
