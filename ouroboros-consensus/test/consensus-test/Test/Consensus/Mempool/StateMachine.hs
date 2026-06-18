{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | See 'MakeAtomic'.
module Test.Consensus.Mempool.StateMachine (tests) where

import Cardano.Slotting.Slot
import Control.Arrow (second)
import Control.Concurrent.Class.MonadSTM.Strict.TChan
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.Except (Except, runExcept)
import qualified Control.Tracer as CT (Tracer, mkTracer, traceWith)
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Measure as Measure
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff
import qualified Data.TreeDiff.OMap as TD
import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract hiding (TxIn, TxOut)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.Mempool.Impl.Common (MempoolLedgerDBView (..), tickLedgerState)
import Ouroboros.Consensus.Mempool.TxSeq
import Ouroboros.Consensus.Mock.Ledger.Address
import Ouroboros.Consensus.Mock.Ledger.Block
import Ouroboros.Consensus.Mock.Ledger.State
import Ouroboros.Consensus.Mock.Ledger.UTxO (Expiry, Tx)
import qualified Ouroboros.Consensus.Mock.Ledger.UTxO as Mock
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense (condense)
import Ouroboros.Consensus.Util.IOLike hiding (bracket)
import Ouroboros.Network.Block (genesisPoint)
import Test.Cardano.Ledger.TreeDiff ()
import Test.Consensus.Mempool.Util
  ( TestBlock
  , applyTxToLedger
  , genTxs
  , genValidTxs
  , testInitLedger
  , testLedgerConfigNoSizeLimits
  )
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.StateMachine hiding ((:>))
import Test.StateMachine.DotDrawing
import Test.StateMachine.Types (History (..), HistoryEvent (..))
import qualified Test.StateMachine.Types as QC
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.ToExpr ()
import qualified Test.Util.QuickCheck as QC
import Test.Util.ToExpr ()

{-------------------------------------------------------------------------------
  Datatypes
-------------------------------------------------------------------------------}

-- | A ledger state together with its full UTxO 'Values'.
--
-- In the @mk@-free design the on-disk table data no longer lives inside the
-- ledger state, so the model threads it explicitly alongside. Equality and
-- ordering are by the state's tip only (the values are a function of the
-- reached tip), which is what the model relies on.
data DBState blk = DBState
  { dbsState :: !(LedgerState blk)
  , dbsValues :: !(Values blk)
  }
  deriving Generic

-- | A ticked ledger state together with its forwarded UTxO 'Values'.
data TickedState blk = TickedState
  { tsState :: !(TickedLedgerState blk)
  , tsValues :: !(Values blk)
  }

-- | The tip of a 'DBState' (used for its 'Eq'\/'Ord').
dbsTip :: GetTip (LedgerState blk) => DBState blk -> Point blk
dbsTip = castPoint . getTip . dbsState

-- | The model
data Model blk r = Model
  { modelMempoolIntermediateState :: !(TickedState blk)
  -- ^ The current tip on the mempool
  , modelTxs :: ![(GenTx blk, TicketNo)]
  , modelAllValidTxs :: ![(GenTx blk, TicketNo)]
  -- ^ The current list of transactions
  , modelCurrentSize :: !(TxMeasure blk)
  -- ^ The current size of the mempool
  , modelCapacity :: !(TxMeasure blk)
  , modelLastSeenTicketNo :: !TicketNo
  -- ^ Last seen ticket number
  --
  -- This indicates how many transactions have ever been added to the mempool.
  , modelConfig :: !(LedgerCfg LedgerState blk)
  , --  * LedgerDB

    modelLedgerDBTip :: !(DBState blk)
  -- ^ The current tip on the ledgerdb
  , modelLedgerDBOtherStates :: !(Set (DBState blk))
  -- ^ The old states which are still on the LedgerDB.
  , modelIsSyncing :: !Bool
  }

-- | The commands used by QSM
--
-- We divide them in 'Action' which are the ones that we on purpose perform on
-- the mempool, and 'Event's which happen by external triggers. This is a mere
-- convenience, in the eyes of QSM they are the same thing.
data Command blk r
  = Action !(Action blk r)
  | Event !(Event blk r)
  deriving Generic1
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

-- | Actions on the mempool
data Action blk r
  = -- | Add some transactions to the mempool
    TryAddTxs ![GenTx blk]
  | -- | Unconditionally sync with the ledger db
    SyncLedger
  | -- | Ask for the current snapshot
    GetSnapshot
  -- TODO: maybe add 'GetSnapshotFor (Point blk)', but this requires to keep
  -- track of some more states to make it meaningful.
  deriving Generic1
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

-- | Events external to the mempool
data Event blk r
  = ChangeLedger
      !(DBState blk)
  deriving Generic1
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

instance CommandNames (Command blk) where
  cmdName (Action action) = cmdName action
  cmdName (Event event) = cmdName event

  cmdNames :: forall r. Proxy (Command blk r) -> [String]
  cmdNames _ =
    cmdNames (Proxy @(Action blk r))
      ++ cmdNames (Proxy @(Event blk r))

-- | Wether or not this test must be atomic.
--
-- The reason behind this data type is that 'TryAddTxs' is on its nature prone
-- to race-conditions. And that is OK with us. For example take the following
-- sequence of commands:
--
-- @@@
--  TryAddTxs [Tx1, Tx2] || GetSnapshot
-- @@@
--
-- If we happen to hit the following interleaving:
--
-- @@@
--  AddTx Tx1; GetSnapshot; AddTx Tx2
-- @@@
--
-- the model will never be able to reproduce the result of the snapshot.
--
-- So in order to do a meaningful testing, what we do is:
--
-- 1. Run a sequential test of actions ensuring that the responses of the model
--    and SUT match on 'GetSnaphsot'. This provides us with assurance that the
--    model works as expected on single-threaded/sequential scenarios.
--
-- 2. Run a parallel test where 'TryAddTxs' is unitary (i.e. use the 'Atomic'
--    modifier) ensuring that the responses of the model and SUT match on
--    'GetSnaphsot'. This ensures that there are no race conditions on this
--    case, or rephrased, that the operations on the mempool remain atomic even
--    if executed on separate threads.
--
-- 3. Run a parallel test where 'TryAddTxs' is not unitary (using the
--    'NonAtomic' modifier) and **NOT** checking the responses of the model
--    versus the SUT. This ensures that there are no deadlocks and no
--    errors/exceptions thrown when running in parallel.
--
-- We believe that these test cover all the interesting cases and provide enough
-- assurance on the implementation of the Mempool.
data MakeAtomic = Atomic | NonAtomic | DontCare

generator ::
  ( Arbitrary (DBState blk)
  , UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  ) =>
  MakeAtomic ->
  -- | Transaction generator based on an state
  (Int -> DBState blk -> Gen [GenTx blk]) ->
  Model blk Symbolic ->
  Maybe (Gen (Command blk Symbolic))
generator ma gTxs model =
  Just $
    frequency
      [
        ( 100
        , Action . TryAddTxs <$> case ma of
            Atomic -> do
              gTxs 1 intermediateDBState
            _ -> do
              n <- getPositive <$> arbitrary
              gTxs n intermediateDBState
        )
      , (10, pure $ Action SyncLedger)
      ,
        ( 10
        , do
            ls <-
              oneof
                ( [ arbitrary
                      `suchThat` ( not
                                     . flip
                                       elem
                                       ( dbsTip modelLedgerDBTip
                                           `Set.insert` Set.map
                                             dbsTip
                                             modelLedgerDBOtherStates
                                       )
                                     . dbsTip
                                 )
                  ]
                    ++ (if Set.null modelLedgerDBOtherStates then [] else [elements (Set.toList modelLedgerDBOtherStates)])
                )
                `suchThat` (not . (== (dbsTip modelLedgerDBTip)) . dbsTip)
            pure $ Event $ ChangeLedger ls
        )
      , (10, pure $ Action GetSnapshot)
      ]
 where
  Model
    { modelMempoolIntermediateState
    , modelLedgerDBTip
    , modelLedgerDBOtherStates
    } = model

  -- The intermediate (ticked) mempool tip, re-presented as an (un-ticked)
  -- 'DBState' so the transaction generator can read its UTxO.
  intermediateDBState =
    DBState
      (unTick (tsState modelMempoolIntermediateState))
      (tsValues modelMempoolIntermediateState)

data Response blk r
  = -- | Nothing to tell
    Void
  | -- | Return the contents of a snapshot
    GotSnapshot ![(GenTx blk, TicketNo)]
  | AddResult ![MempoolAddTxResult blk]
  | Synced !(Point blk, [(GenTx blk, TicketNo)])
  deriving Generic1
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

{-------------------------------------------------------------------------------
  Model side
-------------------------------------------------------------------------------}

initModel ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  ) =>
  LedgerConfig blk ->
  TxMeasure blk ->
  DBState blk ->
  Model blk r
initModel cfg capacity initialState =
  Model
    { modelMempoolIntermediateState = ticked
    , modelLedgerDBOtherStates = Set.empty
    , modelLedgerDBTip = initialState
    , modelTxs = []
    , modelCurrentSize = Measure.zero
    , modelAllValidTxs = []
    , modelLastSeenTicketNo = zeroTicketNo
    , modelCapacity = capacity
    , modelConfig = cfg
    , modelIsSyncing = False
    }
 where
  ticked = tick cfg initialState

mock ::
  Model blk Symbolic ->
  Command blk Symbolic ->
  GenSym (Response blk Symbolic)
mock model = \case
  Action (TryAddTxs _) -> pure $ AddResult []
  Action SyncLedger -> pure $ Synced (genesisPoint, [])
  Action GetSnapshot -> pure $ GotSnapshot $ modelTxs model
  Event (ChangeLedger _) -> pure Void

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

doSync ::
  ( ValidateEnvelope blk
  , LedgerSupportsMempool blk
  , UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  ) =>
  Model blk r ->
  Model blk r
doSync model =
  if tickedTip st == tickedTip st'
    then model
    else
      let
        (validTxs, _tk, newSize, st'') =
          foldTxs modelConfig zeroTicketNo modelCapacity Measure.zero st' $ map (second Just) modelTxs
       in
        model
          { modelMempoolIntermediateState = st''
          , modelTxs = validTxs
          , modelCurrentSize = newSize
          }
 where
  st' = tick modelConfig modelLedgerDBTip

  tickedTip = getTip . unTick . tsState

  Model
    { modelMempoolIntermediateState = st
    , modelLedgerDBTip
    , modelTxs
    , modelCapacity
    , modelConfig
    } = model

doChangeLedger ::
  (StandardHash blk, GetTip (LedgerState blk)) =>
  Model blk r ->
  DBState blk ->
  Model blk r
doChangeLedger model l' =
  model
    { modelLedgerDBTip = l'
    , modelLedgerDBOtherStates =
        Set.insert modelLedgerDBTip modelLedgerDBOtherStates
    }
 where
  Model
    { modelLedgerDBTip
    , modelLedgerDBOtherStates
    } = model

doTryAddTxs ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  ) =>
  Model blk r ->
  [GenTx blk] ->
  Model blk r
doTryAddTxs model [] = model
doTryAddTxs model txs =
  case Foldable.find
    ((castPoint (getTip (unTick (tsState st))) ==) . dbsTip)
    (Set.insert modelLedgerDBTip modelLedgerDBOtherStates) of
    Nothing -> error "Impossible!"
    Just _ ->
      let nextTicket = succ $ modelLastSeenTicketNo model
          (validTxs, tk, newSize, st'') =
            foldTxs cfg nextTicket modelCapacity modelCurrentSize st $ map (,Nothing) txs
          modelTxs' = modelTxs ++ validTxs
       in model
            { modelMempoolIntermediateState = st''
            , modelTxs = modelTxs'
            , modelAllValidTxs = modelAllValidTxs ++ validTxs
            , modelLastSeenTicketNo = pred tk
            , modelCurrentSize = newSize
            }
 where
  Model
    { modelMempoolIntermediateState = st
    , modelTxs
    , modelAllValidTxs
    , modelCurrentSize
    , modelLedgerDBOtherStates
    , modelLedgerDBTip
    , modelConfig = cfg
    , modelCapacity
    } = model

transition ::
  ( UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  , LedgerSupportsMempool blk
  , ToExpr (GenTx blk)
  , ValidateEnvelope blk
  , ToExpr (Command blk r)
  ) =>
  Model blk r ->
  Command blk r ->
  Response blk r ->
  Model blk r
transition model cmd resp = case (cmd, resp) of
  (Action (TryAddTxs txs), AddResult _res) -> (doTryAddTxs model txs){modelIsSyncing = False}
  (Event (ChangeLedger l), Void) -> (doChangeLedger model l){modelIsSyncing = False}
  (Action GetSnapshot, GotSnapshot{}) -> model{modelIsSyncing = False}
  (Action SyncLedger, Synced{}) -> (doSync model){modelIsSyncing = True}
  _ ->
    error $
      "mismatched command "
        <> show cmd
        <> " and response "
        <> show resp

{-------------------------------------------------------------------------------
  Ledger helper functions
-------------------------------------------------------------------------------}

-- | Apply a list of transactions short-circuiting if the mempool gets full.
-- Emulates almost exactly the behaviour of 'implTryTryAddTxs'.
foldTxs ::
  forall blk.
  ( LedgerSupportsMempool blk
  , BasicEnvelopeValidation blk
  ) =>
  LedgerConfig blk ->
  TicketNo ->
  TxMeasure blk ->
  TxMeasure blk ->
  TickedState blk ->
  [(GenTx blk, Maybe TicketNo)] ->
  ( [(GenTx blk, TicketNo)]
  , TicketNo
  , TxMeasure blk
  , TickedState blk
  )
foldTxs cfg nextTk capacity initialFilled initialState =
  go ([], nextTk, initialFilled, initialState)
 where
  go (acc, tk, curSize, st) [] =
    ( reverse acc
    , tk
    , curSize
    , st
    )
  go (acc, tk, curSize, st) ((tx, txtk) : next) =
    let slot = case getTipSlot (tsState st) of
          Origin -> minimumPossibleSlotNo (Proxy @blk)
          At v -> v + 1
     in case runExcept $
          (,)
            <$> txMeasureFull cfg st tx
            <*> applyTx cfg DoNotIntervene slot tx (tsValues st) (tsState st) of
          Left{} ->
            go
              ( acc
              , tk
              , curSize
              , st
              )
              next
          Right (txsz, (st', diff, vtx))
            | ( curSize Measure.<= curSize `Measure.plus` txsz
                  -- Overflow
                  && curSize `Measure.plus` txsz Measure.<= capacity
              ) ->
                -- fits

                go
                  ( (txForgetValidated vtx, fromMaybe tk txtk) : acc
                  , succ tk
                  , curSize `Measure.plus` txsz
                  , TickedState st' (forward @blk [diff] (tsValues st))
                  )
                  next
            | otherwise ->
                go
                  ( acc
                  , tk
                  , curSize
                  , st
                  )
                  next

txMeasureFull ::
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  TickedState blk ->
  GenTx blk ->
  Except (ApplyTxErr blk) (TxMeasure blk)
txMeasureFull cfg st tx =
  TxMeasure
    <$> txMeasurePhase1 cfg (tsState st) tx
    <*> txMeasurePhase2 cfg (tsValues st) (tsState st) tx

tick ::
  forall blk.
  ( ValidateEnvelope blk
  , LedgerSupportsMempool blk
  ) =>
  LedgerConfig blk ->
  DBState blk ->
  TickedState blk
tick cfg (DBState st values) =
  TickedState tickedSt (forward @blk [tickDiff] values)
 where
  (_slot, tickedSt, tickDiff) =
    tickLedgerState cfg (ForgeInUnknownSlot st)

{-------------------------------------------------------------------------------
  SUT side
-------------------------------------------------------------------------------}

-- | The System Under Test
data SUT m blk
  = SUT
      -- | A Mempool
      !(Mempool m blk)
      -- | Emulates a ledger db to the extent needed by the ledger interface.
      !(StrictTVar m (MockedLedgerDB blk))
  deriving Generic

deriving instance
  ( NoThunks (Mempool m blk)
  , NoThunks (StrictTVar m (MockedLedgerDB blk))
  , IOLike m
  ) =>
  NoThunks (SUT m blk)

-- | A very minimal mock of the ledger db.
--
-- The ledger interface will serve the values from this datatype.
data MockedLedgerDB blk = MockedLedgerDB
  { ldbTip :: !(DBState blk)
  -- ^ The current LedgerDB tip
  , reachableTips :: !(Set (DBState blk))
  -- ^ States which are still reachable in the LedgerDB
  }
  deriving Generic

-- | Create a ledger interface and provide the tvar to modify it when switching
-- ledgers.
newLedgerInterface ::
  forall blk m.
  ( NoThunks (MockedLedgerDB blk)
  , LedgerSupportsMempool blk
  , IOLike m
  ) =>
  DBState blk ->
  m (LedgerInterface m blk, StrictTVar m (MockedLedgerDB blk))
newLedgerInterface initialLedger = do
  t <- newTVarIO $ MockedLedgerDB initialLedger Set.empty
  pure
    ( LedgerInterface
        { getCurrentLedgerState = do
            DBState st values <- ldbTip <$> readTVar t
            pure $
              MempoolLedgerDBView
                st
                ( pure $
                    Right $
                      ReadOnlyForker
                        { roforkerClose = pure ()
                        , roforkerReadStatistics = pure $ Statistics 0
                        , roforkerReadTables = \keys -> pure (restrictValues @blk keys values)
                        , roforkerGetLedgerState = pure st
                        }
                )
        }
    , t
    )

-- | Make a SUT
mkSUT ::
  forall m blk.
  ( NoThunks (MockedLedgerDB blk)
  , IOLike m
  , MonadTimer m
  , LedgerSupportsProtocol blk
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  LedgerConfig blk ->
  DBState blk ->
  m (SUT m blk, CT.Tracer m String)
mkSUT cfg initialLedger = do
  (lif, t) <- newLedgerInterface initialLedger
  trcrChan <- atomically newTChan :: m (StrictTChan m (Either String (TraceEventMempool blk)))
  let trcr =
        CT.mkTracer $ -- Dbg.traceShowM @(Either String (TraceEventMempool blk))
          atomically . writeTChan trcrChan
  mempool <-
    openMempoolWithoutSyncThread
      lif
      cfg
      (MempoolCapacityBytesOverride $ unIgnoringOverflow $ tmPhase1 txMaxBytes')
      (Nothing :: Maybe MempoolTimeoutConfig)
      (CT.mkTracer $ CT.traceWith trcr . Right)
  pure (SUT mempool t, CT.mkTracer $ atomically . writeTChan trcrChan . Left)

semantics ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  , IOLike m
  ) =>
  CT.Tracer m String ->
  Command blk Concrete ->
  StrictTVar m (SUT m blk) ->
  m (Response blk Concrete)
semantics trcr cmd r = do
  SUT m t <- atomically $ readTVar r
  case cmd of
    Action (TryAddTxs txs) -> do
      AddResult <$> mapM (addTx m AddTxForRemotePeer) txs
    Action SyncLedger -> do
      snap <- testSyncWithLedger m
      pure (Synced (snapshotPoint snap, [(txForgetValidated tt, tk) | (tt, tk, _) <- snapshotTxs snap]))
    Action GetSnapshot -> do
      txs <- snapshotTxs <$> atomically (getSnapshot m)
      pure $ GotSnapshot [(txForgetValidated vtx, tk) | (vtx, tk, _) <- txs]
    Event (ChangeLedger l') -> do
      CT.traceWith trcr $ "ChangingLedger to " <> show (dbsTip l')
      atomically $ do
        MockedLedgerDB ledgerTip oldReachableTips <- readTVar t
        if dbsTip l' == dbsTip ledgerTip
          then
            pure ()
          else
            writeTVar t (MockedLedgerDB l' (Set.insert ledgerTip oldReachableTips))
        pure Void

{-------------------------------------------------------------------------------
  Conditions
-------------------------------------------------------------------------------}

precondition :: Model blk Symbolic -> Command blk Symbolic -> Logic
-- precondition cfg Model {modelCurrentSize} (Action (TryAddTxs txs)) =
--   Boolean $ not (null txs) && modelCurrentSize > 0 && sum (map tSize rights $ init txs) < modelCurrentSize
precondition m (Action SyncLedger) = Boolean $ not (modelIsSyncing m)
precondition _ _ = Top

postcondition ::
  ( LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  , ValidateEnvelope blk
  , ToExpr (Command blk Concrete)
  , ToExpr (GenTx blk)
  , Show (Diff blk)
  ) =>
  Model blk Concrete ->
  Command blk Concrete ->
  Response blk Concrete ->
  Logic
postcondition model (Action GetSnapshot) (GotSnapshot txs) =
  Annotate "Mismatch getting snapshot" $
    Annotate (show $ modelAllValidTxs model) $
      modelTxs model .== txs
postcondition model c@(Action (TryAddTxs txs)) r@(AddResult res) =
  let model' = transition model c r
   in Annotate "Mismatch result adding transaction" $
        Annotate (show (modelTxs model', zip txs res)) $
          Boolean $
            and
              [ tx `elem` map fst (modelTxs model')
              | (tx, res') <- zip txs res
              , case res' of MempoolTxAdded{} -> True; _ -> False
              ]
postcondition model c@(Action SyncLedger) r@(Synced (_, txs)) =
  let model' = transition model c r
   in Annotate "Mismatch revalidating transactions in Sync" $
        Annotate (show (modelTxs model', txs)) $
          modelTxs model' .== txs
postcondition _ _ _ = Top

noPostcondition ::
  Model blk Concrete ->
  Command blk Concrete ->
  Response blk Concrete ->
  Logic
noPostcondition _ _ _ = Top

shrinker ::
  Model blk Symbolic ->
  Command blk Symbolic ->
  [Command blk Symbolic]
shrinker _ (Action (TryAddTxs txs)) =
  Action . TryAddTxs <$> shrinkList shrinkNothing txs
shrinker _ _ = []

{-------------------------------------------------------------------------------
  State Machine
-------------------------------------------------------------------------------}

sm ::
  ( LedgerSupportsMempool blk
  , IOLike m
  , ValidateEnvelope blk
  ) =>
  StateMachine (Model blk) (Command blk) m (Response blk) ->
  CT.Tracer m String ->
  StrictTVar m (SUT m blk) ->
  StateMachine (Model blk) (Command blk) m (Response blk)
sm sm0 trcr ior = sm0{QC.semantics = \c -> semantics trcr c ior}

smUnused ::
  ( blk ~ TestBlock
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , Monad m
  ) =>
  LedgerConfig blk ->
  DBState blk ->
  TxMeasure blk ->
  MakeAtomic ->
  (Int -> DBState blk -> Gen [GenTx blk]) ->
  StateMachine (Model blk) (Command blk) m (Response blk)
smUnused cfg initialState capacity ma gTxs =
  StateMachine
    { QC.initModel = initModel cfg capacity initialState
    , QC.transition = transition
    , QC.precondition = precondition
    , QC.postcondition =
        case ma of
          NonAtomic -> noPostcondition
          Atomic -> postcondition
          DontCare -> postcondition
    , QC.invariant = Nothing
    , QC.generator = generator ma gTxs
    , QC.shrinker = shrinker
    , QC.semantics = undefined
    , QC.mock = mock
    , QC.cleanup = noCleanup
    }

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_mempoolSequential ::
  forall blk.
  ( HasTxId (GenTx blk)
  , blk ~ TestBlock
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerConfig blk ->
  TxMeasure blk ->
  -- | Initial state
  DBState blk ->
  -- | Transaction generator
  (Int -> DBState blk -> Gen [GenTx blk]) ->
  Property
prop_mempoolSequential cfg capacity initialState gTxs = forAllCommands sm0 Nothing $
  \cmds ->
    monadicIO
      ( do
          (sut, trcr) <- run $ mkSUT cfg initialState
          ior <- run $ newTVarIO sut
          let sm' = sm sm0 trcr ior
          (hist, model, res) <- runCommands sm' cmds
          prettyCommands sm0 hist
            $ checkCommandNames cmds
            $ tabulate
              "Command sequence length"
              [QC.lengthCommands cmds `bucketiseBy` 10]
            $ tabulate
              "Maximum ticket number"
              [(\(TicketNo t) -> t) (modelLastSeenTicketNo model) `bucketiseBy` 5]
            $ tabulate
              "Number of txs to add"
              [ length txs `bucketiseBy` 10
              | (_, Invocation (Action (TryAddTxs txs)) _) <- unHistory hist
              ]
            $ res === Ok
      )
 where
  sm0 = smUnused cfg initialState capacity DontCare gTxs

  bucketiseBy v n =
    let
      l = (v `div` n) * n
     in
      "[" <> show l <> "-" <> show (l + n) <> ")"

prop_mempoolParallel ::
  ( HasTxId (GenTx blk)
  , blk ~ TestBlock
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerConfig blk ->
  TxMeasure blk ->
  DBState blk ->
  MakeAtomic ->
  (Int -> DBState blk -> Gen [GenTx blk]) ->
  Property
prop_mempoolParallel cfg capacity initialState ma gTxs = forAllParallelCommandsNTimes sm0 Nothing 100 $
  \cmds -> monadicIO $ do
    (sut, trcr) <- run $ mkSUT cfg initialState
    ior <- run $ newTVarIO sut
    let sm' = sm sm0 trcr ior
    res <- runParallelCommands sm' cmds
    prettyParallelCommandsWithOpts
      cmds
      (Just (GraphOptions "./mempoolParallel.png" Png))
      res
 where
  sm0 = smUnused cfg initialState capacity ma gTxs

-- | See 'MakeAtomic' on the reasoning behind having these tests.
tests :: TestTree
tests =
  testGroup
    "QSM"
    [ testProperty "sequential" $
        QC.withNumTests 1000 $
          prop_mempoolSequential testLedgerConfigNoSizeLimits txMaxBytes' initDBState $
            genTxsFor
    , testGroup
        "parallel"
        [ testProperty "atomic" $
            QC.withNumTests 10000 $
              prop_mempoolParallel testLedgerConfigNoSizeLimits txMaxBytes' initDBState Atomic $
                genTxsFor
        , testProperty "non atomic" $
            QC.withNumTests 10 $
              prop_mempoolParallel testLedgerConfigNoSizeLimits txMaxBytes' initDBState NonAtomic $
                genTxsFor
        ]
    ]
 where
  -- The genesis UTxO lives inside the mock ledger state, so the model's
  -- 'DBState' values are projected straight out of it.
  initDBState = DBState testInitLedger (mockUtxo (simpleLedgerState testInitLedger))

  genTxsFor i = fmap (fmap fst . fst) . genTxs i . dbsState

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | The 'TestBlock' txMaxBytes is fixed to a very high number. We use this
-- local declaration to have a mempool that sometimes fill but still don't make
-- it configurable.
txMaxBytes' :: TxMeasure TestBlock
txMaxBytes' = TxMeasure (IgnoringOverflow $ ByteSize32 maxBound) TrivialTxMeasurePhase2

instance
  (StandardHash blk, GetTip (LedgerState blk)) =>
  Eq (DBState blk)
  where
  (==) = (==) `on` dbsTip

instance
  (UnTick blk, StandardHash blk, GetTip (LedgerState blk)) =>
  Eq (TickedState blk)
  where
  (==) = (==) `on` (getTip . unTick . tsState)

instance
  (StandardHash blk, GetTip (LedgerState blk)) =>
  Ord (DBState blk)
  where
  compare = compare `on` dbsTip

instance (Eq (Validated (GenTx blk)), m ~ TxMeasure blk, Eq m) => Eq (TxSeq m (Validated (GenTx blk))) where
  s1 == s2 = toList s1 == toList s2

instance NoThunks (Mempool IO TestBlock) where
  showTypeOf _ = showTypeOf (Proxy @(Mempool IO TestBlock))
  wNoThunks _ _ = return Nothing

instance
  ( ToExpr (TxId (GenTx blk))
  , ToExpr (GenTx blk)
  , ToExpr (DBState blk)
  , ToExpr (TickedState blk)
  , LedgerSupportsMempool blk
  ) =>
  ToExpr (Model blk r)
  where
  toExpr model =
    Rec "Model" $
      TD.fromList
        [ ("mempoolTip", toExpr $ modelMempoolIntermediateState model)
        , ("ledgerTip", toExpr $ modelLedgerDBTip model)
        , ("txs", toExpr $ modelTxs model)
        , ("size", toExpr $ unByteSize32 $ txMeasureByteSize $ modelCurrentSize model)
        , ("capacity", toExpr $ unByteSize32 $ txMeasureByteSize $ modelCapacity model)
        , ("lastTicket", toExpr $ modelLastSeenTicketNo model)
        ]

instance
  ( ToExpr (TxId (GenTx blk))
  , ToExpr (GenTx blk)
  , ToExpr (TickedState blk)
  , ToExpr (DBState blk)
  , LedgerSupportsMempool blk
  ) =>
  Show (Model blk r)
  where
  show = show . toExpr

instance ToExpr (Action TestBlock r) where
  toExpr (TryAddTxs txs) =
    App "TryAddTxs" $
      [ App
          ( take 8 (tail $ init $ show txid)
              <> " "
              <> filter (/= '"') (show [(take 8 (tail $ init $ show a), b) | (a, b) <- Set.toList txins])
              <> " ->> "
              <> filter (/= '"') (show [(condense a, b) | (_, (a, b)) <- Map.toList txouts])
              <> ""
          )
          []
      | SimpleGenTx tx txid <- txs
      , let txins = Mock.txIns tx
      , let txouts = Mock.txOuts tx
      ]
  toExpr SyncLedger = App "SyncLedger" []
  toExpr GetSnapshot = App "GetSnapshot" []

instance ToExpr (DBState blk) => ToExpr (Event blk r) where
  toExpr (ChangeLedger ls) =
    App "ChangeLedger" [toExpr ls]

instance ToExpr (Command TestBlock r) where
  toExpr (Action act) = toExpr act
  toExpr (Event ev) = toExpr ev

instance ToExpr (Command blk r) => Show (Command blk r) where
  show =
    -- unwords . take 2 . words .
    show . toExpr

instance
  ( ToExpr (GenTx blk)
  , LedgerSupportsMempool blk
  ) =>
  ToExpr (Response blk r)
  where
  toExpr Void = App "Void" []
  toExpr (GotSnapshot s) =
    App
      "GotSnapshot"
      [Lst [toExpr s]]
  toExpr (AddResult res) =
    App "AddResult" $
      [ Lst $
          map
            ( (flip App []) . \case
                MempoolTxAdded{} -> "OK"
                MempoolTxRejected{} -> "NO"
            )
            res
      ]
  toExpr (Synced res) =
    App "Synced" [App (show res) []]

instance
  ( ToExpr (GenTx blk)
  , LedgerSupportsMempool blk
  ) =>
  Show (Response blk r)
  where
  show =
    -- unwords . take 2 . words .
    show . toExpr

deriving anyclass instance
  (NoThunks (LedgerState blk), NoThunks (Values blk)) =>
  NoThunks (DBState blk)

deriving instance NoThunks (DBState blk) => NoThunks (MockedLedgerDB blk)

instance Arbitrary (DBState TestBlock) where
  arbitrary = do
    n <- getPositive <$> arbitrary
    (txs, _) <- genValidTxs n testInitLedger
    case runExcept $ repeatedlyM (flip (applyTxToLedger testLedgerConfigNoSizeLimits)) txs testInitLedger of
      Left _ -> error "Must not happen"
      Right st -> pure (DBState st (mockUtxo (simpleLedgerState st)))

instance ToExpr (DBState TestBlock) where
  toExpr (DBState (SimpleLedgerState st) vals) =
    App "LedgerState" $
      [ Lst
          [ toExpr (pointSlot $ mockTip st, pointHash $ mockTip st)
          , valuesToExpr vals
          ]
      ]

instance ToExpr (TickedState TestBlock) where
  toExpr (TickedState st vals) =
    App "Ticked" [toExpr (DBState (getTickedSimpleLedgerState st) vals)]

-- | Render the on-disk UTxO 'Values' (a @'Map' 'TxIn' 'TxOut'@) for the model's
-- 'ToExpr'.
valuesToExpr :: Values TestBlock -> Expr
valuesToExpr v =
  Lst [toExpr (condense txin, condense txout) | (txin, txout) <- Map.toList v]

instance ToExpr Addr where
  toExpr a = App (show a) []

deriving instance ToExpr (GenTx TestBlock)
deriving instance ToExpr Tx
deriving instance ToExpr Expiry

class UnTick blk where
  unTick :: TickedLedgerState blk -> LedgerState blk

instance UnTick TestBlock where
  unTick = getTickedSimpleLedgerState
