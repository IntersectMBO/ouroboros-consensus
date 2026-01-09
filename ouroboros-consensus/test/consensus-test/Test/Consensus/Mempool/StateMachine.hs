{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Except (runExcept)
import Control.ResourceRegistry
import qualified Control.Tracer as CT (Tracer (..), traceWith)
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
import Ouroboros.Consensus.Ledger.Basics hiding (TxIn, TxOut)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.Mempool.Impl.Common (MempoolLedgerDBView (..), tickLedgerState)
import Ouroboros.Consensus.Mempool.TxSeq
import Ouroboros.Consensus.Mock.Ledger.Address
import Ouroboros.Consensus.Mock.Ledger.Block
import Ouroboros.Consensus.Mock.Ledger.State
import Ouroboros.Consensus.Mock.Ledger.UTxO (Expiry, Tx, TxIn, TxOut)
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
import Test.Util.ToExpr ()

{-------------------------------------------------------------------------------
  Datatypes
-------------------------------------------------------------------------------}

-- | The model
data Model blk r = Model
  { modelMempoolIntermediateState :: !(TickedLedgerState blk ValuesMK)
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
  , modelConfig :: !(LedgerCfg (LedgerState blk))
  , --  * LedgerDB

    modelLedgerDBTip :: !(LedgerState blk ValuesMK)
  -- ^ The current tip on the ledgerdb
  , modelLedgerDBOtherStates :: !(Set (LedgerState blk ValuesMK))
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
      !(LedgerState blk ValuesMK)
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
  ( Arbitrary (LedgerState blk ValuesMK)
  , UnTick blk
  , StandardHash blk
  , GetTip (LedgerState blk)
  ) =>
  MakeAtomic ->
  -- | Transaction generator based on an state
  (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk]) ->
  Model blk Symbolic ->
  Maybe (Gen (Command blk Symbolic))
generator ma gTxs model =
  Just $
    frequency
      [
        ( 100
        , Action . TryAddTxs <$> case ma of
            Atomic -> do
              gTxs 1 . unTick $ modelMempoolIntermediateState
            _ -> do
              n <- getPositive <$> arbitrary
              gTxs n . unTick $ modelMempoolIntermediateState
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
                                       ( getTip modelLedgerDBTip
                                           `Set.insert` Set.map
                                             getTip
                                             modelLedgerDBOtherStates
                                       )
                                     . getTip
                                 )
                  ]
                    ++ (if Set.null modelLedgerDBOtherStates then [] else [elements (Set.toList modelLedgerDBOtherStates)])
                )
                `suchThat` (not . (== (getTip modelLedgerDBTip)) . getTip)
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
  LedgerState blk ValuesMK ->
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
  , Eq (TickedLedgerState blk ValuesMK)
  ) =>
  Model blk r ->
  Model blk r
doSync model =
  if st == st'
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
  LedgerState blk ValuesMK ->
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
  ) =>
  Model blk r ->
  [GenTx blk] ->
  Model blk r
doTryAddTxs model [] = model
doTryAddTxs model txs =
  case Foldable.find
    ((castPoint (getTip st) ==) . getTip)
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
  ( Eq (TickedLedgerState blk ValuesMK)
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
  TickedLedgerState blk ValuesMK ->
  [(GenTx blk, Maybe TicketNo)] ->
  ( [(GenTx blk, TicketNo)]
  , TicketNo
  , TxMeasure blk
  , TickedLedgerState blk ValuesMK
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
    let slot = case getTipSlot st of
          Origin -> minimumPossibleSlotNo (Proxy @blk)
          At v -> v + 1
     in case runExcept $ (,) <$> txMeasure cfg st tx <*> applyTx cfg DoNotIntervene slot tx st of
          Left{} ->
            go
              ( acc
              , tk
              , curSize
              , st
              )
              next
          Right (txsz, (st', vtx))
            | ( curSize Measure.<= curSize `Measure.plus` txsz
                  -- Overflow
                  && curSize `Measure.plus` txsz Measure.<= capacity
              ) ->
                -- fits

                go
                  ( (txForgetValidated vtx, fromMaybe tk txtk) : acc
                  , succ tk
                  , curSize `Measure.plus` txsz
                  , applyDiffs st st'
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

tick ::
  ( ValidateEnvelope blk
  , LedgerSupportsMempool blk
  ) =>
  LedgerConfig blk ->
  LedgerState blk ValuesMK ->
  TickedLedgerState blk ValuesMK
tick cfg st = applyDiffs st ticked
 where
  ticked =
    snd
      . tickLedgerState cfg
      . ForgeInUnknownSlot
      . forgetLedgerTables
      $ st

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
  ) =>
  NoThunks (SUT m blk)

-- | A very minimal mock of the ledger db.
--
-- The ledger interface will serve the values from this datatype.
data MockedLedgerDB blk = MockedLedgerDB
  { ldbTip :: !(LedgerState blk ValuesMK)
  -- ^ The current LedgerDB tip
  , reachableTips :: !(Set (LedgerState blk ValuesMK))
  -- ^ States which are still reachable in the LedgerDB
  }
  deriving Generic

-- | Create a ledger interface and provide the tvar to modify it when switching
-- ledgers.
newLedgerInterface ::
  ( MonadSTM m
  , NoThunks (MockedLedgerDB blk)
  , LedgerSupportsMempool blk
  ) =>
  LedgerState blk ValuesMK ->
  m (LedgerInterface m blk, StrictTVar m (MockedLedgerDB blk))
newLedgerInterface initialLedger = do
  t <- newTVarIO $ MockedLedgerDB initialLedger Set.empty
  pure
    ( LedgerInterface
        { getCurrentLedgerState = \_reg -> do
            st <- ldbTip <$> readTVar t
            pure $
              MempoolLedgerDBView
                (forgetLedgerTables st)
                ( pure $
                    Right $
                      ReadOnlyForker
                        { roforkerClose = pure ()
                        , roforkerReadStatistics = pure Nothing
                        , roforkerReadTables = pure . (projectLedgerTables st `restrictValues'`)
                        , roforkerRangeReadTables = const $ pure (emptyLedgerTables, Nothing)
                        , roforkerGetLedgerState = pure $ forgetLedgerTables st
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
  , LedgerSupportsProtocol blk
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , HasTxHash (GenTx blk)
  ) =>
  LedgerConfig blk ->
  LedgerState blk ValuesMK ->
  m (SUT m blk, CT.Tracer m String, ResourceRegistry m)
mkSUT cfg initialLedger = do
  (lif, t) <- newLedgerInterface initialLedger
  trcrChan <- atomically newTChan :: m (StrictTChan m (Either String (TraceEventMempool blk)))
  let trcr =
        CT.Tracer $ -- Dbg.traceShowM @(Either String (TraceEventMempool blk))
          atomically . writeTChan trcrChan
  reg <- unsafeNewRegistry
  mempool <-
    openMempoolWithoutSyncThread
      reg
      lif
      cfg
      (MempoolCapacityBytesOverride $ unIgnoringOverflow txMaxBytes')
      (CT.Tracer $ CT.traceWith trcr . Right)
  pure (SUT mempool t, CT.Tracer $ atomically . writeTChan trcrChan . Left, reg)

semantics ::
  ( MonadSTM m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
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
      CT.traceWith trcr $ "ChangingLedger to " <> show (getTip l')
      atomically $ do
        MockedLedgerDB ledgerTip oldReachableTips <- readTVar t
        if getTip l' == getTip ledgerTip
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
  , --  , Show (TickedLedgerState blk ValuesMK)
    UnTick blk
  , ValidateEnvelope blk
  , ToExpr (Command blk Concrete)
  , ToExpr (GenTx blk)
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
  ResourceRegistry m ->
  StateMachine (Model blk) (Command blk) m (Response blk)
sm sm0 trcr ior reg =
  sm0
    { QC.semantics = \c -> semantics trcr c ior
    , QC.cleanup = \_ -> closeRegistry reg
    }

smUnused ::
  ( blk ~ TestBlock
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , Monad m
  ) =>
  LedgerConfig blk ->
  LedgerState blk ValuesMK ->
  TxMeasure blk ->
  MakeAtomic ->
  (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk]) ->
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
  LedgerState blk ValuesMK ->
  -- | Transaction generator
  (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk]) ->
  Property
prop_mempoolSequential cfg capacity initialState gTxs = forAllCommands sm0 Nothing $
  \cmds ->
    monadicIO
      ( do
          (sut, trcr, reg) <- run $ mkSUT cfg initialState
          ior <- run $ newTVarIO sut
          let sm' = sm sm0 trcr ior reg
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
  LedgerState blk ValuesMK ->
  MakeAtomic ->
  (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk]) ->
  Property
prop_mempoolParallel cfg capacity initialState ma gTxs = forAllParallelCommandsNTimes sm0 Nothing 100 $
  \cmds -> monadicIO $ do
    (sut, trcr, reg) <- run $ mkSUT cfg initialState
    ior <- run $ newTVarIO sut
    let sm' = sm sm0 trcr ior reg
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
        withMaxSuccess 1000 $
          prop_mempoolSequential testLedgerConfigNoSizeLimits txMaxBytes' testInitLedger $
            \i -> fmap (fmap fst . fst) . genTxs i
    , testGroup
        "parallel"
        [ testProperty "atomic" $
            withMaxSuccess 10000 $
              prop_mempoolParallel testLedgerConfigNoSizeLimits txMaxBytes' testInitLedger Atomic $
                \i -> fmap (fmap fst . fst) . genTxs i
        , testProperty "non atomic" $
            withMaxSuccess 10 $
              prop_mempoolParallel testLedgerConfigNoSizeLimits txMaxBytes' testInitLedger NonAtomic $
                \i -> fmap (fmap fst . fst) . genTxs i
        ]
    ]

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | The 'TestBlock' txMaxBytes is fixed to a very high number. We use this
-- local declaration to have a mempool that sometimes fill but still don't make
-- it configurable.
txMaxBytes' :: IgnoringOverflow ByteSize32
txMaxBytes' = IgnoringOverflow $ ByteSize32 maxBound

instance
  (StandardHash blk, GetTip (LedgerState blk)) =>
  Eq (LedgerState blk ValuesMK)
  where
  (==) = (==) `on` getTip

instance
  (UnTick blk, StandardHash blk, GetTip (LedgerState blk)) =>
  Eq (TickedLedgerState blk ValuesMK)
  where
  (==) = (==) `on` (getTip . unTick)

instance
  (StandardHash blk, GetTip (LedgerState blk)) =>
  Ord (LedgerState blk ValuesMK)
  where
  compare = compare `on` getTip

instance (Eq (Validated (GenTx blk)), m ~ TxMeasure blk, Eq m) => Eq (TxSeq m (Validated (GenTx blk))) where
  s1 == s2 = toList s1 == toList s2

instance NoThunks (Mempool IO TestBlock) where
  showTypeOf _ = showTypeOf (Proxy @(Mempool IO TestBlock))
  wNoThunks _ _ = return Nothing

instance
  ( ToExpr (TxId (GenTx blk))
  , ToExpr (GenTx blk)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (TickedLedgerState blk ValuesMK)
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
  , ToExpr (TickedLedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
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

instance ToExpr (LedgerState blk ValuesMK) => ToExpr (Event blk r) where
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

deriving instance NoThunks (LedgerState blk ValuesMK) => NoThunks (MockedLedgerDB blk)

instance Arbitrary (LedgerState TestBlock ValuesMK) where
  arbitrary = do
    n <- getPositive <$> arbitrary
    (txs, _) <- genValidTxs n testInitLedger
    case runExcept $ repeatedlyM (flip (applyTxToLedger testLedgerConfigNoSizeLimits)) txs testInitLedger of
      Left _ -> error "Must not happen"
      Right st -> pure st

instance ToExpr (TickedLedgerState TestBlock ValuesMK) where
  toExpr (TickedSimpleLedgerState st) = App "Ticked" [toExpr st]

instance ToExpr (LedgerState TestBlock ValuesMK) where
  toExpr (SimpleLedgerState st tbs) =
    App "LedgerState" $
      [ Lst
          [ toExpr (pointSlot $ mockTip st, pointHash $ mockTip st)
          , toExpr tbs
          ]
      ]

instance ToExpr Addr where
  toExpr a = App (show a) []

deriving instance ToExpr (GenTx TestBlock)
deriving instance ToExpr Tx
deriving instance ToExpr Expiry

instance ToExpr (LedgerTables (LedgerState TestBlock) ValuesMK) where
  toExpr (LedgerTables (ValuesMK v)) = Lst [toExpr (condense txin, condense txout) | (txin, txout) <- Map.toList v]

instance ToExpr (ValuesMK TxIn TxOut) where
  toExpr (ValuesMK m) = App "Values" [toExpr m]

class UnTick blk where
  unTick :: forall mk. TickedLedgerState blk mk -> LedgerState blk mk

instance UnTick TestBlock where
  unTick = getTickedSimpleLedgerState
