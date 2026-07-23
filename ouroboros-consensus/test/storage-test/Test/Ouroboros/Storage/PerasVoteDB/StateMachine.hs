{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Ouroboros.Storage.PerasVoteDB.StateMachine (tests) where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import qualified Cardano.Ledger.Keys as SL
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Monad (join)
import Control.Monad.Class.MonadThrow (MonadCatch (..))
import Control.Monad.State
  ( MonadState (..)
  , MonadTrans (..)
  , StateT
  , evalStateT
  )
import Control.Tracer (nullTracer)
import Data.Char (chr)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract (Point (..), SlotNo (..))
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , HasPerasVoteBlock (..)
  , HasPerasVoteRound (..)
  , PerasRoundNo (..)
  , PerasVote (..)
  , PerasVoteId
  , PerasVoteStake (..)
  , PerasVoteTarget (..)
  , PerasVoterId (..)
  , ValidatedPerasCert
  , ValidatedPerasVote (..)
  , mkPerasParams
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.Storage.PerasVoteDB
  ( AddPerasVoteResult (..)
  , PerasVoteDB
  , PerasVoteDbError
  , PerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB as PerasVoteDB
import Ouroboros.Consensus.Storage.PerasVoteDB.Impl (PerasVoteDbError (..))
import Ouroboros.Consensus.Util.Orphans ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Ouroboros.Storage.Orphans ()
import qualified Test.Ouroboros.Storage.PerasVoteDB.Model as Model
import Test.QuickCheck
  ( Arbitrary (..)
  , Property
  , choose
  , elements
  , frequency
  , ioProperty
  , tabulate
  )
import Test.QuickCheck.Monadic (PropertyM, monadic)
import Test.QuickCheck.StateModel
  ( Actions
  , Any (..)
  , HasVariables (..)
  , RunModel (..)
  , StateModel (..)
  , counterexamplePost
  , runActions
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock (TestBlock, TestHash (..))
import Test.Util.TestEnv (adjustQuickCheckMaxSize, adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "PerasVoteDB"
    [ adjustQuickCheckMaxSize (* 5) $
        adjustQuickCheckTests (* 10) $
          testProperty "q-d" $
            prop_qd
    ]

perasTestCfg :: PerasCfg TestBlock
perasTestCfg = mkPerasParams

prop_qd :: Actions Model -> Property
prop_qd actions = monadic runActualImplemMonad resultAsPropertyM
 where
  -- This runs actions on the model and the actual implementation alongside,
  -- and checks the postconditions after each action.
  -- The `PropertyM` wrapper is keeping track of success/failure of the
  -- postconditions, so we don't care about the payload returned by `runActions`.
  resultAsPropertyM :: PropertyM (StateT (PerasVoteDB IO TestBlock) IO) ()
  resultAsPropertyM = runActions actions $> ()

  -- This function is in charge of collapsing/running the different monadic
  -- layers on top of our property.
  runActualImplemMonad :: StateT (PerasVoteDB IO TestBlock) IO Property -> Property
  runActualImplemMonad statefulIoProp =
    -- The _Model_ starts in `Model.open = False`, so `precondition` dictates
    -- that the first action must be `CreateDB`, and this `CreateDB` action will
    -- initialize the `StateT` state with a valid `PerasVoteDB` instance.
    -- The actual implementation state can only be read in `postcondition`, so
    -- we are sure that the `StateT` state will be properly initialized beforehand.
    let ioProp = evalStateT statefulIoProp (error "Trying to access uninitialized PerasVoteDB")
     in ioProperty ioProp

newtype Model = Model (Model.Model TestBlock)
  deriving (Show, Generic)

instance StateModel Model where
  data Action Model a where
    CreateDB ::
      Action Model ()
    AddVote ::
      WithArrivalTime (ValidatedPerasVote TestBlock) ->
      Action
        Model
        ( Either
            (PerasVoteDbError TestBlock)
            (AddPerasVoteResult TestBlock)
        )
    GetVoteIds ::
      Action Model (Set (PerasVoteId TestBlock))
    GetVotesAfter ::
      PerasVoteTicketNo ->
      Action
        Model
        ( Map
            PerasVoteTicketNo
            (WithArrivalTime (ValidatedPerasVote TestBlock))
        )
    GetForgedCertForRound ::
      PerasRoundNo ->
      Action Model (Maybe (ValidatedPerasCert TestBlock))
    GarbageCollect ::
      PerasRoundNo ->
      Action Model ()

  arbitraryAction _ (Model m)
    | not (Model.open m) =
        Some <$> genCreateDB
    | otherwise =
        frequency
          [ (1000, Some <$> genAddVote)
          , (10, Some <$> genGetVoteIds)
          , (10, Some <$> genGetVotesAfter)
          , (5, Some <$> genGetForgedCertForRound)
          , (2, Some <$> genGarbageCollect)
          ]
   where
    genCreateDB = do
      pure CreateDB

    genAddVote = do
      roundNo <- genRoundNo
      point <- genPoint
      voterId <- genVoterId
      stake <- genVoteStake
      now <- genRelativeTime
      let voteWithTime =
            WithArrivalTime now $
              ValidatedPerasVote
                { vpvVote =
                    PerasVote
                      { pvVoteRound = roundNo
                      , pvVoteBlock = point
                      , pvVoteVoterId = voterId
                      }
                , vpvVoteStake = stake
                }
      return (AddVote voteWithTime)

    genGetVoteIds = do
      pure GetVoteIds

    genGetVotesAfter = do
      ticketNo <- toEnum <$> choose (0, fromEnum (Model.lastTicketNo m) + 1)
      pure (GetVotesAfter ticketNo)

    genGetForgedCertForRound = do
      roundNo <- genRoundNo
      pure (GetForgedCertForRound roundNo)

    genGarbageCollect = do
      roundNo <- genRoundNo
      pure (GarbageCollect roundNo)

    genPoint = do
      frequency
        [
          ( 1
          , return GenesisPoint
          )
        ,
          ( 50
          , do
              slotNo <- SlotNo <$> choose @Word64 (0, 9)
              hash <- TestHash <$> NE.fromList . pure <$> choose (0, 9)
              return (BlockPoint slotNo hash)
          )
        ]

    genRoundNo = do
      n <- choose @Word64 (0, 9)
      pure (PerasRoundNo n)

    genVoterId = do
      -- We want to force collisions when adding votes, so we need to restrict
      -- the key space a lot here. Otherwise we might never hit the case where
      -- the same voter casts two votes for the same round/block.
      let mkVoterKey = fromString . replicate 32
      bytes <- mkVoterKey <$> elements [chr c | c <- [0 .. 99]]
      let signKey = SL.genKeyDSIGN (SL.mkSeedFromBytes bytes)
      let verKey = SL.deriveVerKeyDSIGN signKey
      let keyHash = SL.hashKey (SL.VKey verKey)
      pure (PerasVoterId keyHash)

    genVoteStake = do
      -- Make it so that we always require multiple votes to reach a quorum.
      -- This is assumming a quorum threshold strictly larger than 50%, which is
      -- a very conservative assumption for Peras.
      stake <- (1 %) <$> choose (2, 10) -- stake between 1/2 and 1/10
      pure (PerasVoteStake stake)

    genRelativeTime = do
      time <- fromIntegral <$> arbitrary @Word64
      pure (RelativeTime time)

  initialState =
    Model (Model.initModel perasTestCfg)

  nextState (Model m) action _ =
    case action of
      CreateDB -> Model $ Model.openDB m
      AddVote vote -> Model $ snd $ Model.addVote vote m
      GetVoteIds -> Model $ m
      GetVotesAfter _ -> Model $ m
      GetForgedCertForRound _ -> Model $ m
      GarbageCollect roundNo -> Model $ Model.garbageCollect roundNo m

  precondition (Model m) action =
    case action of
      CreateDB -> not (Model.open m)
      AddVote _ -> Model.open m
      GetVoteIds -> Model.open m
      GetVotesAfter _ -> Model.open m
      GetForgedCertForRound _ -> Model.open m
      GarbageCollect _ -> Model.open m

deriving stock instance Show (Action Model a)
deriving stock instance Eq (Action Model a)

instance HasVariables (Action Model a) where
  getAllVariables _ = mempty

instance RunModel Model (StateT (PerasVoteDB IO TestBlock) IO) where
  perform _ action _ =
    case action of
      CreateDB -> do
        let args = PerasVoteDB.PerasVoteDbArgs nullTracer perasTestCfg
        voteDB <- lift $ PerasVoteDB.createDB args
        put voteDB
      AddVote vote -> do
        voteDB <- get
        lift $ try $ join $ atomically $ PerasVoteDB.addVote voteDB vote
      GetVoteIds -> do
        voteDB <- get
        lift $ atomically $ PerasVoteDB.getVoteIds voteDB
      GetVotesAfter ticketNo -> do
        voteDB <- get
        lift $ atomically $ PerasVoteDB.getVotesAfter voteDB ticketNo
      GetForgedCertForRound roundNo -> do
        voteDB <- get
        lift $ atomically $ PerasVoteDB.getForgedCertForRound voteDB roundNo
      GarbageCollect roundNo -> do
        voteDB <- get
        lift $ join $ atomically $ PerasVoteDB.garbageCollect voteDB roundNo

  postcondition (Model model, _) (AddVote vote) _ actual = do
    let (expected, _) = Model.addVote vote model
    case (expected, actual) of
      (Right expectedRes, Right actualRes) -> do
        counterexamplePost $ "Mismatched success results:"
        counterexamplePost $ "Model: " <> show expectedRes
        counterexamplePost $ "SUT: " <> show actualRes
        pure $ expectedRes == actualRes
      (Left expectedErr, Left actualErr) -> do
        counterexamplePost $ "Mismatched failure results:"
        counterexamplePost $ "Model: " <> show expectedErr
        counterexamplePost $ "SUT: " <> show actualErr
        case (expectedErr, actualErr) of
          ( Model.MultipleWinnersInRound roundNo
            , MultipleWinnersInRound roundNo' _ _
            ) ->
              pure $ roundNo == roundNo'
          _ ->
            pure False
      (Right expectedRes, Left actualErr) -> do
        counterexamplePost $ "Expected success, but got error"
        counterexamplePost $ "Model: " <> show expectedRes
        counterexamplePost $ "SUT: " <> show actualErr
        pure False
      (Left expectedErr, Right actualRes) -> do
        counterexamplePost $ "Expected error, but got success:"
        counterexamplePost $ "Model: " <> show expectedErr
        counterexamplePost $ "SUT: " <> show actualRes
        pure False
  postcondition (Model model, _) (GetVoteIds) _ actual = do
    let expected = Model.getVoteIds model
    counterexamplePost $ "Mismatched result:"
    counterexamplePost $ "Model: " <> show expected
    counterexamplePost $ "SUT: " <> show actual
    pure $ expected == actual
  postcondition (Model model, _) (GetVotesAfter ticketNo) _ actual = do
    let expected = Model.getVotesAfter ticketNo model
    counterexamplePost $ "Mismatched result:"
    counterexamplePost $ "Model: " <> show expected
    counterexamplePost $ "SUT: " <> show actual
    pure $ expected == actual
  postcondition (Model model, _) (GetForgedCertForRound roundNo) _ actual = do
    let expected = Map.lookup roundNo (Model.certs model)
    counterexamplePost $ "Mismatched result:"
    counterexamplePost $ "Model: " <> show expected
    counterexamplePost $ "SUT: " <> show actual
    pure $ expected == actual
  postcondition _ _ _ _ = pure True

  monitoring (_, Model model') (AddVote vote) _ (Right res) = do
    tabulate "AddVote" [either perasVoteDBErrorTag addVoteResultTag res]
      -- In addition to the result of the command, we also tabulate how many
      -- votes were needed to reach quorum (if quorum was reached).
      . tabulate "Votes until quorum" (votesToReachQuorum model' vote res)
  monitoring _ (GetVoteIds) _ (Right res) = do
    let tag = if Set.null res then "NoVoteIds" else "HasVoteIds"
    tabulate "GetVoteIds" [tag]
  monitoring _ (GetVotesAfter _) _ (Right res) = do
    let tag = if Map.null res then "NoVotesAfter" else "HasVotesAfter"
    tabulate "GetVotesAfter" [tag]
  monitoring _ (GetForgedCertForRound{}) _ (Right res) = do
    let tag = maybe "NoCert" (const "FoundCert") res
    tabulate "GetForgedCertForRound" [tag]
  monitoring _ _ _ _ =
    id

-- * Helpers

perasVoteDBErrorTag :: PerasVoteDbError TestBlock -> String
perasVoteDBErrorTag err =
  case err of
    MultipleWinnersInRound{} ->
      "MultipleWinnersInRound"
    ForgingCertError{} ->
      "ForgingCertError"

addVoteResultTag :: AddPerasVoteResult TestBlock -> String
addVoteResultTag res =
  case res of
    PerasVoteAlreadyInDB ->
      "PerasVoteAlreadyInDB"
    AddedPerasVoteAndGeneratedNewCert{} ->
      "AddedPerasVoteAndGeneratedNewCert"
    AddedPerasVoteButDidntGenerateNewCert ->
      "AddedPerasVoteButDidntGenerateNewCert"

votesToReachQuorum ::
  Model.Model TestBlock ->
  WithArrivalTime (ValidatedPerasVote TestBlock) ->
  Either (PerasVoteDbError TestBlock) (AddPerasVoteResult TestBlock) ->
  [String]
votesToReachQuorum model vote res =
  case res of
    Right AddedPerasVoteAndGeneratedNewCert{} ->
      [show (Set.size votesInRound)]
    _ ->
      []
 where
  votesInRound =
    Map.findWithDefault
      Set.empty
      PerasVoteTarget
        { pvtRoundNo = getPerasVoteRound vote
        , pvtBlock = getPerasVoteBlock vote
        }
      (Model.votes model)
