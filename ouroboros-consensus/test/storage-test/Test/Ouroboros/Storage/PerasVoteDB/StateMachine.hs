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
import Control.Monad.Class.MonadThrow (MonadCatch (..))
import Control.Monad.State
  ( MonadState (..)
  , MonadTrans (..)
  , StateT
  , evalStateT
  )
import Control.Tracer (nullTracer)
import Data.Char (chr)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ratio ((%))
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
  , PerasVoteSnapshot
  , getVotesAfter
  , zeroPerasVoteTicketNo
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
  , Testable (..)
  , choose
  , elements
  , frequency
  , ioProperty
  , tabulate
  )
import Test.QuickCheck.Monadic (monadic)
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
prop_qd actions = monadic f $ property () <$ runActions actions
 where
  f :: StateT (PerasVoteDB IO TestBlock) IO Property -> Property
  f = ioProperty . flip evalStateT (error "unreachable")

newtype Model = Model (Model.Model TestBlock)
  deriving (Show, Generic)

instance StateModel Model where
  data Action Model a where
    OpenDB ::
      Action Model ()
    CloseDB ::
      Action Model ()
    AddVote ::
      WithArrivalTime (ValidatedPerasVote TestBlock) ->
      Action
        Model
        ( Either
            (PerasVoteDbError TestBlock)
            (AddPerasVoteResult TestBlock)
        )
    GetForgedCertForRound ::
      PerasRoundNo ->
      Action Model (Maybe (ValidatedPerasCert TestBlock))
    GetVoteSnapshot ::
      Action Model (PerasVoteSnapshot TestBlock)
    GarbageCollect ::
      PerasRoundNo ->
      Action Model ()

  arbitraryAction _ (Model m)
    | not (Model.open m) =
        Some <$> genOpenDB
    | otherwise =
        frequency
          [ (1, Some <$> genCloseDB)
          , (1000, Some <$> genAddVote)
          , (5, Some <$> genGetForgedCertForRound)
          , (5, Some <$> genGetVoteSnapshot)
          , (2, Some <$> genGarbageCollect)
          ]
   where
    genOpenDB = do
      pure OpenDB

    genCloseDB = do
      pure CloseDB

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

    genGetForgedCertForRound = do
      roundNo <- genRoundNo
      pure (GetForgedCertForRound roundNo)

    genGetVoteSnapshot = do
      pure GetVoteSnapshot

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
      OpenDB -> Model $ Model.openDB m
      CloseDB -> Model $ Model.closeDB m
      AddVote vote -> Model $ snd $ Model.addVote vote m
      GetForgedCertForRound _ -> Model $ m
      GetVoteSnapshot -> Model $ m
      GarbageCollect roundNo -> Model $ Model.garbageCollect roundNo m

  precondition (Model m) action =
    case action of
      OpenDB -> not (Model.open m)
      CloseDB -> Model.open m
      AddVote _ -> Model.open m
      GetForgedCertForRound _ -> Model.open m
      GetVoteSnapshot -> Model.open m
      GarbageCollect _ -> Model.open m

deriving stock instance Show (Action Model a)
deriving stock instance Eq (Action Model a)

instance HasVariables (Action Model a) where
  getAllVariables _ = mempty

instance RunModel Model (StateT (PerasVoteDB IO TestBlock) IO) where
  perform _ action _ =
    case action of
      OpenDB -> do
        let args = PerasVoteDB.PerasVoteDbArgs nullTracer perasTestCfg
        voteDB <- lift $ PerasVoteDB.openDB args
        put voteDB
      CloseDB -> do
        voteDB <- get
        lift $ PerasVoteDB.closeDB voteDB
      AddVote vote -> do
        voteDB <- get
        lift $ try $ PerasVoteDB.addVote voteDB vote
      GetForgedCertForRound roundNo -> do
        voteDB <- get
        lift $ atomically $ PerasVoteDB.getForgedCertForRound voteDB roundNo
      GetVoteSnapshot -> do
        voteDB <- get
        lift $ atomically $ PerasVoteDB.getVoteSnapshot voteDB
      GarbageCollect roundNo -> do
        voteDB <- get
        lift $ PerasVoteDB.garbageCollect voteDB roundNo

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
  postcondition (Model model, _) (GetForgedCertForRound roundNo) _ actual = do
    let expected = Map.lookup roundNo (Model.certs model)
    counterexamplePost $ "Mismatched result:"
    counterexamplePost $ "Model: " <> show expected
    counterexamplePost $ "SUT: " <> show actual
    pure $ expected == actual
  postcondition (Model model, _) GetVoteSnapshot _ actual = do
    let expected = Model.getVoteSnapshot model
    let allVotesExpected = getVotesAfter expected zeroPerasVoteTicketNo
    let allVotesActual = getVotesAfter actual zeroPerasVoteTicketNo
    counterexamplePost $ "Mismatched result:"
    counterexamplePost $ "Model: " <> show allVotesExpected
    counterexamplePost $ "SUT: " <> show allVotesActual
    pure $ allVotesExpected == allVotesActual
  postcondition _ _ _ _ = pure True

  monitoring (_, Model model') (AddVote vote) _ (Right res) = do
    tabulate "AddVote" [either perasVoteDBErrorTag addVoteResultTag res]
      -- In addition to the result of the command, we also tabulate how many
      -- votes were needed to reach quorum (if quorum was reached).
      . tabulate "Votes until quorum" (votesToReachQuorum model' vote res)
  monitoring _ (GetForgedCertForRound{}) _ (Right res) = do
    let tag = maybe "NoCert" (const "FoundCert") res
    tabulate "GetForgedCertForRound" [tag]
  monitoring _ _ _ _ =
    id

-- * Helpers

perasVoteDBErrorTag :: PerasVoteDbError TestBlock -> String
perasVoteDBErrorTag err =
  case err of
    ClosedDBError{} ->
      "ClosedDBError"
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
