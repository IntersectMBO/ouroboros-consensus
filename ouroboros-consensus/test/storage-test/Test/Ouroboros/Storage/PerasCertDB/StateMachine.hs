{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if !MIN_VERSION_QuickCheck(2,16,0)
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
#endif

module Test.Ouroboros.Storage.PerasCertDB.StateMachine (tests) where

import Control.Monad.State
import Control.Tracer (nullTracer)
import Data.Function ((&))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot)
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import Ouroboros.Consensus.Storage.PerasCertDB.API (AddPerasCertResult (..), PerasCertDB)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
import qualified Test.Ouroboros.Storage.PerasCertDB.Model as Model
import Test.QuickCheck hiding (Some (..))
import qualified Test.QuickCheck.Monadic as QC
import Test.QuickCheck.StateModel
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Some (..))
import Test.Util.TestBlock (TestBlock, TestHash (..))
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "PerasCertDB"
    [ adjustQuickCheckTests (* 100) $ testProperty "q-d" $ prop_qd
    ]

prop_qd :: Actions Model -> Property
prop_qd actions = QC.monadic f $ property () <$ runActions actions
 where
  f :: StateT (PerasCertDB IO TestBlock) IO Property -> Property
  f = ioProperty . flip evalStateT (error "unreachable")

newtype Model = Model (Model.Model TestBlock) deriving (Show, Generic)

instance StateModel Model where
  data Action Model a where
    OpenDB :: Action Model ()
    CloseDB :: Action Model ()
    AddCert :: ValidatedPerasCert TestBlock -> Action Model AddPerasCertResult
    GetWeightSnapshot :: Action Model (PerasWeightSnapshot TestBlock)
    GarbageCollect :: SlotNo -> Action Model ()

  arbitraryAction _ (Model model)
    | model.open =
        frequency
          [ (1, pure $ Some CloseDB)
          , (20, Some <$> genAddCert)
          , (20, pure $ Some GetWeightSnapshot)
          , (5, Some . GarbageCollect . SlotNo <$> arbitrary)
          ]
    | otherwise = pure $ Some OpenDB
   where
    genAddCert = do
      roundNo <- genRoundNo
      boostedBlock <- genPoint
      pure $
        AddCert
          ValidatedPerasCert
            { vpcCert =
                PerasCert
                  { pcCertRound = roundNo
                  , pcCertBoostedBlock = boostedBlock
                  }
            , vpcCertBoost = boostPerCert
            }

    -- Generators are heavily skewed toward collisions, to get equivocating certificates
    -- and certificates boosting the same block
    genPoint :: Gen (Point TestBlock)
    genPoint =
      oneof
        [ return GenesisPoint
        , BlockPoint <$> (SlotNo <$> arbitrary) <*> genHash
        ]
    genRoundNo :: Gen (PerasRoundNo)
    genRoundNo =
      frequency
        [ (1, pure $ PerasRoundNo 1)
        , (1, pure $ PerasRoundNo 2)
        , (8, PerasRoundNo <$> arbitrary)
        ]
    genHash = TestHash . NE.fromList . getNonEmpty <$> arbitrary

  initialState = Model Model.initModel

  nextState (Model model) action _ = Model $ case action of
    OpenDB -> Model.openDB model
    CloseDB -> Model.closeDB model
    AddCert cert -> Model.addCert model cert
    GetWeightSnapshot -> model
    GarbageCollect slot -> Model.garbageCollect slot model

  precondition (Model model) = \case
    OpenDB -> not model.open
    action ->
      model.open && case action of
        CloseDB -> True
        -- Do not add equivocating certificates.
        AddCert cert -> all p model.certs
         where
          p cert' = getPerasCertRound cert /= getPerasCertRound cert' || cert == cert'
        GetWeightSnapshot -> True
        GarbageCollect _slot -> True

deriving stock instance Show (Action Model a)
deriving stock instance Eq (Action Model a)

instance HasVariables (Action Model a) where
  getAllVariables _ = mempty

instance RunModel Model (StateT (PerasCertDB IO TestBlock) IO) where
  perform _ action _ = case action of
    OpenDB -> do
      perasCertDB <- lift $ PerasCertDB.openDB (PerasCertDB.PerasCertDbArgs nullTracer)
      put perasCertDB
    CloseDB -> do
      perasCertDB <- get
      lift $ PerasCertDB.closeDB perasCertDB
    AddCert cert -> do
      perasCertDB <- get
      lift $ PerasCertDB.addCert perasCertDB cert
    GetWeightSnapshot -> do
      perasCertDB <- get
      lift $ atomically $ forgetFingerprint <$> PerasCertDB.getWeightSnapshot perasCertDB
    GarbageCollect slot -> do
      perasCertDB <- get
      lift $ PerasCertDB.garbageCollect perasCertDB slot

  postcondition (Model model, _) (AddCert cert) _ actual = do
    let expected
          | model.certs `Model.hasRoundNo` cert = PerasCertAlreadyInDB
          | otherwise = AddedPerasCertToDB
    counterexamplePost $ show expected <> " /= " <> show actual
    pure $ expected == actual
  postcondition (Model model, _) GetWeightSnapshot _ actual = do
    let expected = Model.getWeightSnapshot model
    counterexamplePost $ "Model: " <> show expected
    counterexamplePost $ "SUT: " <> show actual
    pure $ expected == actual
  postcondition _ _ _ _ = pure True

  monitoring (Model model, _) (AddCert cert) _ _ prop =
    prop
      & tabulate
        "Certificate round collision"
        [show $ model.certs `Model.hasRoundNo` cert]
      & tabulate
        "Certificate block collision"
        [ show $
            Set.member
              (getPerasCertBoostedBlock cert)
              (Set.map getPerasCertBoostedBlock model.certs)
        ]
  monitoring _ _ _ _ prop = prop
