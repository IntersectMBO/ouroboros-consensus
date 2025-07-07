
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Ouroboros.Storage.PerasCertDB.StateMachine (tests) where
import Test.Tasty (TestTree)
import Test.Ouroboros.Storage.TestBlock (TestBlock)
import Ouroboros.Consensus.Block.SupportsPeras
import Test.Ouroboros.Storage.PerasCertDB.Model
import Test.QuickCheck.StateModel
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasWeightSnapshot)

tests :: TestTree
tests = undefined

type Block = TestBlock
newtype Model = Model (PerasCertDBModel Block) deriving (Show, Generic)

instance StateModel Model where
  data Action Model a where
    OpenDB :: Action Model ()
    CloseDB :: Action Model ()
    AddCert :: PerasCert Block -> Action Model ()
    GetWeightSnapshot :: Action Model (PerasWeightSnapshot Block)

  arbitraryAction _ _ = error "arbitraryAction not implemented"
  initialState = error "initialState not implemented"
