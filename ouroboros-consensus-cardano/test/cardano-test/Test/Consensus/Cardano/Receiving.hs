{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.Cardano.Receiving (tests) where

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary (DecCBOR (decCBOR), decodeFullAnnotator, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Shelley.API (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (utxoG)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Control.Monad.Except (runExcept)
import Control.Monad.State.Strict (gets)
import Data.Either (isLeft)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (^.))
import Ouroboros.Consensus.Block (WithOrigin (Origin))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (slotLengthFromSec)
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( LedgerSupportsMempool (..)
  , WhetherToIntervene (..)
  )
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( ShelleyLedgerConfig (..)
  , ShelleyTransition (..)
  , Ticked (..)
  , mkShelleyLedgerConfig
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import qualified Test.Cardano.Ledger.Dijkstra.ImpTest as Imp
import qualified Test.Cardano.Ledger.Imp.Common as IC
import Test.Cardano.Ledger.Shelley.Examples (testShelleyGenesis)
import Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import Test.ImpSpec (ImpSpec (impInitIO), evalImpM)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Protected Receiving consensus integration"
    [ testCase "wire decode, MEMPOOL acceptance/rejection and ledger-table state" $ do
        initial <- impInitIO @(Imp.LedgerSpec DijkstraEra) (mkQCGen 2023)
        evalImpM (Just (mkQCGen 2024)) (Just 30) initial receivingMempool
    ]

receivingMempool :: Imp.ImpTestM DijkstraEra ()
receivingMempool = do
  recipient <- Imp.freshKeyHash @Payment
  stake <- Imp.freshKeyHash @Staking
  nativeHash <- Imp.impAddNativeScript (RequireAllOf mempty :: NativeScript DijkstraEra)
  let keyAddr = AddrProtected Testnet (KeyHashObj recipient) (StakeRefBase (KeyHashObj stake))
      scriptAddr = AddrProtected Testnet (ScriptHashObj nativeHash) StakeRefNull
      body =
        mkBasicTxBody @DijkstraEra @TopTx
          & outputsTxBodyL
            .~ SSeq.fromList
              [mkCoinTxOut keyAddr (Coin 2000000), mkCoinTxOut scriptAddr (Coin 2000000)]
  fixed <- Imp.fixupTx (mkBasicTx body)
  tx <-
    IC.expectRight $
      decodeFullAnnotator
        (eraProtVerLow @DijkstraEra)
        "Receiving transaction"
        decCBOR
        (serialize (eraProtVerLow @DijkstraEra) fixed)
  nes <- gets (^. Imp.impNESL)
  globals <- gets (^. Imp.impGlobalsL)
  slot <- Imp.getCurSlotNo
  translationContext <- IC.arbitrary
  let cfg =
        ( mkShelleyLedgerConfig
            testShelleyGenesis
            translationContext
            (fixedEpochInfo (sgEpochLength testShelleyGenesis) (slotLengthFromSec 2))
        )
          { shelleyLedgerGlobals = globals
          }
      initial :: TickedLedgerState (ShelleyBlock (Praos MockCryptoCompatByron) DijkstraEra) ValuesMK
      initial =
        unstowLedgerTables $ TickedShelleyLedgerState Origin (ShelleyTransitionInfo 0) nes emptyLedgerTables
      missingRecipient = tx & witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness recipient) . witVKeyHash)
      apply candidate = runExcept $ applyTx cfg Intervene slot (mkShelleyTx candidate) initial
  apply missingRecipient `IC.shouldSatisfy` isLeft
  (changed, _) <- IC.expectRight $ apply tx
  let actual = tickedShelleyLedgerState $ stowLedgerTables $ applyDiffs initial changed
  _ <- Imp.withNoFixup $ Imp.submitTx tx
  expected <- Imp.getUTxO
  actual ^. utxoG `IC.shouldBe` expected
  tx ^. bodyTxL . outputsTxBodyL `IC.shouldBe` fixed ^. bodyTxL . outputsTxBodyL
  tickedShelleyLedgerState (stowLedgerTables initial) ^. utxoG `IC.shouldBe` nes ^. utxoG
