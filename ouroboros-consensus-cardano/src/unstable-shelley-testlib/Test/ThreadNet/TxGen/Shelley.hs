{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.TxGen.Shelley
  ( ShelleyTxGenExtra (..)
  , WhetherToGeneratePPUs (..)
  , genTx
  , mkGenEnv
  ) where

import qualified Cardano.Ledger.Shelley.API as SL
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import qualified Test.Cardano.Ledger.Shelley.Constants as Gen
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Gen
import Test.Cardano.Ledger.Shelley.Generator.EraGen
  ( EraGen (genEraTwoPhase2Arg, genEraTwoPhase3Arg)
  )
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Gen.Presets
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import qualified Test.Cardano.Ledger.Shelley.Generator.Utxo as Gen
import Test.Consensus.Shelley.MockCrypto (MockCrypto)
import Test.QuickCheck
import Test.ThreadNet.Infra.Shelley
import Test.ThreadNet.TxGen (TxGen (..))

data ShelleyTxGenExtra = ShelleyTxGenExtra
  { stgeGenEnv :: Gen.GenEnv MockCrypto ShelleyEra
  -- ^ Generator environment.
  , stgeStartAt :: SlotNo
  -- ^ Generate no transactions before this slot.
  }

instance TxGen (ShelleyBlock (TPraos MockCrypto) ShelleyEra) where
  type TxGenExtra (ShelleyBlock (TPraos MockCrypto) ShelleyEra) = ShelleyTxGenExtra

  -- The Shelley test transaction generator was already disabled before the
  -- UTxO-HD v3 work (see #2680: failing assertion in TxSubmission.Inbound).
  -- The original aspirational body relied on the now-removed pure
  -- 'applyChainTick' / 'applyTx' / 'forgetLedgerTables' / 'applyDiffs'.
  -- Restoring it needs a port to the new per-tx mempool workflow
  -- ('TxLocalData' / 'MempoolAcc' / 'prepareTx') together with a way to
  -- obtain a 'TickedStateHandle'. Tracked as a deferred follow-up in
  -- fixing-tests.md.
  testGenTxs _coreNodeId _numCoreNodes _curSlotNo _cfg _extra _lst = pure []

genTx ::
  TopLevelConfig (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
  SlotNo ->
  TickedLedgerState (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
  Gen.GenEnv MockCrypto ShelleyEra ->
  Gen (Maybe (GenTx (ShelleyBlock (TPraos MockCrypto) ShelleyEra)))
genTx _cfg slotNo TickedShelleyLedgerState{tickedShelleyLedgerState} genEnv =
  Just . mkShelleyTx
    <$> Gen.genTx
      genEnv
      ledgerEnv
      (SL.LedgerState utxoSt dpState)
 where
  epochState :: SL.EpochState ShelleyEra
  epochState = SL.nesEs tickedShelleyLedgerState

  ledgerEnv :: SL.LedgerEnv ShelleyEra
  ledgerEnv =
    SL.LedgerEnv
      { ledgerEpochNo = Nothing
      , ledgerSlotNo = slotNo
      , ledgerIx = minBound
      , ledgerPp = getPParams tickedShelleyLedgerState
      , ledgerAccount = SL.esChainAccountState epochState
      }

  utxoSt :: SL.UTxOState ShelleyEra
  utxoSt =
    SL.lsUTxOState
      . SL.esLState
      $ epochState

  dpState :: SL.CertState ShelleyEra
  dpState =
    SL.lsCertState
      . SL.esLState
      $ epochState

data WhetherToGeneratePPUs = DoNotGeneratePPUs | DoGeneratePPUs
  deriving Show

mkGenEnv ::
  WhetherToGeneratePPUs ->
  [CoreNode MockCrypto] ->
  Gen.GenEnv MockCrypto ShelleyEra
mkGenEnv whetherPPUs coreNodes = Gen.GenEnv keySpace scriptSpace constants
 where
  -- Configuration of the transaction generator
  constants :: Gen.Constants
  constants =
    setCerts $
      setPPUs $
        Gen.defaultConstants
          { Gen.frequencyMIRCert = 0
          , Gen.genTxStableUtxoSize = 100
          , Gen.genTxUtxoIncrement = 3
          }
   where
    -- Testing with certificates requires additional handling in the
    -- testing framework, because, for example, they may transfer block
    -- issuance rights from one node to another, and we must have the
    -- relevant nodes brought online at that point.
    setCerts cs = cs{Gen.maxCertsPerTx = 0}

    setPPUs cs = case whetherPPUs of
      DoGeneratePPUs -> cs
      DoNotGeneratePPUs -> cs{Gen.frequencyTxUpdates = 0}

  keySpace :: Gen.KeySpace MockCrypto ShelleyEra
  keySpace =
    Gen.KeySpace
      (cnkiCoreNode <$> cn)
      ksGenesisDelegates
      ksStakePools
      (ksKeyPairs <> (cnkiKeyPair <$> cn))
      ksMSigScripts
   where
    cn = coreNodeKeys <$> coreNodes
    Gen.KeySpace_
      { ksKeyPairs
      , ksMSigScripts
      , ksGenesisDelegates
      , ksStakePools
      } =
        Gen.Presets.keySpace @ShelleyEra constants

  scriptSpace :: Gen.ScriptSpace ShelleyEra
  scriptSpace =
    Gen.Presets.scriptSpace @ShelleyEra
      (genEraTwoPhase3Arg @ShelleyEra)
      (genEraTwoPhase2Arg @ShelleyEra)
