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
import Control.Monad.Except (runExcept)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.LedgerCallShim (stowUTxO)
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

  testGenTxs _coreNodeId _numCoreNodes curSlotNo cfg extra lst values
    | stgeStartAt > curSlotNo = pure []
    -- TODO Temporarily disable the transaction generator until we fix the
    -- failing assertion in TxSubmission.Inbound, see #2680.
    --
    -- When fixed, remove the True case keepig the else case below to re-enable
    -- the transaction generator.

    | otherwise =
        if True
          then pure []
          else do
            n <- choose (0, 20)
            -- The values were read against the un-ticked state; forward them
            -- through the tick diff to the ticked tip.
            let (ticked, tickDiff) = applyChainTick OmitLedgerEvents lcfg curSlotNo lst
            go [] n ticked (forward @(ShelleyBlock (TPraos MockCrypto) ShelleyEra) [tickDiff] values)
   where
    ShelleyTxGenExtra
      { stgeGenEnv
      , stgeStartAt
      } = extra

    lcfg :: LedgerConfig (ShelleyBlock (TPraos MockCrypto) ShelleyEra)
    lcfg = configLedger cfg

    go ::
      [GenTx (ShelleyBlock (TPraos MockCrypto) ShelleyEra)] ->
      -- \^ Accumulator
      Integer ->
      -- \^ Number of txs to still produce
      TickedLedgerState (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
      -- \^ The ticked state
      Values (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
      -- \^ The UTxO values at the virtual tip
      Gen [GenTx (ShelleyBlock (TPraos MockCrypto) ShelleyEra)]
    go acc 0 _ _ = return (reverse acc)
    go acc n st vals = do
      mbTx <- genTx cfg curSlotNo st vals stgeGenEnv
      case mbTx of
        Nothing -> return (reverse acc) -- cannot afford more transactions
        Just tx ->
          case runExcept $ applyTx lcfg DoNotIntervene curSlotNo tx vals st of
            -- We don't mind generating invalid transactions
            Left _ -> go (tx : acc) (n - 1) st vals
            Right (st', diff, _vtx) ->
              go
                (tx : acc)
                (n - 1)
                st'
                (forward @(ShelleyBlock (TPraos MockCrypto) ShelleyEra) [diff] vals)

genTx ::
  TopLevelConfig (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
  SlotNo ->
  TickedLedgerState (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
  Values (ShelleyBlock (TPraos MockCrypto) ShelleyEra) ->
  Gen.GenEnv MockCrypto ShelleyEra ->
  Gen (Maybe (GenTx (ShelleyBlock (TPraos MockCrypto) ShelleyEra)))
genTx _cfg slotNo tickedSt values genEnv =
  Just . mkShelleyTx
    <$> Gen.genTx
      genEnv
      ledgerEnv
      (SL.LedgerState utxoSt dpState)
 where
  -- Stow the read values back in so the generator sees the real UTxO set.
  nes :: SL.NewEpochState ShelleyEra
  nes = stowUTxO values (tickedShelleyLedgerStateNoUTxO tickedSt)

  epochState :: SL.EpochState ShelleyEra
  epochState = SL.nesEs nes

  ledgerEnv :: SL.LedgerEnv ShelleyEra
  ledgerEnv =
    SL.LedgerEnv
      { ledgerEpochNo = Nothing
      , ledgerSlotNo = slotNo
      , ledgerIx = minBound
      , ledgerPp = getPParams nes
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
