{-# OPTIONS --safe #-}

open import Ledger.Prelude
open import Ledger.Transaction

module Ledger.Abstract (txs : TransactionStructure) where

open TransactionStructure txs

record indexOf : Type where
  field
    indexOfDCert    : DCert → List DCert → Maybe Ix
    indexOfRwdAddr  : RwdAddr → Wdrl → Maybe Ix
    indexOfTxIn     : TxIn → ℙ TxIn → Maybe Ix
    indexOfPolicyId : ScriptHash → ℙ ScriptHash → Maybe Ix
    indexOfVote     : Voter → List Voter → Maybe Ix
    indexOfProposal : GovProposal → List GovProposal → Maybe Ix

record AbstractFunctions : Type where
  field txscriptfee  : Prices → ExUnits → Coin
        serSize      : Value → MemoryEstimate
        indexOfImp   : indexOf
        runPLCScript : CostModel → P2Script → ExUnits → List Data → Bool
        scriptSize   : Script → ℕ
