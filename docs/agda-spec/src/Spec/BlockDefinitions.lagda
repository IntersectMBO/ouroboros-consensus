\subsection{Block Definitions}
\label{sec:defs-blocks}

\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Spec.BaseTypes using (Nonces)
open import Ledger.Prelude
open import Ledger.Crypto
open import Ledger.Types.Epoch

module Spec.BlockDefinitions
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)  
  (es     : _) (open EpochStructure es)
  where

open import Ledger.PParams using (ProtVer)

record BlockStructure : Type₁ where
  field
\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Abstract types}
\begin{code}
    HashHeader : Type -- hash of a block header
    HashBBody  : Type -- hash of a block body
    VRFRes     : Type -- VRF result value
\end{code}
\begin{code}[hide]
    ⦃ DecEq-HashHeader ⦄ : DecEq HashHeader
    ⦃ DecEq-HashBBody  ⦄ : DecEq HashBBody
    ⦃ DecEq-VRFRes     ⦄ : DecEq VRFRes
    ⦃ Show-HashHeader  ⦄ : Show HashHeader
    ⦃ Show-HashBBody   ⦄ : Show HashBBody
    ⦃ Show-VRFRes      ⦄ : Show VRFRes
\end{code}
\emph{Concrete types}
\begin{code}
  BlockNo = ℕ -- block number
  Certifiedℕ = ∃[ n ] n < 2 ^ 512 -- [0, 2^512) (64-byte VRF output)
\end{code}
\emph{Operational Certificate}
\begin{AgdaSuppressSpace}
\begin{code}
  record OCert : Type where
\end{code}
\begin{code}[hide]
    constructor ⟦_,_,_,_⟧ᵒᶜ
    field
\end{code}
\begin{code}
      vkₕ : VKeyᵏ     -- operational (hot) key
      n   : ℕ         -- certificate issue number
      c₀  : KESPeriod -- start KES period
      σ   : Sigˢ      -- cold key signature
\end{code}
\end{AgdaSuppressSpace}
\emph{Block Header Body}
\begin{AgdaSuppressSpace}
\begin{code}
  record BHBody : Type where
\end{code}
\begin{code}[hide]
    field
\end{code}
\begin{code}
      prevHeader : Maybe HashHeader   -- hash of previous block header
      issuerVk   : VKeyˢ              -- block issuer
      vrfVk      : VKeyᵛ              -- VRF verification key
      blockNo    : BlockNo            -- block number
      slot       : Slot               -- block slot
      vrfRes     : VRFRes             -- VRF result value
      vrfPrf     : Proof              -- VRF proof
      bodySize   : ℕ                  -- size of the block body
      bodyHash   : HashBBody          -- block body hash
      oc         : OCert              -- operational certificate
      pv         : ProtVer            -- protocol version
\end{code}
\end{AgdaSuppressSpace}
\emph{Block Types}
\begin{code}
  record BHeader : Type where
    constructor 〖_,_〗
    field
      body : BHBody
      sig  : Sigᵏ
\end{code}
\emph{Abstract functions}
\begin{code}[hide]
  record AbstractFunctions : Type where
    field
\end{code}
\begin{code}
      headerHash      : BHeader → HashHeader -- hash of a block header
      headerSize      : BHeader → ℕ -- size of a block header
      slotToSeed      : Slot → Seed -- big-endian encoding of the slot number in 8 bytes
      prevHashToNonce : Maybe HashHeader → Nonce
      serHashToℕ      : SerHash → Certifiedℕ
      serHashToNonce  : SerHash → Nonce
\end{code}
\begin{code}[hide]
open BlockStructure
open import Tactic.Derive.Show

instance
  _ = Show-Maybe

  Show-Certifiedℕ : ∀ {bs} → Show (Certifiedℕ bs)
  Show-Certifiedℕ .show = show ∘ proj₁

  unquoteDecl Show-OCert = derive-Show
    [(quote OCert , Show-OCert)]

  unquoteDecl Show-BHBody = derive-Show
    [(quote BHBody , Show-BHBody)]

  unquoteDecl Show-BHeader = derive-Show
    [(quote BHeader , Show-BHeader)]
\end{code}
\end{AgdaAlign}
\caption{Block definitions}
\label{fig:defs:blocks}
\end{figure*}
