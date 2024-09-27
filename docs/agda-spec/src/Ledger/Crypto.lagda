\section{Cryptographic Primitives}
\begin{code}[hide]
{-# OPTIONS --safe #-}
module Ledger.Crypto where

open import Ledger.Prelude hiding (T)

module _ (M : Type↑) where
  _⁴ : ∀ {ℓ} {A B C D : Type ℓ} → (A → B → C → D → Type ℓ) → Type _
  _⁴ _~_~_~_ = ∀ {x y z w} → M (x ~ y ~ z ~ w)

_⁇⁴ = _⁇ ⁴

\end{code}

\subsection{Serialization}

-- TODO: Add paragraph

\begin{figure*}[h]
\begin{code}[hide]
record Serializer : Type₁ where
  field
\end{code}
\emph{Types \& functions}
\begin{code}
        Ser             : Type
        encode          : {T : Type} → T → Ser
        decode          : {T : Type} → Ser → Maybe T
        _∥_             : Ser → Ser → Ser
\end{code}
\emph{Properties}
\begin{code}[hide]
        ⦃ DecEq-Ser ⦄ : DecEq Ser
        enc-dec-correct :
\end{code}
\begin{code}
          ∀ {T : Type} (x : T) → decode (encode x) ≡ just x
\end{code}
\caption{Definitions for serialization}
\label{fig:defs:serialization}
\end{figure*}

\subsection{Cryptographic Hashes}

-- TODO: Add paragraph and show only the relevant bits of the code below.

\begin{code}
record isHashableSet (T : Type) : Set₁ where
  constructor mkIsHashableSet
  field THash : Type
        ⦃ DecEq-THash ⦄ : DecEq      THash
        ⦃ Show-THash  ⦄ : Show       THash
        ⦃ DecEq-T     ⦄ : DecEq    T
        ⦃ T-Hashable  ⦄ : Hashable T THash
open isHashableSet

record HashableSet : Type₁ where
  constructor mkHashableSet
  field T : Type; ⦃ T-isHashable ⦄ : isHashableSet T
  open isHashableSet T-isHashable public
\end{code}

\subsection{Public-Key Cryptography}

The Cardano blockchain system is based on a public-key cryptographic system.

\begin{figure*}[h]
\begin{code}[hide]
record PKScheme : Type₁ where
  field
\end{code}
\emph{Types \& functions}
\begin{code}
        SKey VKey : Type
        isKeyPair : SKey → VKey → Type

  KeyPair = Σ[ sk ∈ SKey ] Σ[ vk ∈ VKey ] isKeyPair sk vk
\end{code}
\caption{Definitions for the public-key cryptographic system}
\label{fig:defs:pkscheme}
\end{figure*}

\subsection{Digital Signatures}

-- TODO: Add paragraph.

\begin{figure*}[h]
\begin{code}[hide]
record DSigScheme (srl : Serializer) : Type₁ where
  open Serializer srl
  
  field
        pks : PKScheme

  open PKScheme pks public

  field
\end{code}
\emph{Types \& functions}
\begin{code}
        Sig      : Type
        isSigned : VKey → Ser → Sig → Type
        sign     : SKey → Ser → Sig
\end{code}
\emph{Properties}
\begin{code}[hide]
  field ⦃ Dec-isSigned ⦄ : isSigned ⁇³
        isSigned-correct :
\end{code}
\begin{code}
          ∀ ((sk , vk , _) : KeyPair) (d : Ser) (σ : Sig) → sign sk d ≡ σ → isSigned vk d σ
\end{code}
\begin{code}[hide]
        ⦃ DecEq-Sig  ⦄ : DecEq Sig
\end{code}        
\caption{Definitions for the digital signature scheme}
\label{fig:defs:dsigscheme}
\end{figure*}

\subsection{Key-Evolving Signatures}

-- TODO: Add paragraph.

\begin{figure*}[h]
\begin{code}[hide]
record KESScheme (srl : Serializer) : Type₁ where
  open Serializer srl
  
  field
        pks : PKScheme

  open PKScheme pks public

  field
\end{code}
\emph{Types \& functions}
\begin{code}
        Sig      : Type
        isSigned : VKey → ℕ → Ser → Sig → Type
        sign     : (ℕ → SKey) → ℕ → Ser → Sig
\end{code}
\emph{Properties}
\begin{code}[hide]
  field ⦃ Dec-isSigned ⦄ : isSigned ⁇⁴
        isSigned-correct :
\end{code}
\begin{code}        
          ∀ (n : ℕ) (sk : ℕ → SKey) ((skₙ , vk , _) : KeyPair) (d : Ser) (σ : Sig)
            → skₙ ≡ sk n → sign sk n d ≡ σ → isSigned vk n d σ
\end{code}
\begin{code}[hide]          
        ⦃ DecEq-Sig  ⦄ : DecEq Sig
\end{code}        
\caption{Definitions for key-evolving signatures}
\label{fig:defs:kesscheme}
\end{figure*}

\subsection{Verifiable Random Functions}

-- TODO: Add paragraph.

\begin{figure*}[h]
\begin{code}[hide]
record VRFScheme : Type₁ where
  field
        pks : PKScheme

  open PKScheme pks public

  field
\end{code}
\emph{Types \& functions}
\begin{code}
        Seed Proof : Type
        verify     : {T : Type} → VKey → Seed → Proof × T → Type
        evaluate   : {T : Type} → SKey → Seed → Proof × T
        _XOR_      : Seed → Seed → Seed
\end{code}
\emph{Properties}
\begin{code}[hide]
  field ⦃ Dec-verify ⦄ : {T : Type} → verify {T} ⁇³
        verify-correct :
\end{code}
\begin{code}        
          ∀ {T : Type} ((sk , vk , _) : KeyPair) (seed : Seed)
            → verify {T = T} vk seed (evaluate sk seed)
\end{code}
\begin{code}[hide]         
        ⦃ DecEq-Seed  ⦄ : DecEq Seed
        ⦃ DecEq-Proof ⦄ : DecEq Proof
\end{code}        
\caption{Definitions for verifiable random functions}
\label{fig:defs:vrfscheme}
\end{figure*}

\begin{code}[hide]         
record Crypto : Type₁ where
  field srl  : Serializer
        dsig : DSigScheme srl
        kes  : KESScheme srl
        vrf  : VRFScheme

  open Serializer srl public
  open DSigScheme dsig renaming (VKey to VKeyˢ; Sig to Sigˢ; isSigned to isSignedˢ) public
  open KESScheme kes renaming (KeyPair to KeyPairᵏ; VKey to VKeyᵏ; Sig to Sigᵏ; isSigned to isSignedᵏ) public
  open VRFScheme vrf renaming (KeyPair to KeyPairᵛ; VKey to VKeyᵛ) public

  field ⦃ khsˢ ⦄    : isHashableSet VKeyˢ
        ⦃ khsᵛ ⦄    : isHashableSet VKeyᵛ
        ⦃ shs  ⦄    : isHashableSet Ser
        ScriptHash : Type; ⦃ DecEq-ScriptHash ⦄ : DecEq ScriptHash

  open isHashableSet khsˢ renaming (THash to KeyHashˢ) hiding (DecEq-T) public
  open isHashableSet khsᵛ renaming (THash to KeyHashᵛ) hiding (DecEq-T) public
  open isHashableSet shs  renaming (THash to SerHash)  hiding (DecEq-T) public
\end{code}
