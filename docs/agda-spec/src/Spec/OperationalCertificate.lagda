\subsection{$\mathsf{OCERT}$ Transition}
\label{sec:oper-cert-trans}

The Operational Certificate Transition ($\mathsf{OCERT}$) validates
the operational certificate and signature in the block header and updates the mapping
of operational certificate issue numbers. The environment is shown in Figure~\ref{fig:ts-types:ocert}
and consists of the set of stake pools. The state is shown in Figure~\ref{fig:ts-types:ocert}
and consists of the mapping of operation certificate issue numbers. Its signal is a block header.

\begin{code}[hide]
{-# OPTIONS --safe #-}
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions

module Spec.OperationalCertificate
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)  
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  where

open import Spec.BaseTypes crypto using (OCertCounters)
open import Ledger.Prelude

\end{code}

\begin{figure*}[h]
\emph{Operational Certificate environments}
\begin{code}
OCertEnv = ℙ KeyHashˢ
\end{code}
\emph{Operational Certificate states}
\begin{code}
OCertState = OCertCounters
\end{code}
\emph{Operational Certificate transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,OCERT⦈_ : OCertEnv → OCertState → BHeader → OCertState → Type
\end{code}
\emph{Operational Certificate helper function}
\begin{code}
currentIssueNo : OCertEnv → OCertState → KeyHashˢ → Maybe ℕ
currentIssueNo stpools cs hk =
  if hk ∈ dom (cs ˢ) then
    just (lookupᵐ cs hk)
  else
  if hk ∈ stpools then
    just 0
  else
    nothing
\end{code}
\caption{Operational Certificate transition-system types and functions}
\label{fig:ts-types:ocert}
\end{figure*}

The transition rule $\mathsf{OCERT}$ is shown in Figure~\ref{fig:ts-rules:ocert}. From the block
header body \aarg{bhb} we first extract the following:

\begin{itemize}
  \item The operational certificate \afun{oc}, consisting of the hot key \aarg{vkₕ},
    the certificate issue number \aarg{n}, the KES period start \aarg{c₀} and the cold key
  signature \aarg{τ}.
\item The cold key \afun{issuerVk}.
\item The slot \afun{slot} for the block.
\item The number \aarg{t} of KES periods that have elapsed since the start period on the certificate.
\end{itemize}

Using this we verify the preconditions of the operational certificate state
transition which are the following:

\begin{itemize}
\item The KES period \aarg{kp} of the slot in the block header body must be greater than or equal to
  the start value \aarg{c₀} listed in the operational certificate,
  and less than \afld{MaxKESEvo}-many KES periods after \aarg{c₀}.
  The value of \afld{MaxKESEvo} is the agreed-upon lifetime of an operational certificate,
  see \cite{delegation_design}.
\item \aarg{hk} exists as key in the mapping of certificate issues numbers to a KES
  period \aarg{m} and that period is less than or equal to \aarg{n}. Also, \aarg{n} must
  be less than or equal to the successor of \aarg{m}.
\item The signature \aarg{τ} can be verified with the cold verification key \afun{issuerVk}.
\item The KES signature \aarg{σ} can be verified with the hot verification key \aarg{vkₕ}.
\end{itemize}

After this, the transition system updates the operational certificate state by
updating the mapping of operational certificates where it overwrites the entry
of the key \aarg{hk} with the KES period \aarg{n}.

\begin{figure*}[h]
\begin{code}[hide]
private variable
  stpools : OCertEnv
  cs      : OCertState
  bh      : BHeader

data _⊢_⇀⦇_,OCERT⦈_ where
\end{code}
\begin{code}
  Update-OCert :
    let (bhb , σ) = bh; open BHBody bhb
        ⟦ vkₕ , n , c₀ , τ ⟧ᵒᶜ = oc
        hk = hash issuerVk
        kp = kesPeriod slot
        t = kp -ᵏ c₀
    in
    ∙ c₀ ≤ kp
    ∙ kp < c₀ +ᵏ MaxKESEvo
    ∙ ∃[ m ] (just m ≡ currentIssueNo stpools cs hk × (n ≡ m ⊎ n ≡ suc m))
    ∙ isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ
    ∙ isSignedᵏ vkₕ t (encode bhb) σ
    ────────────────────────────────
    stpools ⊢ cs ⇀⦇ bh ,OCERT⦈ (❴ hk , n ❵ ∪ˡ cs)
\end{code}
\caption{Operational Certificate transition-system rules}
\label{fig:ts-rules:ocert}
\end{figure*}

The $\mathsf{OCERT}$ rule has the following predicate failures:
\begin{enumerate}
\item If the KES period is less than the KES period start in the certificate,
  there is a \emph{KESBeforeStart} failure.
\item If the KES period is greater than or equal to the KES period end (start + \afld{MaxKESEvo})
  in the certificate, there is a \emph{KESAfterEnd} failure.
\item If the period counter in the original key hash counter mapping is larger
  than the period number in the certificate, there is a \emph{CounterTooSmall} failure.
\item If the period number in the certificate is larger than the successor of the period counter
  in the original key hash counter mapping, there is a \emph{CounterOverIncremented} failure.
\item If the signature of the hot key, KES period number and period start is incorrect,
  there is an \emph{InvalidSignature} failure.
\item If the KES signature using the hot key of the block header body is incorrect,
  there is an \emph{InvalideKesSignature} failure.
\item If there is no entry in the key hash to counter mapping for the cold key,
  there is a \emph{NoCounterForKeyHash} failure.
\end{enumerate}
