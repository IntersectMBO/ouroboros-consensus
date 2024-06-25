{-# OPTIONS --safe --without-K #-}

module PreludeMeta where

-- ** Re-exporting from stdlib
open import Agda.Builtin.Reflection public
  using (withReduceDefs)
open import Reflection public
  using (TC; Name; Meta; extendContext; withNormalisation)
open import Reflection.AST.Term public
  hiding (_≟_)
open import Reflection.AST.Argument public
  using (Arg; arg; unArg; Args; vArg; hArg; iArg; _⟨∷⟩_; _⟅∷⟆_)
open import Reflection.AST.Argument.Visibility public
  using (Visibility; visible; hidden; instance′)
open import Reflection.AST.Abstraction public
  using (Abs; abs; unAbs)
open import Reflection.AST.DeBruijn public
  using (weaken; strengthen; η-expand)

-- ** Re-exporting from stdlib-meta
open import Reflection.Syntax public
  hiding (toℕ)
open import Reflection.Utils public
  hiding (mkRecord)
open import Reflection.Debug public
open import Reflection.Utils.Debug public
  hiding (error)
open import Reflection.Tactic public

-- * MonadTC interface for TC
open import Class.MonadTC public
  hiding (extendContext)
open MonadTC ⦃...⦄ public
open import Class.MonadError public
  using (MonadError; MonadError-TC)
open MonadError ⦃...⦄ public
  using (error; catch)
instance
  iTC  = MonadTC-TC
  iTCE = MonadError-TC

-- * MonadTC interface for TCI
open import Meta public
  renaming (TC to TCI)
  hiding (Monad-TC; MonadError-TC; toℕ)
open import Reflection.Utils.TCI public
