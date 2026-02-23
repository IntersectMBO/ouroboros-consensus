{-# OPTIONS --safe --without-K #-}
module MyDebugOptions where

open import Meta.Init

instance
  defaultDebugOptionsI : DebugOptions
  defaultDebugOptionsI = record defaultDebugOptions
    { selection = All
    ; filter = Filter.⊥ }

open import Tactic.Defaults public
