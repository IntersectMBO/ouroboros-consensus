{-# OPTIONS --safe --without-K #-}
module MyDebugOptions where

open import PreludeMeta

instance
  defaultDebugOptionsI : DebugOptions
  defaultDebugOptionsI = record defaultDebugOptions
    { selection = All
    ; filter = Filter.⊥ }

open import Tactic.Defaults public
