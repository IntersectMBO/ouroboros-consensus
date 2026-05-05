{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Ledger tables are barbie-types (see @barbies@ package), though unfortunately
-- we can not implement classes like 'FunctorB' for ledger tables because the
-- class expects a type that is indexed over a /(uni-)functor/. Ledger tables
-- are indexed over /bifunctors/ (mapkinds), so the kinds do not match. To cut
-- on boilerplate, we do not define variants of 'FunctorB' (and similar classes)
-- for types that are indexed over bifunctors. Instead, we define specialised
-- variants of class functions and utility functions. For example:
--
-- * 'ltmap' instead of 'bmap' or 'bmapC'
--
-- * 'lttraverse' instead of 'btraverse' or 'btraverseC'
--
-- * 'ltsequence' instead of 'bsequence'.
--
-- TODO: if we make mapkinds of kind @(k1, k2) -> Type@ instead of @k1 -> k2 ->
-- Type@, then we could reuse most of the @barbies@ machinery.
module Ouroboros.Consensus.Ledger.Tables.Combinators
  ( -- * Common constraints
  ) where
