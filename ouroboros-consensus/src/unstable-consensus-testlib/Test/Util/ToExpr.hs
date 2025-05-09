{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module implements QSM's @CanDiff@ typeclass using @tree-diff@'s
-- @ToExpr@.
module Test.Util.ToExpr () where

import Data.TreeDiff as T
import Test.StateMachine qualified as QSM
import Test.StateMachine.Diffing (CanDiff (..))
import Test.StateMachine.Types.References qualified as QSM

instance ToExpr x => CanDiff x where
  type ADiff x = Edit EditExpr
  type AnExpr x = Expr

  toDiff = toExpr
  exprDiff _ = T.exprDiff
  diffToDocCompact _ = ansiWlBgEditExprCompact
  diffToDoc _ = ansiWlBgEditExpr
  exprToDoc _ = ansiWlBgExpr

{-------------------------------------------------------------------------------
  QSM's References instances
-------------------------------------------------------------------------------}

instance ToExpr (r k) => ToExpr (QSM.Reference k r)

instance ToExpr a => ToExpr (QSM.Concrete a) where
  toExpr (QSM.Concrete x) = toExpr x

instance ToExpr (QSM.Opaque a) where
  toExpr _ = App "Opaque" []
