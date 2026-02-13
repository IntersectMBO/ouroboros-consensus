{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.ValidatedDiff (ValidatedChainDiff (..))
-- > import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
module Ouroboros.Consensus.Fragment.ValidatedDiff
  ( ValidatedChainDiff (ValidatedChainDiff)
  , getChainDiff
  , getLedger

    -- * Monadic
  , newM
  ) where

import Control.Monad.Except (throwError)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Fragment.Diff (ChainDiff)
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Util.Assert
import Ouroboros.Consensus.Util.IOLike (MonadSTM (..))

-- | A 'ChainDiff' along with the ledger state after validation.
--
-- INVARIANT:
--
-- > getTip chainDiff == ledgerTipPoint ledger
--
-- The invariant is only checked on construction, maintaining it afterwards is
-- up to the user.
data ValidatedChainDiff b l = UnsafeValidatedChainDiff
  { getChainDiff :: ChainDiff b
  , getLedger :: l
  }

-- | Allow for pattern matching on a 'ValidatedChainDiff' without exposing the
-- (unsafe) constructor. Use 'new' to construct a 'ValidatedChainDiff'.
pattern ValidatedChainDiff ::
  ChainDiff b -> l -> ValidatedChainDiff b l
pattern ValidatedChainDiff d l <- UnsafeValidatedChainDiff d l

{-# COMPLETE ValidatedChainDiff #-}

pointInvariant ::
  forall l b.
  (HeaderHash b ~ HeaderHash l, HasHeader b) =>
  Point l ->
  ChainDiff b ->
  Either String ()
pointInvariant ledgerTip0 chainDiff = precondition
 where
  chainDiffTip, ledgerTip :: Point b
  chainDiffTip = castPoint $ Diff.getTip chainDiff
  ledgerTip = castPoint ledgerTip0
  precondition
    | chainDiffTip == ledgerTip =
        return ()
    | otherwise =
        throwError $
          "tip of ChainDiff doesn't match ledger: "
            <> show chainDiffTip
            <> " /= "
            <> show ledgerTip

{-------------------------------------------------------------------------------
  Monadic
-------------------------------------------------------------------------------}

-- | Create a 'ValidatedChainDiff'.
--
-- PRECONDITION:
--
-- > getTip chainDiff == ledgerTipPoint ledger
newM ::
  forall m b l.
  ( MonadSTM m
  , GetTipSTM m l
  , HasHeader b
  , HeaderHash l ~ HeaderHash b
  , HasCallStack
  ) =>
  ChainDiff b ->
  l ->
  m (ValidatedChainDiff b l)
newM chainDiff ledger = do
  ledgerTip <- getTipM ledger
  pure $
    assertWithMsg (pointInvariant ledgerTip chainDiff) $
      UnsafeValidatedChainDiff chainDiff ledger
