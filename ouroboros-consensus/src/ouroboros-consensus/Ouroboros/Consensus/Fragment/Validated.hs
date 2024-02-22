{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
-- > import qualified Ouroboros.Consensus.Fragment.Validated as VF
module Ouroboros.Consensus.Fragment.Validated (
    ValidatedFragment (ValidatedFragment)
  , validatedFragment
  , validatedLedger
  , validatedTip
    -- * Monadic
  , newM
  ) where

import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike hiding (invariant)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Validated chain fragment along with the ledger state after validation
--
-- INVARIANT:
--
-- > AF.headPoint validatedFragment == ledgerTipPoint validatedLedger
data ValidatedFragment b l = UnsafeValidatedFragment {
      -- | Chain fragment
      validatedFragment :: !(AnchoredFragment b)

      -- | Ledger after validation
    , validatedLedger   :: !l
    }
  deriving (Functor, Foldable, Traversable)

{-# COMPLETE ValidatedFragment #-}

pattern ValidatedFragment ::
     (GetTip l, HasHeader b, HeaderHash b ~ HeaderHash l, HasCallStack)
  => AnchoredFragment b -> l mk -> ValidatedFragment b (l mk)
pattern ValidatedFragment f l <- UnsafeValidatedFragment f l
  where
    ValidatedFragment f l = new f l

validatedTip :: HasHeader b => ValidatedFragment b l -> Point b
validatedTip = AF.headPoint . validatedFragment

invariant ::
     forall l mk b.
     (GetTip l , HasHeader b, HeaderHash b ~ HeaderHash l)
  => ValidatedFragment b (l mk)
  -> Either String ()
invariant (ValidatedFragment fragment ledger) =
  pointInvariant (getTip ledger :: Point l) fragment

pointInvariant ::
     forall l b. (HeaderHash b ~ HeaderHash l, HasHeader b)
  => Point l
  -> AnchoredFragment b
  -> Either String ()
pointInvariant ledgerTip0 fragment
    | ledgerTip /= headPoint
    = Left $ concat [
          "ledger tip "
        , show ledgerTip
        , " /= head point "
        , show headPoint
        ]
    | otherwise
    = Right ()
  where
   ledgerTip, headPoint :: Point b
   ledgerTip = castPoint ledgerTip0
   headPoint = castPoint $ AF.headPoint fragment

-- | Constructor for 'ValidatedFragment' that checks the invariant
new ::
     forall l mk b.
     (GetTip l, HasHeader b, HeaderHash b ~ HeaderHash l, HasCallStack)
  => AnchoredFragment b
  -> l mk
  -> ValidatedFragment b (l mk)
new fragment ledger =
    assertWithMsg (invariant validated) $
      validated
  where
    validated :: ValidatedFragment b (l mk)
    validated = UnsafeValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }

{-------------------------------------------------------------------------------
  Monadic
-------------------------------------------------------------------------------}

invariantM ::
     forall m l b.
     (MonadSTM m, GetTipSTM m l, HasHeader b, HeaderHash b ~ HeaderHash l)
  => ValidatedFragment b l
  -> m (Either String ())
invariantM (UnsafeValidatedFragment fragment ledger) = do
    ledgerTip <- getTipM ledger
    pure $ pointInvariant ledgerTip fragment

-- | Constructor for 'ValidatedFragment' that checks the invariant
newM ::
     forall m l b.
     (MonadSTM m, GetTipSTM m l, HasHeader b, HeaderHash b ~ HeaderHash l, HasCallStack)
  => AnchoredFragment b
  -> l
  -> m (ValidatedFragment b l)
newM fragment ledger = do
    msg <- invariantM validated
    pure $ assertWithMsg msg validated
  where
    validated :: ValidatedFragment b l
    validated = UnsafeValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }
