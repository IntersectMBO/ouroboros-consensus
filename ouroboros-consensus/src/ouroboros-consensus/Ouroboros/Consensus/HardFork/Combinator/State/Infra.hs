{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Infra (
    -- * Initialization
    initHardForkState
    -- * Lifting 'Telescope' operations
  , fromTZ
  , match
  , sequence
  , tip
    -- * Situated
  , Situated (..)
  , situate
    -- * Towing
  , CrossEra (..)
  , check
  , tow
    -- * EpochInfo/Summary
  , reconstructSummary
  ) where

import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Counting
import           Data.SOP.InPairs (InPairs)
import           Data.SOP.InPairs (type (--.-->) (..), (:..:) (..), Le (..), Ri (..))
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Match (Mismatch)
import qualified Data.SOP.Match as Match
import           Data.SOP.NonEmpty
import           Data.SOP.Strict
import           Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..), SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Prelude hiding (sequence)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initHardForkState :: f x -> HardForkState f (x ': xs)
initHardForkState st = HardForkState $ TZ $ Current {
      currentStart = History.initBound
    , currentState = st
    }

{-------------------------------------------------------------------------------
  Lift telescope operations
-------------------------------------------------------------------------------}

tip :: SListI xs => HardForkState f xs -> NS f xs
tip (HardForkState st) = hmap currentState $ Telescope.tip st

match :: SListI xs
      => NS h xs
      -> HardForkState f xs
      -> Either (Mismatch h (Current f) xs) (HardForkState (Product h f) xs)
match ns (HardForkState t) =
    HardForkState . hmap distrib <$> Match.matchTelescope ns t
  where
    distrib :: Product h (Current f) blk -> Current (Product h f) blk
    distrib (Pair x (Current start y)) =
        Current start (Pair x y)

sequence :: forall f m xs. (SListI xs, Functor m)
         => HardForkState (m :.: f) xs -> m (HardForkState f xs)
sequence = \(HardForkState st) -> HardForkState <$>
    Telescope.sequence (hmap distrib st)
  where
    distrib :: Current (m :.: f) blk -> (m :.: Current f) blk
    distrib (Current start st) = Comp $
        Current start <$> unComp st

fromTZ :: HardForkState f '[blk] -> f blk
fromTZ = currentState . Telescope.fromTZ . getHardForkState

{-------------------------------------------------------------------------------
  Situated
-------------------------------------------------------------------------------}

-- | A @h@ situated in time
data Situated h f xs where
  SituatedCurrent :: Current f x ->    h x  -> Situated h f (x ': xs)
  SituatedNext    :: Current f x ->    h y  -> Situated h f (x ': y ': xs)
  SituatedFuture  :: Current f x -> NS h xs -> Situated h f (x ': y ': xs)
  SituatedPast    :: K Past    x ->    h x  -> Situated h f (x ': xs)
  SituatedShift   :: Situated h f xs        -> Situated h f (x ': xs)

situate :: NS h xs -> HardForkState f xs -> Situated h f xs
situate ns = go ns . getHardForkState
  where
    go :: NS h xs'
       -> Telescope (K Past) (Current f) xs'
       -> Situated h f xs'
    go (Z    era)  (TZ cur)    = SituatedCurrent cur era
    go (S (Z era)) (TZ cur)    = SituatedNext    cur era
    go (S (S era)) (TZ cur)    = SituatedFuture  cur era
    go (Z    era)  (TS past _) = SituatedPast   past era
    go (S    era)  (TS _ st)   = SituatedShift $ go era st

{-------------------------------------------------------------------------------
  Towing
-------------------------------------------------------------------------------}

-- | Advance to the next era if the current end 'Bound' is known
check :: forall xs f. (All SingleEraBlock xs, IsNonEmpty xs)
      => NP (Current f -.-> K (Maybe Bound)) xs
         -- ^ the era end 'Bound', if known
      -> HardForkState f                     xs
      -> HardForkState Proxy                 xs
check endChecks (HardForkState tele) =
    HardForkState . unI
  $ Telescope.extend
      (hcmap proxySingle doEndCheck endChecks)
      (hpure $ fn $ \fx -> fx { currentState = Proxy })
      (InPairs.hcpure
         proxySingle
         ( InPairs.Require $ \(K end) -> Telescope.Extend $ \fx ->
             return $ step fx end
         )
      )
      tele
  where
    doEndCheck :: (Current f -.-> K (Maybe Bound)  ) x
               -> (Current f -.-> Maybe :.: K Bound) x
    doEndCheck endCheck =
      fn $ \curfx -> Comp $ fmap K . unK $ endCheck `apFn` curfx

    step :: Current f x -> Bound -> (K Past x, Current Proxy y)
    step cur end =
      (,)
        (K Past { pastStart = currentStart cur
                , pastEnd   = end
                })
        Current {
                  currentState = Proxy
                , currentStart = end
                }

-- | See 'tow'
newtype CrossEra f f' f'' x y = CrossEra (f x -> EpochNo -> f' y -> f'' y)

-- | Advance to the next era if the second state argument is already there
tow :: forall xs f f' f''. All SingleEraBlock xs
    => InPairs (CrossEra f f' f'')  xs   -- ^ how to cross a boundary
    -> NP      (f -.-> f' -.-> f'') xs   -- ^ how to not cross a boundary
    -> HardForkState f   xs   -- ^ eg the unticked chain dep state
    -> HardForkState f'  xs   -- ^ eg the forecasted ledger view
    -> HardForkState f'' xs   -- ^ eg the ticked chain dep state
tow cross noCross (HardForkState src) (HardForkState tgt) =
    HardForkState . unI $
      Telescope.align
        (InPairs.hmap adaptExt   cross)
        (hmap         adaptNoExt noCross)
        src
        tgt
  where
    adaptNoExt :: (        f -.->         f' -.->         f'') blk
               -> (Current f -.-> Current f' -.-> Current f'') blk
    adaptNoExt f = fn_2 $ \a b -> Current {
        currentStart = currentStart a
      , currentState = f `apFn` currentState a `apFn` currentState b
      }

    adaptExt :: CrossEra f f' f''                blk blk'
             -> (              Le (Current f)
                 --.-->        Le (K Past)
                 --.-->        Ri (Current f')
                 --.--> I :..: Ri (Current f'')) blk blk'
    adaptExt (CrossEra f) =
        DoubleFun $ \(Le fx      ) ->
        DoubleFun $ \(Le (K past)) ->
        let curEnd :: Bound
            curEnd = pastEnd past
        in
        DoubleFun $ \(Ri f'y) ->
          Comp2 $ pure $ Ri $ Current {
              currentStart = curEnd
            , currentState =
                f (currentState fx) (boundEpoch curEnd) (currentState f'y)
            }

{-------------------------------------------------------------------------------
  Summary/EpochInfo
-------------------------------------------------------------------------------}

reconstructSummary :: History.Shape xs
                   -> TransitionInfo         -- ^ At the tip
                   -> HardForkState f xs
                   -> History.Summary xs
reconstructSummary (History.Shape shape) transition (HardForkState st) =
    History.Summary $ go shape st
  where
    go :: Exactly xs' EraParams
       -> Telescope (K Past) (Current f) xs'
       -> NonEmpty xs' EraSummary
    go ExactlyNil              t                   = case t of {}
    go (ExactlyCons params ss) (TS (K Past{..}) t) =
        NonEmptyCons (EraSummary pastStart (EraEnd pastEnd) params) $ go ss t
    go (ExactlyCons params ss) (TZ Current{..}) =
        case transition of
          TransitionKnown epoch ->
            -- We haven't reached the next era yet, but the transition is
            -- already known. The safe zone applies from the start of the
            -- next era.
            let currentEnd = History.mkUpperBound params currentStart epoch
                nextStart  = currentEnd
            in case ss of
              ExactlyCons nextParams _ ->
                  NonEmptyCons EraSummary {
                      eraStart  = currentStart
                    , eraParams = params
                    , eraEnd    = EraEnd currentEnd
                    }
                $ NonEmptyOne EraSummary {
                      eraStart  = nextStart
                    , eraParams = nextParams
                    , eraEnd    = applySafeZone
                                    nextParams
                                    nextStart
                                    (boundSlot nextStart)
                    }
              ExactlyNil               ->
                -- HOWEVER, this code doesn't know what that next era is! This
                -- can arise when a node has not updated its code despite an
                -- imminent hard fork.
                --
                -- In the specific case of 'ShelleyBlock' and 'CardanoBlock', a
                -- lot would have to go wrong in the PR review process for
                -- 'TransitionKnown' to arise during the last known era in the
                -- code. The 'ShelleyBlock' 'singleEraTransition' method leads
                -- to 'TransitionKnown' here only based on the
                -- 'shelleyTriggerHardFork' field of its ledger config, which is
                -- statically set by a quite obvious pattern in
                -- 'protocolInfoCardano', which is passed concrete arguments by
                -- a similarly obvious pattern in
                -- 'mkSomeConsensusProtocolCardano' defined in the
                -- @cardano-node@ repo.
                NonEmptyOne EraSummary {
                    eraStart  = currentStart
                  , eraParams = params
                  , eraEnd    = EraEnd currentEnd
                  }
          TransitionUnknown ledgerTip -> NonEmptyOne $ EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              -- Even if the safe zone is 0, the first slot at
                              -- which the next era could begin is the /next/
                              (next ledgerTip)
              }
          -- 'TransitionImpossible' is used in one of two cases: we are in the
          -- final era this chain will ever have (handled by the corresponding
          -- 'UnsafeIndefiniteSafeZone' case within 'applySafeZone' below) or
          -- this era is a future era that hasn't begun yet, in which case the
          -- safe zone must start at the beginning of this era.
          TransitionImpossible -> NonEmptyOne $ EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              (boundSlot currentStart)
              }

    -- Apply safe zone from the specified 'SlotNo'
    --
    -- All arguments must be referring to or in the same era.
    applySafeZone :: EraParams -> Bound -> SlotNo -> EraEnd
    applySafeZone params@EraParams{..} start =
        case eraSafeZone of
          UnsafeIndefiniteSafeZone ->
              const EraUnbounded
          StandardSafeZone safeFromTip ->
              EraEnd
            . History.mkUpperBound params start
            . History.slotToEpochBound params start
            . History.addSlots safeFromTip

    next :: WithOrigin SlotNo -> SlotNo
    next Origin        = SlotNo 0
    next (NotOrigin s) = succ s
