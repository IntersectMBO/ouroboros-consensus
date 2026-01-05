{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains benchmarks for Peras chain weight calculation as
--   implemented in the 'Ouroboros.Consensus.Peras.Weight' module.
--
--   We benchmark the calculation on a static sequence of chain fragments of
--   increasing length, ranging from 0 to 'fragmentMaxLength', with a step size
--   of 'fragmentLengthStepSize'. The chain fragments are instantiated with
--   'TestBlock', and every 'boostedBlockGap' blocks there is a booster block
--   with weight 'boostWeight'. All parameters are set in 'benchmarkParams'.
module Main (main) where

import Cardano.Ledger.BaseTypes.NonZero (knownNonZeroBounded)
import Data.List (iterate')
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Peras.Params (PerasWeight (..))
import Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot
  , mkPerasWeightSnapshot
  , takeVolatileSuffix
  , weightBoostOfFragment
  )
import Ouroboros.Network.AnchoredFragment qualified as AF
import Test.Ouroboros.Storage.TestBlock (TestBlock (..), TestBody (..), TestHeader (..))
import Test.Ouroboros.Storage.TestBlock qualified as TestBlock
import Test.Tasty.Bench

data BenchmarkParams = BenchmarkParams
  { slotGap :: Word64
  -- ^ The slot gap between blocks on the fragments, ie the inverse of the
  -- active slot coefficient. Measured in slots.
  , fragmentLengthStepSize :: Natural
  -- ^ Step size for the fragment lengths between different benchmarks, in
  -- blocks.
  , fragmentMaxLength :: Natural
  -- ^ The maximum length of a fragment, in blocks.
  , boostedBlockGap :: Natural
  -- ^ How often boosted blocks occur, in blocks.
  , boostWeight :: PerasWeight
  -- ^ The weight of the boost.
  }

benchmarkParams :: BenchmarkParams
benchmarkParams =
  BenchmarkParams
    { -- On Cardano mainnet, the active slot coefficient f=1/20, so there are 20
      -- slots between blocks on average assuming nominal chain density.
      slotGap = 20
    , -- Represents a decent balance between the number of benchmarks we run and
      -- the granularity at which we can observe results.
      fragmentLengthStepSize = 100
    , -- This is the maximum size of header fragments while syncing (the current
      -- selection (k) plus one forecast window under nominal chain density
      -- (3k), where k=2160 on Cardano mainnet).
      fragmentMaxLength = 2160 + 3 * 2160
    , -- A plausible value for the Peras round length is 90 slots, which means
      -- that we expect to see 4-5 blocks per Peras round (and therefore between
      -- boosted blocks) on mainnet where the active slot coefficient f=1/20.
      boostedBlockGap = 5
    , -- This is a plausible mainnet value (the exact value does not impact the
      -- benchmark).
      boostWeight = PerasWeight 15
    }

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain $
    concat
      [ map benchWeightBoostOfFragment inputs
      , map benchTakeVolatileSuffix inputs
      ]
 where
  -- NOTE: we do not use the 'env' combinator to set up the test data since
  --       it requires 'NFData' for 'AF.AnchoredFragment'. While the necessary
  --       instances could be provided, we do not think is necessary for this
  --       benchmark, as the input data is rather small.
  inputs :: [(Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock))]
  inputs =
    getEveryN (fragmentLengthStepSize benchmarkParams) $
      take (fromIntegral $ fragmentMaxLength benchmarkParams) $
        zip [0 ..] $
          zip (map uniformWeightSnapshot fragments) fragments

benchWeightBoostOfFragment ::
  (Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock)) -> Benchmark
benchWeightBoostOfFragment (i, (weightSnapshot, fragment)) =
  bench ("weightBoostOfFragment of length " <> show i) $
    whnf (weightBoostOfFragment weightSnapshot) fragment

benchTakeVolatileSuffix ::
  (Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock)) -> Benchmark
benchTakeVolatileSuffix (i, (weightSnapshot, fragment)) =
  bench ("takeVolatileSuffix of length " <> show i) $
    whnf (takeVolatileSuffix weightSnapshot k) fragment
 where
  k = SecurityParam $ knownNonZeroBounded @2160

-- | An infinite list of chain fragments
fragments :: [AF.AnchoredFragment TestBlock]
fragments = iterate' addSuccessorBlock genesisFragment
 where
  genesisFragment :: AF.AnchoredFragment TestBlock
  genesisFragment = AF.Empty AF.AnchorGenesis

  addSuccessorBlock :: AF.AnchoredFragment TestBlock -> AF.AnchoredFragment TestBlock
  addSuccessorBlock = \case
    AF.Empty _ -> (AF.Empty AF.AnchorGenesis) AF.:> (TestBlock.firstBlock 0 dummyBody)
    (xs AF.:> x) ->
      let nextBlockSlot = SlotNo (slotGap benchmarkParams) + thSlotNo (testHeader x)
       in (xs AF.:> x) AF.:> TestBlock.mkNextBlock x nextBlockSlot dummyBody

  dummyBody :: TestBody
  dummyBody = TestBody{tbForkNo = 0, tbIsValid = True}

-- | Given a chain fragment, construct a weight snapshot where there's a boosted block every 90 slots
uniformWeightSnapshot :: AF.AnchoredFragment TestBlock -> PerasWeightSnapshot TestBlock
uniformWeightSnapshot fragment =
  let pointsToBoost =
        map snd
          . getEveryN (boostedBlockGap benchmarkParams)
          . zip [0 ..]
          . map AF.blockPoint
          . AF.toOldestFirst
          $ fragment
      weights = repeat (boostWeight benchmarkParams)
   in mkPerasWeightSnapshot $ pointsToBoost `zip` weights

getEveryN :: Natural -> [(Natural, a)] -> [(Natural, a)]
getEveryN n = filter (\(i, _) -> (i `mod` n) == 0)
