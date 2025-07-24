{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | This module contains benchmarks for Peras chain weight calculation as
--   implemented by the by the
--   'Ouroboros.Consensus.Peras.Weight.boostedWeightForFragment' function.
--
--   We benchmark the calculation on a static sequence of chain fragments of increasing
--   length, ranging from 0 to around 8640, with a sampling rate of 100. The chain fragments
--   are instantiated with 'TestBlock', and every 5 blocks there is a booster block with
--   weight 15. All parameters are set in 'benchmarkParams'.
module Main (main) where

import Data.List (iterate')
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (PerasWeight (PerasWeight), SlotNo (..))
import Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot
  , boostedWeightForFragment
  , mkPerasWeightSnapshot
  )
import Ouroboros.Network.AnchoredFragment qualified as AF
import Test.Ouroboros.Storage.TestBlock (TestBlock (..), TestBody (..), TestHeader (..))
import Test.Ouroboros.Storage.TestBlock qualified as TestBlock
import Test.Tasty.Bench

data BenchmarkParams = BenchmarkParams
  { blockRate :: SlotNo
  -- ^ How often the fragments will contain blocks, in slots
  , fragmentLenghtSamplingRate :: Natural
  -- ^ The rate of length increase for generate chain fragments
  , fragmentMaxLenght :: Natural
  -- ^ the maximum length of a fragment
  , boostedBlockRate :: Natural
  -- ^ How often boosted blocks occur, in blocks
  , boostWeight :: PerasWeight
  -- ^ The weight of the boost
  }

benchmarkParams :: BenchmarkParams
benchmarkParams =
  BenchmarkParams
    { blockRate = 20
    , fragmentLenghtSamplingRate = 100
    , fragmentMaxLenght = 2160 + 3 * 2160
    , boostedBlockRate = 5
    , boostWeight = PerasWeight 15
    }

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain $ map benchBoostedWeightForFragment inputs
 where
  -- NOTE: we do not use the 'env' combinator to set up the test data since
  --       it requires 'NFData' for 'AF.AnchoredFragment'. While the necessary
  --       instances could be provided, we do not think is necessary for this
  --       benchmark, as the input data is rather small.
  inputs :: [(Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock))]
  inputs =
    getEveryN (fragmentLenghtSamplingRate benchmarkParams) $
      take (fromIntegral $ fragmentMaxLenght benchmarkParams) $
        zip [0 ..] $
          zip (map uniformWeightSnapshot fragments) fragments

benchBoostedWeightForFragment ::
  (Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock)) -> Benchmark
benchBoostedWeightForFragment (i, (weightSnapshot, fragment)) =
  bench ("boostedWeightForFragment of length " <> show i) $
    whnf (boostedWeightForFragment weightSnapshot) fragment

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
      let nextBlockSlot = blockRate benchmarkParams + (thSlotNo . testHeader $ x)
       in (xs AF.:> x) AF.:> TestBlock.mkNextBlock x nextBlockSlot dummyBody

  dummyBody :: TestBody
  dummyBody = TestBody{tbForkNo = 0, tbIsValid = True}

-- | Given a chain fragment, construct a weight snapshot where there's a boosted block every 90 slots
uniformWeightSnapshot :: AF.AnchoredFragment TestBlock -> PerasWeightSnapshot TestBlock
uniformWeightSnapshot fragment =
  let pointsToBoost =
        map snd
          . getEveryN (boostedBlockRate benchmarkParams)
          . zip [0 ..]
          . map AF.blockPoint
          . AF.toOldestFirst
          $ fragment
      weights = repeat (boostWeight benchmarkParams)
   in mkPerasWeightSnapshot $ pointsToBoost `zip` weights

getEveryN :: Natural -> [(Natural, a)] -> [(Natural, a)]
getEveryN n = filter (\(i, _) -> (i `mod` n) == 0)
