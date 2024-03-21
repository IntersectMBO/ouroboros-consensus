{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Consensus.Genesis.Setup.GenChains (
    GenesisTest (..)
  , genChains
  ) where

import           Cardano.Slotting.Time (SlotLength, getSlotLength,
                     slotLengthFromSec)
import           Control.Monad (replicateM)
import qualified Control.Monad.Except as Exn
import           Data.List (foldl')
import           Data.Proxy (Proxy (..))
import           Data.Time.Clock (DiffTime, secondsToDiffTime)
import qualified Data.Vector.Unboxed as Vector
import           Data.Word (Word8)
import           Ouroboros.Consensus.Block.Abstract hiding (Header)
import           Ouroboros.Consensus.Protocol.Abstract
                     (SecurityParam (SecurityParam))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           Ouroboros.Network.Protocol.Limits (shortWait)
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.PointSchedule
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import           Test.Ouroboros.Consensus.ChainGenerator.Adversarial
                     (genPrefixBlockCount)
import           Test.Ouroboros.Consensus.ChainGenerator.Counting
                     (Count (Count), getVector)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (ChainSchema (ChainSchema), HonestRecipe (..))
import           Test.Ouroboros.Consensus.ChainGenerator.Params
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (S)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.QuickCheck.Random (QCGen)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock hiding (blockTree)

-- | Random generator for an honest chain recipe and schema.
genHonestChainSchema :: QC.Gen (Asc, H.HonestRecipe, H.SomeHonestChainSchema)
genHonestChainSchema = do
  asc <- genAsc
  honestRecipe <- H.genHonestRecipe

  H.SomeCheckedHonestRecipe Proxy Proxy honestRecipe' <-
    case Exn.runExcept $ H.checkHonestRecipe honestRecipe of
      Left exn            -> error $ "impossible! " <> show (honestRecipe, exn)
      Right honestRecipe' -> pure honestRecipe'
  (seed :: QCGen) <- QC.arbitrary
  let schema = H.uniformTheHonestChain (Just asc) honestRecipe' seed

  pure (asc, honestRecipe, H.SomeHonestChainSchema Proxy Proxy schema)

-- | Random generator for one alternative chain schema forking off a given
-- honest chain schema. The alternative chain schema is returned as the pair of
-- a slot number on the honest chain schema and a list of active slots.
--
-- REVIEW: Use 'SlotNo' instead of 'Int'?
genAlternativeChainSchema :: (H.HonestRecipe, H.ChainSchema base hon) -> QC.Gen (Int, [S])
genAlternativeChainSchema (testRecipeH, arHonest) =
  unsafeMapSuchThatJust $ do
    let H.HonestRecipe kcp scg delta _len = testRecipeH

    (seedPrefix :: QCGen) <- QC.arbitrary
    let arPrefix = genPrefixBlockCount testRecipeH seedPrefix arHonest

    let testRecipeA = A.AdversarialRecipe {
      A.arPrefix,
      A.arParams = (kcp, scg, delta),
      A.arHonest
    }

    alternativeAsc <- ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

    case Exn.runExcept $ A.checkAdversarialRecipe testRecipeA of
      Left e -> case e of
        A.NoSuchAdversarialBlock -> pure Nothing
        A.NoSuchCompetitor       -> error $ "impossible! " <> show e
        A.NoSuchIntersection     -> error $ "impossible! " <> show e

      Right (A.SomeCheckedAdversarialRecipe _ testRecipeA'') -> do
        let Count prefixCount = arPrefix
        (seed :: QCGen) <- QC.arbitrary
        let H.ChainSchema _ v = A.uniformAdversarialChain (Just alternativeAsc) testRecipeA'' seed
        pure $ Just (prefixCount, Vector.toList (getVector v))

-- | Random generator for a block tree. The block tree contains one trunk (the
-- “honest” chain) and as many branches as given as a parameter (the
-- “alternative” chains or “bad” chains). For instance, one such tree could be
-- graphically represented as:
--
--     slots:    1  2  3  4  5  6  7  8  9
--     trunk: O─────1──2──3──4─────5──6──7
--                     │           ╰─────6
--                     ╰─────3──4─────5
genChains :: QC.Gen Word -> QC.Gen (GenesisTest TestBlock ())
genChains genNumForks = do
  (asc, honestRecipe, someHonestChainSchema) <- genHonestChainSchema

  H.SomeHonestChainSchema _ _ honestChainSchema <- pure someHonestChainSchema
  let ChainSchema _ vH = honestChainSchema
      goodChain = mkTestFragment goodBlocks
      -- blocks for the good chain in reversed order
      goodBlocks = mkTestBlocks [] slotsH 0
      slotsH = Vector.toList (getVector vH)
      HonestRecipe (Kcp kcp) (Scg scg) delta _len = honestRecipe

  numForks <- genNumForks
  alternativeChainSchemas <- replicateM (fromIntegral numForks) (genAlternativeChainSchema (honestRecipe, honestChainSchema))
  pure $ GenesisTest {
    gtSecurityParam = SecurityParam (fromIntegral kcp),
    gtGenesisWindow = GenesisWindow (fromIntegral scg),
    gtForecastRange = ForecastRange (fromIntegral scg), -- REVIEW: Do we want to generate those randomly?
    gtDelay = delta,
    gtSlotLength,
    gtChainSyncTimeouts = chainSyncTimeouts gtSlotLength asc,
    gtBlockFetchTimeouts = blockFetchTimeouts,
    gtLoPBucketParams = LoPBucketParams { lbpCapacity = 10_000, lbpRate = 1_000 }, -- REVIEW: Do we want to generate those randomly?
    gtBlockTree = foldl' (flip BT.addBranch') (BT.mkTrunk goodChain) $ zipWith (genAdversarialFragment goodBlocks) [1..] alternativeChainSchemas,
    gtSchedule = ()
    }

  where
    gtSlotLength = slotLengthFromSec 20

    genAdversarialFragment :: [TestBlock] -> Int -> (Int, [S]) -> AnchoredFragment TestBlock
    genAdversarialFragment goodBlocks forkNo (prefixCount, slotsA)
      = mkTestFragment (mkTestBlocks prefix slotsA forkNo)
      where
        -- blocks in the common prefix in reversed order
        prefix = drop (length goodBlocks - prefixCount) goodBlocks

    mkTestFragment :: [TestBlock] -> AnchoredFragment TestBlock
    mkTestFragment =
      AF.fromNewestFirst AF.AnchorGenesis

    mkTestBlocks :: [TestBlock] -> [S] -> Int -> [TestBlock]
    mkTestBlocks pre active forkNo =
      fst (foldl' folder ([], 0) active)
      where
        folder (chain, inc) s | S.test S.notInverted s = (issue inc chain, 0)
                              | otherwise = (chain, inc + 1)
        issue inc (h : t) = incSlot inc (successorBlock h) : h : t
        issue inc [] | [] <- pre = [incSlot inc ((firstBlock (fromIntegral forkNo)) {tbSlot = 0})]
                     | h : t <- pre = incSlot inc (modifyFork (const (fromIntegral forkNo)) (successorBlock h)) : h : t

    incSlot :: SlotNo -> TestBlock -> TestBlock
    incSlot n b = b { tbSlot = tbSlot b + n }

chainSyncTimeouts ::
  SlotLength ->
  Asc ->
  ChainSyncTimeout
chainSyncTimeouts t f =
  ChainSyncTimeout
    { canAwaitTimeout,
      intersectTimeout,
      mustReplyTimeout,
      idleTimeout
    }
  where
    canAwaitTimeout :: Maybe DiffTime
    canAwaitTimeout = shortWait -- 10s
    intersectTimeout :: Maybe DiffTime
    intersectTimeout = shortWait -- 10s
    idleTimeout :: Maybe DiffTime
    idleTimeout = Just 3673 -- taken from Ouroboros.Consensus.Node.stdChainSyncTimeout
    -- | The following timeout is derived from the average length of a streak of
    -- empty slots. If the probability of the election of a leader is @f@ and
    -- @Y@ is a probability, then a streak of empty slots will be shorter than
    -- @log (1 - Y) / log (1 - f)@ with probability @Y@. Main net nodes pick a
    -- random value for @Y@ between 99.9% and 99.999%. For our use case, we
    -- choose the tightest bound of 99.9%.
    mustReplyTimeout :: Maybe DiffTime
    mustReplyTimeout =
      Just $
        secondsToDiffTime $
          round $
            realToFrac (getSlotLength t)
              * log (1 - 0.999)
              / log (1 - ascVal f)

blockFetchTimeouts :: BlockFetchTimeout
blockFetchTimeouts =
  BlockFetchTimeout
    { busyTimeout = Just 60,
      streamingTimeout = Just 60
    }
