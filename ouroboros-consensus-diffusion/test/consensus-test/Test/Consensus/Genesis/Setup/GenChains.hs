{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Setup.GenChains
  ( GenesisTest (..)
  , genChains
  , genChainsWithExtraHonestPeers
  ) where

import Cardano.Ledger.BaseTypes (nonZeroOr)
import Cardano.Slotting.Time (slotLengthFromSec)
import Control.Monad (replicateM)
import qualified Control.Monad.Except as Exn
import Data.List as List (foldl')
import Data.Proxy (Proxy (..))
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8)
import Ouroboros.Consensus.Block.Abstract hiding (Header)
import Ouroboros.Consensus.Protocol.Abstract
  ( SecurityParam (SecurityParam)
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Protocol.Limits (shortWait)
import qualified Test.Consensus.BlockTree as BT
import Test.Consensus.PointSchedule
import Test.Ouroboros.Consensus.ChainGenerator.Adversarial
  ( genPrefixBlockCount
  )
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import Test.Ouroboros.Consensus.ChainGenerator.Counting
  ( Count (Count)
  , getVector
  )
import Test.Ouroboros.Consensus.ChainGenerator.Honest
  ( ChainSchema (ChainSchema)
  , HonestRecipe (..)
  )
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import Test.Ouroboros.Consensus.ChainGenerator.Params
import Test.Ouroboros.Consensus.ChainGenerator.Slot (S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import Test.Ouroboros.Consensus.QuickCheck.Extras
  ( unsafeMapSuchThatJust
  )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random (QCGen)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock hiding (blockTree)

-- | Random generator for an honest chain recipe and schema.
genHonestChainSchema :: QC.Gen (Asc, H.HonestRecipe, H.SomeHonestChainSchema)
genHonestChainSchema = do
  asc <- genAsc
  honestRecipe <- H.genHonestRecipe

  H.SomeCheckedHonestRecipe Proxy Proxy honestRecipe' <-
    case Exn.runExcept $ H.checkHonestRecipe honestRecipe of
      Left exn -> error $ "impossible! " <> show (honestRecipe, exn)
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

    let testRecipeA =
          A.AdversarialRecipe
            { A.arPrefix
            , A.arParams = (kcp, scg, delta)
            , A.arHonest
            }

    alternativeAsc <- ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

    case Exn.runExcept $ A.checkAdversarialRecipe testRecipeA of
      Left e -> case e of
        A.NoSuchAdversarialBlock -> pure Nothing
        A.NoSuchCompetitor -> error $ "impossible! " <> show e
        A.NoSuchIntersection -> error $ "impossible! " <> show e
      Right (A.SomeCheckedAdversarialRecipe _ testRecipeA'') -> do
        let Count prefixCount = arPrefix
        (seed :: QCGen) <- QC.arbitrary
        let H.ChainSchema _ v = A.uniformAdversarialChain (Just alternativeAsc) testRecipeA'' seed
        pure $ Just (prefixCount, Vector.toList (getVector v))

genChains :: QC.Gen Word -> QC.Gen (GenesisTest TestBlock ())
genChains = genChainsWithExtraHonestPeers (pure 0)

-- | Random generator for a block tree. The block tree contains one trunk (the
-- “honest” chain) and as many branches as given as a parameter (the
-- “alternative” chains or “bad” chains). For instance, one such tree could be
-- graphically represented as:
--
--     slots:    1  2  3  4  5  6  7  8  9
--     trunk: O─────1──2──3──4─────5──6──7
--                     │           ╰─────6
--                     ╰─────3──4─────5
-- For now, the @extraHonestPeers@ generator is only used to fill the GenesisTest field.
-- However, in the future it could also be used to generate "short forks" near the tip of the trunk.
genChainsWithExtraHonestPeers :: QC.Gen Word -> QC.Gen Word -> QC.Gen (GenesisTest TestBlock ())
genChainsWithExtraHonestPeers genNumExtraHonest genNumForks = do
  (_, honestRecipe, someHonestChainSchema) <- genHonestChainSchema

  H.SomeHonestChainSchema _ _ honestChainSchema <- pure someHonestChainSchema
  let ChainSchema _ vH = honestChainSchema
      goodChain = mkTestFragment goodBlocks
      -- blocks for the good chain in reversed order
      goodBlocks = mkTestBlocks [] slotsH 0
      slotsH = Vector.toList (getVector vH)
      HonestRecipe (Kcp kcp) (Scg scg) delta _len = honestRecipe

  numForks <- genNumForks
  gtExtraHonestPeers <- genNumExtraHonest
  alternativeChainSchemas <-
    replicateM (fromIntegral numForks) (genAlternativeChainSchema (honestRecipe, honestChainSchema))
  pure $
    GenesisTest
      { gtSecurityParam =
          SecurityParam $
            -- As long as `genKSD` generates a `k` that is > 0, this won't lead to an ErrorCall.
            nonZeroOr (fromIntegral kcp) $
              error "Generated Kcp was zero. Cannot construct a NonZero value for the SecurityParam."
      , gtGenesisWindow = GenesisWindow (fromIntegral scg)
      , gtForecastRange = ForecastRange (fromIntegral scg) -- REVIEW: Do we want to generate those randomly?
      , gtDelay = delta
      , gtSlotLength = slotLengthFromSec 20
      , gtChainSyncTimeouts = chainSyncTimeouts
      , gtBlockFetchTimeouts = blockFetchTimeouts
      , gtLoPBucketParams = LoPBucketParams{lbpCapacity = 50, lbpRate = 10}
      , -- These values give little enough leeway (5s) so that some adversaries get disconnected
        -- by the LoP during the stalling attack test. Maybe we should design a way to override
        -- those values for individual tests?
        -- Also, we might want to generate these randomly.
        gtCSJParams = CSJParams $ fromIntegral scg
      , gtBlockTree =
          List.foldl' (flip BT.addBranch') (BT.mkTrunk goodChain) $
            zipWith (genAdversarialFragment goodBlocks) [1 ..] alternativeChainSchemas
      , gtExtraHonestPeers
      , gtSchedule = ()
      }
 where
  genAdversarialFragment :: [TestBlock] -> Int -> (Int, [S]) -> AnchoredFragment TestBlock
  genAdversarialFragment goodBlocks forkNo (prefixCount, slotsA) =
    mkTestFragment (mkTestBlocks prefix slotsA forkNo)
   where
    -- blocks in the common prefix in reversed order
    prefix = drop (length goodBlocks - prefixCount) goodBlocks

  mkTestFragment :: [TestBlock] -> AnchoredFragment TestBlock
  mkTestFragment =
    AF.fromNewestFirst AF.AnchorGenesis

  mkTestBlocks :: [TestBlock] -> [S] -> Int -> [TestBlock]
  mkTestBlocks pre active forkNo =
    fst (List.foldl' folder ([], 0) active)
   where
    folder (chain, inc) s
      | S.test S.notInverted s = (issue inc chain, 0)
      | otherwise = (chain, inc + 1)
    issue inc (h : t) = incSlot inc (successorBlock h) : h : t
    issue inc []
      | [] <- pre = [incSlot inc ((firstBlock (fromIntegral forkNo)){tbSlot = 0})]
      | h : t <- pre = incSlot inc (modifyFork (const (fromIntegral forkNo)) (successorBlock h)) : h : t

  incSlot :: SlotNo -> TestBlock -> TestBlock
  incSlot n b = b{tbSlot = tbSlot b + n}

chainSyncTimeouts :: ChainSyncTimeout
chainSyncTimeouts =
  ChainSyncTimeout
    { canAwaitTimeout
    , intersectTimeout
    , mustReplyTimeout
    , idleTimeout
    }
 where
  canAwaitTimeout :: Maybe DiffTime
  canAwaitTimeout = shortWait
  intersectTimeout :: Maybe DiffTime
  intersectTimeout = shortWait
  idleTimeout :: Maybe DiffTime
  -- \| The default from 'Ouroboros.Consensus.Node.stdChainSyncTimeout' is
  -- 3673s, which is virtually infinite, so let us make it actually infinite
  -- for our test environment.
  idleTimeout = Nothing
  -- \| The 'mustReplyTimeout' must be disabled in our context, because the
  -- chains are finite, and therefore an honest peer can only serve it all,
  -- then send 'MsgAwaitReply' (therefore entering 'StMustReply'), and then
  -- stall forever, and it must not be killed for it.
  --
  -- Note that this allows the adversaries to stall us forever in that same
  -- situation. However, that peer is only allowed to send 'MsgAwaitReply'
  -- when they have served their tip, which leaves them fully vulnerable to
  -- the Genesis Density Disconnection (GDD) logic. A bug related to this
  -- disabled timeout is in fact either a bug in the GDD or in the tests.
  mustReplyTimeout :: Maybe DiffTime
  mustReplyTimeout = Nothing

blockFetchTimeouts :: BlockFetchTimeout
blockFetchTimeouts =
  BlockFetchTimeout
    { busyTimeout = Just 60
    , streamingTimeout = Just 60
    }
