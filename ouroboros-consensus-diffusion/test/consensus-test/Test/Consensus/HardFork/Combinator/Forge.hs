{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.HardFork.Combinator.Forge (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad.Except (runExcept)
import Data.ByteString.Short (fromShort)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.SOP.BasicFunctors
import Data.SOP.Counting (exactlyTwo)
import Data.SOP.OptNP (OptNP (..))
import Data.SOP.Strict
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (SystemStart (..), slotLengthFromSec)
import Ouroboros.Consensus.Config
  ( SecurityParam (..)
  , TopLevelConfig (..)
  , emptyCheckpointsMap
  )
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraIsLeader (..)
  , OneEraValidatedGenTx (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.SupportsMempool (toRawTxIdHash, txId)
import Ouroboros.Consensus.Leios.Types
  ( ForgedLeiosEb (..)
  , LeiosEb (..)
  , TxHash (..)
  )
import Ouroboros.Consensus.TypeFamilyWrappers
import Test.Consensus.HardFork.Combinator.A
import Test.Consensus.HardFork.Combinator.B
import Test.Consensus.HardFork.Combinator.Setup (TestBlock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Util.Time (dawnOfTime)

tests :: TestTree
tests =
  testGroup
    "Forge"
    [ testCase
        "Forges in the same era as the ticked ledger state"
        unit_forgeBlockForgesInSameEra
    ]

-- | The hard fork 'forgeBlock' calls the 'forgeBlock' of the era that the
-- ticked ledger state is in. This era gets the ranking-block transactions in
-- 'fbTxs' and the endorser-block transactions in 'fbEbTxs', with no change.
-- The hard fork 'forgeBlock' returns the endorser block that this era forges.
unit_forgeBlockForgesInSameEra :: Assertion
unit_forgeBlockForgesInSameEra = do
  let result = runExcept $ propHardForkForgesInSameEra hardForkForging args
  case result of
    Left (msg, _) -> assertFailure msg
    Right (HardForkBlock (OneEraBlock (Z (I blkA))), forgedEb) -> do
      blkA_body blkA @?= [txA 1, txA 2]
      forgedEb @?= Just (ebListing [txA 3])
    Right _ -> assertFailure "impossible: era was verified by propHardForkForgesInSameEra"
 where
  hardForkForging :: BlockForging Identity TestBlock
  hardForkForging =
    runIdentity $
      mkBlockForging $
        hardForkBlockForging (const "Test") $
          OptCons (MkBlockForging $ pure echoBlockForgingA) $
            OptCons (MkBlockForging $ pure blockForgingB) $
              OptNil

  -- This is the forge of era A, with two changes. The block body holds
  -- 'fbTxs', and the endorser block lists 'fbEbTxs'. Thus the test can see
  -- the two lists that era A gets.
  echoBlockForgingA :: BlockForging Identity BlockA
  echoBlockForgingA =
    blockForgingA
      { forgeBlock = \argsA -> do
          (blkA, _) <- forgeBlock blockForgingA argsA
          pure
            ( blkA{blkA_body = map forgetValidatedGenTxA (fbTxs argsA)}
            , Just $ ebListing $ map forgetValidatedGenTxA (fbEbTxs argsA)
            )
      }

  ebListing :: [GenTx BlockA] -> ForgedLeiosEb
  ebListing txs =
    ForgedLeiosEb $
      MkLeiosEb $
        V.fromList [(MkTxHash (fromShort (toRawTxIdHash (txId tx))), 0) | tx <- txs]

  txA :: Int -> GenTx BlockA
  txA n = TxA (TxIdA n) InitiateAtoB

  validatedInA :: GenTx BlockA -> Validated (GenTx TestBlock)
  validatedInA =
    HardForkValidatedGenTx . OneEraValidatedGenTx . Z . WrapValidatedGenTx . ValidatedGenTxA

  args :: ForgeBlockArgs TestBlock
  args =
    ForgeBlockArgs
      { fbConfig = cfg
      , fbCurrentBlockNo = BlockNo 1
      , fbCurrentSlotNo = SlotNo 1
      , fbPerasCert = Nothing
      , fbCurrentTickedLedgerState =
          TickedHardForkLedgerState (TransitionUnknown Origin) $
            initHardForkState $
              FlipTickedLedgerState $
                TickedLedgerStateA $
                  LgrA{lgrA_tip = GenesisPoint, lgrA_transition = Nothing}
      , fbTxs = map validatedInA [txA 1, txA 2]
      , fbEbTxs = map validatedInA [txA 3]
      , fbIsLeader = OneEraIsLeader $ Z $ WrapIsLeader ()
      }

  k :: SecurityParam
  k = SecurityParam $ knownNonZeroBounded @2

  shape :: History.Shape '[BlockA, BlockB]
  shape = History.Shape $ exactlyTwo eraParams eraParams
   where
    eraParams = History.defaultEraParams k (slotLengthFromSec 1) History.NoPerasEnabled

  cfg :: TopLevelConfig TestBlock
  cfg =
    TopLevelConfig
      { topLevelConfigProtocol =
          HardForkConsensusConfig
            { hardForkConsensusConfigK = k
            , hardForkConsensusConfigShape = shape
            , hardForkConsensusConfigPerEra =
                PerEraConsensusConfig $
                  WrapPartialConsensusConfig CfgA{cfgA_k = k, cfgA_leadInSlots = mempty}
                    :* WrapPartialConsensusConfig CfgB{cfgB_k = k, cfgB_leadInSlots = mempty}
                    :* Nil
            }
      , topLevelConfigLedger =
          HardForkLedgerConfig
            { hardForkLedgerConfigShape = shape
            , hardForkLedgerConfigPerEra =
                PerEraLedgerConfig $
                  WrapPartialLedgerConfig
                    LCfgA
                      { lcfgA_k = k
                      , lcfgA_systemStart = SystemStart dawnOfTime
                      , lcfgA_forgeTxs = mempty
                      }
                    :* WrapPartialLedgerConfig ()
                    :* Nil
            }
      , topLevelConfigBlock =
          HardForkBlockConfig $ PerEraBlockConfig $ BCfgA :* BCfgB :* Nil
      , topLevelConfigCodec =
          HardForkCodecConfig $ PerEraCodecConfig $ CCfgA :* CCfgB :* Nil
      , topLevelConfigStorage =
          HardForkStorageConfig $ PerEraStorageConfig $ SCfgA :* SCfgB :* Nil
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }
