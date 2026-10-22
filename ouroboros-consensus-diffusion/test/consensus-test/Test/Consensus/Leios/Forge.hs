{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that the node stores the endorser blocks that it forges.
module Test.Consensus.Leios.Forge (tests) where

import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract (GetTip (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.ChainDB.API (getCurrentChain)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..), withReader)
import Ouroboros.Consensus.Util.IOLike (atomically)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestBlock (TestHash (..))

tests :: TestTree
tests =
  testGroup
    "Forging"
    [ testCase
        "the node stores each endorser block that it forges, at the point that the header announces"
        $ do
          let ForgeOutcome{announcedEbs, storedPoints} = runSimOrThrow forgedEbs
          assertBool "the node forged no block" (not (null announcedEbs))
          sort storedPoints @?= sort [(announcedSlot eb, announcedHash eb) | eb <- announcedEbs]
          map storedTxs announcedEbs @?= map (Just . forgedTxs) announcedEbs
    ]

-- | What the chain and the LeiosDB hold after the node forges.
data ForgeOutcome = ForgeOutcome
  { announcedEbs :: [AnnouncedEb]
  -- ^ The endorser blocks that the blocks on the chain announce, oldest first.
  , storedPoints :: [(SlotNo, EndorserBlockHash)]
  -- ^ The points in the LeiosDB.
  }

-- | An endorser block that a block on the chain announces.
data AnnouncedEb = AnnouncedEb
  { announcedSlot :: SlotNo
  , announcedHash :: EndorserBlockHash
  , forgedTxs :: [ByteString]
  -- ^ The bytes of the transactions that 'ebForging' puts in the endorser block.
  , storedTxs :: Maybe [ByteString]
  -- ^ The bytes of the transactions that the LeiosDB holds for 'announcedHash',
  -- or 'Nothing' if it does not hold all of them.
  }

-- | The node forges in each of the first slots.
forgedEbs :: IOSim s ForgeOutcome
forgedEbs = withNode $ \node -> do
  setBlockForging (nodeKernel node) [MkBlockForging (pure ebForging)]
  -- A slot is one second, so the node forges in slots 0 to 3. The half slot
  -- lets the forge of slot 3 add its block to the ChainDB.
  threadDelay 3.5
  chain <- atomically $ getCurrentChain (nodeChainDB node)
  withReader (getLeiosDB (nodeKernel node)) $ \reader -> do
    announcedEbs <-
      sequence
        [ AnnouncedEb (blockSlot hdr) ebHash [txInSlot (blockSlot hdr)]
            . fmap (map snd)
            <$> lookupEbClosure reader ebHash
        | hdr <- AF.toOldestFirst chain
        , Just (ebHash, _size) <- [lthAnnouncement hdr]
        ]
    storedPoints <- testScanEbPoints reader
    pure ForgeOutcome{announcedEbs, storedPoints}

-- | A forging that leads in every slot. Each block extends the ledger tip and
-- announces an endorser block with one transaction, whose bytes depend on the
-- slot, so that each endorser block has its own hash.
ebForging :: BlockForging (IOSim s) LeiosTestBlock
ebForging =
  BlockForging
    { forgeLabel = "ebForging"
    , canBeLeader = ()
    , updateForgeState = \_ _ _ -> pure $ ForgeStateUpdated ()
    , checkCanForge = \_ _ _ _ _ -> Right ()
    , forgeBlock = \ForgeBlockArgs{fbCurrentSlotNo, fbCurrentTickedLedgerState} -> do
        let forgedEb = forgeLeiosEb (txInSlot fbCurrentSlotNo :| [])
            eb = forgedEbBody forgedEb
            ebHash = endorserBlockHash eb
            tip = castPoint (getTip fbCurrentTickedLedgerState)
        pure
          ForgeBlockResult
            { rankingBlock =
                setAnnouncement ebHash (encodeEndorserBlockSize eb) $
                  setSlot fbCurrentSlotNo $
                    blockAfter tip
            , endorserBlock = Just forgedEb
            }
    , finalize = pure ()
    }

-- | A block whose predecessor is the given point. 'forgeBlock' sets its slot.
blockAfter :: Point LeiosTestBlock -> LeiosTestBlock
blockAfter = \case
  GenesisPoint -> firstLeiosBlock 0
  BlockPoint _ (TestHash path) ->
    successorLeiosBlock $
      LeiosTestBlock (ltbHeader (firstLeiosBlock 0)){lthHash = TestHash path}

txInSlot :: SlotNo -> ByteString
txInSlot slot = BS8.pack ("tx in slot " <> show (unSlotNo slot))
