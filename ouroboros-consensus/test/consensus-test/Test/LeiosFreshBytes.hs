-- | 'adoptedBytes' replayed over generated chain histories: blocks adopted one
-- at a time under a clock that only moves forward, each seen some time after
-- its slot's onset, with forks that replace the newest blocks. At every
-- adoption the whole chain is handed over newest first with each block's age,
-- as the node's watcher does; the exact oracle counts each block once, when it
-- is first adopted, if it was within the window then and newer than every
-- block adopted before it. The slot length varies between histories and must
-- not matter.
module Test.LeiosFreshBytes (tests) where

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Data.List (foldl')
import Data.Time.Clock (NominalDiffTime)
import LeiosFreshBytes (adoptedBytes, newestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock (TestBlock, firstBlock, successorBlock)

tests :: TestTree
tests =
  testGroup
    "LeiosFreshBytes"
    [ testProperty "replay matches the first-adoption oracle" prop_replay
    , testProperty "the same chain again adds nothing" prop_again
    , testProperty "the lazy walk lists the fragment newest first" prop_newestFirst
    ]

-- | The window every history is judged by.
window :: NominalDiffTime
window = 300

-- | One step of a history: drop this many of the newest blocks (a fork), then
-- a block this many slots after the tip, of this size, seen this many seconds
-- after its slot's onset (negative: a block from the near future, as clock
-- skew allows).
data Step = Step Word Word Int Int
  deriving (Eq, Show)

-- | A history and the length of its slots in seconds.
data History = History NominalDiffTime [Step]
  deriving (Eq, Show)

instance Arbitrary History where
  arbitrary = History <$> elements [0.5, 1, 2, 20] <*> listOf step
   where
    step =
      Step
        <$> frequency [(6, pure 0), (1, choose (1, 4))]
        <*> choose (1, 40)
        <*> choose (0, 90000)
        <*> delay
    -- straddle the window: on it, just past it, around it, and early
    w = round window
    delay = oneof [pure w, pure (w + 1), choose (-2, w + 30), choose (0, 5)]
  shrink (History slotLength steps) =
    [History slotLength steps' | steps' <- shrinkList shrinkStep steps]
   where
    shrinkStep (Step dropped gap size delay) =
      [Step dropped' gap size delay | dropped' <- shrink dropped]
        ++ [Step dropped gap' size delay | gap' <- shrink gap, gap' > 0]
        ++ [Step dropped gap size' delay | size' <- shrink size]
        ++ [Step dropped gap size delay' | delay' <- shrink delay]

-- | What a replay leaves: the counter, the oracle, the last slot considered
-- and the final chain with its ages.
data Replay = Replay Int Int (WithOrigin SlotNo) [(SlotNo, NominalDiffTime, Int)]

-- | Replay a history through 'adoptedBytes' and through the oracle. The clock
-- is the latest instant any block has been seen at, and a block's age is the
-- clock less its slot's onset.
replayHistory :: History -> Replay
replayHistory (History slotLength steps) = Replay got expected lastSlot aged
 where
  (got, expected, lastSlot, _, _, aged) = foldl' step (0, 0, Origin, Origin, 0, []) steps
  onset (SlotNo s) = fromIntegral s * slotLength
  -- (counter, oracle, last considered, newest ever adopted, clock, chain newest first with ages)
  step (n, o, considered, newest, clock, chain) (Step dropped gap size delay) =
    let keep = drop (fromIntegral dropped) chain
        tipSlot = case keep of
          (s, _, _) : _ -> unSlotNo s
          [] -> 0
        slot = SlotNo (tipSlot + fromIntegral gap)
        clock' = max clock (onset slot + fromIntegral delay)
        chain' = [(s, clock' - onset s, b) | (s, _, b) <- (slot, 0, size) : keep]
        (added, considered') = adoptedBytes window considered chain'
        counts = At slot > newest && clock' - onset slot <= window
     in (n + added, o + (if counts then size else 0), considered', max newest (At slot), clock', chain')

prop_replay :: History -> Property
prop_replay h =
  let Replay got expected _ _ = replayHistory h
   in counterexample (show (got, expected)) (got === expected)

prop_again :: History -> Property
prop_again h =
  let Replay _ _ lastSlot aged = replayHistory h
   in fst (adoptedBytes window lastSlot aged) === 0

-- | A fragment of this many blocks, anchored this many blocks into a chain
-- (zero: at genesis), so the anchor is sometimes a block and the fragment is
-- sometimes empty.
data Fragment = Fragment Int Int
  deriving (Eq, Show)

instance Arbitrary Fragment where
  arbitrary = do
    n <- choose (0, 60)
    k <- choose (0, 3)
    pure (Fragment n k)
  shrink (Fragment n k) =
    [Fragment n' k | n' <- shrink n] ++ [Fragment n k' | k' <- shrink k]

fragmentOf :: Fragment -> AF.AnchoredFragment TestBlock
fragmentOf (Fragment n k) =
  AF.fromOldestFirst anchor (take n rest)
 where
  chain = iterate successorBlock (firstBlock 0)
  (before, rest) = splitAt k chain
  anchor = case reverse before of
    [] -> AF.AnchorGenesis
    b : _ -> AF.anchorFromBlock b

-- | The kernel walks the current chain newest first and stops at the first
-- block past the window; the walk must list exactly what 'AF.toNewestFirst'
-- does, tip first.
prop_newestFirst :: Fragment -> Property
prop_newestFirst f =
  newestFirst af === AF.toNewestFirst af
 where
  af = fragmentOf f
