{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A pretty-printer and tracer that shows the current peer simulator state in
-- a block tree, highlighting the candidate fragments, selection, and forks in
-- different colors, omitting uninteresting segments.
module Test.Consensus.PeerSimulator.StateDiagram (
    PeerSimState (..)
  , RenderConfig (..)
  , defaultRenderConfig
  , peerSimStateDiagram
  , peerSimStateDiagramSTMTracer
  , peerSimStateDiagramSTMTracerDebug
  , peerSimStateDiagramTracer
  , peerSimStateDiagramWith
  ) where

import           Cardano.Slotting.Block (BlockNo (BlockNo))
import           Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin (..),
                     fromWithOrigin, withOrigin)
import           Control.Monad (guard)
import           Control.Monad.State.Strict (State, gets, modify', runState,
                     state)
import           Control.Tracer (Tracer (Tracer), debugTracer, traceWith)
import           Data.Bifunctor (first)
import           Data.Foldable (foldl', foldr')
import           Data.List (find, intersperse, mapAccumL, sort, transpose)
import           Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.String (IsString (fromString))
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import           Data.Word (Word64)
import qualified Debug.Trace as Debug
import           GHC.Exts (IsList (..))
import           Ouroboros.Consensus.Block (ChainHash (BlockHash), Header,
                     WithOrigin (NotOrigin), blockHash, blockNo, blockSlot,
                     getHeader)
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM),
                     atomically, modifyTVar, readTVar, uncheckedNewTVarM)
import           Ouroboros.Network.AnchoredFragment (anchor, anchorToSlotNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HeaderHash)
import           Test.Consensus.BlockTree (BlockTree (btBranches, btTrunk),
                     BlockTreeBranch (btbSuffix), prettyBlockTree)
import           Test.Consensus.PointSchedule.NodeState (NodeState (..),
                     genesisNodeState)
import           Test.Consensus.PointSchedule.Peers (PeerId (..))
import           Test.Util.TestBlock (TestBlock, TestHash (TestHash))

enableDebug :: Bool
enableDebug = False

debugRender :: String -> a -> a
debugRender
  | enableDebug
  = Debug.trace
  | otherwise
  = const id

----------------------------------------------------------------------------------------------------
-- Colors
----------------------------------------------------------------------------------------------------

data SGR =
  Color Word64
  |
  BgColor Word64
  |
  Bold
  |
  Reset
  |
  Keep

renderSgr :: [SGR] -> String
renderSgr =
  foldMap $ \case
    Color n -> sgr ("38;5;" ++ show n)
    BgColor n -> sgr ("48;5;" ++ show n)
    Bold -> sgr "1"
    Reset -> sgr "0"
    Keep -> ""
  where
    sgr x = "\ESC[" ++ x ++ "m"

data Col =
  ColAspect (NonEmpty Aspect) Col
  |
  Col [SGR] Col
  |
  ColString String
  |
  ColCat [Col]

instance IsString Col where
  fromString = ColString

instance IsList Col where
  type Item Col = Col
  fromList = ColCat
  toList = \case
    ColCat cols -> cols
    c -> [c]

instance Semigroup Col where
  l <> r = ColCat [l, r]

instance Monoid Col where
  mempty = ""

colLength :: Col -> Int
colLength = \case
  ColAspect _ c -> colLength c
  Col _ c -> colLength c
  ColString s -> length s
  ColCat cs -> sum (colLength <$> cs)

data Colors =
  Colors {
    candidates :: [Word64],
    selection  :: Maybe Word64,
    slotNumber :: Word64,
    cache      :: Map PeerId Word64,
    stack      :: [[SGR]]
  }

candidateColor :: PeerId -> Colors -> (Maybe Word64, Colors)
candidateColor pid s@Colors {candidates, cache}
  | Just c <- cached
  = (Just c, s)

  | h : t <- filter unused candidates
  = (Just h, s {candidates = t, cache = Map.insert pid h cache})

  | otherwise
  = (Nothing, s)
  where
    cached = cache !? pid
    unused c = not (elem c cache)

getColor :: Bool -> Aspect -> State Colors (Maybe [SGR])
getColor bg = \case
  Selection -> do
    c <- gets selection
    pure (Just (Bold : maybe [] (pure . mkColor) c))
  Candidate pid ->
    peerColor pid
  Fork -> pure Nothing
  SlotNumber -> do
    c <- gets slotNumber
    pure (Just [mkColor c])
  TipPoint pid ->
    peerColor pid
  where
    peerColor pid =
      fmap (pure . mkColor) <$> state (candidateColor pid)
    mkColor | bg = BgColor
            | otherwise = Color

getColors :: NonEmpty Aspect -> State Colors [SGR]
getColors aspects = do
  (main, rest) <- findColor False (NonEmpty.toList aspects)
  (bg, _) <- findColor True rest
  pure (main ++ bg)
  where
    findColor bg (h : t) =
      getColor bg h >>= \case
        Just c -> pure (c, t)
        Nothing -> findColor bg t
    findColor _ [] = pure ([], [])

renderCol :: Col -> State Colors String
renderCol col =
  spin col
  where
    spin = \case
      ColAspect aspects sub -> do
        sgr <- getColors aspects
        withSgr sgr sub
      Col sgr sub ->
        withSgr sgr sub
      ColString s -> pure s
      ColCat cols -> concat <$> traverse spin cols

    withSgr sgr sub = do
      pre <- push sgr
      s <- spin sub
      pop
      pure (renderSgr sgr ++ s ++ renderSgr pre)

    push sgr =
      state $ \case
        s@Colors {stack = []} -> ([Reset], s {stack = [sgr, [Reset]]})
        s@Colors {stack = h : t} -> ([Reset], s {stack = sgr : h : t})

    pop = modify' $ \ s@Colors {stack} -> s {stack = drop 1 stack}

runCol :: [Word64] -> Maybe Word64 -> Word64 -> Map PeerId Word64 -> State Colors a -> (a, Colors)
runCol cand selection slotNumber cache s =
  runState s Colors {candidates = cand, selection, slotNumber, cache, stack = []}

----------------------------------------------------------------------------------------------------
-- Slots
----------------------------------------------------------------------------------------------------

slotInt :: SlotNo -> Int
slotInt (SlotNo s) = fromIntegral s

blockInt :: BlockNo -> Int
blockInt (BlockNo s) = fromIntegral s

data Range =
  Range {
    from :: Int,
    to   :: Int
  }
  deriving (Show, Eq, Ord)

instance Condense Range where
  condense (Range from to) = "[" ++ condense from ++ "," ++ condense to ++ "]"

data Aspect =
  Fork
  |
  Selection
  |
  Candidate PeerId
  |
  SlotNumber
  |
  TipPoint PeerId
  deriving (Eq, Show, Ord)

instance Condense Aspect where
  condense = \case
    Selection -> "s"
    Candidate _ -> "c"
    Fork -> "f"
    SlotNumber -> "n"
    TipPoint _ -> "t"

data AspectEdge =
  EdgeLeft
  |
  EdgeRight
  |
  NoEdge
  deriving (Show)

data SlotAspect =
  SlotAspect {
    slotAspect :: Aspect,
    edge       :: AspectEdge
  }
  deriving (Show)

data SlotCapacity =
  SlotOutside
  |
  SlotBlock Int
  |
  SlotEmpty
  deriving (Show)

instance Condense SlotCapacity where
  condense = \case
    SlotOutside -> ""
    SlotBlock n -> "-" ++ condense n
    SlotEmpty -> ""

data Slot =
  Slot {
    num      :: WithOrigin Int,
    capacity :: SlotCapacity,
    aspects  :: [SlotAspect]
  }
  deriving (Show)

instance Condense Slot where
  condense Slot {num, capacity} =
    sn ++ condense capacity
    where
      sn = case num of
        Origin -> "G"
        At n   -> show n

----------------------------------------------------------------------------------------------------
-- Slots vectors
----------------------------------------------------------------------------------------------------

data BranchSlots =
  BranchSlots {
    frag   :: AF.AnchoredFragment (Header TestBlock),
    slots  :: Vector Slot,
    cands  :: [PeerId],
    forkNo :: Word64
  }
  deriving (Show)

addAspect :: Aspect -> Range -> Bool -> Vector Slot -> Vector Slot
addAspect slotAspect (Range l u) overFork slots =
  debugRender (show (l, u, slotAspect)) $
  debugRender (condense (Vector.toList (ins <$> sub))) $
  Vector.update slots (ins <$> sub)
  where
    ins (i, slot) =
      (i, slot {aspects = newAspect : aspects slot})
      where
        newAspect = SlotAspect {slotAspect, edge = mkEdge i}

    mkEdge i | i == l && not overFork = EdgeLeft
             | i == u = EdgeRight
             | otherwise = NoEdge

    sub = Vector.slice l count (Vector.indexed slots)

    count = u - l + 1

initSlots :: Int -> Range -> AF.AnchoredFragment TestBlock -> Vector Slot
initSlots lastSlot (Range l u) blocks =
  Vector.fromList (snd (mapAccumL step (AF.toOldestFirst blocks) [-1 .. lastSlot]))
  where
    step bs cur
      | cur == -1
      = (bs, Slot {num = Origin, capacity = SlotOutside, aspects = []})

      | b : rest <- bs
      , s <- slotInt (blockSlot b)
      , s == cur
      = (rest, mkSlot cur (SlotBlock (blockInt (blockNo b))))

      | cur >= l - 1 && cur <= u
      = (bs, mkSlot cur SlotEmpty)

      | otherwise
      = (bs, mkSlot cur SlotOutside)

    mkSlot num capacity =
      Slot {num = At num, capacity, aspects = []}

hashForkNo :: HeaderHash TestBlock -> Word64
hashForkNo (TestHash h) =
  fromMaybe 0 (find (/= 0) h)

blockForkNo :: ChainHash TestBlock -> Word64
blockForkNo = \case
  BlockHash h -> hashForkNo h
  _ -> 0

initBranch :: Int -> Range -> AF.AnchoredFragment TestBlock -> BranchSlots
initBranch lastSlot fragRange fragment =
  BranchSlots {
    frag = AF.mapAnchoredFragment getHeader fragment,
    slots = initSlots lastSlot fragRange fragment,
    cands = [],
    forkNo = blockForkNo (AF.headHash fragment)
  }

data TreeSlots =
  TreeSlots {
    lastSlot :: Int,
    branches :: [BranchSlots]
  }
  deriving (Show)

initTree :: BlockTree TestBlock -> TreeSlots
initTree blockTree =
  TreeSlots {lastSlot, branches = trunk : branches}
  where
    trunk = initFR trunkRange

    branches = initFR <$> branchRanges

    initFR = uncurry (initBranch lastSlot)

    lastSlot = foldr' (max . (to . fst)) 0 (trunkRange : branchRanges)

    trunkRange = withRange (btTrunk blockTree)

    branchRanges = withRange . btbSuffix <$> btBranches blockTree

    withRange f = (mkRange f, f)

    mkRange f =
      Range l u
      where
        l = withOrigin 0 slotInt (AF.lastSlot f)
        u = withOrigin 0 slotInt (AF.headSlot f)

commonRange :: AF.AnchoredFragment (Header TestBlock) -> AF.AnchoredFragment (Header TestBlock) -> Maybe (Range, Bool)
commonRange branch segment = do
  (preB, preS, _, _) <- AF.intersect branch segment
  lower <- findLower (AF.toNewestFirst preB) (AF.toNewestFirst preS)
  upper <- eitherToMaybe (AF.head preB)
  let
    aB = anchor preB
    aS = anchor preS
    asB = anchorToSlotNo aB
    asS = anchorToSlotNo aS
    l = blockSlot lower
    u = blockSlot upper
    overFork = asS < asB && aB == anchor branch
  guard (u >= l)
  pure (Range (slotInt l + (if overFork then 0 else 1)) (slotInt u + 1), overFork)
  where
    findLower preB preS =
      foldl' step Nothing (zip preB preS)
    step prev (b1, b2) | b1 == b2 = Just b1
                       | otherwise = prev

addFragRange :: Aspect -> AF.AnchoredFragment (Header TestBlock) -> TreeSlots -> TreeSlots
addFragRange aspect selection TreeSlots {lastSlot, branches} =
  TreeSlots {lastSlot, branches = forBranch <$> branches}
  where
    forBranch branch@BranchSlots {frag, slots, cands} =
      case commonRange frag selection of
        Just (range, overFork) -> branch {slots = addAspect aspect range overFork slots, cands = addCandidate cands}
        _          -> branch

    addCandidate old | Candidate peerId <- aspect = peerId : old
                     | otherwise = old

addCandidateRange :: TreeSlots -> (PeerId, AF.AnchoredFragment (Header TestBlock)) -> TreeSlots
addCandidateRange treeSlots (pid, candidate) =
  addFragRange (Candidate pid) candidate treeSlots

updateSlot :: Int -> (Slot -> Slot) -> Vector Slot -> Vector Slot
updateSlot i f =
  Vector.modify (\ mv -> MV.modify mv f i)

addForks :: TreeSlots -> TreeSlots
addForks treeSlots@TreeSlots {branches} =
  treeSlots {branches = addFork <$> branches}
  where
    addFork fr@BranchSlots {frag, slots, forkNo}
      | forkNo == 0
      = fr
      | otherwise
      = fr {slots = updateSlot s update slots}
      where
        update slot =
          slot {
            capacity = SlotEmpty,
            aspects = SlotAspect {slotAspect = Fork, edge = NoEdge} : aspects slot
          }
        s = slotInt (withOrigin 0 (+ 1) (anchorToSlotNo (anchor frag)))

addTipPoint :: PeerId -> WithOrigin TestBlock -> TreeSlots -> TreeSlots
addTipPoint pid (NotOrigin b) TreeSlots {lastSlot, branches} =
  TreeSlots {lastSlot, branches = tryBranch <$> branches}
  where
    tryBranch branch@BranchSlots {forkNo, slots}
      | tipForkNo == forkNo
      = branch {slots = updateSlot (slotInt (blockSlot b + 1)) update slots}
      | otherwise
      = branch
      where
        update slot =
          slot {aspects = SlotAspect {slotAspect = TipPoint pid, edge = NoEdge} : aspects slot}

    tipForkNo = hashForkNo (blockHash b)

addTipPoint _ _ treeSlots = treeSlots

addPoints :: Map PeerId (NodeState TestBlock) -> TreeSlots -> TreeSlots
addPoints peerPoints treeSlots =
  foldl' step treeSlots (Map.toList peerPoints)
  where
    step z (pid, ap) = addTipPoint pid (nsTip ap) z

----------------------------------------------------------------------------------------------------
-- Cells
----------------------------------------------------------------------------------------------------

data CellSort =
  CellHere (NonEmpty Aspect)
  |
  CellOther
  deriving (Show)

instance Condense CellSort where
  condense = \case
    CellHere a -> "h" ++ condense (toList a)
    CellOther -> "o"

data FragCell =
  FragCell {
    fcLabel       :: Maybe String,
    fcSort        :: CellSort,
    fcLineAspects :: [Aspect]
  }
  deriving (Show)

instance Condense FragCell where
  condense (FragCell l s a) =
    lb ++ " " ++ condense s ++ condense a
    where
      lb = case l of
        Just x  -> x
        Nothing -> "-"

data Cell =
  Cell FragCell
  |
  CellEmpty
  |
  CellSlotNo (WithOrigin Int)
  |
  CellPeers [PeerId]
  deriving (Show)

instance Condense Cell where
  condense = \case
    Cell c -> condense c
    CellEmpty -> "E"
    CellSlotNo n -> "S" ++ show n
    CellPeers _ -> "L"

mainAspects :: [SlotAspect] -> Maybe (NonEmpty Aspect)
mainAspects =
  nonEmpty . sort . fmap slotAspect

lineAspects :: [SlotAspect] -> [Aspect]
lineAspects =
  sort . mapMaybe check
  where
    check SlotAspect {edge, slotAspect}
      | EdgeLeft <- edge
      = Nothing
      | otherwise
      = Just slotAspect

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList = \case
  [] -> id
  h : t -> ((h :| t) <>)

branchCells :: BranchSlots -> NonEmpty Cell
branchCells BranchSlots {cands, slots} =
  prependList (fragCell <$> Vector.toList slots) (pure peers)
  where
    fragCell Slot {capacity, aspects}
      | SlotOutside <- capacity
      = CellEmpty

      | otherwise
      , cellSort <- maybe CellOther CellHere (mainAspects aspects)
      = Cell (FragCell (content capacity) cellSort (lineAspects aspects))

    content = \case
      SlotBlock num -> Just (show num)
      _ -> Nothing

    peers = CellPeers cands

slotNoCells :: Int -> NonEmpty Cell
slotNoCells lastSlot =
  CellSlotNo Origin :| (CellSlotNo . At <$> [0 .. lastSlot]) ++ [CellEmpty]

treeCells :: TreeSlots -> NonEmpty (NonEmpty Cell)
treeCells TreeSlots {lastSlot, branches} =
  slotNoCells lastSlot :| (branchCells <$> branches)

----------------------------------------------------------------------------------------------------
-- Render cells
----------------------------------------------------------------------------------------------------

newtype SlotWidth =
  SlotWidth Int
  deriving (Eq, Show, Ord, Num)

data RenderSlot =
  SlotEllipsis Int
  |
  RenderSlot Int SlotWidth (NonEmpty Cell)
  deriving (Show)

data RenderCell =
  CellEllipsis
  |
  RenderCell SlotWidth Cell
  deriving (Show)

instance Condense RenderCell where
  condense = \case
    CellEllipsis -> " .. "
    RenderCell _ cell -> condense cell

slotWidth :: NonEmpty Cell -> SlotWidth
slotWidth =
  maximum . fmap cellWidth
  where
    cellWidth = \case
      Cell FragCell {fcLabel = Just label, fcSort} -> SlotWidth (length label) + sortWidth fcSort
      CellPeers peerIds -> SlotWidth (sum (labelWidth <$> peerIds))
      _ -> 1

    labelWidth pid = 2 + length (show pid)

    sortWidth = \case
      CellHere as -> sum (pointWidth <$> as)
      _ -> 0

    pointWidth = \case
      TipPoint _ -> 1
      _ -> 0

contiguous :: [(Int, Bool, a)] -> [[(Int, a)]]
contiguous ((i0, _, a0) : rest) =
  result (foldl' step (pure (i0, a0), []) rest)
  where
    result (cur, res) = reverse (reverse (toList cur) : res)

    step (cur@((prev, _) :| _), res) (i, force, a)
      | i == prev + 1 || force
      = ((i, a) <| cur, res)
      | otherwise
      = (pure (i, a), reverse (toList cur) : res)
contiguous [] = []

cellSlots :: Int -> [(Int, Bool, NonEmpty Cell)] -> [RenderSlot]
cellSlots branches =
  concat . intersperse [SlotEllipsis branches] . fmap (fmap (uncurry withMaxSize)) . contiguous
  where
    withMaxSize slot cells = RenderSlot slot (slotWidth cells) cells

pruneCells :: NonEmpty (NonEmpty Cell) -> [RenderSlot]
pruneCells branches =
  -- debugRender (unlines (condense <$> branches)) $
  cellSlots (length branches) (mapMaybe cellSlot (zip slotRange (NonEmpty.toList (NonEmpty.transpose branches))))
  where
    cellSlot :: (WithOrigin Int, NonEmpty Cell) -> Maybe (Int, Bool, NonEmpty Cell)
    cellSlot (num, frags)
      | let noEll = any forceNoEllipsis frags
      , noEll || any essential frags
      = keep num noEll frags
      | otherwise
      = Nothing

    keep num noEll frags = Just (fromWithOrigin (-1) num, noEll, frags)

    essential = \case
      Cell FragCell {fcSort = CellHere _} -> True
      _ -> False

    forceNoEllipsis = \case
      CellPeers _ -> True
      _ -> False

    slotRange = Origin : (At <$> [0 ..])

----------------------------------------------------------------------------------------------------
-- Render
----------------------------------------------------------------------------------------------------

data RenderConfig =
  RenderConfig {
    lineWidth       :: Int,
    ellipsis        :: String,
    slotDistance    :: Int,
    boringChar      :: Char,
    candidateChar   :: Char,
    selectionChar   :: Char,
    forkChar        :: Char,
    candidateColors :: [Word64],
    cachedPeers     :: Map PeerId Word64,
    selectionColor  :: Maybe Word64,
    slotNumberColor :: Word64
  }

padCell :: RenderConfig -> Char -> SlotWidth -> String -> String
padCell RenderConfig {slotDistance} padChar (SlotWidth w) s
  | pad <= 0 = s
  |otherwise = replicate pad padChar ++ s
  where
    pad = w - length s + slotDistance

lineChar :: RenderConfig -> [Aspect] -> Char
lineChar config aspects
  | elem Selection aspects
  = selectionChar config

  | any isCandidate aspects
  = candidateChar config

  | otherwise
  = boringChar config
  where
    isCandidate = \case
      Candidate _ -> True
      _ -> False

colorAspects :: [Aspect] -> [Aspect]
colorAspects =
  filter $ \case
    Fork -> False
    Selection -> False
    TipPoint _ -> False
    _ -> True

renderLine :: RenderConfig -> SlotWidth -> [Aspect] -> Int -> Col
renderLine config@RenderConfig {slotDistance, forkChar} (SlotWidth width) aspects labelWidth =
  case colorAspects aspects of
    [] -> ColString lineString
    colors -> ColCat [ColAspect (pure color) (ColString [c]) | (c, color) <- zip lineString (cycle colors)]
  where
    lineString | elem Fork aspects = replicate (lineWidth - 1) ' ' ++ [forkChar]
               | otherwise = replicate lineWidth lc
    lineWidth = max 0 (width - labelWidth + slotDistance)
    lc = lineChar config aspects

labelColor :: CellSort -> Maybe (NonEmpty Aspect)
labelColor = \case
  CellOther -> Nothing
  CellHere aspects ->
    nonEmpty (colorAspects (toList aspects))

renderSpecifiedLabel :: String -> CellSort -> Col
renderSpecifiedLabel label srt =
  case labelColor srt of
    Nothing -> text
    Just as -> ColAspect as text
  where
    text = ColString label

renderLabel :: Maybe String -> CellSort -> Col
renderLabel label srt
  | Just specified <- label
  = renderSpecifiedLabel specified srt
  | otherwise
  = ""

renderPoint :: CellSort -> Col
renderPoint = \case
  CellHere aspects ->
    mconcat (mapMaybe pointMarker (toList aspects))
  _ -> ""
  where
    pointMarker = \case
      TipPoint pid -> Just (ColAspect (pure (TipPoint pid)) "↑")
      _ -> Nothing

renderFragCell :: RenderConfig -> SlotWidth -> FragCell -> Col
renderFragCell config width FragCell {fcLabel, fcSort, fcLineAspects} =
  renderLine config width fcLineAspects (colLength label) <> label
  where
    label = renderLabel fcLabel fcSort <> renderPoint fcSort

renderSlotNo :: RenderConfig -> SlotWidth -> WithOrigin Int -> Col
renderSlotNo config width num =
  ColAspect (pure SlotNumber) (ColString (padCell config ' ' width label))
  where
    label = case num of
      Origin -> "G"
      At s   -> show s

renderPeers :: [PeerId] -> Col
renderPeers peers =
  ColCat [ColAspect (pure (Candidate p)) (ColString ("  " ++ show p)) | p <- peers]

renderCell :: RenderConfig -> RenderCell -> Col
renderCell config@RenderConfig {ellipsis} = \case
  RenderCell width (Cell cell) -> renderFragCell config width cell
  RenderCell width (CellEmpty) -> ColString (padCell config ' ' width "")
  RenderCell width (CellSlotNo n) -> renderSlotNo config width n
  RenderCell _ (CellPeers peers) -> renderPeers peers
  CellEllipsis -> ColString ellipsis

renderBranch :: RenderConfig -> [RenderCell] -> Col
renderBranch config cells =
  debugRender (condense cells) $
  foldMap (renderCell config) cells

-- | Use w + 2 because we want the effective width, which includes the line segment.
renderSlotWidth :: Int -> RenderSlot -> Int
renderSlotWidth ellipsisWidth = \case
  SlotEllipsis _ -> ellipsisWidth
  RenderSlot _ (SlotWidth w) _ -> w + 2

breakLines :: RenderConfig -> [RenderSlot] -> [[RenderSlot]]
breakLines RenderConfig {lineWidth, ellipsis} =
  result . foldl' step (0, [], [])
  where
    result (_, cur, res) = reverse (reverse cur : res)
    step (w, cur, res) slot
      | new <= lineWidth = (new, slot : cur, res)
      | otherwise = (curW, [slot], reverse cur : res)
      where
        new = w + curW
        curW = renderSlotWidth (length ellipsis) slot

renderCells :: [RenderSlot] -> [[RenderCell]]
renderCells =
  transpose . fmap toCells
  where
    toCells = \case
      RenderSlot _ width cells -> RenderCell width <$> toList cells
      SlotEllipsis n -> replicate n CellEllipsis

renderSlotSequence :: RenderConfig -> [RenderSlot] -> [Col]
renderSlotSequence config =
  fmap (renderBranch config) . renderCells

renderSlots :: RenderConfig -> [RenderSlot] -> [[Col]]
renderSlots config slots =
  renderSlotSequence config <$> breakLines config slots

renderColBlocks :: RenderConfig -> [[Col]] -> ([String], Colors)
renderColBlocks RenderConfig {candidateColors, selectionColor, slotNumberColor, cachedPeers} cols =
  first (fmap unlines) (runCol candidateColors selectionColor slotNumberColor cachedPeers (traverse (traverse renderCol) cols))

------------------------------------------------------------------------------------------------------
---- API
------------------------------------------------------------------------------------------------------

-- | All inputs for the state diagram printer.
data PeerSimState =
  PeerSimState {
    pssBlockTree  :: BlockTree TestBlock,
    pssSelection  :: AF.AnchoredFragment (Header TestBlock),
    pssCandidates :: Map PeerId (AF.AnchoredFragment (Header TestBlock)),
    pssPoints     :: Map PeerId (NodeState TestBlock)
  }

-- TODO add an aspect for the last block of each branch?

-- | Pretty-print the current peer simulator state in a block tree, highlighting
-- the candidate fragments, selection, and forks in different colors, omitting
-- uninteresting segments.
peerSimStateDiagramWith :: RenderConfig -> PeerSimState -> (String, Map PeerId Word64)
peerSimStateDiagramWith config PeerSimState {pssBlockTree, pssSelection, pssCandidates, pssPoints} =
  debugRender (unlines (prettyBlockTree pssBlockTree)) $
  (unlines blocks, cache)
  where
    (blocks, Colors {cache}) = renderColBlocks config (renderSlots config frags)

    frags =
      pruneCells $
      treeCells $
      addPoints pssPoints $
      addForks $
      flip (foldl' addCandidateRange) (Map.toList pssCandidates) $
      addFragRange Selection pssSelection $
      initTree pssBlockTree

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig {
    lineWidth = 80,
    ellipsis = " .. ",
    slotDistance = 2,
    boringChar = '·',
    candidateChar = '-',
    selectionChar = '*',
    forkChar = '`',
    candidateColors = [164, 113, 142, 81, 33],
    cachedPeers = mempty,
    selectionColor = Just 123,
    slotNumberColor = 166
  }

peerSimStateDiagram :: PeerSimState -> String
peerSimStateDiagram =
  fst . peerSimStateDiagramWith defaultRenderConfig

-- | Construct a tracer that prints the current peer simulator state in
-- a block tree, highlighting the candidate fragments, selection, and forks in
-- different colors, omitting uninteresting segments.
peerSimStateDiagramTracer ::
  Tracer m String ->
  Tracer m PeerSimState
peerSimStateDiagramTracer tracer =
  Tracer (traceWith tracer . peerSimStateDiagram)

-- | Construct a stateful tracer that prints the current peer simulator state in
-- a block tree, highlighting the candidate fragments, selection, and forks in
-- different colors, omitting uninteresting segments.
--
-- Since the tracer gets its input from concurrent state, it takes only a dummy
-- @()@ value as its argument.
peerSimStateDiagramSTMTracer ::
  IOLike m =>
  Tracer m String ->
  BlockTree TestBlock ->
  STM m (AF.AnchoredFragment (Header TestBlock)) ->
  STM m (Map PeerId (AF.AnchoredFragment (Header TestBlock))) ->
  STM m (Map PeerId (Maybe (NodeState TestBlock))) ->
  m (Tracer m ())
peerSimStateDiagramSTMTracer stringTracer pssBlockTree selectionVar candidatesVar pointsVar = do
  peerCache <- uncheckedNewTVarM mempty
  pure $ Tracer $ const $ do
    (s, cachedPeers) <- atomically $ do
      pssSelection <- selectionVar
      pssCandidates <- candidatesVar
      pssPoints <- fmap (fromMaybe genesisNodeState) <$> pointsVar
      cachedPeers <- readTVar peerCache
      pure (PeerSimState {pssBlockTree, pssSelection, pssCandidates, pssPoints}, cachedPeers)
    let (blocks, newPeers) = peerSimStateDiagramWith (defaultRenderConfig {cachedPeers}) s
    atomically (modifyTVar peerCache (newPeers <>))
    traceWith stringTracer blocks

-- | Construct a stateful tracer that prints the current peer simulator state in
-- a block tree, highlighting the candidate fragments, selection, and forks in
-- different colors, omitting uninteresting segments.
--
-- Since the tracer gets its input from concurrent state, it takes only a dummy
-- @()@ value as its argument.
--
-- This variant uses the global debug tracer.
peerSimStateDiagramSTMTracerDebug ::
  IOLike m =>
  BlockTree TestBlock ->
  STM m (AF.AnchoredFragment (Header TestBlock)) ->
  STM m (Map PeerId (AF.AnchoredFragment (Header TestBlock))) ->
  STM m (Map PeerId (Maybe (NodeState TestBlock))) ->
  m (Tracer m ())
peerSimStateDiagramSTMTracerDebug =
  peerSimStateDiagramSTMTracer debugTracer
