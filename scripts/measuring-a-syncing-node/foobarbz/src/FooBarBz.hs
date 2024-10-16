{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FooBarBz where

import           Control.Monad (when)
import           GHC.RTS.Events (Event, EventInfo (EndGC, StartGC, UserMessage), Timestamp, evSpec, evTime)
import qualified GHC.RTS.Events as EventLog
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           FooBarBz.Index (readSortedEventLogFile)

main :: IO ()
main = main1 `asTypeOf` dummy

-- evSpec = ThreadLabel {thread = thrd, threadLabel = "ChainDB.addBlockRunner"}, evCap = Just cap
-- evSpec = StopThread {thread = thrd, status = ...}, evCap = Just cap  (ThreadYielding, ForeignCall, BlockedOnSTM, StackOverflow, HeapOverflow, ...)
-- evSpec = CreateThread {thread = thrd}, evCap = Just cap
-- evSpec = RunThread {thread = thrd}, evCap = Just cap
-- evSpec = MigrateThread {thread = thrd, newCap = cap'}, evCap = Just cap

dummy :: IO ()
dummy = do
    Right evlog <- EventLog.readEventLogFromFile "cardano-node.eventlog"
    let go = \case
            EventLog.ThreadLabel x "ChainDB.addBlockRunner":_ -> pure x
            _:xs -> go xs
            [] -> fail "never saw ChainDB.addBlockRunner"
    thrd <- go $ map evSpec $ EventLog.events $ EventLog.dat evlog

    let keep ev = relevant ev && case evSpec ev of
            EventLog.RunThread {EventLog.thread = y} -> y == thrd
            EventLog.StopThread {EventLog.thread = y} -> y == thrd
            EventLog.MigrateThread {EventLog.thread = y} -> y == thrd
            _ -> False
    evlog' <- readSortedEventLogFile keep "cardano-node.eventlog"

    mapM_ print $ EventLog.events $ EventLog.dat evlog'

main1 :: IO ()
main1 = do
    Right evlog <- EventLog.readEventLogFromFile "cardano-node.eventlog"
    let go = \case
            EventLog.ThreadLabel x "ChainDB.addBlockRunner":_ -> pure x
            _:xs -> go xs
            [] -> fail "never saw ChainDB.addBlockRunner"
    thrd <- go $ map evSpec $ EventLog.events $ EventLog.dat evlog

    let keep ev = relevant ev && case evSpec ev of
            EventLog.RunThread {EventLog.thread = y} -> y == thrd
            EventLog.StopThread {EventLog.thread = y} -> y == thrd
            EventLog.MigrateThread {EventLog.thread = y} -> y == thrd
            EventLog.UserMessage{} -> True
            _ -> False
    evlog' <- readSortedEventLogFile keep "cardano-node.eventlog"

    process $ EventLog.events $ EventLog.dat evlog'

process :: [Event] -> IO ()
process evs =
    nextUser
        zero
        evs
        (before Nothing)

relevant :: Event -> Bool
relevant ev = case evSpec ev of

    EventLog.ThreadLabel{} -> True
    EventLog.RunThread{} -> True
    EventLog.StopThread{} -> True

    -- TODO maybe interesting to count?
    EventLog.MigrateThread{} -> False

    UserMessage "START garbage collection" -> False
    UserMessage "STOP garbage collection"  -> False

    UserMessage txt -> not $ Text.isPrefixOf "[LEDGER]" txt

    _ -> False

-----

-- | The cumulative GC duration as of some moment
--
-- The approach here is imperfect, but seems a worthwhile trade-off. The
-- assumption implicit in the logic of this module is that all threads
-- immediately stop upon the first 'StartGC' and that all threads immediately
-- resume upon the first 'EndGC'. That is fundamentally untrue, but the
-- analysis cannot be made more precise without including the thread scheduling
-- events in the eventlog, which we're assuming is prohibitively expensive.
newtype CumuGc = CumuGc Word64
  deriving (Show)

plusGc :: CumuGc -> Timestamp -> Timestamp -> CumuGc
plusGc (CumuGc x) t t' = CumuGc $ x + t' - t

diffGc :: CumuGc -> CumuGc -> Word64
diffGc (CumuGc x) (CumuGc y) = y - x

-- | When an event happened and the corresponding 'CumuGC'
data Moment = Moment !CumuGc !Timestamp
  deriving (Show)

zero :: Moment
zero = Moment (CumuGc 0) 0

nonzero :: Moment -> Bool
nonzero = \case
    Moment (CumuGc 0) 0 -> False
    _                   -> True

-----

nextUser ::
     Moment
  -> [Event]
  -> (Moment -> [Event] -> Text -> IO ())
  -> IO ()
nextUser (Moment gc _t) = nextUser_ gc

nextUser_ ::
     CumuGc
  -> [Event]
  -> (Moment -> [Event] -> Text -> IO ())
     -- ^ how to process the next UserMessage event
  -> IO ()
nextUser_ gc = \case
    []     -> \_k -> pure ()
    ev:evs -> \k  -> nextUser1 gc evs ev k

isPause :: EventLog.ThreadStopStatus -> Bool
isPause = \case
    EventLog.BlockedOnSTM{} -> True
    EventLog.HeapOverflow{} -> True
    EventLog.StackOverflow{} -> True
    EventLog.ThreadYielding{} -> True

    EventLog.ThreadFinished{} -> False

    -- Both of these delineate durations that are attributable to ChainSel
    -- itself. If the blackhole overhead is common and high, a redesign could
    -- likely attenuate it.
    EventLog.BlockedOnBlackHoleOwnedBy{} -> False
    EventLog.ForeignCall{} -> False

    x -> error $ "unexpected ThreadStopStatus: " <> show x

skipPause :: [Event] -> (Timestamp, [Event])
skipPause = \case
    [] -> error "a pause didn't finish!"

    ev:evs

      | EventLog.RunThread{} <- evSpec ev -> (evTime ev, evs)

      | otherwise -> skipPause evs

-- | This function works for _either_ @-l-as@ or @-l-agu@, but is wrong for
-- @-l-asgu@.
nextUser1 ::
     CumuGc
  -> [Event]
  -> Event
  -> (Moment -> [Event] -> Text -> IO ())
  -> IO ()
nextUser1 gc evs ev k

  | EventLog.StopThread {EventLog.status = status} <- evSpec ev
  =
    if not (isPause status) then nextUser_ gc evs k else

    let t          = evTime ev
        (t', evs') = skipPause evs
    in
    nextUser_
        (plusGc gc t t')
        evs'
        k

  | StartGC <- evSpec ev
  =
    let t          = evTime ev
        (t', evs') = skipGc id evs
    in
    nextUser_
        (plusGc gc t t')
        evs'
        k

  | UserMessage txt <- evSpec ev
  =
    k (Moment gc (evTime ev)) evs txt

  -- handled by the StopThread case
  | EventLog.RunThread{} <- evSpec ev
  =
    nextUser_ gc evs k

  -- handled by the StartGC case
  | EventLog.EndGC{} <- evSpec ev
  =
    nextUser_ gc evs k

  | otherwise
  =
    error $ "nextUser1: " <> show ev

skipGc :: ([Event] -> [Event]) -> [Event] -> (Timestamp, [Event])
skipGc acc = \case
    [] -> error "a GC didn't finish!"

    -- assume all threads have restarted once the /first/ EndGC is observed
    --
    -- It's possible that the thread executing some interval of interest didn't
    -- actually restart yet, so the interval times might be /slightly/
    -- inflated.
    ev:evs

      | EndGC{} <- evSpec ev -> (evTime ev, acc evs)

      -- occasionally, a thread emits a UserMessage after a different thread
      -- emits StartGC but before any EndGCs
      | otherwise -> skipGc (acc . (ev:)) evs

-----

before :: Maybe (Interval, Text, Moment) -> Moment -> [Event] -> Text -> IO ()
before mbIncoming st evs = \case

    "Q" -> case findPoppedSlot evs of
        Nothing -> pure ()
        Just s  -> do
            case mbIncoming of
                Nothing                   -> pure ()
                Just (current, s', start) -> doEnd current s' start st
            nextUser st evs $ during st Pop s st

    txt | Text.isPrefixOf "A" txt -> do
        putStrLn $ "already " <> Text.unpack (Text.drop 1 txt)
        nextUser st evs $ before mbIncoming

    txt ->
        error $ "before: " <> context <> Text.unpack txt
      where
        context = case mbIncoming of
            Nothing              -> ""
            Just (current, s, _) -> show current <> "@" <> Text.unpack s <> " "

during :: Moment -> Interval -> Text -> Moment -> Moment -> [Event] -> Text -> IO ()
during start_ChainSel current s start st evs = \case

    txt | Text.isPrefixOf "Q" txt, "Q" /= txt -> do
        let s' = Text.drop 1 txt
        when (s /= s') $ fail $ slotMismatchMessage s s'
        go Pop PopWrite

    txt | Text.isPrefixOf "A" txt -> do
        checkSkip PopWrite
        putStrLn $ "Xalready " <> Text.unpack (Text.drop 1 txt)
        nextUser st evs $ during zero Epilogue s start

    "<W" -> go PopWrite Write

    txt | Text.isPrefixOf "B" txt -> do
        checkSkip Write
        -- TODO
        nextUser st evs $ during start_ChainSel Write s start

    ">W" -> go Write WriteTry

    txt | Text.isPrefixOf "X" txt -> fail $ Text.unpack txt

    "T"  -> go WriteTry    TryPipe
    "<P" -> go TryPipe     Pipe
    ">P" -> go Pipe        PipeValid        
    "V"  -> go PipeValid   ValidSelect
    "<S" -> go ValidSelect Select
    ">S" -> go Select      Epilogue

    "R" -> do
        checkSkip Epilogue
        if not (nonzero start_ChainSel)
        then nextUser st evs $ before Nothing
        else do
            doEnd ChainSel s start_ChainSel st
            doEnd current s start st
            nextUser st evs $ before (Just (BetweenSamples, s, st))

    txt -> fail $ "during: " <> Text.unpack txt <> "@" <> Text.unpack s <> " " <> show st

  where
    go x y = do
        checkSkip x
        doEnd current s start st
        nextUser st evs $ during start_ChainSel y s st

    checkSkip x = when (x /= current) $ fail $ skippedMessage x current s

doEnd :: Interval -> Text -> Moment -> Moment -> IO ()
doEnd current s (Moment gc t) (Moment gc' t') = do
    putStrLn $ "END " <> Text.unpack (pp current <> "@" <> s) <> " " <> show (t' - t - diffGc gc gc')

slotMismatchMessage :: Text -> Text -> String
slotMismatchMessage s s' =
    "slot mismatch: expecting " <> Text.unpack s <> ", saw " <> Text.unpack s'

skippedMessage :: Interval -> Interval -> Text -> String
skippedMessage x current s =
    "skipped from " <> show current <> " to " <> show x <> " for " <> Text.unpack s

bailMessage :: Text -> Interval -> Text -> String
bailMessage x current s =
    "bailed out from " <> show current <> " to " <> Text.unpack x <> " for " <> Text.unpack s

capcapMessage :: Text -> String
capcapMessage s =
    "multiple caps for" <> Text.unpack s

-----
    
findPoppedSlot :: [Event] -> Maybe Text
findPoppedSlot = \case

    [] -> Nothing

    ev:evs
      | UserMessage txt <- evSpec ev
      , Text.isPrefixOf "Q" txt
      , "Q" /= txt
      -> Just $ Text.drop 1 txt

      | otherwise -> findPoppedSlot evs
-----

data Interval =
    ChainSel | Pop | PopWrite | Write | WriteTry | TryPipe | Pipe | PipeValid | ValidSelect | Select | Epilogue | BetweenSamples
  deriving (Eq, Show)

pp :: Interval -> Text
pp = \case
    ChainSel -> "ChainSel"
    Pop -> "pop"
    PopWrite -> "pop-write"
    Write -> "write"
    WriteTry -> "write-try"
    TryPipe -> "try-pipe"
    Pipe -> "pipe"
    PipeValid -> "pipe-valid"
    ValidSelect -> "valid-select"
    Select -> "select"
    Epilogue -> "epilogue"
    BetweenSamples -> "between"
