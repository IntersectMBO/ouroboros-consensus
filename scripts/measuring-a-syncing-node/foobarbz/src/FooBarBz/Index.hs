{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module FooBarBz.Index (readSortedEventLogFile) where

import qualified Data.ByteString.Lazy as BSL
import           Data.List (sortOn)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import           Data.Vector.Binary ()
import qualified GHC.Generics as GHC
import           GHC.RTS.Events
import           GHC.RTS.Events.Incremental (readHeader, readEvents')
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withTempDirectory)

deriving instance GHC.Generic CapsetType
deriving instance GHC.Generic Event
deriving instance GHC.Generic EventInfo
--deriving instance GHC.Generic HeapProfBreakdown
--deriving instance GHC.Generic HeapProfFlags
--deriving instance GHC.Generic KernelThreadId
deriving instance GHC.Generic MessageTag
deriving instance GHC.Generic ThreadStopStatus

instance Binary.Binary CapsetType
instance Binary.Binary Event
instance Binary.Binary EventInfo
--instance Binary.Binary HeapProfBreakdown
--instance Binary.Binary HeapProfFlags
--instance Binary.Binary KernelThreadId
instance Binary.Binary MessageTag
instance Binary.Binary ThreadStopStatus

eventsPerFD :: Int
eventsPerFD = 10 * 1000 * 1000

streamEventLog :: FilePath -> IO (Header, [Either String Event])
streamEventLog path = do
    (hdr, datbytes) <- BSL.readFile path >>= (either fail pure . readHeader)
    pure (hdr, readEvents' hdr datbytes)

readSortedEventLogFile :: (Event -> Bool) -> FilePath -> IO EventLog
readSortedEventLogFile keep path = do
    curd <- getCurrentDirectory
    withTempDirectory curd "FooBarBz" $ \tmpd -> do
        (hdr, evs) <- streamEventLog path

        k <- writeChunks tmpd 0 $ filter (either (\_ -> True) keep) evs

        evss <- flip mapM [0 .. k - 1] $ \i -> do
            myDecoder <$> BSL.readFile (tmpd </> show i)

        pure $ EventLog hdr $ Data $ foldr merge [] evss

myDecoder :: BSL.ByteString -> [Event]
myDecoder =
    \bs -> case Binary.runGetOrFail Binary.get bs of
        Left  (_, _, s)   -> error s
        Right (bs', _, n) -> go n bs'
  where
    go n bs = if (0 :: Int) == n then [] else
        case Binary.runGetOrFail Binary.get bs of
            Left  (_, _, s)    -> error s
            Right (bs', _, ev) -> ev : go (n - 1) bs'

writeChunks ::
     FilePath
  -> Int
  -> [Either String Event]
  -> IO Int
writeChunks tmpd !acc evs =
    if null evs then pure acc else do
        now' <- traverse (either fail pure) now

        BSL.writeFile (tmpd </> show acc)
          $ Binary.encode
          $ sortOn evTime
          $ now'
        
        writeChunks tmpd (acc + 1) later
  where
    (now, later) = splitAt eventsPerFD evs

merge :: [Event] -> [Event] -> [Event]
merge = curry $ \case
    ([], ys) -> ys
    (xs, []) -> xs

    (x:xs, y:ys) -> merge2 x xs y ys

mergeY :: Event -> [Event] -> [Event] -> [Event]
mergeY x xs ys = case ys of
    []    -> x:xs
    y:ys' -> merge2 x xs y ys'

mergeX :: [Event] -> Event -> [Event] -> [Event]
mergeX xs y ys = case xs of
    []    -> y:ys
    x:xs' -> merge2 x xs' y ys

merge2 :: Event -> [Event] -> Event -> [Event] -> [Event]
merge2 x xs y ys =
  if   evTime x <= evTime y
  then x : mergeX xs y ys
  else y : mergeY x xs ys
