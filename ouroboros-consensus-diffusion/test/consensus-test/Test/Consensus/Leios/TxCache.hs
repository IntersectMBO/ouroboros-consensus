{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.TxCache (tests) where

import Cardano.Crypto.Hash (castHash)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (forkIO, killThread, yield)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Ouroboros.Consensus.Leios.TxCache
import Ouroboros.Consensus.Leios.TxCache.Internal
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Util.IOLike (atomically, modifyTVar, readTVarIO, uncheckedNewTVarM)
import qualified System.Timeout as Timeout
import qualified Test.Consensus.Leios.TxCache.Optimized.MutableHashTable as Table
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

cfg :: CacheConfig
cfg = CacheConfig 2 16

tx :: Int -> TxHash
tx = hashLeiosTx . MkLeiosTx . BS.pack . show

body :: Int -> EndorserBlock
body n = mkEndorserBlock [(tx n, 100), (tx (n + 1), 100)]

point :: Int -> LeiosPoint
point n = MkLeiosPoint (SlotNo (fromIntegral n)) (endorserBlockHash (body n))

announce :: LeiosTxCache IO -> Int -> IO ()
announce h n =
  recordAnnouncement
    h
    (pointSlotNo (point n))
    (MkRankingBlockHash (castHash (tx n)))
    (pointEbHash (point n))

track :: LeiosTxCache IO -> Int -> IO ()
track h n = trackDurableReferences h (pointEbHash (point n)) (packReferences (body n))

publish :: LeiosTxCache IO -> Int -> IO ()
publish h n = publishConfirmed h (point n) [(0, tx n), (1, tx (n + 1))]

found :: LeiosTxCache IO -> Int -> IO (Maybe TxLocation)
found h n = lookupLocations h [(0, tx n)] >>= pure . snd . head . snd

backends :: [(String, IO (LeiosTxCache IO))]
backends = [("reference", newReferenceCache cfg), ("optimized", newOptimizedCache cfg 73 19)]

tests :: TestTree
tests =
  testGroup
    "TxCache"
    [ testCase "default cache resource pilot" $ do
        handle <- newOptimizedCache defaultCacheConfig 73 19
        stats <- readCacheStats handle
        cacheArrayBytes stats @?= 168296448
        cacheTransactionCount stats @?= 0
    , Table.tests
    , testCase "trusted geometry rejects overflow and undersized tables" $ do
        validateCacheConfig defaultCacheConfig @?= Right ()
        assertBool "count" (isLeft (validateCacheConfig (CacheConfig 129 22)))
        assertBool "shift" (isLeft (validateCacheConfig (CacheConfig 1 maxBound)))
        assertBool "capacity" (isLeft (validateCacheConfig (CacheConfig 128 6)))
    , testCase "compact references preserve order/hash/size" $
        foldReferences (\xs off h n -> xs ++ [(off, h, n)]) [] (packReferences (body 1))
          @?= [(0, tx 1, 100), (1, tx 2, 100)]
    , testProperty "representations agree under announcement/source/prune churn" $ \(ops :: [Word8]) -> ioProperty $ do
        a <- newReferenceCache cfg
        b <- newOptimizedCache cfg 73 19
        left <- mapM (step a) ops
        right <- mapM (step b) ops
        pure (left === right)
    , testCase "R09 interruption cannot strand cache lock or leak partial refcounts" interrupted
    , testGroup "policy" [testGroup name (policy create) | (name, create) <- backends]
    ]
 where
  isLeft (Left _) = True
  isLeft _ = False
  step h op = do
    let n = 1 + fromIntegral op `mod` 5
    case op `mod` 5 of
      0 -> announce h n
      1 -> track h n
      2 -> publish h n
      3 -> pruneTo h (SlotNo (fromIntegral n))
      _ -> invalidateLocationIfMatches h (tx n) (MkTxLocation (point n) 0)
    (epoch, locations) <- lookupLocations h [(i, tx i) | i <- [1 .. 6]]
    stats <- readCacheStats h
    pure
      (epoch, locations, cacheAnnouncementCount stats, cacheBodyCount stats, cacheTransactionCount stats)

policy :: IO (LeiosTxCache IO) -> [TestTree]
policy create =
  [ testCase "repeated references contribute one refcount per body" $ do
      h <- create
      let b = mkEndorserBlock (replicate 300 (tx 1, 100))
      recordAnnouncement h (SlotNo 1) (MkRankingBlockHash (castHash (tx 1))) (endorserBlockHash b)
      trackDurableReferences h (endorserBlockHash b) (packReferences b)
      cacheTransactionCount <$> readCacheStats h >>= (@?= 1)
      pruneTo h (SlotNo 2)
      cacheTransactionCount <$> readCacheStats h >>= (@?= 0)
  , testCase "hint appears only after confirmation, duplicate updates are idempotent" $ do
      h <- create
      announce h 1
      track h 1
      found h 1 >>= (@?= Nothing)
      publish h 1
      found h 1 >>= (@?= Just (MkTxLocation (point 1) 0))
      epoch <- atomically (cacheSourceEpoch h)
      announce h 1
      track h 1
      publish h 1
      atomically (cacheSourceEpoch h) >>= (@?= epoch)
  , testCase "R10 stale invalidation preserves newer location" $ do
      h <- create
      announce h 1
      track h 1
      publish h 1
      announce h 2
      track h 2
      publish h 2
      invalidateLocationIfMatches h (tx 2) (MkTxLocation (point 1) 1)
      found h 2 >>= (@?= Just (MkTxLocation (point 2) 0))
  , testCase "R10 prune frontier rejects late publication" $ do
      h <- create
      announce h 1
      track h 1
      publish h 1
      announce h 2
      track h 2
      pruneTo h (SlotNo 2)
      publish h 1
      announce h 1
      track h 1
      found h 2 >>= (@?= Nothing)
      publish h 2
      found h 2 >>= (@?= Just (MkTxLocation (point 2) 0))
      pruneTo h (SlotNo 1)
      found h 2 >>= (@?= Just (MkTxLocation (point 2) 0))
  , testCase "body eviction releases references while shared TX survives" $ do
      h <- create
      announce h 1
      track h 1
      publish h 1
      announce h 2
      track h 2
      publish h 2
      announce h 3
      track h 3
      found h 1 >>= (@?= Nothing)
      found h 2 >>= (@?= Just (MkTxLocation (point 2) 0))
      cacheTransactionCount <$> readCacheStats h >>= (@?= 3)
      pruneTo h (SlotNo 4)
      cacheTransactionCount <$> readCacheStats h >>= (@?= 0)
  , testCase "repeated batches retain a single source-ring slot" $ do
      h <- create
      announce h 1
      track h 1
      forM_ [1 .. 10 :: Int] (const (publish h 1))
      found h 1 >>= (@?= Just (MkTxLocation (point 1) 0))
      announce h 2
      track h 2
      publish h 2
      found h 1 >>= (@?= Just (MkTxLocation (point 1) 0))
  , testCase "R01 cache invalidation advances coherent source epoch" $ do
      h <- create
      announce h 1
      track h 1
      publish h 1
      before <- atomically (cacheSourceEpoch h)
      invalidateLocationIfMatches h (tx 1) (MkTxLocation (point 1) 0)
      (after, locations) <- lookupLocations h [(0, tx 1)]
      assertBool "source changed" (after > before)
      locations @?= [(0, Nothing)]
  ]

-- The internal table seam supplies an explicit scheduling barrier inside the
-- same masked update used by the optimized backend. No blocking callback is
-- inserted under the production cache lock.
interrupted :: IO ()
interrupted = do
  table <- uncheckedNewTVarM Map.empty
  inserts <- uncheckedNewTVarM (0 :: Int)
  entered <- newEmptyMVar
  done <- newEmptyMVar
  let insert h w = do
        atomically $ modifyTVar table (Map.insert h w) >> modifyTVar inserts (+ 1)
        n <- readTVarIO inserts
        when (n == 50) $ void (tryPutMVar entered ()) >> yield
  h <-
    newCache
      cfg
      TxTable
        { tableLookup = \key -> Map.lookup key <$> readTVarIO table
        , tableInsert = insert
        , tableDelete = \key -> atomically $ modifyTVar table (Map.delete key)
        , tableSize = Map.size <$> readTVarIO table
        , tableArrayBytes = 0
        }
  let b = mkEndorserBlock [(tx n, 100) | n <- [1 .. 100]]
  recordAnnouncement h (SlotNo 1) (MkRankingBlockHash (castHash (tx 0))) (endorserBlockHash b)
  worker <-
    forkIO $
      trackDurableReferences h (endorserBlockHash b) (packReferences b)
        `finally` putMVar done ()
  takeMVar entered
  killThread worker
  takeMVar done
  result <- Timeout.timeout 1000000 $ do
    trackDurableReferences h (endorserBlockHash b) (packReferences b)
    cacheTransactionCount <$> readCacheStats h >>= (@?= 100)
    pruneTo h (SlotNo 2)
    cacheTransactionCount <$> readCacheStats h >>= (@?= 0)
  result @?= Just ()
