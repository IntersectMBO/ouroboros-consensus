{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmark of 'validateEbClosure', the Leios voting thread's validation of
-- an EB's closure, against the real Cardano ledger in the Dijkstra era.
--
-- The closures are of designed shape, the two kinds the voters meet:
--
--   * __old-input__: every tx spends outputs that were in the UTxO set before
--     the closure, so the closure's resolved input table holds one entry per
--     input;
--   * __chained__: tx @n@ spends tx @n-1@'s outputs, so the table holds only
--     the first tx's inputs.
--
-- Every closure is validated twice against the same tx-cache: the first pass
-- applies every tx in full (the cache holds them unapplied, as a fetch leaves
-- them), the second reapplies every tx (the first pass tagged them). The
-- figures are the whole call's wall-clock time over the tx count, the best of
-- the repetitions.
--
-- @cabal run leios-voting-bench -- [--reps N] [SIZE ...]@, default sizes
-- 1000 3000 6000, three repetitions.
module Main (main) where

import qualified Cardano.Chain.Update as CC.Update
import Cardano.Ledger.Api
  ( Addr (..)
  , DijkstraEra
  , Tx
  , TxOut
  , eraProtVerLow
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , mkBasicTxOut
  , outputsTxBodyL
  , txIdTx
  )
import Cardano.Ledger.Api.Tx.In (TxIn (..))
import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer (..), TxIx (..), knownNonZeroBounded)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (TopTx)
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Val (inject)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Control.DeepSeq (force)
import Control.Monad (foldM, forM_, void, when)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.SOP.Index (Index (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import LeiosDemoTypes
  ( LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity
  , RbHash (..)
  , SerializedEbBody
  , encodeLeiosEbSize
  , hashLeiosEb
  , hashLeiosTx
  , serializeEbBody
  , serializeTxForLeios
  )
import LeiosTxCache
  ( LeiosTxCache (..)
  , defaultLeiosTxCacheShift
  , newPureLeiosTxCache
  )
import LeiosVoting (EbClosureVerdict (..), validateEbClosure)
import Lens.Micro ((.~))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras)
import Ouroboros.Consensus.Config (configLedger)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.HardFork.Combinator.Ledger (injectLedgerTables)
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables (LedgerTables (..), ValuesMK (..))
import Ouroboros.Consensus.Ledger.Tables.Combinators (ltliftA2)
import Ouroboros.Consensus.Ledger.Tables.MapKind (EmptyMK)
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables, restrictValuesMK)
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..), ProtocolInfo (..))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Protocol.PBFT (PBftSignatureThreshold (..))
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger (BigEndianTxIn (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbReader
  , LeiosDbWriter (..)
  , Promise (..)
  , newLeiosDBInMemory
  , withReader
  , withWriter
  )
import System.Environment (getArgs)
import System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as MockFS
import qualified System.FS.Sim.STM as Sim
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Mem (performMajorGC)
import Test.Consensus.Cardano.ProtocolInfo (Era (Dijkstra), hardForkInto, mkTestProtocolInfo)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import qualified Test.ThreadNet.Infra.Byron as Byron
import Test.ThreadNet.Infra.Shelley
  ( CoreNode (..)
  , DecentralizationParam (..)
  , genCoreNode
  , mkCredential
  , mkGenesisConfig
  , mkKesConfig
  , networkId
  , signTx
  )
import Test.Util.Slots (NumSlots (..))
import Text.Printf (printf)

type Blk = CardanoBlock StandardCrypto

type DijkstraBlock = ShelleyBlock (Praos2 StandardCrypto) DijkstraEra

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (reps, sizes) <- parseArgs <$> getArgs
  let coreNode = unGen (genCoreNode (KESPeriod 0)) (mkQCGen 0) 30
  pinfo <- mkProtocolInfo coreNode
  let lcfg = configLedger (pInfoConfig pinfo)
      base = forgetLedgerTables (ledgerState (pInfoInitLedger pinfo))
  putStrLn
    "| closure | inputs per tx | txs | table entries | apply: ms | per applied tx, us | reapply: ms | per reapplied tx, us |"
  putStrLn "|---|---|---|---|---|---|---|---|"
  forM_ sizes $ \n -> forM_ [OldInput, Chained] $ \kind -> forM_ [1, 2] $ \nIn -> do
    let (txs, funded) = buildClosure coreNode kind nIn n
        entries = Map.size funded
    results <- mapM (\_ -> runOnce lcfg base funded txs) [1 .. reps]
    let (applyNs, reapplyNs) = (minimum (map fst results), minimum (map snd results))
        ms ns = fromIntegral ns / 1e6 :: Double
        perTx ns = fromIntegral ns / 1e3 / fromIntegral n :: Double
    printf
      "| %s | %d | %d | %d | %.1f | %.1f | %.1f | %.1f |\n"
      (kindName kind)
      nIn
      n
      entries
      (ms applyNs)
      (perTx applyNs)
      (ms reapplyNs)
      (perTx reapplyNs)
 where
  parseArgs :: [String] -> (Int, [Int])
  parseArgs = go 3 []
   where
    go _ acc ("--reps" : r : rest) = go (read r) acc rest
    go reps acc (s : rest) = go reps (acc ++ [read s]) rest
    go reps [] [] = (reps, [1000, 3000, 6000])
    go reps acc [] = (reps, acc)

-- * The closures

data Kind = OldInput | Chained

kindName :: Kind -> String
kindName = \case
  OldInput -> "old-input"
  Chained -> "chained"

-- | Every funded output carries this much, and every tx moves exactly what it
-- spends (fees are zero in the test genesis).
outputCoin :: Coin
outputCoin = Coin 1_000_000

-- | The closure's txs and the UTxO entries they spend, in the Dijkstra ledger's
-- own table type. An old-input closure of @n@ txs with @nIn@ inputs each needs
-- @n * nIn@ funded outputs; a chained one needs @nIn@.
buildClosure ::
  CoreNode StandardCrypto ->
  Kind ->
  Int ->
  Int ->
  ([Tx TopTx DijkstraEra], Map.Map BigEndianTxIn (TxOut DijkstraEra))
buildClosure CoreNode{cnDelegateKey} kind nIn n = (txs, funded)
 where
  addr = Addr networkId (mkCredential cnDelegateKey) StakeRefNull
  fundedOut = mkBasicTxOut addr (inject outputCoin)

  -- The funded outputs: output 0 of never-applied dummy txs that differ only
  -- in the coin they name, so their ids differ.
  fundedIn :: Int -> TxIn
  fundedIn i = TxIn (txIdTx dummy) (TxIx 0)
   where
    dummy :: Tx TopTx DijkstraEra
    dummy =
      mkBasicTx $
        mkBasicTxBody
          & outputsTxBodyL .~ StrictSeq.singleton (mkBasicTxOut addr (inject (Coin (fromIntegral i))))

  mkTx :: [TxIn] -> Tx TopTx DijkstraEra
  mkTx ins =
    force . signTx cnDelegateKey . mkBasicTx $
      mkBasicTxBody
        & inputsTxBodyL .~ Set.fromList ins
        & outputsTxBodyL .~ StrictSeq.fromList (replicate nIn fundedOut)

  outsOf :: Tx TopTx DijkstraEra -> [TxIn]
  outsOf tx = [TxIn (txIdTx tx) (TxIx (fromIntegral k)) | k <- [0 .. nIn - 1]]

  fundedIns = case kind of
    OldInput -> [fundedIn i | i <- [0 .. n * nIn - 1]]
    Chained -> [fundedIn i | i <- [0 .. nIn - 1]]

  funded = Map.fromList [(BigEndianTxIn i, fundedOut) | i <- fundedIns]

  txs = case kind of
    OldInput -> [mkTx (take nIn (drop (j * nIn) fundedIns)) | j <- [0 .. n - 1]]
    Chained -> take n (chain (mkTx fundedIns))
   where
    chain tx = tx : chain (mkTx (outsOf tx))

-- * One run: a fresh LeiosDB and cache, the closure validated twice

-- | Nanoseconds of the apply pass and of the reapply pass.
runOnce ::
  LedgerConfig Blk ->
  LedgerState Blk EmptyMK ->
  Map.Map BigEndianTxIn (TxOut DijkstraEra) ->
  [Tx TopTx DijkstraEra] ->
  IO (Word64, Word64)
runOnce lcfg base funded txs = do
  db <- newLeiosDBInMemory
  withWriter db $ \writer -> do
    void $ await =<< writeEbPoint writer point (encodeLeiosEbSize eb)
    void $ await =<< writeEbBody writer point eb []
    void $ await =<< writeTxs writer point (zip [0 ..] bytes)
  withReader db $ \reader -> do
    cache <- newPureLeiosTxCache defaultLeiosTxCacheShift
    void $ insertAnnouncement cache slot rbHash (pointEbHash point)
    void $ insertBody cache (pointEbHash point) (serializeEbBody eb)
    -- acquired, as a fetch leaves them: unapplied
    void $ withLockedInsertUnappliedTx cache $ \w0 step ->
      foldM (\w (h, s) -> step w h s ()) w0 (zip hashes sizes)
    applyNs <- timed "apply" (validate reader cache)
    reapplyNs <- timed "reapply" (validate reader cache)
    pure (applyNs, reapplyNs)
 where
  slot = SlotNo 1
  rbHash = MkRbHash (BS.replicate 32 0)
  bytes = map serializeTxForLeios txs
  hashes = map (hashLeiosTx . MkLeiosTx) bytes
  sizes = map (fromIntegral . BS.length) bytes
  eb = MkLeiosEb (V.fromList (zip hashes sizes))
  point = MkLeiosPoint slot (hashLeiosEb eb)

  -- The closure's input table, as the node reads it from its ledger tables:
  -- the funded entries the closure's keys name.
  tables :: LedgerTables Blk ValuesMK
  tables =
    injectLedgerTables
      dijkstraIndex
      (LedgerTables (ValuesMK funded) :: LedgerTables DijkstraBlock ValuesMK)

  resolveValues keys = pure (ltliftA2 restrictValuesMK tables keys)

  validate :: LeiosDbReader IO -> LeiosTxCache IO () ProtVerParity SerializedEbBody -> IO ()
  validate reader cache =
    validateEbClosure lcfg reader cache resolveValues point base >>= \case
      EbClosureValid reapplied applied _ ->
        when (reapplied + applied /= length txs) $
          fail ("validated " <> show (reapplied, applied) <> " of " <> show (length txs))
      EbClosureInvalid err -> fail ("closure invalid: " <> show err)
      EbClosureUnreadable err -> fail ("closure unreadable: " <> show err)

  timed :: String -> IO () -> IO Word64
  timed _ act = do
    performMajorGC
    t0 <- getMonotonicTimeNSec
    act
    t1 <- getMonotonicTimeNSec
    pure (t1 - t0)

dijkstraIndex :: Index (CardanoEras StandardCrypto) DijkstraBlock
dijkstraIndex = IS (IS (IS (IS (IS (IS (IS IZ))))))

-- * The ledger: one core node, Dijkstra from epoch 0, as the ThreadNet Leios

-- tests build it

mkProtocolInfo :: CoreNode StandardCrypto -> IO (ProtocolInfo Blk)
mkProtocolInfo coreNode = do
  fs <- SomeHasFS <$> Sim.simHasFS' MockFS.empty
  fst
    <$> mkTestProtocolInfo @IO
      fs
      (CoreNodeId 0, coreNode)
      shelleyGenesis
      (CC.Update.ProtocolVersion 0 0 0)
      NeutralNonce
      genesisByron
      secretsByron
      (Just (PBftSignatureThreshold 1))
      dijkstraProtVer
      (hardForkInto Dijkstra)
 where
  k = SecurityParam (knownNonZeroBounded @10)
  slotLength = slotLengthFromSec 1
  dijkstraProtVer = ProtVer (eraProtVerLow @DijkstraEra) 0
  (genesisByron, secretsByron) =
    Byron.generateGenesisConfig slotLength (Byron.byronPBftParams k (NumCoreNodes 1))
  shelleyGenesis =
    mkGenesisConfig
      dijkstraProtVer
      k
      (1 / 20)
      (DecentralizationParam 0)
      100_000_000_000_000
      slotLength
      (mkKesConfig (Proxy @StandardCrypto) (NumSlots 100))
      [coreNode]
