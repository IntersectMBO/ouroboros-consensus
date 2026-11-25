{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Mempool.Util
  ( TestBlock
  , TestTx
  , TestTxError
  , TestTxId
  , TheMeasure
  , applyTxToLedger
  , applyTxToTickedLedger
  , bumpTip
  , genInvalidTx
  , genLargeInvalidTx
  , genTxs
  , genValidTx
  , genValidTxs
  , mkTestLedgerConfig
  , mustBeValid
  , testInitLedger
  , testLedgerConfigNoSizeLimits
  , testSlot
  , txIsValid
  , genConnectedTxLattice
  , genTickedLedgerState
  ) where

import Cardano.Binary (Encoding, toCBOR)
import Cardano.Crypto.Hash
import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Cardano.Slotting.Slot
import Control.Exception (assert)
import Control.Monad (guard, unless)
import Control.Monad.Except (Except)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (isRight)
import Data.Foldable (foldlM)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract hiding (TxIn, TxOut)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Mock.Ledger hiding (TxId)
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.Protocol.BFT
import Test.Crypto.Hash ()
import Test.QuickCheck
import Test.Util.Orphans.IOLike ()

type TestBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

type TestTx = GenTx TestBlock

type TestTxId = TxId TestTx

type TestTxError = ApplyTxErr TestBlock

type TheMeasure = IgnoringOverflow ByteSize32

-- There are 5 (core)nodes and each gets 1000.
testInitLedger :: LedgerState TestBlock ValuesMK
testInitLedger = genesisSimpleLedgerState $ mkAddrDist (NumCoreNodes 5)

-- | Test config
--
-- (We don't really care about these values here)
mkTestLedgerConfig :: MockConfig -> LedgerConfig TestBlock
mkTestLedgerConfig mockCfg =
  SimpleLedgerConfig
    { simpleMockLedgerConfig = ()
    , simpleLedgerEraParams =
        HardFork.defaultEraParams
          (SecurityParam (knownNonZeroBounded @4))
          (slotLengthFromSec 20)
          NoPerasEnabled
    , simpleLedgerMockConfig = mockCfg
    }

testLedgerConfigNoSizeLimits :: LedgerConfig TestBlock
testLedgerConfigNoSizeLimits = mkTestLedgerConfig defaultMockConfig

testSlot :: SlotNo
testSlot = 1

-- | Generate a number of valid and invalid transactions and apply the valid
-- transactions to the given 'LedgerState'. The transactions along with a
-- 'Bool' indicating whether its valid ('True') or invalid ('False') and the
-- resulting 'LedgerState' are returned.
genTxs ::
  -- | The number of transactions to generate
  Int ->
  LedgerState TestBlock ValuesMK ->
  Gen ([(TestTx, Bool)], LedgerState TestBlock ValuesMK)
genTxs = go []
 where
  go txs n ledger
    | n <= 0 = return (reverse txs, ledger)
    | otherwise = do
        valid <- arbitrary
        if valid
          then do
            (validTx, ledger') <- genValidTx ledger
            go ((validTx, True) : txs) (n - 1) ledger'
          else do
            invalidTx <- genInvalidTx ledger
            go ((invalidTx, False) : txs) (n - 1) ledger

-- | Generate a number of valid transactions and apply these to the given
-- 'LedgerState'. The transactions and the resulting 'LedgerState' are
-- returned.
genValidTxs ::
  -- | The number of valid transactions to generate
  Int ->
  LedgerState TestBlock ValuesMK ->
  Gen ([TestTx], LedgerState TestBlock ValuesMK)
genValidTxs = go []
 where
  go txs n ledger
    | n <= 0 = return (reverse txs, ledger)
    | otherwise = do
        (tx, ledger') <- genValidTx ledger
        go (tx : txs) (n - 1) ledger'

mustBeValid ::
  HasCallStack =>
  Except TestTxError a ->
  a
mustBeValid ex = case runExcept ex of
  Left _ -> error "Invalid Ledger State was produced by a generated transaction"
  Right st -> st

txIsValid :: LedgerConfig TestBlock -> LedgerState TestBlock ValuesMK -> TestTx -> Bool
txIsValid cfg ledgerState tx =
  isRight $ runExcept $ applyTxToLedger cfg ledgerState tx

-- | Generate a transaction spending a weighted random subset of the given
-- UTxOs (favoring those with more funds), distributing them over at most as
-- many outputs as inputs.
--
-- PRECONDITION (checked): the given UTxO set is non-empty.
genTxSpendingFrom :: Map TxIn (Addr, Amount) -> Gen TestTx
genTxSpendingFrom utxos = do
  unless (not (Map.null utxos)) $ error "genTxSpendingFrom: the UTxO set must be non-empty"
  nIns <- choose (1, length utxos)
  nOuts <- choose (1, nIns)
  (ins, _rest) <- pickWithFrequency (fromIntegral . snd) nIns utxos
  distribInsOverOuts ins nOuts

-- | Generate a valid transaction (but ignoring any per-tx size limits, see Note
-- [Transaction size limit]).
--
-- PRECONDITION: the state's UTxO set is non-empty (see 'genTxSpendingFrom').
genValidTx :: LedgerState TestBlock ValuesMK -> Gen (TestTx, LedgerState TestBlock ValuesMK)
genValidTx ledgerState@(SimpleLedgerState MockState{} (LedgerTables (ValuesMK utxos))) = do
  tx <- genTxSpendingFrom utxos
  return (tx, mustBeValid (applyTxToLedger testLedgerConfigNoSizeLimits ledgerState tx))

-- | Generate a ticked ledger state evolved from genesis by applying some
-- random transactions at a random slot.  Uses 'applyTxToTickedLedger' so the
-- block tip stays at genesis and 'mockConfirmed' is consistent with what
-- 'reapplyTxs' will produce.
genTickedLedgerState :: Gen (SlotNo, TickedLedgerState TestBlock ValuesMK)
genTickedLedgerState = do
  slot <- SlotNo <$> choose (1, 100)
  nTxs <- choose (0, 10)
  let initSt =
        applyDiffs testInitLedger $
          applyChainTick
            OmitLedgerEvents
            testLedgerConfigNoSizeLimits
            slot
            (forgetLedgerTables testInitLedger)
  evolvedSt <- applyNRandomTxs slot nTxs initSt
  return (slot, evolvedSt)
 where
  applyNRandomTxs ::
    SlotNo -> Int -> TickedLedgerState TestBlock ValuesMK -> Gen (TickedLedgerState TestBlock ValuesMK)
  applyNRandomTxs _slot 0 st = return st
  applyNRandomTxs slot n st = do
    let LedgerTables (ValuesMK utxos) = projectLedgerTables st
    if Map.null utxos
      then return st
      else do
        tx <- genTxSpendingFrom utxos
        let st' = mustBeValid (applyTxToTickedLedger testLedgerConfigNoSizeLimits DoNotIntervene slot tx st)
        applyNRandomTxs slot (n - 1) st'

-- | Generate a connected lattice of dependent transactions, valid in order
-- against the given state: a seed transaction spending some of the state's
-- UTxOs, and layers of transactions spending the previous layer's outputs.
-- The ledger state is threaded through internally only to keep the generated
-- transactions valid; compute any expected final state by applying the
-- returned transactions sequentially.
--
-- PRECONDITION: the state's UTxO set is non-empty (see 'genTxSpendingFrom').
genConnectedTxLattice ::
  SlotNo ->
  Int ->
  TickedLedgerState TestBlock ValuesMK ->
  Gen (TestTx, [TestTx])
genConnectedTxLattice slot depth tickedSt = do
  let LedgerTables (ValuesMK utxos) = projectLedgerTables tickedSt
  txSeed <- genTxSpendingFrom utxos
  tickedSt' <-
    return $
      mustBeValid
        (applyTxToTickedLedger testLedgerConfigNoSizeLimits DoNotIntervene slot txSeed tickedSt)
  (allInnerTxs, _finalSt) <- growTxLattice slot depth tickedSt' txSeed
  return (txSeed, allInnerTxs)

growTxLattice ::
  SlotNo ->
  Int ->
  TickedLedgerState TestBlock ValuesMK ->
  TestTx ->
  Gen ([TestTx], TickedLedgerState TestBlock ValuesMK)
growTxLattice _slot 0 tickedSt _txSeed = return ([], tickedSt)
growTxLattice slot depth tickedSt txSeed = do
  txs <- distribInsOverTxs (txOuts txSeed) 5 (\ins -> distribInsOverOuts ins (length ins))
  tickedSt' <-
    return $
      mustBeValid
        (foldlM (flip (applyTxToTickedLedger testLedgerConfigNoSizeLimits DoNotIntervene slot)) tickedSt txs)
  (allInnerTxs, tickedSt'') <-
    foldlM
      ( \(allInnerTxs, st) tx -> do
          (innerTxs, st') <- growTxLattice slot (depth - 1) st tx
          return (allInnerTxs <> innerTxs, st')
      )
      ([], tickedSt')
      txs
  return (txs <> allInnerTxs, tickedSt'')

-- | @txs <- distribInsOverTxs ins nTxs txWithIns@ generates between 1 and
-- @nTxs@ transactions using the @txWithIns@ generator, with @ins@ partitioned
-- randomly amongst them (every input is consumed by exactly one transaction).
-- A @nTxs@ of 1 or less yields a single transaction consuming all of @ins@.
distribInsOverTxs ::
  Map TxIn (Addr, Amount) -> Int -> (Map TxIn (Addr, Amount) -> Gen TestTx) -> Gen [TestTx]
distribInsOverTxs ins nTxs txWithIns = do
  unless (length ins > 0) $ error "ins must be non-empty"

  txs <-
    iterateUntilRightM
      ( \(txs, insLeft, nTxsLeft) -> do
          if Map.empty == insLeft
            then return $ Right txs
            else
              if nTxsLeft <= 1
                then do
                  tx <- txWithIns insLeft
                  return $ Right (tx : txs)
                else do
                  nIns <- choose (1, Map.size insLeft)
                  (insPicked, insLeft') <- pickWithFrequency (fromIntegral . snd) nIns insLeft
                  tx <- txWithIns insPicked
                  return $ Left $ (tx : txs, insLeft', nTxsLeft - 1)
      )
      ([], ins, nTxs)

  return txs

-- | @tx <- distribInsOverOuts utxos nOuts@ generates a transaction @tx@ with @utxos@ as inputs
-- and at most @nOuts@ outputs distributing the inputs randomly over outputs.
-- The recipients are randomly sourced from addresses found in @utxos@.
distribInsOverOuts :: Map TxIn (Addr, Amount) -> Int -> Gen TestTx
distribInsOverOuts ins nOuts = do
  let potentialRecipients = Set.fromList [addr | (_txIn, (addr, _amount)) <- Map.toList ins]
      totalInAmount = sum . fmap (\(_, (_, amount)) -> amount) . Map.toList $ ins

  outs <-
    iterateUntilRightM
      ( \(outs, inAmountLeft, nOutsLeft) -> do
          if inAmountLeft <= 0
            then return $ Right outs
            else
              if nOutsLeft <= 1
                then do
                  recipientPicked <- elements $ Set.toList potentialRecipients
                  return $ Right $ (recipientPicked, inAmountLeft) : outs
                else do
                  outAmountPicked <- choose (1, inAmountLeft)
                  recipientPicked <- elements $ Set.toList potentialRecipients
                  return $
                    Left
                      ((recipientPicked, outAmountPicked) : outs, inAmountLeft - outAmountPicked, nOutsLeft - 1)
      )
      ([], totalInAmount, nOuts)

  return $ mkSimpleGenTx $ Tx DoNotExpire (Map.keysSet ins) outs

iterateUntilRightM :: Monad m => (a -> m (Either a b)) -> a -> m b
iterateUntilRightM f x = do
  res <- f x
  case res of
    Left !x' -> iterateUntilRightM f x'
    Right !y -> return y

-- | Pick up to @n@ entries from the map, one at a time, each drawn with
-- probability proportional to its weight; returns the picked entries and the
-- rest.
--
-- PRECONDITION: @weight@ must be positive for every value in the map
-- ('frequency' errors out when no weight is positive, and never picks
-- zero-weight entries; beware 'fromIntegral' overflowing 'Int' into negative
-- weights).
pickWithFrequency :: (Ord k, Eq v) => (v -> Int) -> Int -> Map k v -> Gen (Map k v, Map k v)
pickWithFrequency weight n xs = pickManyWith (\rest -> frequency [(weight v, return (k, v)) | (k, v) <- Map.toList rest]) n xs

pickManyWith :: (Ord k, Eq v) => (Map k v -> Gen (k, v)) -> Int -> Map k v -> Gen (Map k v, Map k v)
pickManyWith pickOne n xs = go n (Map.empty, xs)
 where
  go i res@(picked, rest)
    | i == 0 || rest == Map.empty = return res
    | otherwise = do
        (k, v) <- pickOne rest
        go (i - 1) (picked `Map.union` Map.singleton k v, Map.delete k rest)

genInvalidTx :: LedgerState TestBlock ValuesMK -> Gen TestTx
genInvalidTx ledgerState = do
  let peopleWithFunds = nub $ map fst $ Map.elems utxo
  sender <- elements peopleWithFunds
  recipient <- elements peopleWithFunds
  let assets = filter (\(_, (addr, _)) -> addr == sender) $ Map.toList utxo
      ins = Set.fromList $ map fst assets
  -- There is only 5 000 in 'testInitLedger', so any transaction spending
  -- more than 5 000 is invalid.
  amount <- choose (5_001, 10_000)
  let outs = [(recipient, amount)]
      tx = mkSimpleGenTx $ Tx DoNotExpire ins outs
  return $ assert (not (txIsValid testLedgerConfigNoSizeLimits ledgerState tx)) tx
 where
  SimpleLedgerState
    { simpleLedgerTables = LedgerTables (ValuesMK utxo)
    } = ledgerState

-- | Generate an invalid tx that is larger than the given measure.
genLargeInvalidTx :: TheMeasure -> Gen TestTx
genLargeInvalidTx (IgnoringOverflow sz) = go Set.empty
 where
  go ins = case isLargeTx ins of
    Just tx -> pure tx
    Nothing -> do
      newTxIn <- arbitrary
      go (Set.insert newTxIn ins)

  isLargeTx :: Set TxIn -> Maybe TestTx
  isLargeTx ins = do
    let outs = []
        tx = mkSimpleGenTx $ Tx DoNotExpire ins outs
    guard $ genTxSize tx > sz
    pure tx

-- | Advance the ledger tip by one slot while keeping the UTxO unchanged.
--
-- Unlike 'applyTxToLedger', this consumes no inputs, so any transaction that was
-- valid against the input state is still valid against the result. This is
-- exactly the base change needed to make a mempool sync do real work without
-- invalidating any of the transactions it already holds.
bumpTip ::
  LedgerState TestBlock ValuesMK ->
  LedgerState TestBlock ValuesMK
bumpTip (SimpleLedgerState mockState tbls) =
  SimpleLedgerState mockState{mockTip = BlockPoint slot' hash'} tbls
 where
  slot' = case pointSlot $ mockTip mockState of
    Origin -> 0
    NotOrigin s -> succ s

  -- See 'applyTxToLedger' for the phantom-parameter trick behind this hash.
  hash' :: HeaderHash TestBlock
  hash' = hashWithSerialiser fakeEncodeHeader (error "fake header")

  fakeEncodeHeader :: Header TestBlock -> Encoding
  fakeEncodeHeader _ = toCBOR slot'

-- | Apply a transaction to the ledger
--
-- We don't have blocks in this test, but transactions only. In this function
-- we pretend the transaction /is/ a block, apply it to the UTxO, and then
-- update the tip of the ledger state, incrementing the slot number and faking
-- a hash.
applyTxToLedger ::
  LedgerConfig TestBlock ->
  LedgerState TestBlock ValuesMK ->
  TestTx ->
  Except TestTxError (LedgerState TestBlock ValuesMK)
applyTxToLedger cfg st tx =
  let SimpleLedgerState mockState _ = stowLedgerTables st
   in unstowLedgerTables . mkNewLedgerState <$> updateMockUTxO mockCfg dummy tx mockState
 where
  mockCfg = simpleLedgerMockConfig cfg

  -- All expiries in this test are 'DoNotExpire', so the current time is
  -- irrelevant.
  dummy :: SlotNo
  dummy = 0

  mkNewLedgerState mockState' =
    SimpleLedgerState mockState'{mockTip = BlockPoint slot' hash'} emptyLedgerTables

  slot' = case pointSlot $ mockTip (simpleLedgerState st) of
    Origin -> 0
    NotOrigin s -> succ s

  -- A little trick to instantiate the phantom parameter of 'Hash' (and
  -- 'HeaderHash') with 'TestBlock' while actually hashing the slot number:
  -- use a custom serialiser to instantiate the phantom type parameter with
  -- @Header TestBlock@, but actually encode the slot number instead.
  hash' :: HeaderHash TestBlock
  hash' = hashWithSerialiser fakeEncodeHeader (error "fake header")

  fakeEncodeHeader :: Header TestBlock -> Encoding
  fakeEncodeHeader _ = toCBOR slot'

-- | Apply a transaction to a ticked ledger state using the real mempool API.
-- Mirrors 'applyTx' but folds the diff back in, returning 'ValuesMK'.
applyTxToTickedLedger ::
  LedgerConfig TestBlock ->
  WhetherToIntervene ->
  SlotNo ->
  TestTx ->
  TickedLedgerState TestBlock ValuesMK ->
  Except TestTxError (TickedLedgerState TestBlock ValuesMK)
applyTxToTickedLedger cfg wti slot tx st = do
  (stDiff, _) <- applyTx cfg wti slot tx st
  return $ applyDiffs st stDiff
