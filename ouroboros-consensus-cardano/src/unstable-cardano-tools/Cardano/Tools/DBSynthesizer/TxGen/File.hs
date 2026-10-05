{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Transaction generation that replays a stream of transactions from a file.
--
-- == What a caller has to know
--
-- * __The file has to be complete before the run starts.__
--
-- * __Resuming with @-a@ is not supported__, and the tool refuses the two
--   flags together.
module Cardano.Tools.DBSynthesizer.TxGen.File
  ( mkFileTxGen

    -- * Exposed for testing
  , decodeStream
  ) where

import Cardano.Ledger.Alonzo.Core (TopTx, Tx, eraDecoder)
import Cardano.Ledger.Binary (Annotator (runAnnotator), DecCBOR (decCBOR), FullByteString (Full))
import Cardano.Ledger.Binary.Plain (DecoderError (DecoderErrorDeserialiseFailure))
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Tools.DBSynthesizer.Forging (GenTxs)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (Proxy))
import Data.SOP.BasicFunctors (K (K))
import Data.SOP.Dict (Dict (Dict))
import Data.SOP.Index (Index (IS, IZ), dictIndexAll, hcimap, injectNS)
import Data.SOP.Strict (hcollapse)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras, LedgerState)
import Ouroboros.Consensus.Cardano.Ledger ()
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (proxySingle)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraGenTx (OneEraGenTx))
import Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (TickedHardForkLedgerState))
import Ouroboros.Consensus.HardFork.Combinator.Mempool (GenTx (HardForkGenTx))
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( HasByteSize (txMeasureByteSize)
  , TxEbMeasure
  , TxMeasure
  , Validated
  , WhetherToIntervene (DoNotIntervene)
  , applyTx
  , blockCapacityTxMeasure
  , ebCapacityTxMeasure
  , getTransactionKeySets
  , txEbMeasure
  , txMeasure
  , txWireSize
  , unByteSize32
  )
import Ouroboros.Consensus.Ledger.Tables (KeysMK, LedgerTables, ValuesMK, castLedgerTables)
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffForKeysOnTables, applyDiffs)
import Ouroboros.Consensus.Shelley.Ledger (IsShelleyBlock, ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( DijkstraEbMeasure (ebClosureMeasure)
  , mkShelleyTx
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ReadOnlyForker (roforkerReadTables))

type Cardano = CardanoBlock StandardCrypto

-- | What the generator carries from one forged block to the next.
data FileState = FileState
  { fsTxs :: !Stream
  -- ^ What the stream holds if the endorser block does not apply: the tail the
  -- ranking block's transactions alone left behind.
  , fsTxsIfCertified :: !Stream
  -- ^ What it holds if a later block certifies the endorser block, so that the
  -- endorser block's transactions apply as well. If the block announced no
  -- endorser block, this equals 'fsTxs'.
  }

-- | The transactions the file still holds.
data Stream
  = -- | The file's bytes, read but not yet decoded. A transaction's encoding
    -- belongs to one era, and the era is not known until the tool leads a slot,
    -- so the stream stays bytes until then.
    Unread !BSL.ByteString
  | -- | What is left of the stream. Lazy: forcing a cell decodes one more
    -- transaction, and holding the tail alone lets the ones already forged go.
    Decoded [GenTx Cardano]

-- | Build the generator that the forge loop runs on each slot that the tool
-- leads.
mkFileTxGen ::
  FilePath ->
  IO (Either String (TopLevelConfig Cardano -> GenTxs Cardano))
mkFileTxGen path = do
  bytes <- BSL.readFile path
  fileState <- newIORef FileState{fsTxs = Unread bytes, fsTxsIfCertified = Unread bytes}
  pure $ Right $ fileTxGen fileState path

-- | Fill the block of this slot with transactions off the head of the stream.
--
-- The generator does not drop what it takes. It returns an action that stores
-- the unconsumed tail, and the forge loop runs that action once the ChainDB
-- adopts the block, so a block that is not adopted leaves the stream where it
-- was.
fileTxGen ::
  IORef FileState ->
  FilePath ->
  TopLevelConfig Cardano ->
  GenTxs Cardano
fileTxGen fileState path cfg slot certifies forker ticked
  -- The certified endorser block's transactions apply with this block, and
  -- 'mkBody' drops any transaction this block makes of its own. Those were
  -- taken from the stream when the endorser block was announced, so there is
  -- nothing to take here: the stream only moves past them now, which is what
  -- promoting the certified tail does.
  | certifies =
      pure
        ( []
        , []
        , modifyIORef' fileState $ \previous ->
            previous{fsTxs = fsTxsIfCertified previous}
        )
  | otherwise = do
      before <- readIORef fileState
      case ticked of
        TickedHardForkLedgerState _transition perEra ->
          hcollapse $ hcimap proxySingle (\idx _state -> K (genForEra before idx)) perEra
 where
  lcfg = configLedger cfg

  -- Neither capacity needs the ledger's values, so both are known before a
  -- single transaction is decoded.
  rbCapacity :: TxMeasure Cardano
  rbCapacity = blockCapacityTxMeasure lcfg ticked

  ebCapacity :: TxEbMeasure Cardano
  ebCapacity = ebCapacityTxMeasure lcfg ticked

  -- The bytes one forging opportunity can want: the ranking block's capacity
  -- plus the closure capacity of the endorser block it announces. An endorser
  -- block measures the references it carries as well, but those are not
  -- transactions taken from the stream, so they do not bound the look-ahead.
  boundBytes :: Integer
  boundBytes =
    fromIntegral (unByteSize32 (txMeasureByteSize rbCapacity))
      + fromIntegral (unByteSize32 (txMeasureByteSize (ebClosureMeasure ebCapacity)))

  genForEra ::
    FileState ->
    Index (CardanoEras StandardCrypto) x ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)], IO ())
  genForEra before = \case
    IZ ->
      throwIO . userError $
        "db-synthesizer: transaction generation not supported in the Byron era."
    IS idx -> case dictIndexAll (Proxy @IsShelleyBlock) idx of
      Dict -> genFor before (IS idx)

  genFor ::
    forall proto era.
    -- Decoding a transaction of this era needs more than 'ShelleyBasedEra':
    -- 'DecCBOR' for its transaction body comes with the block, which
    -- 'IsShelleyBlock' carries.
    IsShelleyBlock (ShelleyBlock proto era) =>
    FileState ->
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)], IO ())
  genFor before idx = do
    stateAtSlot <- stateHolding (lookAhead stream)
    fillBlock stream stateAtSlot
   where
    -- The era is the one of the block this slot forges, and the whole file is
    -- decoded at it. A run that crosses a hard fork keeps reading at the era it
    -- began in.
    stream :: [GenTx Cardano]
    stream = case fsTxs before of
      Decoded txs -> txs
      Unread bytes -> decodeStream @proto @era path wrap bytes

    wrap :: GenTx (ShelleyBlock proto era) -> GenTx Cardano
    wrap = HardForkGenTx . OneEraGenTx . injectNS idx

  -- Take transactions until their wire sizes total twice 'boundBytes'. That is
  -- enough to fill one ranking block and the endorser block it announces,
  -- because a transaction's measure is at least its wire size minus 3 bytes,
  -- so at least half of it. If it is not enough, 'applyTx' rejects a
  -- transaction whose inputs 'stateHolding' did not read.
  --
  -- They are taken ahead of the block so that 'stateHolding' can read the UTxO
  -- entries of all of them in one batch.
  lookAhead :: [GenTx Cardano] -> [GenTx Cardano]
  lookAhead = go 0
   where
    go !used = \case
      tx : rest
        | used <= 2 * boundBytes ->
            tx : go (used + fromIntegral (txWireSize tx)) rest
      _ -> []

  -- Read the entries those transactions name out of the ledger, in one go, so
  -- that 'applyTx' can spend them. A value that is absent is not an error here:
  -- the ledger reports it, naming the input, when the transaction is applied.
  stateHolding :: [GenTx Cardano] -> IO (TickedLedgerState Cardano ValuesMK)
  stateHolding txs = do
    values <- roforkerReadTables forker keys
    let keysForSlot :: LedgerTables (TickedLedgerState Cardano) KeysMK
        keysForSlot = castLedgerTables keys

        valuesForSlot :: LedgerTables (TickedLedgerState Cardano) ValuesMK
        valuesForSlot = castLedgerTables values
    pure $ applyDiffForKeysOnTables valuesForSlot keysForSlot ticked
   where
    keys :: LedgerTables (ExtLedgerState Cardano) KeysMK
    keys =
      castLedgerTables
        (foldMap getTransactionKeySets txs :: LedgerTables (LedgerState Cardano) KeysMK)

  -- Fill the ranking block, then the endorser block it announces, from the
  -- same run of the stream.
  fillBlock ::
    [GenTx Cardano] ->
    TickedLedgerState Cardano ValuesMK ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)], IO ())
  fillBlock stream stateAtSlot = do
    -- Exhaustion first, so that it is not reported as the transaction that
    -- comes next being too large: once the file runs out there is no such
    -- transaction, and 'fillBatch' takes nothing for the same reason.
    when (null stream) $
      throwIO . userError $
        "db-synthesizer: the transaction stream in "
          ++ path
          ++ " ran out before slot "
          ++ show slot
          ++ ". The run asks for more blocks than the file has transactions to"
          ++ " fill: write a longer stream, or forge fewer blocks."
    rb <- fillBatch id rbCapacity stream stateAtSlot
    when (null (tookTxs rb)) $
      throwIO . userError $
        "db-synthesizer: the next transaction of the stream does not fit in the"
          ++ " ranking block at slot "
          ++ show slot
          ++ ", which leaves the block empty and the stream stuck on it."
    -- The endorser block is bounded by the measure of its own transactions
    -- alone, so this batch starts its count from zero. An era without Leios
    -- has a zero capacity, so nothing fits and the block announces none.
    eb <-
      fillBatch
        (txEbMeasure (Proxy @Cardano))
        ebCapacity
        (tookRest rb)
        (tookState rb)
    pure
      ( tookTxs rb
      , tookTxs eb
      , writeIORef
          fileState
          FileState
            { fsTxs = Decoded (tookRest rb)
            , fsTxsIfCertified = Decoded (tookRest eb)
            }
      )

  -- Take transactions in stream order while they fit. Stop at the first that
  -- takes the total over the bound, and leave it for the next block.
  fillBatch ::
    forall m.
    Measure.Measure m =>
    -- How a transaction's block measure is charged against this batch's bound.
    (TxMeasure Cardano -> m) ->
    -- The bound on the total.
    m ->
    [GenTx Cardano] ->
    TickedLedgerState Cardano ValuesMK ->
    IO Took
  fillBatch charge limit stream0 state0 = go [] Measure.zero stream0 state0
   where
    go accepted used stream state = case stream of
      -- The stream's break. What is accepted still makes a block; the next one
      -- stops the run. A stream that instead ran out of bytes never gets here:
      -- 'decodeStream' crashes on it.
      [] -> pure stopped
      genTx : rest -> do
        measured <- case runExcept (txMeasure lcfg state genTx) of
          Left err ->
            throwIO . userError $
              "db-synthesizer: a transaction of the stream breaks a per-transaction limit at slot "
                ++ show slot
                ++ ": "
                ++ show err
          Right measured -> pure measured
        let used' = used `Measure.plus` charge measured
        if not (used' Measure.<= limit)
          then pure stopped
          else case runExcept (applyTx lcfg DoNotIntervene slot genTx state) of
            Left err ->
              throwIO . userError $
                "db-synthesizer: the ledger rejected a transaction of the stream at slot "
                  ++ show slot
                  ++ ": "
                  ++ show err
            Right (stateAfterTx, validatedTx) ->
              go (validatedTx : accepted) used' rest (applyDiffs state stateAfterTx)
     where
      stopped =
        Took
          { tookTxs = reverse accepted
          , tookRest = stream
          , tookState = state
          }

-- | The result of 'fillBatch'.
data Took = Took
  { tookTxs :: [Validated (GenTx Cardano)]
  -- ^ The transactions it accepted, in the order the ledger applies them.
  , tookRest :: [GenTx Cardano]
  -- ^ What is left of the stream.
  , tookState :: TickedLedgerState Cardano ValuesMK
  -- ^ The ledger state after those transactions applied.
  }

-- | The transactions of a stream file, decoded as they are demanded.
--
-- The list ends at the stream's break, and the break has to end the file.
-- Anything else -- a stream cut short while it was being written, bytes that
-- are not this era's transactions, or bytes left over behind the break --
-- stops the tool dead, by 'error' from inside the list.
--
-- Exported for the tests, which feed it the damaged streams a run never should
-- meet. No other caller wants it: the generator reaches it through 'fsTxs'.
--
-- Two things follow from crashing rather than carrying the failure as a value.
-- It is raised wherever a consumer forces that far, which is not necessarily
-- the block that wanted the transaction; and it cannot name the slot, because
-- this function has no idea which block is being filled. It does name the file
-- and what the decoder made of the bytes.
decodeStream ::
  forall proto era.
  IsShelleyBlock (ShelleyBlock proto era) =>
  FilePath ->
  (GenTx (ShelleyBlock proto era) -> GenTx Cardano) ->
  BSL.ByteString ->
  [GenTx Cardano]
decodeStream path wrap bytes = case run CBOR.decodeListLenIndef bytes of
  Left err -> crash err
  Right (rest, ()) -> go rest
 where
  go bs = case run CBOR.decodeBreakOr bs of
    Left err -> crash err
    -- The break: the writer finished the stream. Nothing may follow it, and
    -- asking also settles the file handle: a lazy read closes it on reaching
    -- end of file, which stopping at the break alone never would.
    Right (rest, True)
      | BSL.null rest -> []
      | otherwise -> trailing
    -- Not a break, so another transaction. 'decodeBreakOr' took nothing, so
    -- this decodes from where that one started.
    Right (_, False) -> case CBOR.deserialiseFromBytesWithSize txDecoder bs of
      Left err -> crash (asDecoderError err)
      Right (rest, size, mkTx) -> case mkTx (BSL.take size bs) of
        Left err -> crash err
        Right tx -> wrap (mkShelleyTx tx) : go rest

  trailing :: a
  trailing =
    error $
      "db-synthesizer: " ++ path ++ " has bytes after the end of the stream."

  crash :: DecoderError -> a
  crash err =
    error $
      "db-synthesizer: the transaction stream in "
        ++ path
        ++ " does not decode: "
        ++ show err
        ++ ". Either it was cut short while it was being written -- a finished \
           \stream ends in a CBOR break -- or it does not hold this era's \
           \transactions."

  run ::
    forall a.
    (forall s. CBOR.Decoder s a) ->
    BSL.ByteString ->
    Either DecoderError (BSL.ByteString, a)
  run dec bs = first asDecoderError (CBOR.deserialiseFromBytes dec bs)

  asDecoderError = DecoderErrorDeserialiseFailure "transaction stream"

  -- The transaction's own encoding, as the ledger writes it: no envelope and no
  -- cbor-in-cbor. Its decoder is an annotator, so it wants the bytes it was
  -- decoded from, which is what 'deserialiseFromBytesWithSize' gives the size
  -- for.
  txDecoder :: forall s. CBOR.Decoder s (BSL.ByteString -> Either DecoderError (Tx TopTx era))
  txDecoder = eraDecoder @era ((. Full) . runAnnotator <$> decCBOR)
