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
-- The file is one CBOR indefinite-length array of transactions -- @9f@, the
-- transactions, @ff@ -- which is the shape a stream takes when its writer does
-- not know in advance how long it will be. Each transaction is the ledger's own
-- encoding of it, with no envelope. The file holds transactions and nothing
-- else: no block boundaries and no endorser block markers. This generator takes
-- as many as fit, first into the ranking block and then into the endorser block
-- it announces.
--
-- == The stream is a lazy list
--
-- The file's bytes are read lazily and 'decodeStream' turns them into a list of
-- transactions decoded as they are demanded: forcing a cell decodes one
-- transaction and no more. What the generator carries from one block to the
-- next is the unconsumed tail of that list, which is why it needs no byte
-- offset into the file and no arithmetic to maintain one.
--
-- 'lookAhead' forces a little more of the list than the block will use. The
-- ledger values for a block's transactions are read in one go, so their key set
-- has to be known before the first 'applyTx' -- but a block only learns which
-- transactions it wants by measuring them, and measuring needs the values. The
-- look-ahead breaks that circle with a bound that needs no values: twice the
-- block's capacity in wire bytes, which is certainly more than fits, because a
-- transaction's measure in a block never exceeds its size on the wire.
--
-- Reading a transaction at a time instead would force the list no further than
-- the block consumes it, and on small blocks it is marginally faster. It does
-- not scale: each transaction's values have to be merged into those gathered
-- before it, so the cost grows with the square of the transactions in a block.
-- At the few hundred transactions of a small block that is invisible; at the
-- tens of thousands of a 6912k block it is most of the run.
--
-- Three things follow from lazy reading, all deliberate:
--
-- * __The file has to be complete before the run starts.__ The bytes stop at
--   end of file and cannot resume, so a writer still appending is invisible --
--   and not merely unwaited for: the list has already ended, and forcing it
--   again returns the same end however much the file has grown since. The @ff@
--   of a finished stream is what tells the two apart.
--
-- * __A decode failure stops the tool.__ A lazy list has nowhere to raise from,
--   so 'decodeStream' calls 'error'. It fires wherever a consumer forces that
--   far and cannot name the slot, because it does not know which block is being
--   filled; it does name the file and what the decoder made of the bytes.
--
-- * __Resuming with @-a@ is not supported.__ A new process starts the list
--   again from the head of the file and replays transactions the chain has
--   already spent, which the ledger rejects for inputs it no longer holds.
--   Supporting it would need a position persisted across runs, which is the
--   byte offset this design does without.
--
-- == One era, chosen once
--
-- A transaction's encoding belongs to an era, so the bytes stay bytes until the
-- tool leads a slot and an era is known. The first block the tool /forges/ --
-- not the first slot of the run -- decides it: the list is built there, and its
-- unforced cells hold that era's decoder and that era's index for the rest of
-- the run.
--
-- A run that crosses a hard fork therefore keeps decoding at the era it began
-- in and keeps tagging its transactions with that era, leaving the hard fork
-- combinator to translate them forward -- which changes a transaction's id, so
-- the chain does not hold the transaction this generator handed it.
--
-- Nothing here checks the file's era against the block's, and decoding is not
-- the check it might look like: a transaction that uses no field an era added
-- decodes just as well at its neighbours, so a file written for one era can be
-- read whole at another and only go wrong later, in the ledger. A configuration
-- that puts every era boundary at epoch 0 keeps the question from arising; a
-- file that says which era it holds would answer it.
--
-- == Endorser blocks
--
-- The alternation this generator requires is that a block announces an endorser
-- block and the next one certifies it. It needs no state for that, because an
-- endorser block's transactions are ones it has already taken from the list. It
-- does stop the run if a block would announce an endorser block while one is
-- uncertified: the transactions of an uncertified endorser block never reach
-- the ledger, so the ones that spend their outputs would later be rejected for
-- an input the ledger never held, and that error would name neither the cause
-- nor the block.
module Cardano.Tools.DBSynthesizer.TxGen.File
  ( mkFileTxGen
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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
  , fsAnnounced :: !Bool
  -- ^ Whether the block just forged announced an endorser block that no block
  -- has certified yet.
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
  fileState <- newIORef FileState{fsTxs = Unread bytes, fsAnnounced = False}
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
  -- nothing to take and nothing to put back.
  | certifies = do
      before <- readIORef fileState
      pure ([], [], writeIORef fileState before{fsAnnounced = False})
  | otherwise = do
      before <- readIORef fileState
      when (fsAnnounced before) $
        throwIO . userError $
          "db-synthesizer: the endorser block announced before slot "
            ++ show slot
            ++ " was not certified. This generator requires that the block after"
            ++ " an announcement certifies it. Give the forger a --shelley-bls-key"
            ++ " whose pool registers the matching leiosKey."
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
    -- decoded at it. A file written for another era stops the tool at the first
    -- transaction rather than silently decoding to something else, and a run
    -- that crosses a hard fork keeps reading at the era it began in.
    stream :: [GenTx Cardano]
    stream = case fsTxs before of
      Decoded txs -> txs
      Unread bytes -> decodeStream @proto @era path wrap bytes

    wrap :: GenTx (ShelleyBlock proto era) -> GenTx Cardano
    wrap = HardForkGenTx . OneEraGenTx . injectNS idx

  -- Force the transactions this block could want, and no more than it could.
  --
  -- A transaction's measure inside a block never exceeds its size on the wire,
  -- so transactions whose wire sizes total twice the capacity are certainly
  -- more than fit.
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
    rb <- fillBatch id rbCapacity stream stateAtSlot
    when (null (tookTxs rb)) $
      throwIO . userError $
        "db-synthesizer: not one transaction of the stream fits in the ranking block at slot "
          ++ show slot
          ++ "."
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
            { fsTxs = Decoded (tookRest eb)
            , fsAnnounced = not (null (tookTxs eb))
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
-- The list ends at the stream's break. Anything else -- a stream cut short
-- while it was being written, or bytes that are not this era's transactions --
-- stops the tool dead, by 'error' from inside the list.
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
    -- The break: the writer finished the stream.
    Right (_, True) -> []
    -- Not a break, so another transaction. 'decodeBreakOr' took nothing, so
    -- this decodes from where that one started.
    Right (_, False) -> case CBOR.deserialiseFromBytesWithSize txDecoder bs of
      Left err -> crash (asDecoderError err)
      Right (rest, size, mkTx) -> case mkTx (BSL.take size bs) of
        Left err -> crash err
        Right tx -> wrap (mkShelleyTx tx) : go rest

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
