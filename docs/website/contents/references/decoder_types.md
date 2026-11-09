# How decoders are specified in the `ouroboros-consensus` repository

The following excerpts and discussions explain how Consensus defines its Node-to-Node codecs.
It focuses on decoding, since that's the more interesting/challenging part.
Because Cardano avoids requiring canonical CBOR, encoding Node-to-Node messages has a simple rule: relay something using the same bytes the original issuer did (and so the same bytes you received), to preserve the hash.

```
$ date
Mon Nov  9 12:32:44 PM EST 2026

$ cd ../ouroboros-consensus

$ git rev-parse origin/main
932b114020df56dad796832c99adb35682932dec

$ git show origin/main:ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Node/Serialisation.hs | tail -n +72 | head -n 5
-- | Serialise a type @a@ so that it can be sent across network via a
-- node-to-node protocol.
class SerialiseNodeToNode blk a where
  encodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  decodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a

$ git grep -h -e 'SerialiseNodeToNode[^C].*Shelley' origin/main -- '*hs'
  SerialiseNodeToNode (ShelleyBlock proto era) (ShelleyBlock proto era)
instance SerialiseNodeToNode (ShelleyBlock proto era) (Serialised (ShelleyBlock proto era))
  SerialiseNodeToNode (ShelleyBlock proto era) (Header (ShelleyBlock proto era))
instance SerialiseNodeToNode (ShelleyBlock proto era) (SerialisedHeader (ShelleyBlock proto era)) where
  SerialiseNodeToNode (ShelleyBlock proto era) (GenTx (ShelleyBlock proto era))
  SerialiseNodeToNode (ShelleyBlock proto era) (GenTxId (ShelleyBlock proto era))
```

Those six instances control how received bytes are decoded.

```
┌───────────────────────────────────┬────────────────────────────────────────────────────────┬──────────────────────────┐
│             Instance              │                        Decoder                         │      Ledger type         │
│  (Shelley/Node/Serialisation.hs)  │                                                        │                          │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ ShelleyBlock :203                 │ unwrapCBORinCBOR decodeShelleyBlock                    │ Annotator (Block h era)  │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ Serialised block :209             │ instance Serialise (Serialised a)                      │ none                     │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ Header :216                       │ unwrapCBORinCBOR ((Right .) <$> decodeShelleyHeader)   │ Annotator (Header c)     │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ SerialisedHeader :222             │ instance Serialise (Serialised a)                      │ none                     │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ GenTx :230                        │ fromCBOR → Mempool.hs:290: unwrapCBORinCBOR $          │ Annotator (Tx TopTx era) │
│                                   │ eraDecoder @era $ (. Full) . runAnnotator <$> decCBOR  │                          │
├───────────────────────────────────┼────────────────────────────────────────────────────────┼──────────────────────────┤
│ GenTxId :237                      │ fromEraCBOR @era, which is eraDecoder @era decCBOR     │ plain TxId               │
└───────────────────────────────────┴────────────────────────────────────────────────────────┴──────────────────────────┘
```

The following details elaborate a bit on how the "Decoder" column delegates to the "Ledger type" column.

```
$ git grep -h -A10 -E -e ^decodeShelleyBlock -e ^decodeShelleyHeader origin/main -- '*hs' | head -n8
decodeShelleyBlock ::
  forall proto era.
  ShelleyCompatible proto era =>
  forall s.
  Plain.Decoder s (Lazy.ByteString -> Either Plain.DecoderError (ShelleyBlock proto era))
decodeShelleyBlock =
  eraDecoder @era $
    (. Full) . runAnnotator <$> decCBOR

$ git grep -h -A10 -E -e ^decodeShelleyHeader origin/main -- '*hs' | head -n11
decodeShelleyHeader ::
  forall proto era.
  ShelleyCompatible proto era =>
  forall s.
  Plain.Decoder s (Lazy.ByteString -> Header (ShelleyBlock proto era))
decodeShelleyHeader =
  eraDecoder @era $
    (. Full)
      . (either (\e -> error ("Impossible, header decoder failed: " <> show e)) id .)
      . runAnnotator
      <$> decCBOR

$ git grep -h -A2 -e 'instance.*DecCBOR (Annotator.*Shelley' origin/main -- '*hs'
instance ShelleyCompatible proto era => DecCBOR (Annotator (ShelleyBlock proto era)) where
  decCBOR = fmap mkShelleyBlock <$> decCBOR

--
instance ShelleyCompatible proto era => DecCBOR (Annotator (Header (ShelleyBlock proto era))) where
  decCBOR = fmap mkShelleyHeader <$> decCBOR
```

## The corresponding interface from the `cardano-ledger` repository

`libs/cardano-ledger-core/src/Cardano/Ledger/Core/Era.hs` defines

```
eraDecoder :: forall era t s. Era era => Decoder s t -> Plain.Decoder s t
eraDecoder = toPlainDecoder Nothing (eraProtVerLow @era)

eraDecoderWithBytes :: forall era t s. Era era => BSL.ByteString -> Decoder s t -> Plain.Decoder s t
eraDecoderWithBytes bsl = toPlainDecoder (Just bsl) (eraProtVerLow @era)
```

Note that `eraDecoderWithBytes` does not seem to occur anywhere in the Cardano code base.

`libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Decoding/Decoder.hs` defines

```
newtype Decoder s a = Decoder
  { runDecoder :: Maybe BSL.ByteString -> Version -> C.Decoder s a
  }
```

and

```
toPlainDecoder ::
  -- | Some decoders require the original bytes to be supplied as well. Such decoders will
  -- fail whenever `Nothing` is supplied.
  Maybe BSL.ByteString ->
  Version ->
  Decoder s a ->
  C.Decoder s a
```

where the `C` import qualifier refers to the `cborg` library

Note again that Consensus's Node-to-Node decoders are always passing `Nothing`.
That's because of the decoding approach explained here https://github.com/IntersectMBO/cardano-ledger/blob/7827506b7518284d7f9dbbe3502a20431e513fe9/libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Decoding.hs#L242-L272.
Thus, these decoders happen in two passes.
The first pass decodes the bytes into a function that awaits the original bytes.
The second pass is the execution of that function.
It's not easy to determine which pass allocates which parts of the ultimate result.
Note that the first pass must allocate lambdas.
Also note that the second pass cannot necessarily begin until the first pass has finished, otherwise it might not know which prefix of the incoming bytes to provide to the second pass.

`libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Decoding/Annotated.hs` defines

```
newtype Annotator a = Annotator {runAnnotator :: FullByteString -> Either DecoderError a}
```

where `FullByteString` is just a newtype around `ByteString` emphasizing that it's all the bytes fed in until the whole decoder returned.

## The corresponding interface to the `ouroboros-network` repository

`ouroboros-network/api/lib/Ouroboros/Network/Block.hs` defines

```
newtype Serialised a = Serialised { unSerialised :: Lazy.ByteString }
```

and

```
instance Serialise (Serialised a)
```

(which only decodes the `ByteString` not the `a`: note the empty instance context)

and

```
unwrapCBORinCBOR :: (forall s. Decoder s (Lazy.ByteString -> Either DecoderError a))
                 -> (forall s. Decoder s a)
unwrapCBORinCBOR dec = fromSerialised dec =<< decode

fromSerialised :: (forall s. Decoder s (Lazy.ByteString -> Either DecoderError a))
               -> Serialised a -> (forall s. Decoder s a)
```

and

```
wrapCBORinCBOR :: (a -> Encoding) -> a -> Encoding
wrapCBORinCBOR enc = encode . mkSerialised enc

mkSerialised :: (a -> Encoding) -> a -> Serialised a
mkSerialised enc = Serialised . Write.toLazyByteString . enc
```

## The _alternative_ interface to the `typed-protocols` repository

As of mid 2025, there is another option that supports the above two-pass decoding without requiring CBOR-in-CBOR.
In particular, the first pass could be incremental; the type-specific decoding would happen as each chunk of bytes arrive instead of only once the last byte arrived.
And, as such, it would _force_ the type-specific decoding to happen as the message arrives, instead of possibly deferring like `instance Serialise (Serialised a)` does but `unwrapCBORinCBOR` does not.
Dually, `wrapCBORinCBOR` would be replaced by `encodePreEncoded`.

This has not yet been integrated into the mini protocol design.
It was for MsgLeiosBlock, but that is being reverted; see PR https://github.com/IntersectMBO/ouroboros-consensus/pull/2495.
If we do minimize the use of CBOR-in-CBOR/similar, it'll happen as part of a concerted effort (likely motivated by incremental first-pass decoding) rather than as a coincidental byproduct of introducing Leios.
Note that, in that case, MsgLeiosBlockTxs will instead stay CBOR-in-CBOR (or `bytes`), since LeiosFetch does not decode transactions; it's useful to avoid the decoding work until LeiosVoting or ChainSel need it, if they ever do.

`typed-protocols/src/Network/TypedProtocol/Codec.hs` defines

```
data CodecF ps failure m (f :: ps -> Type) bytes = Codec {
       encode :: forall (st :: ps) (st' :: ps).
                 StateTokenI st
              => ActiveState st
              => Message ps st st'
              -> bytes,
       -- ^ encode a message into `bytes`

       decode :: forall (st :: ps).
                 ActiveState st
              => StateToken st
              -- evidence for an active state
              -> m (DecodeStep bytes failure m (f st))
       -- ^ decode a message knowing that the current state is `st`
     }
```

and

```
data DecodeStep bytes failure m a =

    -- | The decoder has consumed the available input and needs more
    -- to continue. Provide @'Just'@ if more input is available and
    -- @'Nothing'@ otherwise, and you will get a new @'DecodeStep'@.
    DecodePartial (Maybe bytes -> m (DecodeStep bytes failure m a))

    -- | The decoder has successfully finished. This provides the decoded
    -- result value plus any unused input.
  | DecodeDone a (Maybe bytes)
```

and

```
type AnnotatedCodec ps failure m bytes = CodecF ps failure m (Annotator bytes) bytes
```

and

```
newtype Annotator bytes st = Annotator { runAnnotator :: bytes -> SomeMessage st }
```

Note that in an `AnnotatedCodec` the `DecodeStep`'s `a` is ultimately `Annotator bytes st`.

`typed-protocols/cborg/Network/TypedProtocol/Codec/CBOR.hs` defines

```
mkCodecCborLazyBS
  :: forall ps m f. MonadST m

  => (forall (st :: ps) (st' :: ps).
             StateTokenI st
          => ActiveState st
          => Message ps st st' -> CBOR.Encoding)
  -- ^ cbor encoder

  -> (forall (st :: ps) s.
             ActiveState st
          => StateToken st
          -> CBOR.Decoder s (f st))
  -- ^ cbor decoder

  -> CodecF ps CBOR.DeserialiseFailure m f LBS.ByteString
```

and an analogous `mkCodecCborStrictBS`.

Note that the `CBOR` import qualifier refers to the `cborg` library.

Note that in the case of `AnnotatedCodec`, `f` here will be `Annotator LBS.ByteString`, which is very similar to the Ledger type with the same name.
