{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Util.Serialisation.Roundtrip (
    -- * Basic test helpers
    roundtrip
  , roundtrip'
  , roundtripAnd
    -- * Test skeleton
  , Arbitrary'
  , Coherent (..)
  , WithVersion (..)
  , prop_hashSize
  , roundtrip_ConvertRawHash
  , roundtrip_SerialiseDisk
  , roundtrip_SerialiseNodeToClient
  , roundtrip_SerialiseNodeToNode
  , roundtrip_all
  , roundtrip_all_skipping
  , roundtrip_envelopes
    -- ** Exclusion of CBOR validity tests
  , ShouldCheckCBORValidity (CheckCBORValidity, DoNotCheckCBORValidity)
    -- * Roundtrip tests for 'Example's
  , examplesRoundtrip
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm (toFlatTerm, validFlatTerm)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (decode, encode)
import           Control.Arrow (left)
import           Control.Monad (unless, when)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Short as Short
import           Data.Constraint
import           Data.Function (on)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import           Data.Typeable
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended (decodeDiskExtLedgerState,
                     encodeDiskExtLedgerState)
import           Ouroboros.Consensus.Ledger.Query
import qualified Ouroboros.Consensus.Ledger.Query as Query
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints,
                     SerialiseNodeToNodeConstraints (..))
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Network.Block (Serialised (..), fromSerialised,
                     mkSerialised)
import           Quiet (Quiet (..))
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Examples (Examples (..), Labelled)
import           Test.Util.Serialisation.SomeResult (SomeResult (..))
import           Test.Util.TestEnv (adjustQuickCheckTests)
import           Text.Pretty.Simple (pShow)

{------------------------------------------------------------------------------
  Basic test helpers
------------------------------------------------------------------------------}

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Perform roundtrip tests, checking the validity of the encoded CBOR.
--
-- See 'roundtripAnd'
--
roundtrip' :: forall a.
              (Eq a, Show a)
           => (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' = roundtripAnd CheckCBORValidity

data ShouldCheckCBORValidity = CheckCBORValidity | DoNotCheckCBORValidity
  deriving (Eq, Show)

-- | Roundtrip property for values annotated with their serialized form
--
-- If 'CheckCBORValidity' is passed, then we check that the encoded
-- CBOR is valid using 'validFlatTerm'. In general we want to check
-- this, however there are cases where legacy encoders do not produce
-- valid CBOR but we need to keep them for backwards compatibility. In
-- such cases, the option to skip this check
-- ('DoNotCheckCBORValidity') can be used.
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtripAnd :: forall a.
              (Eq a, Show a)
           => ShouldCheckCBORValidity
           -> (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtripAnd check enc dec a = checkRoundtripResult $ do
    let enc_a = enc a
        bs    = toLazyByteString enc_a

    when (check == CheckCBORValidity) $
      (validFlatTerm (toFlatTerm enc_a)          ?!       "Encoded flat term is not valid: " <> show enc_a)
    (bsRem, a' ) <- deserialiseFromBytes dec bs `onError` showByteString bs
    Lazy.null bsRem                              ?!       "Left-over bytes: " <> toBase16 bsRem
    a == a' bs                                   ?!        pShowNeq a (a' bs)
  where
    (?!) :: Bool -> String -> Either String ()
    cond ?! msg = unless cond $ Left msg
    infix 1 ?!

    pShowNeq x y = T.unpack (pShow x) <> "\n \t/= \n"  <> T.unpack (pShow y)

    onError ::
          Either DeserialiseFailure (Char8.ByteString, Char8.ByteString -> a)
      -> (DeserialiseFailure -> String)
      -> Either String (Char8.ByteString, Char8.ByteString -> a)
    onError result showDeserialiseFailure =
      left showDeserialiseFailure result

    showByteString ::
         Char8.ByteString
      -> DeserialiseFailure
      -> String
    showByteString bs deserialiseFailure =
      show deserialiseFailure <> "\n" <> "When deserialising " <> toBase16 bs

    toBase16 :: Lazy.ByteString -> String
    toBase16 = Char8.unpack . Base16.encode

    checkRoundtripResult :: Either String () -> Property
    checkRoundtripResult (Left str) = counterexample str False
    checkRoundtripResult (Right ()) = property ()

roundtripComparingEncoding ::
     (a -> Encoding)
  -> (forall s. Decoder s a)
  -> a
  -> Property
roundtripComparingEncoding enc dec = roundtripComparingEncoding' enc (const <$> dec)

-- | Like 'roundtrip'', but checks for equality of the encoding (i.e. the byte
-- string) instead of the @a@ values using @Eq a@. This is useful When we don't
-- have an @Eq a@ instance.
roundtripComparingEncoding' ::
    (a -> Encoding)  -- ^ @enc@
  -> (forall s. Decoder s (Lazy.ByteString -> a))
  -> a
  -> Property
roundtripComparingEncoding' enc dec a = case deserialiseFromBytes dec bs of
    Right (remainingBytes, a')
      | let bs' = toLazyByteString (enc (a' bs))
      , Lazy.null remainingBytes
      -> bs === bs'
      | otherwise
      -> counterexample ("left-over bytes: " <> toBase16 remainingBytes) False
    Left e
      -> counterexample (show e) $
         counterexample (toBase16 bs) False
  where
    bs = toLazyByteString (enc a)

    toBase16 :: Lazy.ByteString -> String
    toBase16 = Char8.unpack . Base16.encode

{------------------------------------------------------------------------------
  Test skeleton
------------------------------------------------------------------------------}

-- | Constraints needed in practice for something to be passed in as an
-- 'Arbitrary' argument to a QuickCheck property.
type Arbitrary' a = (Arbitrary a, Eq a, Show a)

-- | All roundtrip tests
roundtrip_all
  :: forall blk.
     ( SerialiseDiskConstraints         blk
     , SerialiseNodeToNodeConstraints   blk
     , SerialiseNodeToClientConstraints blk

     , Show (BlockNodeToNodeVersion   blk)

     , StandardHash blk
     , GetHeader    blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (HeaderHash blk)
     , Arbitrary' (LedgerState blk EmptyMK)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ChainDepState (BlockProtocol blk))

     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Coherent blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (SomeSecond (NestedCtxt Header) blk)

     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeBlockQuery (BlockQuery blk))
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)
     , Arbitrary (WithVersion (BlockNodeToClientVersion blk) (LedgerConfig blk))
     , ArbitraryWithVersion (QueryVersion, BlockNodeToClientVersion blk) (SomeSecond Query blk)

     , Show (BlockNodeToClientVersion blk)
     , BlockSupportsLedgerQuery blk
     )
  => CodecConfig blk
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> TestTree
roundtrip_all = roundtrip_all_skipping (const CheckCBORValidity)

-- | All roundtrip tests, skipping the specified CBOR validity tests.
--
-- TODO: the exclusion rule should only be considered for blocks __before__ Conway!
--
-- The 'TestName' corresponds to the name of the roundtrip property
-- being tested. At the moment we consider for exclusion:
--
-- - Node to client tests due to
--   [this issue](https://github.com/IntersectMBO/cardano-ledger/issues/3800).
--
roundtrip_all_skipping
  :: forall blk.
     ( SerialiseDiskConstraints         blk
     , SerialiseNodeToNodeConstraints   blk
     , SerialiseNodeToClientConstraints blk

     , Show (BlockNodeToNodeVersion   blk)

     , StandardHash blk
     , GetHeader    blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (HeaderHash blk)
     , Arbitrary' (LedgerState blk EmptyMK)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ChainDepState (BlockProtocol blk))

     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Coherent blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (SomeSecond (NestedCtxt Header) blk)

     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeBlockQuery (BlockQuery blk))
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)
     , Arbitrary (WithVersion (BlockNodeToClientVersion blk) (LedgerConfig blk))
     , ArbitraryWithVersion (QueryVersion, BlockNodeToClientVersion blk) (SomeSecond Query blk)

     , Show (BlockNodeToClientVersion blk)
     , BlockSupportsLedgerQuery blk
     )
  => (TestName -> ShouldCheckCBORValidity)
  -> CodecConfig blk
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> TestTree
roundtrip_all_skipping shouldCheckCBORvalidity ccfg dictNestedHdr =
    testGroup "Roundtrip" [
        testGroup "SerialiseDisk"         $ roundtrip_SerialiseDisk         ccfg dictNestedHdr
      , testGroup "SerialiseNodeToNode"   $ roundtrip_SerialiseNodeToNode   ccfg
      , testGroup "SerialiseNodeToClient" $ roundtrip_SerialiseNodeToClient
                                                    shouldCheckCBORvalidity ccfg
      , testProperty "envelopes"          $ roundtrip_envelopes             ccfg
      , testProperty "ConvertRawHash"     $ roundtrip_ConvertRawHash        (Proxy @blk)
      , testProperty "hashSize"           $ prop_hashSize                   (Proxy @blk)
      , testProperty "estimateBlockSize"  $ prop_estimateBlockSize          ccfg
      ]

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseDiskConstraints'?
roundtrip_SerialiseDisk
  :: forall blk.
     ( SerialiseDiskConstraints blk
     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (LedgerState blk EmptyMK)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ChainDepState (BlockProtocol blk))
     )
  => CodecConfig blk
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> [TestTree]
roundtrip_SerialiseDisk ccfg dictNestedHdr =
    [ testProperty "roundtrip block" $
        roundtrip' @blk (encodeDisk ccfg) (decodeDisk ccfg)
    , testProperty "roundtrip Header" $ \hdr ->
        case unnest hdr of
          DepPair ctxt nestedHdr -> case dictNestedHdr (flipNestedCtxt ctxt) of
            Dict ->
              roundtrip'
                (encodeDiskDep ccfg ctxt)
                (decodeDiskDep ccfg ctxt)
                nestedHdr
      -- Since the 'LedgerState' is a large data structure, we lower the
      -- number of tests to avoid slowing down the testsuite too much
    , adjustQuickCheckTests (`div` 10) $
      rt (Proxy @(LedgerState blk EmptyMK)) "LedgerState"
    , rt (Proxy @(AnnTip blk)) "AnnTip"
    , rt (Proxy @(ChainDepState (BlockProtocol blk))) "ChainDepState"
    ]
  where
    rt :: forall a. (Arbitrary' a, EncodeDisk blk a, DecodeDisk blk a)
       => Proxy a -> String -> TestTree
    rt _ name =
      testProperty ("roundtrip " <> name) $
        roundtrip @a
          (encodeDisk ccfg)
          (decodeDisk ccfg)

-- | Used to generate arbitrary values for the serialisation roundtrip tests.
-- As the serialisation format can change with the version, not all arbitrary
-- values of the type might be valid for each version.
--
-- For example, a certain constructor can only be used after a certain version
-- and can thus not be generated for any prior versions.
data WithVersion v a = WithVersion v a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Similar to @Arbitrary'@, but with an 'Arbitrary' instasnce for
-- @('WithVersion' v a)@.
type ArbitraryWithVersion v a = (Arbitrary (WithVersion v a), Eq a, Show a)

instance ( blockVersion ~ BlockNodeToClientVersion blk
         , Arbitrary blockVersion
         , Arbitrary (WithVersion (BlockNodeToClientVersion blk) (SomeBlockQuery (BlockQuery blk)))
         )
      => Arbitrary (WithVersion (QueryVersion, blockVersion) (SomeSecond Query blk)) where
  arbitrary = do
    queryVersion <- arbitrary
    case queryVersion of
      -- This case statement will cause a warning when we add a new top
      -- level query and hence a new QueryVersion. In that case we should
      -- support such top level `Query` constructors in this Arbitrary instance.
      Query.QueryVersion1 -> genTopLevelQuery1
      Query.QueryVersion2 -> genTopLevelQuery2
      Query.QueryVersion3 -> genTopLevelQuery3
    where
      mkEntry ::
           QueryVersion
        -> Query blk query
        -> Gen
            (WithVersion (QueryVersion, blockVersion) (SomeSecond Query blk))
      mkEntry qv q = do
        blockV <- arbitrary
        return (WithVersion (qv, blockV) (SomeSecond q))

      genTopLevelQuery1 =
        let version = Query.QueryVersion1
        in  frequency
              [ (15, arbitraryBlockQuery version    )
              , (1 , mkEntry version GetSystemStart )
              ]

      genTopLevelQuery2 =
        let version = Query.QueryVersion2
        in  frequency
              [ (15, arbitraryBlockQuery version    )
              , (1 , mkEntry version GetSystemStart )
              , (1 , mkEntry version GetChainBlockNo)
              , (1 , mkEntry version GetChainPoint  )
              ]

      genTopLevelQuery3 =
        let version = Query.QueryVersion3
        in  frequency
              [ (15, arbitraryBlockQuery version      )
              , (1 , mkEntry version GetSystemStart   )
              , (1 , mkEntry version GetChainBlockNo  )
              , (1 , mkEntry version GetChainPoint    )
              , (1 , mkEntry version DebugLedgerConfig)
              ]

      arbitraryBlockQuery :: QueryVersion
                          -> Gen (WithVersion (QueryVersion, blockVersion)
                                              (SomeSecond Query blk))
      arbitraryBlockQuery queryVersion = do
        WithVersion blockV (SomeBlockQuery someBlockQuery) <- arbitrary
        return (WithVersion (queryVersion, blockV)
                            (SomeSecond (BlockQuery someBlockQuery)))

-- | This is @OVERLAPPABLE@ because we have to override the default behaviour
-- for e.g. 'Query's.
instance {-# OVERLAPPABLE #-} (Arbitrary version, Arbitrary a)
      => Arbitrary (WithVersion version a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | Used to generate slightly less arbitrary values
--
-- Like some other QuickCheck modifiers, the exact meaning is
-- context-dependent. The original motivating example is that some of our
-- serialization-adjacent properties require that the generated block contains
-- a header and a body that match, ie are /coherent/.
newtype Coherent a = Coherent { getCoherent :: a }
  deriving (Eq, Generic)
  deriving (Show) via (Quiet (Coherent a))

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToNodeConstraints'?
roundtrip_SerialiseNodeToNode
  :: forall blk.
     ( SerialiseNodeToNodeConstraints blk
     , Show (BlockNodeToNodeVersion blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)

       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
       -- Needed for testing the @Serialised (Header blk)@
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     , DecodeDiskDep (NestedCtxt Header) blk
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToNode ccfg =
    [ rt (Proxy @blk)              "blk"
    , rt (Proxy @(Header blk))     "Header"
    , rt (Proxy @(GenTx blk))      "GenTx"
    , rt (Proxy @(GenTxId blk))    "GenTxId"
      -- Roundtrip a @'Serialised' blk@
      --
      -- We generate a random @blk@, convert it to 'Serialised' (using
      -- 'encodeDisk', which doesn't add CBOR-in-CBOR), encode it (adding
      -- CBOR-in-CBOR), decode that 'Serialised' and convert (using
      -- 'decodeNodeToNode') it to a @blk@ again.
    , testProperty "roundtrip Serialised blk" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            blk
      -- Same as above but for 'Header'
    , testProperty "roundtrip Serialised Header" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (enc version . SerialisedHeaderFromDepPair . encodeDepPair ccfg . unnest)
            (nest <$> (decodeDepPair ccfg . serialisedHeaderToDepPair =<< dec version))
            hdr
      -- Check the compatibility between 'encodeNodeToNode' for @'Serialised'
      -- blk@ and 'decodeNodeToNode' for @blk@.
    , testProperty "roundtrip Serialised blk compat 1" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (dec version)
            blk
      -- Check the compatibility between 'encodeNodeToNode' for @blk@ and
      -- 'decodeNodeToNode' for @'Serialised' blk@.
    , testProperty "roundtrip Serialised blk compat 2" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (enc version)
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            blk
      -- Same as above but for 'Header'
    , testProperty "roundtrip Serialised Header compat 1" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (enc version . SerialisedHeaderFromDepPair . encodeDepPair ccfg . unnest)
            (dec version)
            hdr
    , testProperty "roundtrip Serialised Header compat 2" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (enc version)
            (nest <$> (decodeDepPair ccfg . serialisedHeaderToDepPair =<< dec version))
            hdr
    ]
  where
    enc :: SerialiseNodeToNode blk a
        => BlockNodeToNodeVersion blk -> a -> Encoding
    enc = encodeNodeToNode ccfg

    dec :: SerialiseNodeToNode blk a
        => BlockNodeToNodeVersion blk -> forall s. Decoder s a
    dec = decodeNodeToNode ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (BlockNodeToNodeVersion blk) a)
         , Eq a
         , Show a
         , SerialiseNodeToNode blk a
         )
       => Proxy a -> String -> TestTree
    rt _ name =
      testProperty ("roundtrip " <> name) $ \(WithVersion version x) ->
        roundtrip @a (enc version) (dec version) x

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToClientConstraints'?
roundtrip_SerialiseNodeToClient
  :: forall blk.
     ( SerialiseNodeToClientConstraints blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeBlockQuery (BlockQuery blk))
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)
     , Arbitrary (WithVersion (BlockNodeToClientVersion blk) (LedgerConfig blk))
     , ArbitraryWithVersion (QueryVersion, BlockNodeToClientVersion blk) (SomeSecond Query blk)

     , Show (BlockNodeToClientVersion blk)
     , BlockSupportsLedgerQuery blk
       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     )
  => (TestName -> ShouldCheckCBORValidity)
  -> CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToClient shouldCheckCBORvalidity ccfg =
    [ rt (Proxy @blk)                         "blk"
    , rt (Proxy @(GenTx blk))                 "GenTx"
    , rt (Proxy @(GenTxId blk))               "GenTxId"
    , rt (Proxy @(ApplyTxErr blk))            "ApplyTxErr"
    , rt (Proxy @(SomeBlockQuery (BlockQuery blk))) "BlockQuery"
    -- Note: Ideally we'd just use 'rt' to test Ledger config, but that would
    -- require an 'Eq' and 'Show' instance for all ledger config types which
    -- we'd like to avoid (as the EpochInfo is a record of functions).
    , testProperty "roundtrip (comparing encoding) LedgerConfig" $
        withMaxSuccess 20 $ \(Blind (WithVersion version a)) ->
          roundtripComparingEncoding @(LedgerConfig blk) (enc version) (dec version) a
    , rtWith
        @(SomeSecond Query blk)
        @(QueryVersion, BlockNodeToClientVersion blk)
        (\(queryVersion, blockVersion) query -> Query.queryEncodeNodeToClient
                          ccfg
                          queryVersion
                          blockVersion
                          query
        )
        (\(queryVersion, blockVersion) -> Query.queryDecodeNodeToClient
                          ccfg
                          queryVersion
                          blockVersion
        )
        "Query"
      -- See roundtrip_SerialiseNodeToNode for more info
    , let testLabel = "roundtrip Serialised blk" in
        testProperty testLabel $
          \(WithVersion version blk) ->
            roundtripAnd @blk
              (shouldCheckCBORvalidity testLabel)
              (encodeThroughSerialised (encodeDisk ccfg) (enc version))
              (const <$> decodeThroughSerialised (decodeDisk ccfg) (dec version))
              blk
      -- See roundtrip_SerialiseNodeToNode for more info
    , let testLabel = "roundtrip Serialised blk compat" in
        testProperty testLabel $
          \(WithVersion version blk) ->
            roundtripAnd @blk
              (shouldCheckCBORvalidity testLabel)
              (encodeThroughSerialised (encodeDisk ccfg) (enc version))
              (const <$> dec version)
              blk
    , let testLabel = "roundtrip Result" in
        testProperty testLabel $
          \(WithVersion version (SomeResult query result :: SomeResult blk)) ->
            roundtripAnd
              (shouldCheckCBORvalidity testLabel)
              (encodeBlockQueryResult ccfg version query)
              (const <$> decodeBlockQueryResult ccfg version query)
              result
    ]
  where
    enc :: SerialiseNodeToClient blk a
        => BlockNodeToClientVersion blk -> a -> Encoding
    enc = encodeNodeToClient ccfg

    dec :: SerialiseNodeToClient blk a
        => BlockNodeToClientVersion blk -> forall s. Decoder s a
    dec = decodeNodeToClient ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (BlockNodeToClientVersion blk) a)
         , Eq a
         , Show a
         , SerialiseNodeToClient blk a
         )
       => Proxy a -> String -> TestTree
    rt _ name = rtWith (enc @a) (dec @a) name

    rtWith
      :: forall a version.
         ( Arbitrary (WithVersion version a)
         , Eq a
         , Show a
         , Show version
         )
       => (version -> a -> Encoding)
       -> (version -> forall s. Decoder s a)
       -> String
       -> TestTree
    rtWith enc' dec' name =
        testProperty ("roundtrip " <> name) $
          \(WithVersion version a) ->
            roundtripAnd @a (shouldCheckCBORvalidity testLabel)
                            (enc' version)
                            (const <$> dec' version)
                            a
      where
        testLabel = "roundtrip " <> name

{-------------------------------------------------------------------------------
  Checking envelopes
-------------------------------------------------------------------------------}

-- | This is similar to the roundtrip tests for headers, except we don't
-- start with a header but some fixed bytestring in the payload. This makes
-- debugging a bit easier as we can focus on just the envelope.
roundtrip_envelopes ::
     forall blk. (
       SerialiseNodeToNode blk (SerialisedHeader blk)
     , HasNestedContent Header blk
     )
  => CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) (SomeSecond (NestedCtxt Header) blk)
  -> Property
roundtrip_envelopes ccfg (WithVersion v (SomeSecond ctxt)) =
    roundtrip
      (encodeNodeToNode ccfg v . unBase16)
      (Base16 <$> decodeNodeToNode ccfg v)
      (Base16 serialisedHeader)
  where
    serialisedHeader :: SerialisedHeader blk
    serialisedHeader = SerialisedHeaderFromDepPair $
        GenDepPair ctxt (Serialised bs)

    bs :: Lazy.ByteString
    bs = "<PAYLOAD>" -- Something we can easily recognize in test failures

newtype Base16 a = Base16 { unBase16 :: a }

instance HasNestedContent Header blk => Show (Base16 (SerialisedHeader blk)) where
  show = aux . serialisedHeaderToDepPair . unBase16
    where
      aux :: GenDepPair Serialised (NestedCtxt Header blk) -> String
      aux (GenDepPair ctxt (Serialised bs)) =
          "(" <> show ctxt <> "," <> Char8.unpack (Base16.encode bs) <> ")"

instance HasNestedContent Header blk => Eq (Base16 (SerialisedHeader blk)) where
  (==) = aux `on` (serialisedHeaderToDepPair . unBase16)
    where
      aux :: GenDepPair Serialised (NestedCtxt Header blk)
          -> GenDepPair Serialised (NestedCtxt Header blk)
          -> Bool
      aux (GenDepPair ctxt bs) (GenDepPair ctxt' bs') =
          case sameDepIndex ctxt ctxt' of
            Just Refl -> bs == bs'
            Nothing   -> False

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

roundtrip_ConvertRawHash
  :: (StandardHash blk, ConvertRawHash blk)
  => Proxy blk -> HeaderHash blk -> Property
roundtrip_ConvertRawHash p h =
    h === fromShortRawHash p (toShortRawHash p h)

prop_hashSize
  :: ConvertRawHash blk
  => Proxy blk -> HeaderHash blk -> Property
prop_hashSize p h =
    hashSize p === fromIntegral (Short.length (toShortRawHash p h))

{-------------------------------------------------------------------------------
  estimateBlockSize
-------------------------------------------------------------------------------}

prop_estimateBlockSize ::
     (SerialiseNodeToNodeConstraints blk, GetHeader blk)
  => CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) (Coherent blk)
  -> Property
prop_estimateBlockSize ccfg (WithVersion version (Coherent blk))
  | actualBlockSize > expectedBlockSize
  = counterexample
      ("actualBlockSize > expectedBlockSize: "
         <> show actualBlockSize <> " > "
         <> show expectedBlockSize)
      (property False)
  | actualBlockSize < expectedBlockSize - allowedOverestimate
  = counterexample
      ("actualBlockSize < expectedBlockSize - allowedOverestimate: "
         <> show actualBlockSize <> " > "
         <> show expectedBlockSize <> " - "
         <> show allowedOverestimate)
      (property False)
  | otherwise
  = classify (actualBlockSize == expectedBlockSize) "exact"
  $ classify (actualBlockSize <  expectedBlockSize) "overestimate"
  $ property True
  where
    allowedOverestimate :: SizeInBytes
    allowedOverestimate = 10

    actualBlockSize :: SizeInBytes
    actualBlockSize =
          fromIntegral
        . Lazy.length
        . toLazyByteString
        . encodeNodeToNode ccfg version
        $ blk

    expectedBlockSize :: SizeInBytes
    expectedBlockSize =
          estimateBlockSize
        . getHeader
        $ blk

{-------------------------------------------------------------------------------
  Serialised helpers
-------------------------------------------------------------------------------}

encodeThroughSerialised ::
     (a -> Encoding)
  -> (Serialised a -> Encoding)
  -> (a -> Encoding)
encodeThroughSerialised enc encSerialised = encSerialised . mkSerialised enc

decodeThroughSerialised ::
     (forall s. Decoder s (Lazy.ByteString -> a))
  -> (forall s. Decoder s (Serialised a))
  -> (forall s. Decoder s a)
decodeThroughSerialised dec decSerialised = do
    serialised <- decSerialised
    fromSerialised dec serialised

{------------------------------------------------------------------------------
  Roundtrip tests for examples
------------------------------------------------------------------------------}

examplesRoundtrip ::
     forall blk . (SerialiseDiskConstraints blk, Eq blk, Show blk, LedgerSupportsProtocol blk)
  => CodecConfig blk
  -> Examples blk
  -> [TestTree]
examplesRoundtrip codecConfig examples =
    [ testRoundtripFor "Block"                 (encodeDisk codecConfig) (decodeDisk codecConfig)           exampleBlock
    , testRoundtripFor "Header hash"           encode                   (const <$> decode)                 exampleHeaderHash
    , testRoundtripFor "Ledger state"          (encodeDisk codecConfig) (const <$> decodeDisk codecConfig) exampleLedgerState
    , testRoundtripFor "Annotated tip"         (encodeDisk codecConfig) (const <$> decodeDisk codecConfig) exampleAnnTip
    , testRoundtripFor "Chain dependent state" (encodeDisk codecConfig) (const <$> decodeDisk codecConfig) exampleChainDepState
    , testRoundtripFor "Extended ledger state" (encodeDiskExtLedgerState codecConfig) (const <$> decodeDiskExtLedgerState codecConfig)  exampleExtLedgerState
    ]
  where
    testRoundtripFor ::
         forall a . (Eq a, Show a)
      => String
      -> (a -> Encoding)
      -> (forall s . Decoder s (Char8.ByteString -> a))
      -> (Examples blk -> Labelled a)
      -> TestTree
    testRoundtripFor testLabel enc dec field =
        testGroup testLabel
          [ mkTest exampleName example
          | (exampleName, example) <- field examples
          ]
      where
        mkTest exampleName example =
          testProperty (fromMaybe "" exampleName)
              $ once
              $ roundtrip' enc dec example
