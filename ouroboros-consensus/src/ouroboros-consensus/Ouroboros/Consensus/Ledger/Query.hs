{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , BlockSupportsLedgerQuery (..)
  , ConfigSupportsNode (..)
  , Query (..)
  , QueryVersion (..)
  , ShowQuery (..)
  , answerQuery
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
    -- * Table queries
  , DiskLedgerView (..)
  , mkDiskLedgerView
    -- * Footprints
  , QueryFootprint (..)
  , Sing (SQFNoTables, SQFLookupTables, SQFTraverseTables)
  , SomeBlockQuery (..)
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise (Serialise)
import           Codec.Serialise.Class (decode, encode)
import           Control.Exception (throw)
import           Data.Kind (Type)
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Set as Set
import           Data.SOP.Strict ((:.:) (..))
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     headerStateBlockNo, headerStatePoint)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..),
                     SerialiseResult' (..))
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.API (LedgerDBView (..),
                     closeLedgerDBView)
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query as DbChangelog
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Singletons
import           Ouroboros.Network.Block (HeaderHash, Point (..), StandardHash,
                     decodePoint, encodePoint)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

{-------------------------------------------------------------------------------
  Footprints
-------------------------------------------------------------------------------}

-- | Queries on the local state might require reading ledger tables from disk.
-- This datatype (which will sometimes be concretized via @sing@) allows
-- Consensus to categorize the queries.
data QueryFootprint =
    -- | The query doesn't need ledger tables, thus can be answered only with
    -- the ledger state.
    QFNoTables
    -- | The query needs some tables, but doesn't need to traverse the whole
    -- backing store.
  | QFLookupTables
    -- | The query needs to traverse the whole backing store.
  | QFTraverseTables

data instance Sing (qf :: QueryFootprint) where
  SQFNoTables       :: Sing QFNoTables
  SQFLookupTables   :: Sing QFLookupTables
  SQFTraverseTables :: Sing QFTraverseTables

instance SingI QFNoTables where
  sing = SQFNoTables
instance SingI QFLookupTables where
  sing = SQFLookupTables
instance SingI QFTraverseTables where
  sing = SQFTraverseTables

type SomeBlockQuery :: (QueryFootprint -> Type -> Type) -> Type
data SomeBlockQuery q =
  forall footprint result. SingI footprint => SomeBlockQuery (q footprint result)

{-------------------------------------------------------------------------------
  Block Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger, indexed by the result type.
type BlockQuery :: Type -> QueryFootprint ->  Type -> Type
data family BlockQuery

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class
     -- These instances are not needed for BlockSupportsLedgerQuery but we bundle them here
     -- so that we don't need to put them in 'SingleEraBlock' later on
     (forall fp. ShowQuery (BlockQuery blk fp), SameDepIndex2 (BlockQuery blk))
  => BlockSupportsLedgerQuery blk where

  -- | Answer the given query about the extended ledger state, without reading
  -- ledger tables from the disk.
  answerPureBlockQuery ::
       ExtLedgerCfg blk
    -> BlockQuery blk QFNoTables result
    -> ExtLedgerState blk EmptyMK
    -> result

  -- | Answer a query that requires to perform a lookup on the ledger tables. As
  -- consensus always runs with a HardForkBlock, this might result in a
  -- different code path to answer a query compared to the one that a single
  -- block would take, one that is aware of the fact that the ledger tables
  -- might be HF ledger tables thus making use of some utilities to make these
  -- queries faster.
  --
  -- For the hard fork block this will be instantiated to
  -- @answerBlockQueryHFOne@.
  answerBlockQueryLookup ::
       Monad m
    => ExtLedgerCfg blk
    -> BlockQuery blk QFLookupTables result
    -> DiskLedgerView m (ExtLedgerState blk)
    -> m result

  -- | Answer a query that requires to traverse the ledger tables. As consensus
  -- always runs with a HardForkBlock, this might result in a different code
  -- path to answer a query compared to the one that a single block would take,
  -- one that is aware of the fact that the ledger tables might be HF ledger
  -- tables thus making use of some utilities to make these queries faster.
  --
  -- For the hard fork block this will be instantiated to
  -- @answerBlockQueryHFAll@.
  answerBlockQueryTraverse ::
       Monad m
    => ExtLedgerCfg blk
    -> BlockQuery blk QFTraverseTables result
    -> DiskLedgerView m (ExtLedgerState blk)
    -> m result

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

queryName :: Query blk result -> String
queryName query = case query of
  BlockQuery _    -> "BlockQuery"
  GetSystemStart  -> "GetSystemStart"
  GetChainBlockNo -> "GetChainBlockNo"
  GetChainPoint   -> "GetChainPoint"

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
--
-- Additions to the set of queries is versioned by 'QueryVersion'
type Query :: Type -> Type -> Type
data Query blk result where
  -- | This constructor is supported by all @QueryVersion@s. The @BlockQuery@
  -- argument is versioned by the @BlockNodeToClientVersion blk@.
  BlockQuery ::
    SingI footprint => BlockQuery blk footprint result -> Query blk result

  -- | Get the 'SystemStart' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion1'.
  GetSystemStart :: Query blk SystemStart

  -- | Get the 'GetChainBlockNo' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainBlockNo :: Query blk (WithOrigin BlockNo)

  -- | Get the 'GetChainPoint' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainPoint :: Query blk (Point blk)

-- | Answer the given query about the extended ledger state.
answerQuery ::
     forall blk m result.
     (BlockSupportsLedgerQuery blk, ConfigSupportsNode blk, HasAnnTip blk, Monad m)
  => ExtLedgerCfg blk
  -> DiskLedgerView m (ExtLedgerState blk)
  -> Query blk result
  -> m result
answerQuery config dlv query = case query of
    BlockQuery (blockQuery :: BlockQuery blk footprint result) ->
      case sing :: Sing footprint of
        SQFNoTables ->
          pure $ answerPureBlockQuery config blockQuery (dlvCurrent dlv)
        SQFLookupTables ->
          answerBlockQueryLookup config blockQuery dlv
        SQFTraverseTables ->
          answerBlockQueryTraverse config blockQuery dlv
    GetSystemStart ->
      pure $ getSystemStart (topLevelConfigBlock (getExtLedgerCfg config))
    GetChainBlockNo ->
      pure $ headerStateBlockNo (headerState st)
    GetChainPoint ->
      pure $ headerStatePoint (headerState st)
  where
    st = dlvCurrent dlv

{-------------------------------------------------------------------------------
  Query instances
-------------------------------------------------------------------------------}

------
-- Show
------

deriving instance
     (forall footprint result. Show (BlockQuery blk footprint result))
  => Show (SomeBlockQuery (BlockQuery blk))

deriving instance
     (forall footprint. Show (BlockQuery blk footprint result))
  => Show (Query blk result)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) =
    "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance
     (forall footprint. ShowQuery (BlockQuery blk footprint), StandardHash blk)
  => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetChainBlockNo         = show
  showResult GetChainPoint           = show

instance Show (SomeBlockQuery (BlockQuery blk)) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA))  =
    "Query " ++ show (SomeBlockQuery blockQueryA)
  show (SomeSecond GetSystemStart)            = "Query GetSystemStart"
  show (SomeSecond GetChainBlockNo)           = "Query GetChainBlockNo"
  show (SomeSecond GetChainPoint)             = "Query GetChainPoint"

------
-- Eq
------

instance SameDepIndex (Query blk) => Eq (SomeSecond Query blk) where
  SomeSecond l == SomeSecond r = isJust $ sameDepIndex l r

instance SameDepIndex2 query => Eq (SomeBlockQuery query) where
  SomeBlockQuery l == SomeBlockQuery r = isJust $ sameDepIndex2 l r

instance SameDepIndex2 (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = (\Refl -> Refl) <$> sameDepIndex2 blockQueryA blockQueryB
  sameDepIndex (BlockQuery _) _
    = Nothing
  sameDepIndex GetSystemStart GetSystemStart
    = Just Refl
  sameDepIndex GetSystemStart _
    = Nothing
  sameDepIndex GetChainBlockNo GetChainBlockNo
    = Just Refl
  sameDepIndex GetChainBlockNo _
    = Nothing
  sameDepIndex GetChainPoint GetChainPoint
    = Just Refl
  sameDepIndex GetChainPoint _
    = Nothing

------
-- Serialization
------

deriving newtype instance
     SerialiseNodeToClient blk ( SomeBlockQuery     (query blk))
  => SerialiseNodeToClient blk ((SomeBlockQuery :.: query) blk)

-- | Exception thrown in the encoders: A query was submitted that is not
-- supported by the given 'QueryVersion'
data QueryEncoderException = forall blk. Show (SomeSecond Query blk) =>
    QueryEncoderUnsupportedQuery (SomeSecond Query blk) QueryVersion

deriving instance Show QueryEncoderException
instance Show QueryEncoderException => Exception QueryEncoderException

queryEncodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
  => Show (SomeSecond Query blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> SomeSecond Query blk
  -> Encoding
queryEncodeNodeToClient codecConfig queryVersion blockVersion (SomeSecond query)
  = case query of
      BlockQuery blockQuery ->
        requireVersion QueryVersion1 $ mconcat
          [ encodeListLen 2
          , encodeWord8 0
          , encodeBlockQuery blockQuery
          ]

      GetSystemStart ->
        requireVersion QueryVersion1 $ mconcat
          [ encodeListLen 1
          , encodeWord8 1
          ]

      GetChainBlockNo ->
        requireVersion QueryVersion2 $ mconcat
          [ encodeListLen 1
          , encodeWord8 2
          ]

      GetChainPoint ->
        requireVersion QueryVersion2 $ mconcat
          [ encodeListLen 1
          , encodeWord8 3
          ]

  where
    requireVersion :: QueryVersion -> a -> a
    requireVersion expectedVersion a =
      if queryVersion >= expectedVersion
        then a
        else throw $ QueryEncoderUnsupportedQuery (SomeSecond query) queryVersion

    encodeBlockQuery ::
         SingI footprint
      => BlockQuery blk footprint result
      -> Encoding
    encodeBlockQuery blockQuery =
      encodeNodeToClient
        @blk
        @(SomeBlockQuery (BlockQuery blk))
        codecConfig
        blockVersion
        (SomeBlockQuery blockQuery)

queryDecodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeSecond Query blk)
queryDecodeNodeToClient codecConfig queryVersion blockVersion
  = case queryVersion of
      QueryVersion1 -> handleTopLevelQuery
      QueryVersion2 -> handleTopLevelQuery
  where
    handleTopLevelQuery :: Decoder s (SomeSecond Query blk)
    handleTopLevelQuery = do
        size <- decodeListLen
        tag  <- decodeWord8
        case (size, tag) of
          (2, 0) -> requireVersion QueryVersion1 =<< decodeBlockQuery
          (1, 1) -> requireVersion QueryVersion1 $ SomeSecond GetSystemStart
          (1, 2) -> requireVersion QueryVersion2 $ SomeSecond GetChainBlockNo
          (1, 3) -> requireVersion QueryVersion2 $ SomeSecond GetChainPoint
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion :: QueryVersion -> SomeSecond Query blk -> Decoder s (SomeSecond Query blk)
    requireVersion expectedVersion someSecondQuery =
      if queryVersion >= expectedVersion
        then return someSecondQuery
        else case someSecondQuery of
          SomeSecond query -> fail $ "Query: " <> queryName query <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeSecond Query blk)
    decodeBlockQuery = do
      SomeBlockQuery blockQuery <- decodeNodeToClient
        @blk
        @(SomeBlockQuery (BlockQuery blk))
        codecConfig
        blockVersion
      return (SomeSecond (BlockQuery blockQuery))

instance ( SerialiseResult' blk BlockQuery
         , Serialise (HeaderHash blk)
         ) => SerialiseResult blk Query where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeResult' codecConfig blockVersion blockQuery result
  encodeResult _ _ GetSystemStart result
    = toCBOR result
  encodeResult _ _ GetChainBlockNo result
    = toCBOR result
  encodeResult _ _ GetChainPoint result
    = encodePoint encode result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeResult' codecConfig blockVersion query
  decodeResult _ _ GetSystemStart
    = fromCBOR
  decodeResult _ _ GetChainBlockNo
    = fromCBOR
  decodeResult _ _ GetChainPoint
    = decodePoint decode

{-------------------------------------------------------------------------------
  Ledger Tables queries
-------------------------------------------------------------------------------}

data DiskLedgerView m l = DiskLedgerView {
    dlvCurrent        :: !(l EmptyMK)
  , dlvRead           :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  , dlvRangeRead      :: !(RangeQuery (LedgerTables l KeysMK)
                         -> m (LedgerTables l ValuesMK))
  , dlvClose          :: !(m ())
    -- | See 'onDiskQueryBatchSize'.
  , dlvQueryBatchSize :: !Word64
  }

mkDiskLedgerView ::
     (GetTip l, IOLike m, HasLedgerTables l)
  => LedgerDBView m l
  -> DiskLedgerView m l
mkDiskLedgerView h@(LedgerDBView lvh ldb queryBatchSize) =
    DiskLedgerView
      (DbChangelog.current ldb)
      (\ks -> do
          let rew = rewindTableKeySets ldb ks
          unfwd <- readKeySetsWith
                     lvh
                     rew
          case forwardTableKeySets ldb unfwd of
              Left _err -> error "impossible!"
              Right vs  -> pure vs
      )
      (\rq -> do
          let -- Get the differences without the keys that are greater or equal
              -- than the maximum previously seen key.
              diffs =
                maybe
                  id
                  (ltliftA2 doDropLTE)
                  (BackingStore.rqPrev rq)
                  $ ltmap prj
                  $ adcDiffs ldb
              -- (1) Ensure that we never delete everything read from disk (ie
              --     if our result is non-empty then it contains something read
              --     from disk).
              --
              -- (2) Also, read one additional key, which we will not include in
              --     the result but need in order to know which in-memory
              --     insertions to include.
              maxDeletes = ltcollapse $ ltmap (K2 . numDeletesDiffMK) diffs
              nrequested = 1 + max (BackingStore.rqCount rq) (1 + maxDeletes)
          values <- BackingStore.bsvhRangeRead lvh (rq{BackingStore.rqCount = nrequested})
          pure $ ltliftA2 (doFixupReadResult nrequested) diffs values
      )
      (closeLedgerDBView h)
      queryBatchSize
  where
    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

    -- Remove all diff elements that are <= to the greatest given key
    doDropLTE ::
         Ord k
      => KeysMK k v
      -> DiffMK k v
      -> DiffMK k v
    doDropLTE (KeysMK ks) (DiffMK ds) =
        DiffMK
      $ case Set.lookupMax ks of
          Nothing -> ds
          Just k  -> Diff.filterOnlyKey (> k) ds

    -- NOTE: this is counting the deletions wrt disk.
    numDeletesDiffMK :: DiffMK k v -> Int
    numDeletesDiffMK (DiffMK d) =
      getSum $ Diff.foldMapDelta (Sum . oneIfDel) d
      where
        oneIfDel x = case x of
          Diff.Delete _ -> 1
          Diff.Insert _ -> 0

    -- INVARIANT: nrequested > 0
    --
    -- (1) if we reached the end of the store, then simply yield the given diff
    --     applied to the given values
    -- (2) otherwise, the readset must be non-empty, since 'rqCount' is positive
    -- (3) remove the greatest read key
    -- (4) remove all diff elements that are >= the greatest read key
    -- (5) apply the remaining diff
    -- (6) (the greatest read key will be the first fetched if the yield of this
    --     result is next passed as 'rqPrev')
    --
    -- Note that if the in-memory changelog contains the greatest key, then
    -- we'll return that in step (1) above, in which case the next passed
    -- 'rqPrev' will contain it, which will cause 'doDropLTE' to result in an
    -- empty diff, which will result in an entirely empty range query result,
    -- which is the termination case.
    doFixupReadResult ::
         Ord k
      => Int
      -- ^ Number of requested keys from the backing store.
      -> DiffMK   k v
      -- ^ Differences that will be applied to the values read from the backing
      -- store.
      -> ValuesMK k v
      -- ^ Values read from the backing store. The number of values read should
      -- be at most @nrequested@.
      -> ValuesMK k v
    doFixupReadResult
      nrequested
      (DiffMK ds)
      (ValuesMK vs) =
        let includingAllKeys        =
              Diff.applyDiff vs ds
            definitelyNoMoreToFetch = Map.size vs < nrequested
        in
        ValuesMK
      $ case Map.maxViewWithKey vs of
          Nothing             ->
              if definitelyNoMoreToFetch
              then includingAllKeys
              else error $ "Size of values " <> show (Map.size vs) <> ", nrequested " <> show nrequested
          Just ((k, _v), vs') ->
            if definitelyNoMoreToFetch then includingAllKeys else
            Diff.applyDiff
              vs'
              (Diff.filterOnlyKey (< k) ds)
