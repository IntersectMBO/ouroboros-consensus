{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
    BlockQuery (..)
  , BlockSupportsHFLedgerQuery (..)
  , HardForkQueryResult
  , QueryAnytime (..)
  , QueryHardFork (..)
  , QueryIfCurrent (..)
  , decodeQueryAnytimeResult
  , decodeQueryHardForkResult
  , encodeQueryAnytimeResult
  , encodeQueryHardForkResult
  , getHardForkQuery
  , hardForkQueryInfo
  ) where

import           Cardano.Binary (enforceSize)
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Counting (getExactly)
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import           Data.SOP.Match (Mismatch (..), mustMatchNS)
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Typeable (Typeable)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
                     (HardForkHasLedgerTables)
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..), Situated (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     Shape (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState (..),
                     WrapTxOut)
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike (MonadSTM (atomically))

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

data instance BlockQuery (HardForkBlock xs) footprint result where
  -- | Answer a query about an era if it is the current one.
  QueryIfCurrent ::
       QueryIfCurrent xs footprint result
    -> BlockQuery (HardForkBlock xs) footprint (HardForkQueryResult xs result)

  -- | Answer a query about an era from /any/ era.
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryAnytime ::
       IsNonEmpty xs
    => QueryAnytime result
    -> EraIndex (x ': xs)
    -> BlockQuery (HardForkBlock (x ': xs)) QFNoTables result

  -- | Answer a query about the hard fork combinator
  --
  -- NOTE: we don't allow this when there is only a single era, so that the
  -- HFC applied to a single era is still isomorphic to the single era.
  QueryHardFork ::
       IsNonEmpty xs
    => QueryHardFork (x ': xs) result
    -> BlockQuery (HardForkBlock (x ': xs)) QFNoTables result

-- | Queries that use ledger tables usually can be implemented faster if we work
-- with the hard fork tables rather than projecting everything to the
-- appropriate era before we process the query. This class should be used to
-- implement how these queries that have a footprint which is not @QFNoTables@
-- are answered.
class ( All (Compose NoThunks WrapTxOut) xs
      , All (Compose Show WrapTxOut) xs
      , All (Compose Eq WrapTxOut) xs
      , All (Compose HasTickedLedgerTables LedgerState) xs
      , All (Compose HasLedgerTables LedgerState) xs
      ) => BlockSupportsHFLedgerQuery xs where
  answerBlockQueryHFLookup ::
       All SingleEraBlock xs
    => Monad m
    => Index xs x
    -> ExtLedgerCfg x
    -> BlockQuery x QFLookupTables result
    -> ReadOnlyForker' m (HardForkBlock xs)
    -> m result

  answerBlockQueryHFTraverse ::
       All SingleEraBlock xs
    => Monad m
    => Index xs x
    -> ExtLedgerCfg x
    -> BlockQuery x QFTraverseTables result
    -> ReadOnlyForker' m (HardForkBlock xs)
    -> m result

  -- | The @QFTraverseTables@ queries consist of some filter on the @TxOut@. This class
  -- provides that filter so that @answerBlockQueryHFAll@ can be implemented
  -- in an abstract manner depending on this function.
  queryLedgerGetTraversingFilter ::
       Index xs x
    -> BlockQuery x QFTraverseTables result
    -> Value (LedgerState (HardForkBlock xs))
    -> Bool

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

------
-- Show
------

instance Typeable xs => ShowProxy (BlockQuery (HardForkBlock xs)) where
-- Use default implementation

deriving instance All SingleEraBlock xs => Show (BlockQuery (HardForkBlock xs) footprint result)

instance All SingleEraBlock xs
      => ShowQuery (BlockQuery (HardForkBlock xs) footprint) where
  showResult (QueryAnytime   qry _) result = showResult qry result
  showResult (QueryHardFork  qry)   result = showResult qry result
  showResult (QueryIfCurrent qry)  mResult =
      case mResult of
        Left  err    -> show err
        Right result -> showResult qry result

------
-- Eq
------

instance All SingleEraBlock xs => SameDepIndex2 (BlockQuery (HardForkBlock xs)) where
  sameDepIndex2 (QueryIfCurrent qry) (QueryIfCurrent qry') =
      (\Refl -> Refl) <$> sameDepIndex2 qry qry'
  sameDepIndex2 (QueryIfCurrent {}) _ =
      Nothing
  sameDepIndex2 (QueryAnytime qry era) (QueryAnytime qry' era')
    | era == era'
    = (\Refl -> Refl) <$> sameDepIndex qry qry'
    | otherwise
    = Nothing
  sameDepIndex2(QueryAnytime {}) _ =
      Nothing
  sameDepIndex2 (QueryHardFork qry) (QueryHardFork qry') =
      (\Refl -> Refl) <$> sameDepIndex qry qry'
  sameDepIndex2 (QueryHardFork {}) _ =
      Nothing

{-------------------------------------------------------------------------------
  Query Ledger
-------------------------------------------------------------------------------}

instance ( All SingleEraBlock xs
         , HardForkHasLedgerTables xs
         , BlockSupportsHFLedgerQuery xs
         , CanHardFork xs
         )
      => BlockSupportsLedgerQuery (HardForkBlock xs) where
  answerPureBlockQuery
    (ExtLedgerCfg cfg)
    query
    ext@(ExtLedgerState st@(HardForkLedgerState hardForkState) _) =
      case query of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrent
            cfgs
            queryIfCurrent
            (distribExtLedgerState ext)
        QueryAnytime queryAnytime (EraIndex era) ->
          interpretQueryAnytime
            lcfg
            queryAnytime
            (EraIndex era)
            hardForkState
        QueryHardFork queryHardFork ->
          interpretQueryHardFork
            lcfg
            queryHardFork
            st
    where
      cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      lcfg = configLedger cfg
      ei   = State.epochInfoLedger lcfg hardForkState

  answerBlockQueryLookup
    (ExtLedgerCfg cfg)
    qry
    forker = do
      hardForkState <- hardForkLedgerStatePerEra . ledgerState <$> atomically (roforkerGetLedgerState forker)
      let ei   = State.epochInfoLedger lcfg hardForkState
          cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      case qry of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrentOne
                cfgs
                queryIfCurrent
                forker
    where
      lcfg = configLedger cfg

  answerBlockQueryTraverse
    (ExtLedgerCfg cfg)
    qry
    forker = do
      hardForkState <- hardForkLedgerStatePerEra . ledgerState <$> atomically (roforkerGetLedgerState forker)
      let ei   = State.epochInfoLedger lcfg hardForkState
          cfgs = hmap ExtLedgerCfg $ distribTopLevelConfig ei cfg
      case qry of
        QueryIfCurrent queryIfCurrent ->
          interpretQueryIfCurrentAll
                cfgs
                queryIfCurrent
                forker
    where
      lcfg = configLedger cfg

-- | Precondition: the 'ledgerState' and 'headerState' should be from the same
-- era. In practice, this is _always_ the case, unless the 'ExtLedgerState' was
-- manually crafted.
distribExtLedgerState ::
     All SingleEraBlock xs
  => ExtLedgerState (HardForkBlock xs) mk -> NS (Flip ExtLedgerState mk) xs
distribExtLedgerState (ExtLedgerState ledgerState headerState) =
    hmap (\(Pair hst lst) -> Flip $ ExtLedgerState (unFlip lst) hst) $
      mustMatchNS
        "HeaderState"
        (distribHeaderState headerState)
        (State.tip (hardForkLedgerStatePerEra ledgerState))

-- | Precondition: the 'headerStateTip' and 'headerStateChainDep' should be from
-- the same era. In practice, this is _always_ the case, unless the
-- 'HeaderState' was manually crafted.
distribHeaderState ::
     All SingleEraBlock xs
  => HeaderState (HardForkBlock xs) -> NS HeaderState xs
distribHeaderState (HeaderState tip chainDepState) =
    case tip of
      Origin ->
        hmap (HeaderState Origin . unwrapChainDepState) (State.tip chainDepState)
      NotOrigin annTip ->
        hmap
          (\(Pair t cds) -> HeaderState (NotOrigin t) (unwrapChainDepState cds))
          (mustMatchNS "AnnTip" (distribAnnTip annTip) (State.tip chainDepState))

getHardForkQuery :: BlockQuery (HardForkBlock xs) footprint result
                 -> (forall result'.
                          result :~: HardForkQueryResult xs result'
                       -> QueryIfCurrent xs footprint result'
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryAnytime result
                       -> EraIndex xs
                       -> r)
                 -> (forall x' xs'.
                          xs :~: x' ': xs'
                       -> ProofNonEmpty xs'
                       -> QueryHardFork xs result
                       -> r)
                 -> r
getHardForkQuery q k1 k2 k3 = case q of
    QueryIfCurrent qry   -> k1 Refl qry
    QueryAnytime qry era -> k2 Refl (isNonEmpty Proxy) qry era
    QueryHardFork qry    -> k3 Refl (isNonEmpty Proxy) qry

{-------------------------------------------------------------------------------
  Current era queries
-------------------------------------------------------------------------------}

type QueryIfCurrent :: [Type] -> QueryFootprint -> Type -> Type
data QueryIfCurrent xs footprint result where
  QZ :: BlockQuery     x  footprint result -> QueryIfCurrent (x ': xs) footprint result
  QS :: QueryIfCurrent xs footprint result -> QueryIfCurrent (x ': xs) footprint result

deriving instance All SingleEraBlock xs => Show (QueryIfCurrent xs footprint result)

instance All SingleEraBlock xs => ShowQuery (QueryIfCurrent xs footprint) where
  showResult (QZ qry) = showResult qry
  showResult (QS qry) = showResult qry

instance All SingleEraBlock xs => SameDepIndex2 (QueryIfCurrent xs) where
  sameDepIndex2 (QZ qry) (QZ qry') = sameDepIndex2 qry qry'
  sameDepIndex2 (QS qry) (QS qry') = sameDepIndex2 qry qry'
  sameDepIndex2 _        _         = Nothing

interpretQueryIfCurrent ::
     forall result xs. All SingleEraBlock xs
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs QFNoTables result
  -> NS (Flip ExtLedgerState EmptyMK) xs
  -> HardForkQueryResult xs result
interpretQueryIfCurrent = go
  where
    go :: All SingleEraBlock xs'
       => NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' QFNoTables result
       -> NS (Flip ExtLedgerState EmptyMK) xs'
       -> HardForkQueryResult xs' result
    go (c :* _)  (QZ qry) (Z (Flip st)) =
        Right $ answerPureBlockQuery c qry st
    go (_ :* cs) (QS qry) (S st) =
        first shiftMismatch $ go cs qry st
    go _         (QZ qry) (S st) =
        Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle (ledgerInfo . unFlip) st)
    go _         (QS qry) (Z (Flip st)) =
        Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

interpretQueryIfCurrentOne ::
     forall result xs m. (MonadSTM m, BlockSupportsHFLedgerQuery xs, CanHardFork xs)
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs QFLookupTables result
  -> ReadOnlyForker' m (HardForkBlock xs)
  -> m (HardForkQueryResult xs result)
interpretQueryIfCurrentOne cfg q forker = do
    st <- distribExtLedgerState <$> atomically (roforkerGetLedgerState forker)
    go indices cfg q st
  where
    go :: All SingleEraBlock xs'
       => NP (Index xs) xs'
       -> NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' QFLookupTables result
       -> NS (Flip ExtLedgerState EmptyMK) xs'
       -> m (HardForkQueryResult xs' result)
    go (idx :* _) (c :* _)  (QZ qry) _ =
        Right <$> answerBlockQueryHFLookup idx c qry forker
    go (_ :* idx) (_ :* cs) (QS qry) (S st) =
        first shiftMismatch <$> go idx cs qry st
    go _          _         (QS qry) (Z (Flip st)) =
        pure $ Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

interpretQueryIfCurrentAll ::
     forall result xs m. (MonadSTM m, BlockSupportsHFLedgerQuery xs, CanHardFork xs)
  => NP ExtLedgerCfg xs
  -> QueryIfCurrent xs QFTraverseTables result
  -> ReadOnlyForker' m (HardForkBlock xs)
  -> m (HardForkQueryResult xs result)
interpretQueryIfCurrentAll cfg q forker = do
    st <- distribExtLedgerState <$> atomically (roforkerGetLedgerState forker)
    go indices cfg q st
  where
    go :: All SingleEraBlock xs'
       => NP (Index xs) xs'
       -> NP ExtLedgerCfg xs'
       -> QueryIfCurrent xs' QFTraverseTables result
       -> NS (Flip ExtLedgerState EmptyMK) xs'
       -> m (HardForkQueryResult xs' result)
    go (idx :* _) (c :* _)  (QZ qry) _ =
        Right <$> answerBlockQueryHFTraverse idx c qry forker
    go (_ :* idx) (_ :* cs) (QS qry) (S st) =
        first shiftMismatch <$> go idx cs qry st
    go _          _         (QS qry) (Z (Flip st)) =
        pure $ Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

{-------------------------------------------------------------------------------
  Any era queries
-------------------------------------------------------------------------------}

data QueryAnytime result where
  GetEraStart :: QueryAnytime (Maybe Bound)

deriving instance Show (QueryAnytime result)

instance ShowQuery QueryAnytime where
  showResult GetEraStart = show

instance SameDepIndex QueryAnytime where
   sameDepIndex GetEraStart GetEraStart = Just Refl

interpretQueryAnytime ::
     forall result xs mk. All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> EraIndex xs
  -> State.HardForkState (Flip LedgerState mk) xs
  -> result
interpretQueryAnytime cfg query (EraIndex era) st =
    answerQueryAnytime cfg query (State.situate era st)

answerQueryAnytime ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryAnytime result
  -> Situated h (Flip LedgerState mk) xs
  -> result
answerQueryAnytime HardForkLedgerConfig{..} =
    go cfgs (getExactly (getShape hardForkLedgerConfigShape))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    go :: All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> QueryAnytime result
       -> Situated h (Flip LedgerState mk) xs'
       -> result
    go Nil       _             _           ctxt = case ctxt of {}
    go (c :* cs) (K ps :* pss) GetEraStart ctxt = case ctxt of
      SituatedShift ctxt'   -> go cs pss GetEraStart ctxt'
      SituatedFuture _ _    -> Nothing
      SituatedPast past _   -> Just $ pastStart $ unK past
      SituatedCurrent cur _ -> Just $ currentStart cur
      SituatedNext cur _    ->
        History.mkUpperBound ps (currentStart cur) <$>
          singleEraTransition
          (unwrapPartialLedgerConfig c)
          ps
          (currentStart cur)
          (unFlip $ currentState cur)

{-------------------------------------------------------------------------------
  Hard fork queries
-------------------------------------------------------------------------------}

data QueryHardFork xs result where
  GetInterpreter :: QueryHardFork xs (History.Interpreter xs)
  GetCurrentEra  :: QueryHardFork xs (EraIndex xs)

deriving instance Show (QueryHardFork xs result)

instance All SingleEraBlock xs => ShowQuery (QueryHardFork xs) where
  showResult GetInterpreter = show
  showResult GetCurrentEra  = show

instance SameDepIndex (QueryHardFork xs) where
  sameDepIndex GetInterpreter GetInterpreter =
      Just Refl
  sameDepIndex GetInterpreter _ =
      Nothing
  sameDepIndex GetCurrentEra GetCurrentEra =
      Just Refl
  sameDepIndex GetCurrentEra _ =
      Nothing

interpretQueryHardFork ::
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> QueryHardFork xs result
  -> LedgerState (HardForkBlock xs) mk
  -> result
interpretQueryHardFork cfg query st =
    case query of
      GetInterpreter ->
        History.mkInterpreter $ hardForkSummary cfg st
      GetCurrentEra  ->
        eraIndexFromNS $ State.tip $ hardForkLedgerStatePerEra st

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (Some QueryAnytime) where
  encode (Some GetEraStart) = mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]

  decode = do
    enforceSize "QueryAnytime" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ Some GetEraStart
      _ -> fail $ "QueryAnytime: invalid tag " ++ show tag

encodeQueryAnytimeResult :: QueryAnytime result -> result -> Encoding
encodeQueryAnytimeResult GetEraStart = encode

decodeQueryAnytimeResult :: QueryAnytime result -> forall s. Decoder s result
decodeQueryAnytimeResult GetEraStart = decode

encodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs result -> result -> Encoding
encodeQueryHardForkResult = \case
    GetInterpreter -> encode
    GetCurrentEra  -> encode

decodeQueryHardForkResult ::
     SListI xs
  => QueryHardFork xs result -> forall s. Decoder s result
decodeQueryHardForkResult = \case
    GetInterpreter -> decode
    GetCurrentEra  -> decode

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk mk. SingleEraBlock blk
           => ExtLedgerState blk mk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall blk query (footprint :: QueryFootprint) result. SingleEraBlock blk
          => query blk footprint result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => QueryIfCurrent xs footprint result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => QueryIfCurrent xs' footprint result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo
