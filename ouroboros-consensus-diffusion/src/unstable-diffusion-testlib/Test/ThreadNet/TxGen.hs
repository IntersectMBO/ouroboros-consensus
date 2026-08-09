{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transaction generator for testing
module Test.ThreadNet.TxGen
  ( TxGen (..)

    -- * Implementation for HFC
  , WrapTxGenExtra (..)
  , testGenTxsHfc
  , testReadAllValuesHfc

    -- * Reading the full UTxO via the era range reader
  , pageAllEra
  ) where

import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.NodeId (CoreNodeId)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( EraRangeReader (..)
  , EraRangeReaderProvider (..)
  , RangeQueryPrevious (..)
  )
import Ouroboros.Consensus.TypeFamilyWrappers (WrapValues (..))
import Test.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where
  -- | Extra information required to generate transactions
  type TxGenExtra blk :: Type

  type TxGenExtra blk = ()

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function will be called to generate transactions in consensus tests.
  --
  -- Note: this function returns a list so that an empty list can be returned
  -- in case we are unable to generate transactions for a @blk@.
  testGenTxs ::
    CoreNodeId ->
    NumCoreNodes ->
    SlotNo ->
    TopLevelConfig blk ->
    TxGenExtra blk ->
    LedgerState blk ->
    -- | The UTxO values, carried alongside the @mk@-free state.
    Values blk ->
    Gen [GenTx blk]

  -- | Read the /whole/ UTxO at the given state, reconstructing the block-level
  -- @'Values' blk@.
  --
  -- In the @mk@-free design the plain forker can no longer read a whole table
  -- (range reads are era-typed and live behind the 'EraRangeReaderProvider';
  -- see decision 10 in @utxo-hd-4.md@). The transaction generators, however,
  -- need the full UTxO. So the harness pages the current era's values through
  -- the provider and rebuilds @'Values' blk@: for a single-era block the
  -- projection is the identity (default below); the hard-fork combinator pages
  -- the current era and injects the result back into the @NS@
  -- (see 'testReadAllValuesHfc').
  testReadAllValues ::
    Monad m =>
    EraRangeReaderProvider m blk ->
    LedgerState blk ->
    m (Values blk)
  default testReadAllValues ::
    (Monad m, SingleEraBlockSupportsUTxOHD blk) =>
    EraRangeReaderProvider m blk ->
    LedgerState blk ->
    m (Values blk)
  testReadAllValues provider _st =
    pageAllEra (getEraRangeReader provider @blk id)

-- | Page an era's entire table through an 'EraRangeReader', accumulating all
-- entries. Starts at 'NoPreviousQuery' and advances the cursor to the maximum
-- key of each page until a page comes back empty.
pageAllEra ::
  forall m blk.
  (Monad m, SingleEraBlockSupportsUTxOHD blk) =>
  EraRangeReader m blk ->
  m (Values blk)
pageAllEra (EraRangeReader rd) = go NoPreviousQuery []
 where
  go ::
    RangeQueryPrevious blk -> [Values blk] -> m (Values blk)
  go prev acc = do
    page <- rd prev
    case reverse (valuesToList @blk page) of
      [] -> pure (valuesFromList @blk (concatMap (valuesToList @blk) (reverse acc)))
      ((maxKey, _) : _) -> go (PreviousQueryWasUpTo maxKey) (page : acc)

{-------------------------------------------------------------------------------
  Implementation for HFC
-------------------------------------------------------------------------------}

-- | Newtypes wrapper around the 'TxGenExtra' type family so that it can be
-- partially applied.
newtype WrapTxGenExtra blk = WrapTxGenExtra
  { unwrapTxGenExtra :: TxGenExtra blk
  }

-- | Function that can be used for 'TxGen' instances for 'HardForkBlock'.
--
-- We don't provide a generic instance of 'TxGen' because it might be desirable
-- to provide custom implementations for specific instantiations of the eras of
-- 'HardForkBlock'. Instead, we provide this function that can be used when a
-- generic implemenation is desired.
--
-- Choose @NP WrapTxGenExtra xs@ for the instance of the 'TxGenExtra' type
-- family, where @xs@ matches the concrete instantiation.
testGenTxsHfc ::
  forall xs.
  (All TxGen xs, CanHardFork xs) =>
  CoreNodeId ->
  NumCoreNodes ->
  SlotNo ->
  TopLevelConfig (HardForkBlock xs) ->
  NP WrapTxGenExtra xs ->
  LedgerState (HardForkBlock xs) ->
  Values (HardForkBlock xs) ->
  Gen [GenTx (HardForkBlock xs)]
testGenTxsHfc coreNodeId numCoreNodes curSlotNo cfg extras state values =
  case Match.matchNS (State.tip (hardForkLedgerStatePerEra state)) values of
    -- The current era of the state and of the values agree by construction.
    Left _mismatch ->
      error "testGenTxsHfc: current era of state and values disagree"
    Right matched ->
      hcollapse $
        hcizipWith3
          (Proxy @TxGen)
          aux
          cfgs
          extras
          matched
 where
  cfgs = distribTopLevelConfig ei cfg
  ei =
    State.epochInfoLedger
      (configLedger cfg)
      (hardForkLedgerStatePerEra state)

  aux ::
    forall blk.
    TxGen blk =>
    Index xs blk ->
    TopLevelConfig blk ->
    WrapTxGenExtra blk ->
    Product LedgerState WrapValues blk ->
    K (Gen [GenTx (HardForkBlock xs)]) blk
  aux index cfg' (WrapTxGenExtra extra') (Pair state' (WrapValues values')) =
    K $
      fmap (injectNS' (Proxy @GenTx) index)
        <$> testGenTxs coreNodeId numCoreNodes curSlotNo cfg' extra' state' values'

-- | 'testReadAllValues' for 'HardForkBlock': page the current era's table
-- through the provider and inject the resulting @'Values' x@ back into the
-- hard-fork @NS@ at the current era's index.
testReadAllValuesHfc ::
  forall m xs.
  (Monad m, All SingleEraBlockSupportsUTxOHD xs) =>
  EraRangeReaderProvider m (HardForkBlock xs) ->
  LedgerState (HardForkBlock xs) ->
  m (Values (HardForkBlock xs))
testReadAllValuesHfc provider state =
  hcollapse $
    hcimap
      (Proxy @SingleEraBlockSupportsUTxOHD)
      aux
      (State.tip (hardForkLedgerStatePerEra state))
 where
  aux ::
    forall x.
    SingleEraBlockSupportsUTxOHD x =>
    Index xs x ->
    LedgerState x ->
    K (m (Values (HardForkBlock xs))) x
  aux idx _eraState =
    K $ do
      vx <- pageAllEra (getEraRangeReader provider @x (projectNSValues idx))
      pure (injectNS' (Proxy @WrapValues) idx (WrapValues @x vx))

  -- The backend's values are tagged with the current era (the era we read at),
  -- so the requested arm is guaranteed present.
  projectNSValues :: Index xs x -> Values (HardForkBlock xs) -> Values x
  projectNSValues idx0 =
    maybe (error "testReadAllValuesHfc: values in unexpected era") unwrapValues
      . go idx0
   where
    go :: Index xs' x -> NS WrapValues xs' -> Maybe (WrapValues x)
    go IZ (Z x) = Just x
    go (IS idx) (S ns) = go idx ns
    go _ _ = Nothing
