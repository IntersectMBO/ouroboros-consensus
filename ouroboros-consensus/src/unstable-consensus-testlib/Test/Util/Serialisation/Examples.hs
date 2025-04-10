{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Serialisation.Examples (
    -- * Examples
    Examples (..)
    -- ** Operations on examples
  , combineExamples
  , mapExamples
  , prefixExamples
    -- * Labelling
  , Labelled
  , labelled
  , unlabelled
  ) where

import           Data.Bifunctor (first)
import           Ouroboros.Consensus.Block (BlockProtocol, Header, HeaderHash,
                     SlotNo)
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Abstract (EmptyMK, LedgerConfig,
                     LedgerState, LedgerTables, ValuesMK)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Ledger.Query (BlockQuery, SomeBlockQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)
import           Ouroboros.Network.Block (Serialised)
import           Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

data Examples blk = Examples {
      exampleBlock            :: Labelled blk
    , exampleSerialisedBlock  :: Labelled (Serialised blk)
    , exampleHeader           :: Labelled (Header blk)
    , exampleSerialisedHeader :: Labelled (SerialisedHeader blk)
    , exampleHeaderHash       :: Labelled (HeaderHash blk)
    , exampleGenTx            :: Labelled (GenTx blk)
    , exampleGenTxId          :: Labelled (GenTxId blk)
    , exampleApplyTxErr       :: Labelled (ApplyTxErr blk)
    , exampleQuery            :: Labelled (SomeBlockQuery (BlockQuery blk))
    , exampleResult           :: Labelled (SomeResult blk)
    , exampleAnnTip           :: Labelled (AnnTip blk)
    , exampleLedgerState      :: Labelled (LedgerState blk EmptyMK)
    , exampleChainDepState    :: Labelled (ChainDepState (BlockProtocol blk))
    , exampleExtLedgerState   :: Labelled (ExtLedgerState blk EmptyMK)
    , exampleSlotNo           :: Labelled SlotNo
    , exampleLedgerConfig     :: Labelled (LedgerConfig blk)
    , exampleLedgerTables     :: Labelled (LedgerTables (LedgerState blk) ValuesMK)
    }

emptyExamples :: Examples blk
emptyExamples = Examples {
      exampleBlock            = mempty
    , exampleSerialisedBlock  = mempty
    , exampleHeader           = mempty
    , exampleSerialisedHeader = mempty
    , exampleHeaderHash       = mempty
    , exampleGenTx            = mempty
    , exampleGenTxId          = mempty
    , exampleApplyTxErr       = mempty
    , exampleQuery            = mempty
    , exampleResult           = mempty
    , exampleAnnTip           = mempty
    , exampleLedgerState      = mempty
    , exampleChainDepState    = mempty
    , exampleExtLedgerState   = mempty
    , exampleSlotNo           = mempty
    , exampleLedgerConfig     = mempty
    , exampleLedgerTables     = mempty
    }

combineExamples ::
     forall blk.
     (forall a. Labelled a -> Labelled a -> Labelled a)
  -> Examples blk
  -> Examples blk
  -> Examples blk
combineExamples f e1 e2 = Examples {
      exampleBlock            = combine exampleBlock
    , exampleSerialisedBlock  = combine exampleSerialisedBlock
    , exampleHeader           = combine exampleHeader
    , exampleSerialisedHeader = combine exampleSerialisedHeader
    , exampleHeaderHash       = combine exampleHeaderHash
    , exampleGenTx            = combine exampleGenTx
    , exampleGenTxId          = combine exampleGenTxId
    , exampleApplyTxErr       = combine exampleApplyTxErr
    , exampleQuery            = combine exampleQuery
    , exampleResult           = combine exampleResult
    , exampleAnnTip           = combine exampleAnnTip
    , exampleLedgerState      = combine exampleLedgerState
    , exampleChainDepState    = combine exampleChainDepState
    , exampleExtLedgerState   = combine exampleExtLedgerState
    , exampleSlotNo           = combine exampleSlotNo
    , exampleLedgerConfig     = combine exampleLedgerConfig
    , exampleLedgerTables     = combine exampleLedgerTables
    }
  where
    combine :: (Examples blk -> Labelled a) -> Labelled a
    combine getField = f (getField e1) (getField e2)

instance Semigroup (Examples blk) where
  (<>) = combineExamples (<>)

instance Monoid (Examples blk) where
  mempty  = emptyExamples
  mappend = (<>)

mapExamples ::
     forall blk.
     (forall a. Labelled a -> Labelled a)
  -> Examples blk
  -> Examples blk
mapExamples f = combineExamples (const f) mempty

-- | Add the given prefix to each labelled example.
--
-- When a label is empty, the prefix is used as the label. If the label is not
-- empty, the prefix and @_@ are prepended.
prefixExamples :: String -> Examples blk -> Examples blk
prefixExamples prefix = mapExamples addPrefix
  where
    addPrefix :: Labelled a -> Labelled a
    addPrefix l = [
          (Just label, x)
        | (mbLabel, x) <- l
        , let label = case mbLabel of
                Nothing  -> prefix
                Just lbl -> prefix <> "_" <> lbl
        ]

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

type Labelled a = [(Maybe String, a)]

unlabelled :: a -> Labelled a
unlabelled x = [(Nothing, x)]

labelled :: [(String, a)] -> Labelled a
labelled = map (first Just)
