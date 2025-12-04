{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Ledger.Tables.Basics
  ( -- * Kinds
    LedgerStateKind
  , StateKind
  , F2

    -- * Ledger tables
  , Table (..)
  , LedgerTables (..)

    -- * Known tables
  , TABLE (..)
  , TableLabel (..)
  , Key
  , Value
  , TablesForBlock

    -- ** UTxO table
  , TxIn (..)
  , TxOut

    -- ** Instant stake table
  , Credential
  , KeyRole (Staking)
  , CompactForm
  , Coin

    -- * Helpers
  , setterForSing
  , getTableByTag
  , AllTables
  ) where

import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys (KeyRole (Staking))
import qualified Cardano.Ledger.TxIn as SL
import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.MemPack
import Data.SOP.Constraint
import Data.SOP.Index (Index (..), projectNP)
import Data.SOP.Strict
import Data.Singletons
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Ledger.LedgerStateType
import Ouroboros.Consensus.Util.TypeLevel
import Ouroboros.Consensus.Util.IndexedMemPack

--------------------------------------------------------------------------------
-- Keys and values
--------------------------------------------------------------------------------

-- | The possible tables that Consensus is aware of
type data TABLE = UTxOTable | InstantStakeTable

class TableLabel table where
  tableLabel :: Proxy table -> String

instance TableLabel UTxOTable where
  tableLabel _ = "utxo"

instance TableLabel InstantStakeTable where
  tableLabel _ = "instantStake"

------------------------------------------------------------
-- START Singletons
------------------------------------------------------------

data STABLE a where
  SUTxOTable :: STABLE UTxOTable
  SInstantStakeTable :: STABLE InstantStakeTable

type instance Sing = STABLE

instance SingI UTxOTable where
  sing = SUTxOTable
instance SingI InstantStakeTable where
  sing = SInstantStakeTable

instance TestEquality STABLE where
  testEquality SUTxOTable SUTxOTable = Just Refl
  testEquality SUTxOTable _ = Nothing
  testEquality SInstantStakeTable SInstantStakeTable = Just Refl
  testEquality SInstantStakeTable _ = Nothing

------------------------------------------------------------
-- END Singletons
------------------------------------------------------------

-- | The only purpose of this type is to modify the MemPack instance to use big
-- endian serialization. This is necessary to ensure streaming functions of the
-- UTxO set preserve the order of the entries, as otherwise we would get
-- different sortings if sorting via the Serialized form and the Haskell Ord
-- instance.
--
-- TODO: fix this in the Ledger. See cardano-ledger#5336.
newtype TxIn = TxIn {getOriginalTxIn :: SL.TxIn}
  deriving newtype (Eq, Show, Ord, NoThunks)

newtype BigEndianTxIx = BigEndianTxIx {getOriginalTxIx :: SL.TxIx}

instance MemPack BigEndianTxIx where
  typeName = "BigEndianTxIx"
  packedByteCount = packedByteCount . getOriginalTxIx
  packM (BigEndianTxIx (SL.TxIx w)) = packM (byteSwap16 w)
  unpackM = BigEndianTxIx . SL.TxIx . byteSwap16 <$> unpackM

instance MemPack TxIn where
  typeName = "BigEndianTxIn"
  packedByteCount = packedByteCount . getOriginalTxIn
  packM (TxIn (SL.TxIn txid txix)) = do
    packM txid
    packM (BigEndianTxIx txix)
  unpackM = do
    TxIn <$> (SL.TxIn <$> unpackM <*> (getOriginalTxIx <$> unpackM))

-- | Table-indexed type family for key type
type Key :: TABLE -> Type
type family Key table where
  Key UTxOTable = TxIn
  Key InstantStakeTable = Credential 'Staking

-- | Table-indexed type family for value type
type Value :: TABLE -> Type -> Type
type family Value table blk where
  Value UTxOTable blk = TxOut blk
  Value InstantStakeTable blk = CompactForm Coin

-- | Block-indexed type for TxOut, as TxOut is the only value that varies per era.
type TxOut :: Type -> Type
type family TxOut blk

instance IndexedMemPack l blk InstantStakeTable where
  type IndexedValue l InstantStakeTable blk = Value InstantStakeTable blk
  indexedTypeName _ _ _ = typeName @(CompactForm Coin)
  indexedPackM _ _ _ _ = packM
  indexedPackedByteCount _ _ _ _ = packedByteCount
  indexedUnpackM _ _ _ _ = unpackM

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | Useful for partially applying @Table mk blk@
type Table :: F2 -> Type -> TABLE -> Type
newtype Table mk blk table = Table {getTable :: mk (Key table) (Value table blk)}
  deriving Generic

deriving instance NoThunks (mk (Key table) (Value table blk)) => NoThunks (Table mk blk table)
deriving instance Show (mk (Key table) (Value table blk)) => Show (Table mk blk table)
deriving instance Eq (mk (Key table) (Value table blk)) => Eq (Table mk blk table)

-- | Each block will declare its tables.
--
-- > TablesForBlock ByronBlock = '[]
-- > TablesForBlock (ShelleyBlock (TPraos c) ShelleyEra) = '[UTxOTable]
-- > TablesForBlock (ShelleyBlock (Praos c) DijkstraEra) = '[UTxOTable, InstantStakeTable]
-- > TablesForBlock (HardForkBlock (CardanoEras c)) = '[UTxOTable, InstantStakeTable]
type TablesForBlock :: Type -> [TABLE]
type family TablesForBlock blk

class All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l
instance All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l

-- | The Ledger Tables represent the portion of the data on disk that has been
-- pulled from disk and attached to the in-memory Ledger State or that will
-- eventually be written to disk.
--
-- With UTxO-HD and the split of the Ledger /ledger state/ into the in-memory
-- part and the on-disk part, this splitting was reflected in the new type
-- parameter added to the (Consensus)
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState', to which we refer as "the
-- MapKind" or @mk@.
--
-- Every 'Ouroboros.Consensus.Ledger.Basics.LedgerState' (or @LedgerState@-like
-- type, such as the 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState') is
-- associated with a 'LedgerTables' and they both share the @mk@. They both are
-- of kind 'LedgerStateKind'. 'LedgerTables' is just a way to refer /only/ to a
-- partial view of the on-disk data without having the rest of the in-memory
-- 'LedgerState' in scope.
--
-- The @mk@ can be instantiated to anything that is map-like, i.e. that expects
-- two type parameters, the key and the value.
type LedgerTables :: StateKind
newtype LedgerTables blk mk = LedgerTables
  { getLedgerTables :: NP (Table mk blk) (TablesForBlock blk)
  }
  deriving Generic

deriving newtype instance
  AllTables NoThunks mk blk =>
  NoThunks (LedgerTables blk mk)
deriving newtype instance
  AllTables Show mk blk =>
  Show (LedgerTables blk mk)
deriving newtype instance
  AllTables Eq mk blk =>
  Eq (LedgerTables blk mk)

------------------------------------------------------------
-- Type proof of inclusion
------------------------------------------------------------

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

-- | Given a proof of inclusion, create a lens for the particular item in the NP
ixNP :: Index xs x -> Lens' (NP f xs) (f x)
ixNP IZ f (x :* xs) = (:* xs) <$> f x
ixNP (IS m) f (x :* xs) = (x :*) <$> ixNP m f xs

setterForSing ::
  forall l mk table.
  SList (TablesForBlock l) ->
  Sing table ->
  Maybe (ASetter' (LedgerTables l mk) (Table mk l table))
setterForSing sxs sx =
  (\mbm -> \f -> fmap LedgerTables . ixNP mbm f . getLedgerTables) <$> findIndex sxs sx

-- | Extract a table based on the singleton
getTableByTag ::
  forall table mk l.
  ( SListI (TablesForBlock l)
  , SingI (TablesForBlock l)
  ) =>
  Sing table ->
  LedgerTables l mk ->
  Maybe (Table mk l table)
getTableByTag stag (LedgerTables np) =
  let mem = findIndex (sing @(TablesForBlock l)) stag
   in fmap (flip projectNP np) mem
