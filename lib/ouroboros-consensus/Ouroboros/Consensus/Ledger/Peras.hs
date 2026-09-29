{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Peras state to be stored in the extended ledger state.
module Ouroboros.Consensus.Ledger.Peras
  ( PerasState (..)
  , initPerasState
  , decodePerasState
  , encodePerasState
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.SOP (All)
import Data.SOP.Constraint (Top)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.SupportsPeras (PerasVotingCommittee)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import Ouroboros.Consensus.HeaderValidation (HeaderState)
import Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerState)
import Ouroboros.Consensus.Ledger.Tables (HasLedgerTables)
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolver
  , StateSupportsPerasEpochContext
  , initPerasEpochContextResolver
  )
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.CBOR (decodeStrictMaybe, encodeStrictMaybe)

-- | Peras state to be stored in the extended ledger state.
data PerasState blk
  = PerasState
  { perasEpochContextResolver :: !(PerasEpochContextResolver blk)
  , latestPerasCertOnChainRound :: !(StrictMaybe PerasRoundNo)
  }

deriving instance Eq (PerasVotingCommittee blk) => Eq (PerasState blk)
deriving instance Show (PerasVotingCommittee blk) => Show (PerasState blk)
deriving instance NoThunks (PerasVotingCommittee blk) => NoThunks (PerasState blk)
deriving instance Generic (PerasState blk)

initPerasState ::
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  , HasLedgerTables LedgerState blk
  ) =>
  LedgerCfg LedgerState blk ->
  LedgerState blk mk ->
  HeaderState blk ->
  PerasState blk
initPerasState ledgerConfig ledgerState headerState =
  PerasState
    { perasEpochContextResolver =
        initPerasEpochContextResolver
          ledgerConfig
          (forgetLedgerTables ledgerState)
          headerState
    , latestPerasCertOnChainRound =
        SNothing
    }

encodePerasState ::
  (PerasEpochContextResolver blk -> Encoding) ->
  PerasState blk ->
  Encoding
encodePerasState
  encodeResolver
  PerasState
    { perasEpochContextResolver
    , latestPerasCertOnChainRound
    } =
    encodeListLen 2
      <> encodeResolver perasEpochContextResolver
      <> encodeStrictMaybe toCBOR latestPerasCertOnChainRound

decodePerasState ::
  (forall s. Decoder s (PerasEpochContextResolver blk)) ->
  forall s. Decoder s (PerasState blk)
decodePerasState decodeResolver = do
  decodeListLenOf 2
  perasEpochContextResolver <- decodeResolver
  latestPerasCertOnChainRound <- decodeStrictMaybe fromCBOR
  pure
    PerasState
      { perasEpochContextResolver
      , latestPerasCertOnChainRound
      }

instance
  ( Typeable blk
  , FromCBOR (PerasVotingCommittee blk)
  ) =>
  FromCBOR (PerasState blk)
  where
  fromCBOR = decodePerasState fromCBOR

instance
  ( Typeable blk
  , ToCBOR (PerasVotingCommittee blk)
  ) =>
  ToCBOR (PerasState blk)
  where
  toCBOR = encodePerasState toCBOR

instance
  ( Typeable blk
  , FromCBOR (PerasVotingCommittee blk)
  ) =>
  DecodeDisk blk (PerasState blk)
  where
  decodeDisk cfg = decodePerasState (decodeDisk cfg)

instance
  ( Typeable blk
  , ToCBOR (PerasVotingCommittee blk)
  ) =>
  EncodeDisk blk (PerasState blk)
  where
  encodeDisk cfg = encodePerasState (encodeDisk cfg)
