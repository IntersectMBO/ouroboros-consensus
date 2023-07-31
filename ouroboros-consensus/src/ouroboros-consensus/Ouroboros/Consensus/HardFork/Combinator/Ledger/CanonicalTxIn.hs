{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.CanonicalTxIn (HasCanonicalTxIn (..)) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind (Constraint, Type)
import           Data.SOP.Index (Index)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Ledger.Basics (Key, LedgerState)

-- | Canonical TxIn
--
-- The Ledger and Consensus team discussed the fact that we need to be able to
-- reach the TxIn key for an entry from any era, regardless of the era in which
-- it was created, therefore we need to have a "canonical" serialization that
-- doesn't change between eras. For now we are requiring that a 'HardForkBlock'
-- has only one associated 'TxIn' type as a stop-gap, but Ledger will provide a
-- serialization function into something more efficient.
type HasCanonicalTxIn :: [Type] -> Constraint
class ( Show (CanonicalTxIn xs)
      , Ord (CanonicalTxIn xs)
      , NoThunks (CanonicalTxIn xs)
      ) => HasCanonicalTxIn xs where
  data family CanonicalTxIn (xs :: [Type]) :: Type

  -- | Inject an era-specific 'TxIn' into a 'TxIn' for a 'HardForkBlock'.
  injectCanonicalTxIn ::
    Index xs x ->
    Key (LedgerState x) ->
    CanonicalTxIn xs

  -- | Distribute a 'TxIn' for a 'HardForkBlock' to an era-specific 'TxIn'.
  distribCanonicalTxIn ::
    Index xs x ->
    CanonicalTxIn xs ->
    Key (LedgerState x)

  encodeCanonicalTxIn :: CanonicalTxIn xs -> CBOR.Encoding

  decodeCanonicalTxIn :: forall s. CBOR.Decoder s (CanonicalTxIn xs)
