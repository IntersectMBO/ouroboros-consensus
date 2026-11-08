-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , defaultBlockLeiosCert
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, EmptyMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types (ResolveLeiosClosure)

-- | What a block's body says about Leios.
class BlockSupportsLeios blk where
  -- | The Leios certificate the block contains, if any, which certifies the
  -- endorser block its predecessor announced.
  blockLeiosCert :: blk -> Maybe LeiosCert

  -- | Apply the closure of the endorser block that this state's tip announced,
  -- without validating its transactions: the certificate of the block about to
  -- be applied vouches for them. If the tip announced none, apply nothing, and
  -- leave it to that block's validation to reject its certificate.
  --
  -- Only called for the predecessor of a block that contains a certificate.
  -- Throws if the closure cannot be decoded, as a certified one always can.
  applyLeiosClosure ::
    MonadThrow m =>
    LedgerConfig blk ->
    ResolveLeiosClosure m ->
    -- | Read the values of these keys from the state's ledger tables
    (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
    LedgerState blk EmptyMK ->
    m (LedgerState blk DiffMK)
  applyLeiosClosure _cfg _resolve _readTables _st =
    error "applyLeiosClosure: a block of this type contains no Leios certificate"

-- | 'blockLeiosCert' for a block type that never contains a Leios certificate.
defaultBlockLeiosCert :: blk -> Maybe LeiosCert
defaultBlockLeiosCert _blk = Nothing
