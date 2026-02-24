-- | This module extends the committee selection scheme to work across epochs.
--
-- This is needed to support the case of validating an old vote from a previous
-- epoch arriving too late. In the general case, this means we would need to
-- store an arbitrary number of past committee selections. However, since:
--  1. the length of an epoch is much larger than the immutability window, and
--  2. we don't care about validating old votes that cannot affect our current
--   selection beyond the immutability window, it follows that
-- we only need to store the committee selection for the current and previous
-- epochs.
module Ouroboros.Consensus.Committee.AcrossEpochs
  ( InterEpochCommitteeSelection (..)
  , newEpoch
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize
  , ExtCumulativeStakeDistr
  )
import Ouroboros.Consensus.Committee.WFALS
  ( CryptoSupportsWFALS (..)
  )
import qualified Ouroboros.Consensus.Committee.WFALS as WFALS

data InterEpochCommitteeSelection c = InterEpochCommitteeSelection
  { currEpochSelection :: WFALS.CommitteeSelection c
  , prevEpochSelection :: WFALS.CommitteeSelection c
  }

-- | Update an inter-epoch committee selection at the beginning of a new epoch
newEpoch ::
  -- | New epoch nonce
  Nonce ->
  -- | New epoch cumulative stake distribution
  ExtCumulativeStakeDistr (WFALSPublicKey c) ->
  -- | New epoch expected committee size
  CommitteeSize ->
  -- | Current inter-epoch committee selection
  InterEpochCommitteeSelection c ->
  InterEpochCommitteeSelection c
newEpoch
  newEpochNonce
  newEpochStakeDistr
  newEpochCommitteeSize
  interEpochSelection =
    InterEpochCommitteeSelection
      { currEpochSelection =
          WFALS.mkCommitteeSelection
            newEpochNonce
            newEpochCommitteeSize
            newEpochStakeDistr
      , prevEpochSelection =
          currEpochSelection
            interEpochSelection
      }
