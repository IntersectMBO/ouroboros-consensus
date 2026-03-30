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
  ( TargetCommitteeSize
  )
import qualified Ouroboros.Consensus.Committee.WFA as WFA
import qualified Ouroboros.Consensus.Committee.WFALS as WFALS

data InterEpochCommitteeSelection crypto = InterEpochCommitteeSelection
  { currEpochSelection :: WFALS.CommitteeSelection crypto
  , prevEpochSelection :: WFALS.CommitteeSelection crypto
  }

-- | Update an inter-epoch committee selection at the beginning of a new epoch
newEpoch ::
  -- | New epoch nonce
  Nonce ->
  -- | New epoch cumulative stake distribution
  WFA.ExtWFAStakeDistr (WFALS.PublicKey crypto) ->
  -- | New epoch expected committee size
  TargetCommitteeSize ->
  -- | Current inter-epoch committee selection
  InterEpochCommitteeSelection crypto ->
  Either WFA.WFAError (InterEpochCommitteeSelection crypto)
newEpoch
  newEpochNonce
  newEpochStakeDistr
  newEpochCommitteeSize
  interEpochSelection = do
    newEpochSelection <-
      WFALS.mkCommitteeSelection
        newEpochNonce
        newEpochCommitteeSize
        newEpochStakeDistr
    pure $
      InterEpochCommitteeSelection
        { currEpochSelection =
            newEpochSelection
        , prevEpochSelection =
            currEpochSelection
              interEpochSelection
        }
