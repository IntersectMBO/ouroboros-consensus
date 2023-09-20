{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Inspect (
    ProtocolUpdate (..)
  , ShelleyLedgerUpdate (..)
  , UpdateProposal (..)
  , UpdateState (..)
  , protocolUpdates
  ) where

import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.Core (ppuProtocolVersionL)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.PParams as SL
import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Void
import           Data.Word (Word64)
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Ticked (WhetherTickedOrNot (..))
import           Ouroboros.Consensus.Util.Condense

data ProtocolUpdate era = ProtocolUpdate {
      protocolUpdateProposal :: UpdateProposal era
    , protocolUpdateState    :: UpdateState (EraCrypto era)
    }
deriving instance Eq (Core.PParamsUpdate era) => Eq (ProtocolUpdate era)
deriving instance Show (Core.PParamsUpdate era) => Show (ProtocolUpdate era)

-- | Update proposal
--
-- As in Byron, a proposal is a partial map from parameters to their values.
data UpdateProposal era = UpdateProposal {
      -- | The protocol parameters changed by this update proposal
      --
      -- An update is /identified/ by how it updates the protocol parameters.
      proposalParams  :: Core.PParamsUpdate era

      -- | New version (if changed by this proposal)
      --
      -- The protocol version itself is also considered to be just another
      -- parameter, and parameters can change /without/ changing the protocol
      -- version, although a convention /could/ be established that the protocol
      -- version must change if any of the parameters do; but the specification
      -- itself does not mandate this.
      --
      -- We record the version separately for the convenience of the HFC.
    , proposalVersion :: Maybe SL.ProtVer

      -- | The 'EpochNo' the proposal becomes active in, if it is adopted
    , proposalEpoch   :: EpochNo
    }

deriving instance Eq (Core.PParamsUpdate era) => Eq (UpdateProposal era)
deriving instance Show (Core.PParamsUpdate era) => Show (UpdateProposal era)

-- | Proposal state
--
-- The update mechanism in Shelley is simpler than it is in Byron. There is no
-- distinction between votes and proposals: to \"vote\" for a proposal one
-- merely submits the exact same proposal. There is also no separate
-- endorsement step. The procedure is as follows:
--
-- 1. During each epoch, a genesis key can submit (via its delegates) zero,
--    one, or many proposals; each submission overrides the previous one.
-- 2. \"Voting\" (submitting of proposals) ends @2 * stabilityWindow@ slots
--    (i.e. @6k/f@) before the end of the epoch. In other words, proposals
--    for the upcoming epoch must be submitted within the first @4k/f@ slots
--    of this one.
-- 3. At the end of an epoch, if the majority of nodes (as determined by the
--    @Quorum@ specification constant, which must be greater than half the
--    nodes) have most recently submitted the same exact proposal, then it is
--    adopted.
-- 4. The next epoch is always started with a clean slate, proposals from the
--    previous epoch that didn't make it are discarded (except for "future
--    proposals" that are explicitly marked for future epochs).
data UpdateState c = UpdateState {
      -- | The genesis delegates that voted for this proposal
      proposalVotes         :: [SL.KeyHash 'SL.Genesis c]

      -- | Has this proposal reached sufficient votes to be adopted?
    , proposalReachedQuorum :: Bool
    }
  deriving (Show, Eq)

protocolUpdates ::
       forall era proto. ShelleyBasedEra era
    => SL.ShelleyGenesis (EraCrypto era)
    -> WhetherTickedOrNot (LedgerState (ShelleyBlock proto era))
    -> [ProtocolUpdate era]
protocolUpdates genesis wtSt = [
      ProtocolUpdate {
          protocolUpdateProposal = UpdateProposal {
              proposalParams  = proposal
            , proposalEpoch   = succ currentEpoch
            , proposalVersion = strictMaybeToMaybe $
                                  proposal ^. ppuProtocolVersionL
            }
        , protocolUpdateState = UpdateState {
              proposalVotes         = votes
            , proposalReachedQuorum = length votes >= fromIntegral quorum
            }
        }
    | (proposal, votes) <- Map.toList $ invertMap proposals
    ]
  where
    nes = case wtSt of
        YesTicked x -> tickedShelleyLedgerState x
        NoTicked  x -> shelleyLedgerState x

    invertMap :: Ord b => Map a b -> Map b [a]
    invertMap = Map.fromListWith (<>) . fmap swizzle . Map.toList
      where
        swizzle (a, b) = (b, [a])

    -- Updated proposed within the proposal window
    proposals :: Map (SL.KeyHash 'SL.Genesis (EraCrypto era)) (Core.PParamsUpdate era)
    SL.ProposedPPUpdates proposals =
          fromMaybe SL.emptyPPPUpdates
        . Core.getProposedPPUpdates
        . SL.utxosGovState
        . SL.lsUTxOState
        . SL.esLState
        . SL.nesEs
        $ nes
    -- A proposal is accepted if the number of votes is equal to or greater
    -- than the quorum. The quorum itself must be strictly greater than half
    -- the number of genesis keys, but we do not rely on that property here.
    quorum :: Word64
    quorum = SL.sgUpdateQuorum genesis

    -- The proposals in 'SL.proposals' are for the upcoming epoch
    -- (we ignore 'futureProposals')
    currentEpoch :: EpochNo
    currentEpoch = SL.nesEL nes

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data ShelleyLedgerUpdate era =
    ShelleyUpdatedProtocolUpdates [ProtocolUpdate era]

deriving instance Eq (Core.PParamsUpdate era) => Eq (ShelleyLedgerUpdate era)
deriving instance Show (Core.PParamsUpdate era) => Show (ShelleyLedgerUpdate era)

instance Show (Core.PParamsUpdate era) => Condense (ShelleyLedgerUpdate era) where
  condense = show

instance ShelleyBasedEra era => InspectLedger (ShelleyBlock proto era) where
  type LedgerWarning (ShelleyBlock proto era) = Void
  type LedgerUpdate  (ShelleyBlock proto era) = ShelleyLedgerUpdate era

  inspectLedger tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate $ ShelleyUpdatedProtocolUpdates updatesAfter
    where
      genesis :: SL.ShelleyGenesis (EraCrypto era)
      genesis = shelleyLedgerGenesis (configLedger tlc)

      updatesBefore, updatesAfter :: [ProtocolUpdate era]
      updatesBefore = protocolUpdates genesis (NoTicked before)
      updatesAfter  = protocolUpdates genesis (NoTicked after)
