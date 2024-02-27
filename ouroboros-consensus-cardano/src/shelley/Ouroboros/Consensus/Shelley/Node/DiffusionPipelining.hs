{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.DiffusionPipelining (
    HotIdentity (..)
  , ShelleyTentativeHeaderState (..)
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Shelley.API as SL
import           Control.Monad (guard)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Shelley.Eras (StandardConway)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.Abstract

-- | Hot block issuer identity for the purpose of Shelley block diffusion
-- pipelining.
data HotIdentity c = HotIdentity {
    -- | Hash of the cold key.
    hiIssuer  :: !(SL.KeyHash SL.BlockIssuer c)
  , -- | The issue number/opcert counter. Even if the opcert was compromised and
    -- hence an attacker forges blocks with a specific cold identity, the owner
    -- of the cold key can issue a new opcert with an incremented counter, and
    -- their minted blocks will be pipelined.
    hiIssueNo :: !Word64
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data ShelleyTentativeHeaderState proto =
    -- | Legacy state, can be removed once mainnet is in Conway.
    LegacyShelleyTentativeHeaderState !(SelectViewTentativeState proto)
  | ShelleyTentativeHeaderState
      -- | The block number of the last trap tentative header.
      !(WithOrigin BlockNo)
      -- | The set of all hot identies of those who issued trap tentative
      -- headers for the recorded block number.
      --
      -- Remember that 'TentativeHeaderState's are maintained in different
      -- contexts, and we might record different identities per block number in
      -- them:
      --
      --  - In ChainSel, we record all identities of trap headers we sent.
      --
      --  - In the BlockFetch punishment logic, for each upstream peer, we
      --    record the identities of trap headers they sent.
      !(Set (HotIdentity (ProtoCrypto proto)))
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

-- | This is currently a hybrid instance:
--
--  - For eras before Conway, this uses the logic from
--    'SelectViewDiffusionPipelining' for backwards-compatibility.
--
--  - For all eras since Conway, this uses a new scheme: A header can be
--    pipelined iff no trap header with the same block number and by the same
--    issuer was pipelined before. See 'HotIdentity' for what exactly we use for
--    the issuer identity.
--
-- Once mainnet has transitioned to Conway, we can remove the pre-Conway logic
-- here.
instance
  ( ShelleyCompatible proto era
  , BlockSupportsProtocol (ShelleyBlock proto era)
  ) => BlockSupportsDiffusionPipelining (ShelleyBlock proto era) where
  type TentativeHeaderState (ShelleyBlock proto era) =
    ShelleyTentativeHeaderState proto

  initialTentativeHeaderState _
    | L.eraProtVerLow @era < L.eraProtVerLow @StandardConway
    = LegacyShelleyTentativeHeaderState NoLastInvalidSelectView
    | otherwise
    = ShelleyTentativeHeaderState Origin Set.empty

  updateTentativeHeaderState bcfg hdr@(ShelleyHeader sph _) = \case
      LegacyShelleyTentativeHeaderState st ->
        LegacyShelleyTentativeHeaderState <$>
          updateTentativeHeaderState
            (SelectViewDiffusionPipeliningBlockConfig bcfg)
            (SelectViewDiffusionPipeliningHeader hdr)
            st

      ShelleyTentativeHeaderState lastBlockNo badIdentities ->
        case compare (NotOrigin (blockNo hdr)) lastBlockNo of
          LT -> Nothing
          EQ -> do
            -- TODO Usually (especially during syncing), @badIdentities@ will be
            -- empty. However, @hdrIdentity@ will still be forced by
            -- 'Set.notMember'. According to the comment on 'isSelfIssued' for
            -- @BlockSupportsMetrics ShelleyBlock@, we could save 850ns for
            -- hashing the cold key in this case.
            --
            -- One could even replace @badIdentities@ by a list of the unhashed
            -- cold keys when its cardinality is small, for reasons similar to
            -- the ones in the comment mentioned above.
            guard $ hdrIdentity `Set.notMember` badIdentities
            Just $ ShelleyTentativeHeaderState
              lastBlockNo
              (Set.insert hdrIdentity badIdentities)
          GT ->
            Just $ ShelleyTentativeHeaderState
              (NotOrigin (blockNo hdr))
              (Set.singleton hdrIdentity)
      where
        hdrIdentity = HotIdentity {
              hiIssuer  = SL.hashKey $ pHeaderIssuer sph
            , hiIssueNo = pHeaderIssueNo sph
            }
