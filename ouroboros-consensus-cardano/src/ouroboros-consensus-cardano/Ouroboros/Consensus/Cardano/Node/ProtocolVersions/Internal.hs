{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

-- | See "Ouroboros.Consensus.Cardano.Node.ProtocolVersions".
module Ouroboros.Consensus.Cardano.Node.ProtocolVersions.Internal (
    -- * Cardano protocol versions
    CardanoProtocolVersions (CardanoProtocolVersions_, getCardanoProtocolVersions, CardanoProtocolVersions, protVerByron, protVerShelley, protVerAllegra, protVerMary, protVerAlonzo, protVerBabbage, protVerConway)
  , cardanoMainnetProtocolVersions
  , cardanoMaxProtocolVersion
    -- * HasProtocolVersion
  , HasProtocolVersion (..)
  , ProtocolVersion
    -- * Value for /each/ era
  , PerEraProtocolVersion (..)
  , WrapProtocolVersion (..)
    -- * Maximum protocol versions
  , Last
  , lastNP
  , maxProtocolVersion
  ) where

import Data.Proxy (Proxy (..))
import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Ledger.BaseTypes as SL (natVersion)
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Kind (Constraint, Type)
import           Data.SOP.NonEmpty (IsNonEmpty (..), ProofEmpty (ProofEmpty),
                     ProofNonEmpty (ProofNonEmpty), checkEmptiness)
import           Data.SOP.Strict (NP (..))
import Data.SOP (SListI)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Node (ProtocolParams (..))
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras)
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     BabbageEra, ConwayEra, MaryEra, ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node (ProtocolParams (..))

{-------------------------------------------------------------------------------
  Cardano protocol versions
-------------------------------------------------------------------------------}

newtype CardanoProtocolVersions c = CardanoProtocolVersions_ {
    getCardanoProtocolVersions :: PerEraProtocolVersion (CardanoEras c)
  }

{-# COMPLETE CardanoProtocolVersions #-}
pattern CardanoProtocolVersions ::
     ProtocolVersion ByronBlock
  -> ProtocolVersion (ShelleyBlock (TPraos c) (ShelleyEra c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (AllegraEra c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (MaryEra    c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (AlonzoEra  c))
  -> ProtocolVersion (ShelleyBlock (Praos  c) (BabbageEra c))
  -> ProtocolVersion (ShelleyBlock (Praos  c) (ConwayEra  c))
  -> CardanoProtocolVersions c
pattern CardanoProtocolVersions {
      protVerByron
    , protVerShelley
    , protVerAllegra
    , protVerMary
    , protVerAlonzo
    , protVerBabbage
    , protVerConway
    } =
  CardanoProtocolVersions_ {
      getCardanoProtocolVersions = PerEraProtocolVersion {
          getPerEraProtocolVersion =
               WrapProtocolVersion protVerByron
            :* WrapProtocolVersion protVerShelley
            :* WrapProtocolVersion protVerAllegra
            :* WrapProtocolVersion protVerMary
            :* WrapProtocolVersion protVerAlonzo
            :* WrapProtocolVersion protVerBabbage
            :* WrapProtocolVersion protVerConway
            :* Nil
        }
    }

cardanoMainnetProtocolVersions :: Bool -> CardanoProtocolVersions c
cardanoMainnetProtocolVersions experimentalHardForksEnabled =
    CardanoProtocolVersions {
        protVerByron   = Byron.Update.ProtocolVersion 3 0 0
      , protVerShelley = SL.ProtVer (SL.natVersion @3) 0
      , protVerAllegra = SL.ProtVer (SL.natVersion @4) 0
      , protVerMary    = SL.ProtVer (SL.natVersion @5) 0
      , protVerAlonzo  = SL.ProtVer (SL.natVersion @7) 0
      , protVerBabbage = if experimentalHardForksEnabled then
                           SL.ProtVer (SL.natVersion @9) 0
                         else
                           SL.ProtVer (SL.natVersion @8) 0
      , protVerConway  = SL.ProtVer (SL.natVersion @9) 0
      }

cardanoMaxProtocolVersion :: CardanoProtocolVersions c -> SL.ProtVer
cardanoMaxProtocolVersion = maxProtocolVersion . getCardanoProtocolVersions

{-------------------------------------------------------------------------------
  HasProtocolVersion
-------------------------------------------------------------------------------}

-- | This type family is not an associated type of the 'HasProtocolVersion'
-- class, as it reduces duplication: all Shelley blocks have the same
-- 'ProtocolVersion' type.
type ProtocolVersion :: Type -> Type
type family ProtocolVersion blk where
  ProtocolVersion ByronBlock               = Byron.Update.ProtocolVersion
  ProtocolVersion (ShelleyBlock proto era) = SL.ProtVer
  ProtocolVersion (CardanoBlock c)         = CardanoProtocolVersions c

type HasProtocolVersion :: Type -> Constraint
class HasProtocolVersion blk where
  extractProtocolVersion :: ProtocolParams blk -> ProtocolVersion blk

{-------------------------------------------------------------------------------
  HasProtocolVersion: block instances
-------------------------------------------------------------------------------}

instance HasProtocolVersion ByronBlock where
  extractProtocolVersion = byronProtocolVersion

instance HasProtocolVersion (ShelleyBlock (TPraos c) (ShelleyEra c)) where
  extractProtocolVersion = shelleyProtVer

instance HasProtocolVersion (ShelleyBlock (TPraos c) (AllegraEra c)) where
  extractProtocolVersion = allegraProtVer

instance HasProtocolVersion (ShelleyBlock (TPraos c) (MaryEra c)) where
  extractProtocolVersion = maryProtVer

instance HasProtocolVersion (ShelleyBlock (TPraos c) (AlonzoEra c)) where
  extractProtocolVersion = alonzoProtVer

instance HasProtocolVersion (ShelleyBlock (Praos c) (BabbageEra c)) where
  extractProtocolVersion = babbageProtVer

instance HasProtocolVersion (ShelleyBlock (Praos c) (ConwayEra c)) where
  extractProtocolVersion = conwayProtVer

{-------------------------------------------------------------------------------
  Value for /each/ era
-------------------------------------------------------------------------------}

type PerEraProtocolVersion :: [Type] -> Type
newtype PerEraProtocolVersion xs = PerEraProtocolVersion {
    getPerEraProtocolVersion :: NP WrapProtocolVersion xs
  }

type WrapProtocolVersion :: Type -> Type
newtype WrapProtocolVersion blk = WrapProtocolVersion {
    unwrapProtocolVersion :: ProtocolVersion blk
  }

{-------------------------------------------------------------------------------
  Maximum protocol version
-------------------------------------------------------------------------------}

maxProtocolVersion ::
     (IsNonEmpty xs, SListI xs)
  => PerEraProtocolVersion xs
  -> ProtocolVersion (Last xs)
maxProtocolVersion = unwrapProtocolVersion . lastNP . getPerEraProtocolVersion

type Last :: [k] -> k
type family Last xs where
  Last '[x]    = x
  Last (x:xs) = Last xs

lastNP :: forall f xs. (IsNonEmpty xs, SListI xs) => NP f xs -> f (Last xs)
lastNP xs0 =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty Proxy (Proxy :: Proxy xs') ->
        case checkEmptiness (Proxy @xs') of
          Left ProofEmpty ->
            case xs0 of
              x :* Nil -> x
          Right (ProofNonEmpty Proxy (Proxy :: Proxy xs'')) ->
            case xs0 of
              _ :* xs -> lastNP xs
