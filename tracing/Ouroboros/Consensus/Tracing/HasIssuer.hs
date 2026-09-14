{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Tracing.HasIssuer
  ( BlockIssuerVerificationKeyHash (..)
  , HasIssuer (..)
  ) where

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron.Common
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import qualified Cardano.Ledger.Hashes as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import Data.ByteString (ByteString)
import Data.SOP
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock, Header (..))
import Ouroboros.Consensus.HardFork.Combinator
  ( HardForkBlock
  , Header (..)
  , OneEraHeader (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (Header (..), ShelleyBlock)
import Ouroboros.Consensus.Shelley.Protocol.Abstract

-- | Block issuer verification key hash.
data BlockIssuerVerificationKeyHash
  = -- | Serialized block issuer verification key hash.
    BlockIssuerVerificationKeyHash !ByteString
  | -- | There is no block issuer.
    --
    -- For example, this could be relevant for epoch boundary blocks (EBBs),
    -- genesis blocks, etc.
    NoBlockIssuer
  deriving (Eq, Show)

-- | Get the block issuer verification key hash from a block header.
class HasIssuer blk where
  -- | Given a block header, return the serialized block issuer verification
  -- key hash.
  getIssuerVerificationKeyHash :: Header blk -> BlockIssuerVerificationKeyHash

instance HasIssuer ByronBlock where
  getIssuerVerificationKeyHash byronBlkHdr =
    case byronHeaderRaw byronBlkHdr of
      Byron.ABOBBlockHdr hdr ->
        BlockIssuerVerificationKeyHash
          -- The raw bytes of the Blake2b_224 hash of the issuer verification
          -- key. This matches @cardano-api@'s
          -- @serialiseToRawBytes . verificationKeyHash . ByronVerificationKey@.
          . Byron.Crypto.abstractHashToBytes
          . Byron.Common.unKeyHash
          . Byron.Common.hashKey
          $ Byron.headerIssuer hdr
      Byron.ABOBBoundaryHdr _ -> NoBlockIssuer

instance
  ( ProtoCrypto protocol ~ StandardCrypto
  , ProtocolHeaderSupportsProtocol protocol
  ) =>
  HasIssuer (ShelleyBlock protocol era)
  where
  getIssuerVerificationKeyHash shelleyBlkHdr =
    BlockIssuerVerificationKeyHash
      -- The raw bytes of the key hash. This matches @cardano-api@'s
      -- @serialiseToRawBytes . verificationKeyHash . StakePoolVerificationKey@;
      -- the key role is a phantom type, so hashing the block-issuer key
      -- directly yields the same bytes as first converting it to a stake
      -- pool key.
      . Crypto.hashToBytes
      . SL.unKeyHash
      . SL.hashKey
      $ pHeaderIssuer (shelleyHeaderRaw shelleyBlkHdr)

instance All HasIssuer xs => HasIssuer (HardForkBlock xs) where
  getIssuerVerificationKeyHash =
    hcollapse
      . hcmap (Proxy @HasIssuer) (K . getIssuerVerificationKeyHash)
      . getOneEraHeader
      . getHardForkHeader
