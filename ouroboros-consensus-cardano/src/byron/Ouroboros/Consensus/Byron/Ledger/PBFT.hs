{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances required to support PBFT
module Ouroboros.Consensus.Byron.Ledger.PBFT
  ( decodeByronChainDepState
  , encodeByronChainDepState
  , fromPBftLedgerView
  , mkByronContextDSIGN
  , toPBftLedgerView
  ) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Crypto.DSIGN
import Cardano.Ledger.Binary (Annotated)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Data.ByteString (ByteString)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Crypto.DSIGN
import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Byron.Ledger.Config
import Ouroboros.Consensus.Byron.Ledger.Serialisation ()
import Ouroboros.Consensus.Byron.Protocol
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S

type instance BlockProtocol ByronBlock = PBft PBftByronCrypto

-- | Construct DSIGN required for Byron crypto
mkByronContextDSIGN ::
  BlockConfig ByronBlock ->
  VerKeyDSIGN ByronDSIGN ->
  ContextDSIGN ByronDSIGN
mkByronContextDSIGN = (,) . byronProtocolMagicId

instance BlockSupportsProtocol ByronBlock where
  validateView cfg hdr@ByronHeader{..} =
    case byronHeaderRaw of
      CC.ABOBBoundaryHdr _ -> pbftValidateBoundary hdr
      CC.ABOBBlockHdr regular ->
        let pbftFields ::
              PBftFields
                PBftByronCrypto
                (Annotated CC.ToSign ByteString)
            pbftFields =
              PBftFields
                { pbftIssuer =
                    VerKeyByronDSIGN
                      . Delegation.delegateVK
                      . CC.delegationCertificate
                      . CC.headerSignature
                      $ regular
                , pbftGenKey =
                    VerKeyByronDSIGN
                      . CC.headerGenesisKey
                      $ regular
                , pbftSignature =
                    SignedDSIGN
                      . SigByronDSIGN
                      . CC.signature
                      . CC.headerSignature
                      $ regular
                }
         in PBftValidateRegular
              pbftFields
              (CC.recoverSignedBytes epochSlots regular)
              (mkByronContextDSIGN cfg (pbftGenKey pbftFields))
   where
    epochSlots = byronEpochSlots cfg

  tiebreakerView _ = mkPBftTiebreakerView

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftByronCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftByronCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeByronChainDepState ::
  ChainDepState (BlockProtocol ByronBlock) ->
  Encoding
encodeByronChainDepState = S.encodePBftState

decodeByronChainDepState ::
  Decoder s (ChainDepState (BlockProtocol ByronBlock))
decodeByronChainDepState = S.decodePBftState
