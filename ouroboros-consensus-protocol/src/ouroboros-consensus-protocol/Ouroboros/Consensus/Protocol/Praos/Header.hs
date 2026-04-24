{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | Block header associated with Praos.
--
-- The choice of whether to associate the header with the ledger era or the
-- protocol is a little artitrary. Functionally the header contains things which
-- are associated with both ledger and protocol, and which are used by both.
--
-- We choose to associate the header with the protocol, since it more strongly
-- binds in that direction, and to assist with the mental picture that the
-- protocol is concerned with the block header, while the ledger is concerned
-- with the block body. However, in order to more cleanly illustrate which parts
-- of the header are _strictly_ protocol concerns, we also provide a view of the
-- header (in 'Ouroboros.Consensus.Protocol.Praos.Views') which extracts just
-- the fields needed for the Praos protocol. This also allows us to hide the
-- more detailed construction of the header.
module Ouroboros.Consensus.Protocol.Praos.Header
  ( Header (Header, headerBody, headerSig)
  , HeaderBody (..)
  , headerHash
  , headerSize
  ) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util
  ( SignableRepresentation (getSignableRepresentation)
  )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (ProtVer (pvMajor))
import Cardano.Ledger.Binary
  ( Annotator (..)
  , DecCBOR (decCBOR)
  , EncCBOR (..)
  , ToCBOR (..)
  , decodeListLen
  , encodeListLen
  , encodedSigKESSizeExpr
  , serialize'
  , unCBORGroup
  , withSlice
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Binary.Crypto
  ( decodeSignedKES
  , decodeVerKeyVRF
  , encodeSignedKES
  , encodeVerKeyVRF
  )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Hashes
  ( EraIndependentBlockBody
  , EraIndependentBlockHeader
  , HASH
  )
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import Cardano.Protocol.TPraos.BHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word32)
import GHC.Generics (Generic)
import LeiosDemoTypes (EbAnnouncement)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | The body of the header is the part which gets hashed to form the hash
-- chain.
data HeaderBody crypto = HeaderBody
  { hbBlockNo :: !BlockNo
  -- ^ block number
  , hbSlotNo :: !SlotNo
  -- ^ block slot
  , hbPrev :: !PrevHash
  -- ^ Hash of the previous block header
  , hbVk :: !(VKey 'BlockIssuer)
  -- ^ verification key of block issuer
  , hbVrfVk :: !(VRF.VerKeyVRF (VRF crypto))
  -- ^ VRF verification key for block issuer
  , hbVrfRes :: !(VRF.CertifiedVRF (VRF crypto) InputVRF)
  -- ^ Certified VRF value
  , hbBodySize :: !Word32
  -- ^ Size of the block body
  , hbBodyHash :: !(Hash.Hash HASH EraIndependentBlockBody)
  -- ^ Hash of block body
  , hbOCert :: !(OCert crypto)
  -- ^ operational certificate
  , hbProtVer :: !ProtVer
  -- ^ protocol version
  , hbMayEbAnnouncement :: Maybe EbAnnouncement
  -- ^ Leios EB announcement
  }
  deriving Generic

deriving instance Crypto crypto => Show (HeaderBody crypto)

deriving instance Crypto crypto => Eq (HeaderBody crypto)

instance
  Crypto crypto =>
  SignableRepresentation (HeaderBody crypto)
  where
  getSignableRepresentation hb = serialize' (pvMajor (hbProtVer hb)) hb

instance
  Crypto crypto =>
  NoThunks (HeaderBody crypto)

data HeaderRaw crypto = HeaderRaw
  { headerRawBody :: !(HeaderBody crypto)
  , headerRawSig :: !(KES.SignedKES (KES crypto) (HeaderBody crypto))
  }
  deriving (Show, Generic)

instance Crypto c => Eq (HeaderRaw c) where
  h1 == h2 =
    headerRawSig h1 == headerRawSig h2
      && headerRawBody h1 == headerRawBody h2

-- | Checks the binary representation first.
instance Crypto c => Eq (Header c) where
  h1 == h2 =
    headerBytes h1 == headerBytes h2
      && headerRaw h1 == headerRaw h2

instance
  Crypto crypto =>
  NoThunks (HeaderRaw crypto)

-- | Full header type, carrying its own memoised bytes.
data Header crypto = HeaderConstr
  { headerRaw :: !(HeaderRaw crypto)
  , headerBytes :: BS.ByteString -- lazy on purpose, constructed on demand
  }
  deriving (Show, Generic)
  deriving NoThunks via AllowThunksIn '["headerBytes"] (Header crypto)

pattern Header ::
  Crypto crypto =>
  HeaderBody crypto ->
  KES.SignedKES (KES crypto) (HeaderBody crypto) ->
  Header crypto
pattern Header{headerBody, headerSig} <-
  HeaderConstr
    { headerRaw =
      HeaderRaw
        { headerRawBody = headerBody
        , headerRawSig = headerSig
        }
    }
  where
    Header body sig =
      let header =
            HeaderRaw
              { headerRawBody = body
              , headerRawSig = sig
              }
       in HeaderConstr
            { headerRaw = header
            , headerBytes = serialize' (pvMajor (hbProtVer body)) header
            }

{-# COMPLETE Header #-}

-- | Compute the size of the header
headerSize :: Header crypto -> Int
headerSize (HeaderConstr _ bytes) = BS.length bytes

-- | Hash a header
headerHash ::
  Crypto crypto =>
  Header crypto ->
  Hash.Hash HASH EraIndependentBlockHeader
headerHash = Hash.castHash . Hash.hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- NOTE(bladyjoker): The HeaderBody codec is trying to be backwards compatible with tools that only work with plain Praos headers.

instance Crypto crypto => EncCBOR (HeaderBody crypto) where
  encCBOR
    HeaderBody
      { hbBlockNo
      , hbSlotNo
      , hbPrev
      , hbVk
      , hbVrfVk
      , hbVrfRes
      , hbBodySize
      , hbBodyHash
      , hbOCert
      , hbProtVer
      , hbMayEbAnnouncement
      } =
      let (len, encEbAnnouncement) = case hbMayEbAnnouncement of -- TODO(bladyjoker): This shennanigans is here for backwards compatibility, remove it!
            Nothing -> (10, mempty)
            Just ebAnnouncement ->
              ( 11
              , encCBOR ebAnnouncement
              )
       in mconcat
            [ encodeListLen len
            , encCBOR hbBlockNo
            , encCBOR hbSlotNo
            , encCBOR hbPrev
            , encCBOR hbVk
            , encodeVerKeyVRF hbVrfVk
            , encCBOR hbVrfRes
            , encCBOR hbBodySize
            , encCBOR hbBodyHash
            , encCBOR hbOCert
            , encCBOR hbProtVer
            , encEbAnnouncement
            ]

instance Crypto crypto => DecCBOR (HeaderBody crypto) where
  decCBOR = do
    len <- decodeListLen
    hbBlockNo <- decCBOR
    hbSlotNo <- decCBOR
    hbPrev <- decCBOR
    hbVk <- decCBOR
    hbVrfVk <- decodeVerKeyVRF
    hbVrfRes <- decCBOR
    hbBodySize <- decCBOR
    hbBodyHash <- decCBOR
    hbOCert <- unCBORGroup <$> decCBOR
    hbProtVer <- decCBOR
    hbMayEbAnnouncement <- case len of
      10 -> return Nothing
      11 -> Just <$> decCBOR @EbAnnouncement -- TODO(bladyjoker): This shennanigans is here for backwards compatibility, remove it!
      _ -> fail $ "Praos HeaderBody CBOR has wrong length: " <> show len
    return $
      HeaderBody
        { hbBlockNo
        , hbSlotNo
        , hbPrev
        , hbVk
        , hbVrfVk
        , hbVrfRes
        , hbBodySize
        , hbBodyHash
        , hbOCert
        , hbProtVer
        , hbMayEbAnnouncement
        }

encodeHeaderRaw ::
  Crypto crypto =>
  HeaderRaw crypto ->
  Encode ('Closed 'Dense) (HeaderRaw crypto)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR = encode . encodeHeaderRaw

instance Crypto crypto => DecCBOR (HeaderRaw crypto) where
  decCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto c => Plain.ToCBOR (Header c) where
  toCBOR (HeaderConstr _ bytes) = Plain.encodePreEncoded bytes

instance Crypto c => EncCBOR (Header c) where
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (headerRawBody . headerRaw <$> proxy)
      + encodedSigKESSizeExpr (KES.getSig . headerRawSig . headerRaw <$> proxy)

instance Crypto c => DecCBOR (Annotator (Header c)) where
  decCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice decCBOR
    pure (Annotator (\fullbytes -> HeaderConstr (getT fullbytes) (BSL.toStrict (getBytes fullbytes))))
