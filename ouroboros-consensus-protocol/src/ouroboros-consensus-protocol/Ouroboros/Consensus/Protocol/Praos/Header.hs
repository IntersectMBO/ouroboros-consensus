{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
  , decodeListLen
  , encodeListLen
  , serialize'
  , unCBORGroup
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
  , HashAnnotated (..)
  , SafeToHash
  , extractHash
  , originalBytesSize
  )
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import Cardano.Ledger.MemoBytes
  ( Mem
  , MemoBytes
  , MemoHashIndex
  , Memoized (..)
  , getMemoRawType
  , getMemoSafeHash
  , mkMemoized
  )
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import Cardano.Protocol.TPraos.BHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import LeiosDemoTypes (EbAnnouncement)
import NoThunks.Class (NoThunks (..))
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
  , hbVk :: !(VKey BlockIssuer)
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
  , hbLeiosEbAnnouncement :: !(StrictMaybe EbAnnouncement)
  -- ^ Optional Leios endorser-block announcement (Dijkstra-only;
  -- 'SNothing' on earlier eras). Placed on the Praos header for
  -- early-diffusion of EB references before the body arrives.
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

instance
  Crypto crypto =>
  NoThunks (HeaderRaw crypto)

-- | Full header type, carrying its own memoised bytes.
newtype Header crypto = HeaderConstr (MemoBytes (HeaderRaw crypto))
  deriving Generic
  deriving newtype (Eq, Show, NoThunks, Plain.ToCBOR, SafeToHash)

instance Memoized (Header crypto) where
  type RawType (Header crypto) = HeaderRaw crypto

type instance MemoHashIndex (HeaderRaw crypto) = EraIndependentBlockHeader

instance HashAnnotated (Header crypto) EraIndependentBlockHeader where
  hashAnnotated = getMemoSafeHash

pattern Header ::
  Crypto crypto =>
  HeaderBody crypto ->
  KES.SignedKES (KES crypto) (HeaderBody crypto) ->
  Header crypto
pattern Header{headerBody, headerSig} <- (getMemoRawType -> HeaderRaw headerBody headerSig)
  where
    Header body sig = mkMemoized (pvMajor (hbProtVer body)) $ HeaderRaw body sig
{-# COMPLETE Header #-}

-- | Compute the size of the header
headerSize :: Header crypto -> Int
headerSize = originalBytesSize

-- | Hash a header
headerHash ::
  Header crypto ->
  Hash.Hash HASH EraIndependentBlockHeader
headerHash = extractHash . hashAnnotated

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | 10-field encoding when 'SNothing' (pre-Leios-compatible), 11 when 'SJust'.
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
      , hbLeiosEbAnnouncement
      } =
      let (len, encEbAnnouncement) = case hbLeiosEbAnnouncement of
            SNothing -> (10 :: Word, mempty)
            SJust ebAnnouncement -> (11, encCBOR ebAnnouncement)
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
    hbLeiosEbAnnouncement <- case len of
      10 -> pure SNothing
      11 -> SJust <$> decCBOR
      _ -> fail $ "Praos HeaderBody CBOR has wrong length: " <> show len
    pure
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
        , hbLeiosEbAnnouncement
        }

encodeHeaderRaw ::
  Crypto crypto =>
  HeaderRaw crypto ->
  Encode (Closed Dense) (HeaderRaw crypto)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR = encode . encodeHeaderRaw

instance Crypto crypto => DecCBOR (HeaderRaw crypto) where
  decCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto c => EncCBOR (Header c)

deriving via
  Mem (HeaderRaw crypto)
  instance
    Crypto crypto => DecCBOR (Annotator (Header crypto))
