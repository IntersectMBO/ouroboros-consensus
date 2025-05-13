{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Cardano.Block (
    -- * Eras
    CardanoEras
  , CardanoShelleyEras
  , module Ouroboros.Consensus.Shelley.Eras
  , ShelleyBasedLedgerEras
    -- * Block
  , CardanoBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockAllegra, BlockAlonzo, BlockByron, BlockMary, BlockShelley, BlockBabbage, BlockConway)
    -- * Headers
  , CardanoHeader
  , Header (HeaderAllegra, HeaderAlonzo, HeaderByron, HeaderMary, HeaderShelley, HeaderBabbage, HeaderConway)
    -- * Generalised transactions
  , CardanoApplyTxErr
  , CardanoGenTx
  , CardanoGenTxId
  , GenTx (GenTxAllegra, GenTxAlonzo, GenTxByron, GenTxMary, GenTxShelley, GenTxBabbage, GenTxConway)
  , HardForkApplyTxErr (ApplyTxErrAllegra, ApplyTxErrAlonzo, ApplyTxErrByron, ApplyTxErrMary, ApplyTxErrShelley, ApplyTxErrWrongEra, ApplyTxErrBabbage, ApplyTxErrConway)
  , TxId (CardanoGenTxId)
    -- * LedgerError
  , CardanoLedgerError
  , HardForkLedgerError (LedgerErrorAllegra, LedgerErrorAlonzo, LedgerErrorByron, LedgerErrorMary, LedgerErrorShelley, LedgerErrorWrongEra, LedgerErrorBabbage, LedgerErrorConway)
    -- * OtherEnvelopeError
  , CardanoOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorAllegra, OtherHeaderEnvelopeErrorBabbage, OtherHeaderEnvelopeErrorConway, OtherHeaderEnvelopeErrorAlonzo, OtherHeaderEnvelopeErrorByron, OtherHeaderEnvelopeErrorMary, OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , CardanoTipInfo
  , OneEraTipInfo (TipInfoAllegra, TipInfoAlonzo, TipInfoByron, TipInfoBabbage, TipInfoConway, TipInfoMary, TipInfoShelley)
    -- * Query
  , BlockQuery (QueryAnytimeAllegra, QueryAnytimeAlonzo, QueryAnytimeBabbage, QueryAnytimeConway, QueryAnytimeByron, QueryAnytimeMary, QueryAnytimeShelley, QueryHardFork, QueryIfCurrentAllegra, QueryIfCurrentAlonzo, QueryIfCurrentBabbage, QueryIfCurrentConway, QueryIfCurrentByron, QueryIfCurrentMary, QueryIfCurrentShelley)
  , CardanoQuery
  , CardanoQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
    -- * CodecConfig
  , CardanoCodecConfig
  , CodecConfig (CardanoCodecConfig)
    -- * BlockConfig
  , BlockConfig (CardanoBlockConfig)
  , CardanoBlockConfig
    -- * StorageConfig
  , CardanoStorageConfig
  , StorageConfig (CardanoStorageConfig)
    -- * ConsensusConfig
  , CardanoConsensusConfig
  , ConsensusConfig (CardanoConsensusConfig)
    -- * LedgerConfig
  , CardanoLedgerConfig
  , HardForkLedgerConfig (CardanoLedgerConfig)
    -- * LedgerState
  , CardanoLedgerState
  , LedgerState (LedgerStateAllegra, LedgerStateAlonzo, LedgerStateBabbage, LedgerStateConway, LedgerStateByron, LedgerStateMary, LedgerStateShelley)
    -- * ChainDepState
  , CardanoChainDepState
  , HardForkState (ChainDepStateAllegra, ChainDepStateAlonzo, ChainDepStateBabbage, ChainDepStateConway, ChainDepStateByron, ChainDepStateMary, ChainDepStateShelley)
    -- * EraMismatch
  , EraMismatch (..)
  ) where

import           Data.ByteString.Short (ShortByteString)
import           Data.Kind
import           Data.SOP.BasicFunctors
import           Data.SOP.Functors
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError,
                     TipInfo)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  The eras of the Cardano blockchain
-------------------------------------------------------------------------------}

-- | The eras in the Cardano blockchain.
--
-- We parameterise over the crypto used in the post-Byron eras: @c@.
--
-- TODO: parameterise ByronBlock over crypto too
type CardanoEras :: Type -> [Type]
type CardanoEras c = ByronBlock ': CardanoShelleyEras c

type CardanoShelleyEras :: Type -> [Type]
type CardanoShelleyEras c =
  '[ ShelleyBlock (TPraos c) ShelleyEra
   , ShelleyBlock (TPraos c) AllegraEra
   , ShelleyBlock (TPraos c) MaryEra
   , ShelleyBlock (TPraos c) AlonzoEra
   , ShelleyBlock (Praos c)  BabbageEra
   , ShelleyBlock (Praos c)  ConwayEra
   ]

type ShelleyBasedLedgerEras :: Type -> [Type]
type ShelleyBasedLedgerEras c =
  '[ ShelleyEra
   , AllegraEra
   , MaryEra
   , AlonzoEra
   , BabbageEra
   , ConwayEra
   ]

{-------------------------------------------------------------------------------
  INTERNAL A tag function for each era
-------------------------------------------------------------------------------}

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TagByron   :: f ByronBlock                           -> NS f (CardanoEras c)
pattern TagShelley :: f (ShelleyBlock (TPraos c) ShelleyEra) -> NS f (CardanoEras c)
pattern TagAllegra :: f (ShelleyBlock (TPraos c) AllegraEra) -> NS f (CardanoEras c)
pattern TagMary    :: f (ShelleyBlock (TPraos c) MaryEra   ) -> NS f (CardanoEras c)
pattern TagAlonzo  :: f (ShelleyBlock (TPraos c) AlonzoEra ) -> NS f (CardanoEras c)
pattern TagBabbage :: f (ShelleyBlock (Praos c)  BabbageEra) -> NS f (CardanoEras c)
pattern TagConway  :: f (ShelleyBlock (Praos c)  ConwayEra ) -> NS f (CardanoEras c)

pattern TagByron   x =                   Z x
pattern TagShelley x =                S (Z x)
pattern TagAllegra x =             S (S (Z x))
pattern TagMary    x =          S (S (S (Z x)))
pattern TagAlonzo  x =       S (S (S (S (Z x))))
pattern TagBabbage x =    S (S (S (S (S (Z x)))))
pattern TagConway  x = S (S (S (S (S (S (Z x))))))

{-------------------------------------------------------------------------------
  INTERNAL A telescope function for each era

-------------------------------------------------------------------------------}

pattern TeleByron   ::
     f ByronBlock
  -> Telescope g f (CardanoEras c)

pattern TeleShelley ::
     g ByronBlock
  -> f (ShelleyBlock (TPraos c) ShelleyEra)
  -> Telescope g f (CardanoEras c)

pattern TeleAllegra ::
     g ByronBlock
  -> g (ShelleyBlock (TPraos c) ShelleyEra)
  -> f (ShelleyBlock (TPraos c) AllegraEra)
  -> Telescope g f (CardanoEras c)

pattern TeleMary    ::
     g ByronBlock
  -> g (ShelleyBlock (TPraos c) ShelleyEra)
  -> g (ShelleyBlock (TPraos c) AllegraEra)
  -> f (ShelleyBlock (TPraos c) MaryEra)
  -> Telescope g f (CardanoEras c)

pattern TeleAlonzo  ::
     g ByronBlock
  -> g (ShelleyBlock (TPraos c) ShelleyEra)
  -> g (ShelleyBlock (TPraos c) AllegraEra)
  -> g (ShelleyBlock (TPraos c) MaryEra)
  -> f (ShelleyBlock (TPraos c) AlonzoEra)
  -> Telescope g f (CardanoEras c)

pattern TeleBabbage  ::
     g ByronBlock
  -> g (ShelleyBlock (TPraos c) ShelleyEra)
  -> g (ShelleyBlock (TPraos c) AllegraEra)
  -> g (ShelleyBlock (TPraos c) MaryEra)
  -> g (ShelleyBlock (TPraos c) AlonzoEra)
  -> f (ShelleyBlock (Praos c)  BabbageEra)
  -> Telescope g f (CardanoEras c)

pattern TeleConway  ::
     g ByronBlock
  -> g (ShelleyBlock (TPraos c) ShelleyEra)
  -> g (ShelleyBlock (TPraos c) AllegraEra)
  -> g (ShelleyBlock (TPraos c) MaryEra)
  -> g (ShelleyBlock (TPraos c) AlonzoEra)
  -> g (ShelleyBlock (Praos c)  BabbageEra)
  -> f (ShelleyBlock (Praos c)  ConwayEra)
  -> Telescope g f (CardanoEras c)

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TeleByron                                             x =                                                                   TZ x
pattern TeleShelley byron                                     x = TS byron                                                         (TZ x)
pattern TeleAllegra byron shelley                             x = TS byron (TS shelley                                             (TZ x))
pattern TeleMary    byron shelley allegra                     x = TS byron (TS shelley (TS allegra                                 (TZ x)))
pattern TeleAlonzo  byron shelley allegra mary                x = TS byron (TS shelley (TS allegra (TS mary                        (TZ x))))
pattern TeleBabbage byron shelley allegra mary alonzo         x = TS byron (TS shelley (TS allegra (TS mary (TS alonzo             (TZ x)))))
pattern TeleConway  byron shelley allegra mary alonzo babbage x = TS byron (TS shelley (TS allegra (TS mary (TS alonzo (TS babbage (TZ x))))))

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain
-------------------------------------------------------------------------------}

-- | /The/ Cardano block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockByron', 'BlockShelley', etc.
--
-- > f :: CardanoBlock c -> _
-- > f (BlockByron   b) = _
-- > f (BlockShelley s) = _
-- > f (BlockAllegra a) = _
-- > f (BlockMary    m) = _
-- > f (BlockAlonzo  m) = _
--
type CardanoBlock c = HardForkBlock (CardanoEras c)

pattern BlockByron :: ByronBlock -> CardanoBlock c
pattern BlockByron b = HardForkBlock (OneEraBlock (TagByron (I b)))

pattern BlockShelley :: ShelleyBlock (TPraos c) ShelleyEra -> CardanoBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (TagShelley (I b)))

pattern BlockAllegra :: ShelleyBlock (TPraos c) AllegraEra -> CardanoBlock c
pattern BlockAllegra b = HardForkBlock (OneEraBlock (TagAllegra (I b)))

pattern BlockMary :: ShelleyBlock (TPraos c) MaryEra -> CardanoBlock c
pattern BlockMary b = HardForkBlock (OneEraBlock (TagMary (I b)))

pattern BlockAlonzo :: ShelleyBlock (TPraos c) AlonzoEra -> CardanoBlock c
pattern BlockAlonzo b = HardForkBlock (OneEraBlock (TagAlonzo (I b)))

pattern BlockBabbage :: ShelleyBlock (Praos c) BabbageEra -> CardanoBlock c
pattern BlockBabbage b = HardForkBlock (OneEraBlock (TagBabbage (I b)))

pattern BlockConway :: ShelleyBlock (Praos c) ConwayEra -> CardanoBlock c
pattern BlockConway b = HardForkBlock (OneEraBlock (TagConway (I b)))

{-# COMPLETE
    BlockByron
  , BlockShelley
  , BlockAllegra
  , BlockMary
  , BlockAlonzo
  , BlockBabbage
  , BlockConway
  #-}


{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
type CardanoHeader c = Header (CardanoBlock c)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader c
pattern HeaderByron h = HardForkHeader (OneEraHeader (TagByron h))

pattern HeaderShelley ::
     Header (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (TagShelley h))

pattern HeaderAllegra ::
     Header (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoHeader c
pattern HeaderAllegra h = HardForkHeader (OneEraHeader (TagAllegra h))

pattern HeaderMary ::
     Header (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoHeader c
pattern HeaderMary h = HardForkHeader (OneEraHeader (TagMary h))

pattern HeaderAlonzo ::
     Header (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoHeader c
pattern HeaderAlonzo h = HardForkHeader (OneEraHeader (TagAlonzo h))

pattern HeaderBabbage ::
     Header (ShelleyBlock (Praos c) BabbageEra)
  -> CardanoHeader c
pattern HeaderBabbage h = HardForkHeader (OneEraHeader (TagBabbage h))

pattern HeaderConway ::
     Header (ShelleyBlock (Praos c) ConwayEra)
  -> CardanoHeader c
pattern HeaderConway h = HardForkHeader (OneEraHeader (TagConway h))

{-# COMPLETE HeaderByron
           , HeaderShelley
           , HeaderAllegra
           , HeaderMary
           , HeaderAlonzo
           , HeaderBabbage
           , HeaderConway
  #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
type CardanoGenTx c = GenTx (CardanoBlock c)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx c
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (TagByron tx))

pattern GenTxShelley :: GenTx (ShelleyBlock (TPraos c) ShelleyEra) -> CardanoGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (TagShelley tx))

pattern GenTxAllegra :: GenTx (ShelleyBlock (TPraos c) AllegraEra) -> CardanoGenTx c
pattern GenTxAllegra tx = HardForkGenTx (OneEraGenTx (TagAllegra tx))

pattern GenTxMary :: GenTx (ShelleyBlock (TPraos c) MaryEra) -> CardanoGenTx c
pattern GenTxMary tx = HardForkGenTx (OneEraGenTx (TagMary tx))

pattern GenTxAlonzo :: GenTx (ShelleyBlock (TPraos c) AlonzoEra) -> CardanoGenTx c
pattern GenTxAlonzo tx = HardForkGenTx (OneEraGenTx (TagAlonzo tx))

pattern GenTxBabbage :: GenTx (ShelleyBlock (Praos c) BabbageEra) -> CardanoGenTx c
pattern GenTxBabbage tx = HardForkGenTx (OneEraGenTx (TagBabbage tx))

pattern GenTxConway :: GenTx (ShelleyBlock (Praos c) ConwayEra) -> CardanoGenTx c
pattern GenTxConway tx = HardForkGenTx (OneEraGenTx (TagConway tx))

{-# COMPLETE
    GenTxByron
  , GenTxShelley
  , GenTxAllegra
  , GenTxMary
  , GenTxAlonzo
  , GenTxBabbage
  , GenTxConway
  #-}

-- | The ID of a Cardano transaction.
type CardanoGenTxId c = GenTxId (CardanoBlock c)

pattern CardanoGenTxId :: ShortByteString -> CardanoGenTxId c
pattern CardanoGenTxId txid = HardForkGenTxId (OneEraGenTxId txid)

{-# COMPLETE CardanoGenTxId #-}

-- | An error resulting from applying a 'CardanoGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxByronErr', 'ApplyTxErrShelley', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: CardanoApplyTxErr c -> Text
-- > toText (ApplyTxErrByron b) = byronApplyTxErrToText b
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > toText (ApplyTxErrAllegra a) = allegraApplyTxErrToText a
-- > toText (ApplyTxErrMary m) = maryApplyTxErrToText m
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoApplyTxErr c = HardForkApplyTxErr (CardanoEras c)

pattern ApplyTxErrByron :: ApplyTxErr ByronBlock -> CardanoApplyTxErr c
pattern ApplyTxErrByron err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagByron (WrapApplyTxErr err)))

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagShelley (WrapApplyTxErr err)))

pattern ApplyTxErrAllegra ::
     ApplyTxErr (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrAllegra err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAllegra (WrapApplyTxErr err)))

pattern ApplyTxErrMary ::
     ApplyTxErr (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrMary err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagMary (WrapApplyTxErr err)))

pattern ApplyTxErrAlonzo ::
     ApplyTxErr (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrAlonzo err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAlonzo (WrapApplyTxErr err)))

pattern ApplyTxErrBabbage ::
     ApplyTxErr (ShelleyBlock (Praos c) BabbageEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrBabbage err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagBabbage (WrapApplyTxErr err)))

pattern ApplyTxErrConway ::
     ApplyTxErr (ShelleyBlock (Praos c) ConwayEra)
  -> CardanoApplyTxErr c
pattern ApplyTxErrConway err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagConway (WrapApplyTxErr err)))

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrByron
           , ApplyTxErrShelley
           , ApplyTxErrAllegra
           , ApplyTxErrMary
           , ApplyTxErrAlonzo
           , ApplyTxErrBabbage
           , ApplyTxErrConway
           , ApplyTxErrWrongEra
  #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'CardanoBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorByron', 'LedgerErrorShelley', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: CardanoLedgerError c -> Text
-- > toText (LedgerErrorByron b) = byronLedgerErrorToText b
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorAllegra a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorMary m) = maryLedgerErrorToText m
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoLedgerError c = HardForkLedgerError (CardanoEras c)

pattern LedgerErrorByron :: LedgerError ByronBlock -> CardanoLedgerError c
pattern LedgerErrorByron err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (TagByron (WrapLedgerErr err)))

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagShelley (WrapLedgerErr err)))

pattern LedgerErrorAllegra ::
     LedgerError (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoLedgerError c
pattern LedgerErrorAllegra err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAllegra (WrapLedgerErr err)))

pattern LedgerErrorMary ::
     LedgerError (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoLedgerError c
pattern LedgerErrorMary err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagMary (WrapLedgerErr err)))

pattern LedgerErrorAlonzo ::
     LedgerError (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoLedgerError c
pattern LedgerErrorAlonzo err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAlonzo (WrapLedgerErr err)))

pattern LedgerErrorBabbage ::
     LedgerError (ShelleyBlock (Praos c) BabbageEra)
  -> CardanoLedgerError c
pattern LedgerErrorBabbage err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagBabbage (WrapLedgerErr err)))

pattern LedgerErrorConway ::
     LedgerError (ShelleyBlock (Praos c) ConwayEra)
  -> CardanoLedgerError c
pattern LedgerErrorConway err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagConway (WrapLedgerErr err)))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorByron
           , LedgerErrorShelley
           , LedgerErrorAllegra
           , LedgerErrorMary
           , LedgerErrorAlonzo
           , LedgerErrorBabbage
           , LedgerErrorConway
           , LedgerErrorWrongEra
  #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'CardanoHeader'.
type CardanoOtherHeaderEnvelopeError c = HardForkEnvelopeErr (CardanoEras c)

pattern OtherHeaderEnvelopeErrorByron
  :: OtherHeaderEnvelopeError ByronBlock
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorByron err =
    HardForkEnvelopeErrFromEra
      (OneEraEnvelopeErr (TagByron (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagShelley (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAllegra
  :: OtherHeaderEnvelopeError (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAllegra err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAllegra (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorMary
  :: OtherHeaderEnvelopeError (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorMary err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagMary (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAlonzo
  :: OtherHeaderEnvelopeError (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAlonzo err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAlonzo (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorBabbage
  :: OtherHeaderEnvelopeError (ShelleyBlock (Praos c) BabbageEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorBabbage err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagBabbage (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorConway
  :: OtherHeaderEnvelopeError (ShelleyBlock (Praos c) ConwayEra)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorConway err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagConway (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorByron
           , OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorAllegra
           , OtherHeaderEnvelopeErrorMary
           , OtherHeaderEnvelopeErrorAlonzo
           , OtherHeaderEnvelopeErrorBabbage
           , OtherHeaderEnvelopeErrorConway
           , OtherHeaderEnvelopeErrorWrongEra
  #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Cardano chain.
type CardanoTipInfo c = OneEraTipInfo (CardanoEras c)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo c
pattern TipInfoByron ti = OneEraTipInfo (TagByron (WrapTipInfo ti))

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (TagShelley (WrapTipInfo ti))

pattern TipInfoAllegra ::
     TipInfo (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoTipInfo c
pattern TipInfoAllegra ti = OneEraTipInfo (TagAllegra (WrapTipInfo ti))

pattern TipInfoMary ::
     TipInfo (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoTipInfo c
pattern TipInfoMary ti = OneEraTipInfo (TagMary (WrapTipInfo ti))

pattern TipInfoAlonzo ::
     TipInfo (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoTipInfo c
pattern TipInfoAlonzo ti = OneEraTipInfo (TagAlonzo (WrapTipInfo ti))

pattern TipInfoBabbage ::
     TipInfo (ShelleyBlock (Praos c) BabbageEra)
  -> CardanoTipInfo c
pattern TipInfoBabbage ti = OneEraTipInfo (TagBabbage (WrapTipInfo ti))

pattern TipInfoConway ::
     TipInfo (ShelleyBlock (Praos c) ConwayEra)
  -> CardanoTipInfo c
pattern TipInfoConway ti = OneEraTipInfo (TagConway (WrapTipInfo ti))

{-# COMPLETE TipInfoByron
           , TipInfoShelley
           , TipInfoAllegra
           , TipInfoMary
           , TipInfoAlonzo
           , TipInfoBabbage
           , TipInfoConway
  #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Cardano chain.
type CardanoQuery c = BlockQuery (CardanoBlock c)

-- | Byron-specific query that can only be answered when the ledger is in the
-- Byron era.
pattern QueryIfCurrentByron
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery ByronBlock fp result
  -> CardanoQuery c fp a

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (TPraos c) ShelleyEra) fp result
  -> CardanoQuery c fp a

-- | Allegra-specific query that can only be answered when the ledger is in the
-- Allegra era.
pattern QueryIfCurrentAllegra
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (TPraos c) AllegraEra) fp result
  -> CardanoQuery c fp a

-- | Mary-specific query that can only be answered when the ledger is in the
-- Mary era.
pattern QueryIfCurrentMary
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (TPraos c) MaryEra) fp result
  -> CardanoQuery c fp a

-- | Alonzo-specific query that can only be answered when the ledger is in the
-- Alonzo era.
pattern QueryIfCurrentAlonzo
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (TPraos c) AlonzoEra) fp result
  -> CardanoQuery c fp a

-- | Babbage-specific query that can only be answered when the ledger is in the
-- Babbage era.
pattern QueryIfCurrentBabbage
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (Praos c) BabbageEra) fp result
  -> CardanoQuery c fp a

-- | Conway-specific query that can only be answered when the ledger is in the
-- Conway era.
pattern QueryIfCurrentConway
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlock (Praos c) ConwayEra) fp result
  -> CardanoQuery c fp a

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern QueryIfCurrentByron   q = QueryIfCurrent                         (QZ q)
pattern QueryIfCurrentShelley q = QueryIfCurrent                     (QS (QZ q))
pattern QueryIfCurrentAllegra q = QueryIfCurrent                 (QS (QS (QZ q)))
pattern QueryIfCurrentMary    q = QueryIfCurrent             (QS (QS (QS (QZ q))))
pattern QueryIfCurrentAlonzo  q = QueryIfCurrent         (QS (QS (QS (QS (QZ q)))))
pattern QueryIfCurrentBabbage q = QueryIfCurrent     (QS (QS (QS (QS (QS (QZ q))))))
pattern QueryIfCurrentConway  q = QueryIfCurrent (QS (QS (QS (QS (QS (QS (QZ q)))))))

-- | Query about the Byron era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Byron era (whether the tip of
-- the ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeByron EraStart
--
pattern QueryAnytimeByron
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeByron q = QueryAnytime q (EraIndex (TagByron (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (TagShelley (K ())))

-- | Query about the Allegra era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Allegra era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAllegra EraStart
--
pattern QueryAnytimeAllegra
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeAllegra q = QueryAnytime q (EraIndex (TagAllegra (K ())))

-- | Query about the Mary era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Mary era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeMary EraStart
--
pattern QueryAnytimeMary
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeMary q = QueryAnytime q (EraIndex (TagMary (K ())))

-- | Query about the Alonzo era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Alonzo era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAlonzo EraStart
--
pattern QueryAnytimeAlonzo
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeAlonzo q = QueryAnytime q (EraIndex (TagAlonzo (K ())))

-- | Query about the Babbage era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Babbage era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeBabbage EraStart
--
pattern QueryAnytimeBabbage
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeBabbage q = QueryAnytime q (EraIndex (TagBabbage (K ())))

-- | Query about the Conway era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Conway era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeConway EraStart
--
pattern QueryAnytimeConway
  :: QueryAnytime result
  -> CardanoQuery c QFNoTables result
pattern QueryAnytimeConway q = QueryAnytime q (EraIndex (TagConway (K ())))

{-# COMPLETE QueryIfCurrentByron
           , QueryIfCurrentShelley
           , QueryIfCurrentAllegra
           , QueryIfCurrentMary
           , QueryIfCurrentAlonzo
           , QueryIfCurrentBabbage
           , QueryAnytimeByron
           , QueryAnytimeShelley
           , QueryAnytimeAllegra
           , QueryAnytimeMary
           , QueryAnytimeAlonzo
           , QueryAnytimeBabbage
           , QueryAnytimeConway
           , QueryHardFork
  #-}

-- | The result of a 'CardanoQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type CardanoQueryResult c = HardForkQueryResult (CardanoEras c)

pattern QueryResultSuccess :: result -> CardanoQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> CardanoQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'CodecConfig's.
type CardanoCodecConfig c = CodecConfig (CardanoBlock c)

pattern CardanoCodecConfig
  :: CodecConfig ByronBlock
  -> CodecConfig (ShelleyBlock (TPraos c) ShelleyEra)
  -> CodecConfig (ShelleyBlock (TPraos c) AllegraEra)
  -> CodecConfig (ShelleyBlock (TPraos c) MaryEra)
  -> CodecConfig (ShelleyBlock (TPraos c) AlonzoEra)
  -> CodecConfig (ShelleyBlock (Praos c)  BabbageEra)
  -> CodecConfig (ShelleyBlock (Praos c)  ConwayEra)
  -> CardanoCodecConfig c
pattern CardanoCodecConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* cfgConway
          :* Nil
          )
      }

{-# COMPLETE CardanoCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'BlockConfig's.
type CardanoBlockConfig c = BlockConfig (CardanoBlock c)

pattern CardanoBlockConfig
  :: BlockConfig ByronBlock
  -> BlockConfig (ShelleyBlock (TPraos c) ShelleyEra)
  -> BlockConfig (ShelleyBlock (TPraos c) AllegraEra)
  -> BlockConfig (ShelleyBlock (TPraos c) MaryEra)
  -> BlockConfig (ShelleyBlock (TPraos c) AlonzoEra)
  -> BlockConfig (ShelleyBlock (Praos c)  BabbageEra)
  -> BlockConfig (ShelleyBlock (Praos c)  ConwayEra)
  -> CardanoBlockConfig c
pattern CardanoBlockConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* cfgConway
          :* Nil
          )
      }

{-# COMPLETE CardanoBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'StorageConfig's.
type CardanoStorageConfig c = StorageConfig (CardanoBlock c)

pattern CardanoStorageConfig
  :: StorageConfig ByronBlock
  -> StorageConfig (ShelleyBlock (TPraos c) ShelleyEra)
  -> StorageConfig (ShelleyBlock (TPraos c) AllegraEra)
  -> StorageConfig (ShelleyBlock (TPraos c) MaryEra)
  -> StorageConfig (ShelleyBlock (TPraos c) AlonzoEra)
  -> StorageConfig (ShelleyBlock (Praos c)  BabbageEra)
  -> StorageConfig (ShelleyBlock (Praos c)  ConwayEra)
  -> CardanoStorageConfig c
pattern CardanoStorageConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* cfgConway
          :* Nil
          )
      }

{-# COMPLETE CardanoStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type CardanoConsensusConfig c =
  ConsensusConfig (HardForkProtocol (CardanoEras c))

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ByronBlock)
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) ShelleyEra))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) AllegraEra))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) MaryEra))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) AlonzoEra))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c)  BabbageEra))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c)  ConwayEra))
  -> CardanoConsensusConfig c
pattern CardanoConsensusConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgByron
          :* WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgAllegra
          :* WrapPartialConsensusConfig cfgMary
          :* WrapPartialConsensusConfig cfgAlonzo
          :* WrapPartialConsensusConfig cfgBabbage
          :* WrapPartialConsensusConfig cfgConway
          :* Nil
          )
      }

{-# COMPLETE CardanoConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type CardanoLedgerConfig c = HardForkLedgerConfig (CardanoEras c)

pattern CardanoLedgerConfig
  :: PartialLedgerConfig ByronBlock
  -> PartialLedgerConfig (ShelleyBlock (TPraos c) ShelleyEra)
  -> PartialLedgerConfig (ShelleyBlock (TPraos c) AllegraEra)
  -> PartialLedgerConfig (ShelleyBlock (TPraos c) MaryEra)
  -> PartialLedgerConfig (ShelleyBlock (TPraos c) AlonzoEra)
  -> PartialLedgerConfig (ShelleyBlock (Praos c)  BabbageEra)
  -> PartialLedgerConfig (ShelleyBlock (Praos c)  ConwayEra)
  -> CardanoLedgerConfig c
pattern CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (  WrapPartialLedgerConfig cfgByron
          :* WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgAllegra
          :* WrapPartialLedgerConfig cfgMary
          :* WrapPartialLedgerConfig cfgAlonzo
          :* WrapPartialLedgerConfig cfgBabbage
          :* WrapPartialLedgerConfig cfgConway
          :* Nil
          )
      }

{-# COMPLETE CardanoLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoLedgerState c mk = LedgerState (CardanoBlock c) mk

pattern LedgerStateByron
  :: LedgerState ByronBlock mk
  -> CardanoLedgerState c mk
pattern LedgerStateByron st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleByron (State.Current { currentState = Flip st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (TPraos c) ShelleyEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleShelley _ (State.Current { currentState = Flip st })))

pattern LedgerStateAllegra
  :: LedgerState (ShelleyBlock (TPraos c) AllegraEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateAllegra st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAllegra _ _  (State.Current { currentState = Flip st })))

pattern LedgerStateMary
  :: LedgerState (ShelleyBlock (TPraos c) MaryEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateMary st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleMary _ _ _ (State.Current { currentState = Flip st })))

pattern LedgerStateAlonzo
  :: LedgerState (ShelleyBlock (TPraos c) AlonzoEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateAlonzo st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAlonzo _ _ _ _ (State.Current { currentState = Flip st })))

pattern LedgerStateBabbage
  :: LedgerState (ShelleyBlock (Praos c) BabbageEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateBabbage st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleBabbage _ _ _ _ _ (State.Current { currentState = Flip st })))

pattern LedgerStateConway
  :: LedgerState (ShelleyBlock (Praos c) ConwayEra) mk
  -> CardanoLedgerState c mk
pattern LedgerStateConway st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleConway _ _ _ _ _ _ (State.Current { currentState = Flip st })))

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary
           , LedgerStateAlonzo
           , LedgerStateBabbage
           , LedgerStateConway
  #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoChainDepState c = HardForkChainDepState (CardanoEras c)

pattern ChainDepStateByron
  :: ChainDepState (BlockProtocol ByronBlock)
  -> CardanoChainDepState c
pattern ChainDepStateByron st <-
    State.HardForkState
      (TeleByron (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (TPraos c) ShelleyEra))
  -> CardanoChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TeleShelley _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAllegra
  :: ChainDepState (BlockProtocol (ShelleyBlock (TPraos c) AllegraEra))
  -> CardanoChainDepState c
pattern ChainDepStateAllegra st <-
    State.HardForkState
      (TeleAllegra _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateMary
  :: ChainDepState (BlockProtocol (ShelleyBlock (TPraos c) MaryEra))
  -> CardanoChainDepState c
pattern ChainDepStateMary st <-
    State.HardForkState
      (TeleMary _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAlonzo
  :: ChainDepState (BlockProtocol (ShelleyBlock (TPraos c) AlonzoEra))
  -> CardanoChainDepState c
pattern ChainDepStateAlonzo st <-
    State.HardForkState
      (TeleAlonzo _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateBabbage
  :: ChainDepState (BlockProtocol (ShelleyBlock (Praos c) BabbageEra))
  -> CardanoChainDepState c
pattern ChainDepStateBabbage st <-
    State.HardForkState
      (TeleBabbage _ _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateConway
  :: ChainDepState (BlockProtocol (ShelleyBlock (Praos c) ConwayEra))
  -> CardanoChainDepState c
pattern ChainDepStateConway st <-
    State.HardForkState
      (TeleConway _ _ _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

{-# COMPLETE ChainDepStateByron
           , ChainDepStateShelley
           , ChainDepStateAllegra
           , ChainDepStateMary
           , ChainDepStateAlonzo
           , ChainDepStateBabbage
           , ChainDepStateConway
  #-}
