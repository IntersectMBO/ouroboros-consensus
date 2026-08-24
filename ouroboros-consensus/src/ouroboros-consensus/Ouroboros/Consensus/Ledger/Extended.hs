{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Extended
  ( -- * Extended ledger state
    ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ExtValidationError (..)

    -- * Serialisation
  , decodeDiskExtLedgerState
  , decodeExtLedgerState
  , encodeDiskExtLedgerState
  , encodeExtLedgerState
  , initPerasEpochContextResolver
  , mkPerasEpochContextResolverHandle

    -- * Type family instances
  , LedgerTables (..)
  , Ticked (..)
  ) where

import Cardano.Ledger.Binary (DecoderError (..), cborError)
import Codec.CBOR.Decoding (Decoder, decodeListLen)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy
import Data.SOP.Constraint (All, Top)
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.Abstract
  ( BlockConfig
  , BlockProtocol
  , CodecConfig
  , GetHeader (getHeader)
  , HeaderHash
  , StandardHash
  , StorageConfig
  , castPoint
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , PerasWeight (..)
  , ValidatedPerasCert (..)
  , pattern NoPerasEnabled
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (HardForkIndices))
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Peras (PerasState (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextNotFoundForRound
  , PerasEpochContextResolver (..)
  , PerasEpochContextResolverHandle (..)
  , StateSupportsPerasEpochContext (..)
  , initPerasEpochContextResolver
  , tickPerasEpochContextResolver
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike (MonadSTM (STM))
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

data ExtValidationError blk
  = ExtValidationErrorLedger !(LedgerErr LedgerState blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  | ExtValidationErrorPerasEpochContextResolver !PerasEpochContextNotFoundForRound
  | ExtValidationErrorPerasCertInBlock !(PerasError blk)
  deriving Generic

deriving instance
  ( Eq (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  Eq (ExtValidationError blk)
deriving instance
  ( NoThunks (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  NoThunks (ExtValidationError blk)
deriving instance
  ( Show (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  Show (ExtValidationError blk)

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk mk = ExtLedgerState
  { ledgerState :: !(LedgerState blk mk)
  , headerState :: !(HeaderState blk)
  , perasState :: !(PerasState blk)
  }
  deriving Generic

deriving instance
  ( EqMK mk
  , Eq (PerasState blk)
  , LedgerSupportsProtocol blk
  ) =>
  Eq (ExtLedgerState blk mk)
deriving instance
  ( ShowMK mk
  , Show (PerasState blk)
  , LedgerSupportsProtocol blk
  ) =>
  Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  ( NoThunksMK mk
  , NoThunks (PerasState blk)
  , LedgerSupportsProtocol blk
  ) =>
  NoThunks (ExtLedgerState blk mk)
  where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

type instance HeaderHash (ExtLedgerState blk) = HeaderHash (LedgerState blk)
instance
  ( NFData (HeaderHash blk)
  , NoThunks (HeaderHash blk)
  , Typeable (HeaderHash blk)
  , Show (HeaderHash blk)
  , Ord (HeaderHash blk)
  , Eq (HeaderHash blk)
  ) =>
  StandardHash (ExtLedgerState blk)

instance IsLedger LedgerState blk => GetTip (ExtLedgerState blk) where
  getTip = castPoint . getTip . ledgerState

mkPerasEpochContextResolverHandle ::
  MonadSTM m =>
  STM m (ExtLedgerState blk mk) ->
  PerasEpochContextResolverHandle m blk
mkPerasEpochContextResolverHandle getLedgerStateSTM =
  PerasEpochContextResolverHandle $
    perasEpochContextResolver
      . perasState
      <$> getLedgerStateSTM

{-------------------------------------------------------------------------------
  The extended ledger configuration
-------------------------------------------------------------------------------}

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
newtype ExtLedgerCfg blk = ExtLedgerCfg
  { getExtLedgerCfg :: TopLevelConfig blk
  }
  deriving Generic

instance
  ( ConsensusProtocol (BlockProtocol blk)
  , NoThunks (BlockConfig blk)
  , NoThunks (CodecConfig blk)
  , NoThunks (LedgerConfig blk)
  , NoThunks (StorageConfig blk)
  , NoThunks (HeaderHash blk)
  ) =>
  NoThunks (ExtLedgerCfg blk)

type instance LedgerCfg ExtLedgerState blk = ExtLedgerCfg blk

{-------------------------------------------------------------------------------
  The ticked extended ledger state
-------------------------------------------------------------------------------}

data instance Ticked ExtLedgerState blk mk = TickedExtLedgerState
  { tickedLedgerState :: Ticked LedgerState blk mk
  , ledgerView :: LedgerView (BlockProtocol blk)
  , tickedHeaderState :: Ticked (HeaderState blk)
  , tickedPerasState :: PerasState blk
  }

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState blk) where
  getTip = castPoint . getTip . tickedLedgerState

instance
  ( LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  , All Top (HardForkIndices blk)
  ) =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult
    evs
    cfg
    slot
    ExtLedgerState
      { ledgerState
      , headerState
      , perasState
      } =
      castLedgerResult ledgerResult <&> \tickedLedgerState ->
        let ledgerView :: LedgerView (BlockProtocol blk)
            ledgerView = protocolLedgerView lcfg tickedLedgerState

            tickedHeaderState :: Ticked (HeaderState blk)
            tickedHeaderState =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                ledgerView
                slot
                headerState

            tickedPerasState =
              PerasState
                { perasEpochContextResolver =
                    tickPerasEpochContextResolver
                      lcfg
                      (perasEpochContextResolver perasState, ledgerState, headerState)
                      (slot, forgetLedgerTables tickedLedgerState, tickedHeaderState)
                , latestPerasCertOnChainRound =
                    latestPerasCertOnChainRound perasState
                }
         in TickedExtLedgerState{..}
     where
      lcfg :: LedgerConfig blk
      lcfg = configLedger $ getExtLedgerCfg cfg

      ledgerResult = applyChainTickLedgerResult evs lcfg slot ledgerState

applyHelper ::
  forall blk.
  ( HasCallStack
  , LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  ) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg LedgerState blk ->
    blk ->
    Ticked LedgerState blk ValuesMK ->
    Except
      (LedgerErr LedgerState blk)
      (LedgerResult blk (LedgerState blk DiffMK))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  Ticked ExtLedgerState blk ValuesMK ->
  Except
    (LedgerErr ExtLedgerState blk)
    (LedgerResult blk (ExtLedgerState blk DiffMK))
applyHelper f opts cfg blk TickedExtLedgerState{..} = do
  ledgerResult <-
    withExcept ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
  hdr <-
    withExcept ExtValidationErrorHeader $
      validateHeader @blk
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState
  perasState <- do
    -- Only when ticking the 'ExtLedgerState' do we need to update the
    -- 'PerasEpochContextResolver'. When applying block on top of a 'Ticked
    -- ExtLedgerState', the 'PerasEpochContextResolver' has already been put to
    -- the right state by the ticking.
    let perasResolver = perasEpochContextResolver tickedPerasState
    -- Update the latest Peras certificate round if the new block contains a
    -- certificate from a round more recent than the currently cached one.
    mbPerasCert <- extractAndValidatePerasCertFromBlock perasResolver blk
    let latestCertRound =
          case getPerasCertRound <$> mbPerasCert of
            -- The block does not contain a Peras certificate => keep the old one
            Nothing -> do
              latestPerasCertOnChainRound tickedPerasState
            -- The block contains a Peras certificate => compare it with the old one
            Just certInBlockRound -> do
              case latestPerasCertOnChainRound tickedPerasState of
                SNothing ->
                  SJust certInBlockRound
                SJust prevLatestCertOnChainRound ->
                  SJust (certInBlockRound `max` prevLatestCertOnChainRound)
    pure $
      PerasState
        { perasEpochContextResolver =
            perasResolver
        , latestPerasCertOnChainRound =
            latestCertRound
        }

  pure $
    (\l -> ExtLedgerState l hdr perasState) <$> castLedgerResult ledgerResult

-- | Extract and validate a Peras certificate from a block, if it exists.
--
-- NOTE: this is a placeholder until we get rid of the degenerate
-- 'BlockSupportsPeras' instance.
extractAndValidatePerasCertFromBlock ::
  forall blk.
  BlockSupportsPeras blk =>
  PerasEpochContextResolver blk ->
  blk ->
  Except (LedgerErr ExtLedgerState blk) (Maybe (ValidatedPerasCert blk))
extractAndValidatePerasCertFromBlock _ blk = do
  case getPerasCertInBlock blk of
    Nothing ->
      pure Nothing
    Just cert -> do
      pure $
        Just
          ValidatedPerasCert
            { vpcCert = cert
            , vpcCertBoost = PerasWeight 0
            }

instance
  ( GetBlockKeySets blk
  , LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  , All Top (HardForkIndices blk)
  ) =>
  ApplyBlock ExtLedgerState blk
  where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk TickedExtLedgerState{..} =
    (\l -> ExtLedgerState l hdr perasState) <$> castLedgerResult ledgerResult
   where
    ledgerResult =
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
    hdr =
      revalidateHeader
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState
    -- Only when ticking the 'ExtLedgerState' do we need to update the
    -- 'PerasEpochContextResolver'. When applying block on top of a 'Ticked
    -- ExtLedgerState', the 'PerasEpochContextResolver' has already been put to
    -- the right state by the ticking.
    perasState =
      PerasState
        { perasEpochContextResolver =
            perasEpochContextResolver tickedPerasState
        , latestPerasCertOnChainRound =
            latestPerasCertOnChainRound tickedPerasState
        }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState ::
  (LedgerState blk mk -> Encoding) ->
  (ChainDepState (BlockProtocol blk) -> Encoding) ->
  (AnnTip blk -> Encoding) ->
  (PerasState blk -> Encoding) ->
  ExtLedgerState blk mk ->
  Encoding
encodeExtLedgerState
  encodeLedgerState
  encodeChainDepState
  encodeAnnTip
  encodePerasState'
  ExtLedgerState
    { ledgerState
    , headerState
    , perasState
    } =
    mconcat
      [ encodeListLen 3
      , encodeLedgerState ledgerState
      , encodeHeaderState' headerState
      , encodePerasState' perasState
      ]
   where
    encodeHeaderState' =
      encodeHeaderState
        encodeChainDepState
        encodeAnnTip

encodeDiskExtLedgerState ::
  forall blk.
  ( EncodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , EncodeDisk blk (AnnTip blk)
  , EncodeDisk blk (PerasState blk)
  ) =>
  (CodecConfig blk -> ExtLedgerState blk EmptyMK -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState blk EmptyMK)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (PerasState blk)) ->
  (forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeExtLedgerState
  decodeLedgerState
  decodeChainDepState
  decodeAnnTip
  decodePerasState' = do
    len <- decodeListLen
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    -- NOTE: we have to support legacy serialisation here until we can justify
    -- forcing a replay on all nodes even if Peras is not yet supported.
    -- Defaulting to 'defaultPerasState' should be safe for now because 1) epoch
    -- context resolution should not be triggered unless we are trying to
    -- validate a Peras object that shouldn't exist (validation will fail), and
    -- 2) the round number of the latest Peras certificate on chain will always
    -- be absent until Peras enters a cooldown for the first time after getting
    -- enabled.
    --
    -- TODO: enfoce decoding size once Peras is closer to being enabled.
    -- See https://github.com/tweag/cardano-peras/issues/275
    perasState <-
      case len of
        2 -> pure defaultPerasState
        3 -> decodePerasState'
        _ -> cborError (DecoderErrorCustom "ExtLedgerState" "unexpected list length")
    return
      ExtLedgerState
        { ledgerState
        , headerState
        , perasState
        }
   where
    decodeHeaderState' =
      decodeHeaderState
        decodeChainDepState
        decodeAnnTip

    defaultPerasState =
      PerasState
        { perasEpochContextResolver = PerasEpochContextResolver NoPerasEnabled NoPerasEnabled
        , latestPerasCertOnChainRound = SNothing
        }

decodeDiskExtLedgerState ::
  forall blk.
  ( DecodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (AnnTip blk)
  , DecodeDisk blk (PerasState blk)
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

instance
  (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables LedgerState blk) =>
  HasLedgerTables ExtLedgerState blk
  where
  projectLedgerTables (ExtLedgerState lstate _ _) =
    projectLedgerTables lstate
  withLedgerTables (ExtLedgerState lstate hstate perasState) tables =
    ExtLedgerState
      (lstate `withLedgerTables` tables)
      hstate
      perasState

instance
  (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables (Ticked LedgerState) blk) =>
  HasLedgerTables (Ticked ExtLedgerState) blk
  where
  projectLedgerTables (TickedExtLedgerState lstate _view _hstate _perasState) =
    projectLedgerTables lstate
  withLedgerTables
    (TickedExtLedgerState lstate view hstate perasState)
    tables =
      TickedExtLedgerState
        (lstate `withLedgerTables` tables)
        view
        hstate
        perasState

instance
  CanStowLedgerTables (LedgerState blk) =>
  CanStowLedgerTables (ExtLedgerState blk)
  where
  stowLedgerTables (ExtLedgerState lstate hstate perasState) =
    ExtLedgerState (stowLedgerTables lstate) hstate perasState

  unstowLedgerTables (ExtLedgerState lstate hstate perasState) =
    ExtLedgerState (unstowLedgerTables lstate) hstate perasState

instance
  CanUpgradeLedgerTables LedgerState blk =>
  CanUpgradeLedgerTables ExtLedgerState blk
  where
  upgradeTables (ExtLedgerState st0 _ _) (ExtLedgerState st1 _ _) =
    upgradeTables st0 st1

instance
  (txout ~ TxOut blk, IndexedMemPack LedgerState blk txout) =>
  IndexedMemPack ExtLedgerState blk txout
  where
  indexedTypeName p (ExtLedgerState st _ _) = indexedTypeName p st
  indexedPackedByteCount (ExtLedgerState st _ _) = indexedPackedByteCount st
  indexedPackM (ExtLedgerState st _ _) = indexedPackM st
  indexedUnpackM (ExtLedgerState st _ _) = indexedUnpackM st

instance LedgerTablesAreTrivial LedgerState blk => LedgerTablesAreTrivial ExtLedgerState blk where
  convertMapKind (ExtLedgerState st hst perasState) =
    ExtLedgerState (convertMapKind st) hst perasState

instance SerializeTablesWithHint LedgerState blk => SerializeTablesWithHint ExtLedgerState blk where
  decodeTablesWithHint st = decodeTablesWithHint (ledgerState st)
  encodeTablesWithHint st tbs = encodeTablesWithHint (ledgerState st) tbs
