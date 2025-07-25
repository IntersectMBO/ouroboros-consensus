{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Ledger.Dual
  ( Bridge (..)

    -- * Pair types
  , DualBlock (..)
  , DualGenTxErr (..)
  , DualHeader
  , DualLedgerConfig (..)
  , DualLedgerError (..)

    -- * Lifted functions
  , ctxtDualMain
  , dualExtValidationErrorMain
  , dualTopLevelConfigMain

    -- * Type class family instances
  , BlockConfig (..)
  , CodecConfig (..)
  , GenTx (..)
  , Header (..)
  , LedgerState (..)
  , LedgerTables (..)
  , NestedCtxt_ (..)
  , StorageConfig (..)
  , Ticked (..)
  , TxId (..)
  , Validated (..)

    -- * Serialisation
  , decodeDualBlock
  , decodeDualGenTx
  , decodeDualGenTxErr
  , decodeDualGenTxId
  , decodeDualHeader
  , decodeDualLedgerConfig
  , decodeDualLedgerState
  , encodeDualBlock
  , encodeDualGenTx
  , encodeDualGenTxErr
  , encodeDualGenTxId
  , encodeDualHeader
  , encodeDualLedgerConfig
  , encodeDualLedgerState
  ) where

import Cardano.Binary (enforceSize)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Codec.Serialise
import Control.Arrow ((+++))
import Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack
import NoThunks.Class (AllowThunk (..), NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Dual block
--
-- The dual block is used to instantiate the consensus with a dual ledger,
-- consisting of two ledger states associated with two types of blocks. The
-- (consensus) chain state will still be updated based on one block type only,
-- which is therefore designed as the /main/ block, while the other block is
-- designated as the /auxiliary/ block.
--
-- The auxiliary block is optional; this can be used if some " main " blocks
-- should have no effect on the auxiliary ledger state at all. The motivating
-- example is EBBs: if the main blocks are real Byron blocks, and the auxiliary
-- blocks are Byron spec blocks, then regular Byron blocks correspond to Byron
-- spec blocks, but EBBs don't correspond to a spec block at all and should
-- leave the Byron spec ledger state unchanged.
--
-- NOTE: The dual ledger is used for testing purposes only; we do not do any
-- meaningful 'NoThunks' checks here.
data DualBlock m a = DualBlock
  { dualBlockMain :: m
  , dualBlockAux :: Maybe a
  , dualBlockBridge :: BridgeBlock m a
  }

deriving instance (Show m, Show a, Show (BridgeBlock m a)) => Show (DualBlock m a)
deriving instance (Eq m, Eq a, Eq (BridgeBlock m a)) => Eq (DualBlock m a)

instance
  (Typeable m, Typeable a) =>
  ShowProxy (DualBlock m a)

instance Condense m => Condense (DualBlock m a) where
  condense = condense . dualBlockMain

type instance HeaderHash (DualBlock m a) = HeaderHash m
instance StandardHash m => StandardHash (DualBlock m a)

instance ConvertRawHash m => ConvertRawHash (DualBlock m a) where
  toShortRawHash _ = toShortRawHash (Proxy @m)
  fromShortRawHash _ = fromShortRawHash (Proxy @m)
  hashSize _ = hashSize (Proxy @m)

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

newtype instance Header (DualBlock m a) = DualHeader {dualHeaderMain :: Header m}
  deriving NoThunks via AllowThunk (Header (DualBlock m a))

instance Bridge m a => GetHeader (DualBlock m a) where
  getHeader = DualHeader . getHeader . dualBlockMain

  blockMatchesHeader hdr =
    blockMatchesHeader (dualHeaderMain hdr) . dualBlockMain

  -- We can look at the concrete header to see if this is an EBB
  headerIsEBB = headerIsEBB . dualHeaderMain

type DualHeader m a = Header (DualBlock m a)

deriving instance Show (Header m) => Show (DualHeader m a)

instance
  (Typeable m, Typeable a) =>
  ShowProxy (DualHeader m a)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig (DualBlock m a) = DualBlockConfig
  { dualBlockConfigMain :: BlockConfig m
  , dualBlockConfigAux :: BlockConfig a
  }
  deriving NoThunks via AllowThunk (BlockConfig (DualBlock m a))

instance ConfigSupportsNode m => ConfigSupportsNode (DualBlock m a) where
  getSystemStart = getSystemStart . dualBlockConfigMain
  getNetworkMagic = getNetworkMagic . dualBlockConfigMain

{-------------------------------------------------------------------------------
  Splitting the config
-------------------------------------------------------------------------------}

-- | This is only used for block production
dualTopLevelConfigMain :: TopLevelConfig (DualBlock m a) -> TopLevelConfig m
dualTopLevelConfigMain TopLevelConfig{..} =
  TopLevelConfig
    { topLevelConfigProtocol = topLevelConfigProtocol
    , topLevelConfigLedger = dualLedgerConfigMain topLevelConfigLedger
    , topLevelConfigBlock = dualBlockConfigMain topLevelConfigBlock
    , topLevelConfigCodec = dualCodecConfigMain topLevelConfigCodec
    , topLevelConfigStorage = dualStorageConfigMain topLevelConfigStorage
    , topLevelConfigCheckpoints = castCheckpointsMap topLevelConfigCheckpoints
    }

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

data instance CodecConfig (DualBlock m a) = DualCodecConfig
  { dualCodecConfigMain :: !(CodecConfig m)
  , dualCodecConfigAux :: !(CodecConfig a)
  }
  deriving Generic

instance
  ( NoThunks (CodecConfig m)
  , NoThunks (CodecConfig a)
  ) =>
  NoThunks (CodecConfig (DualBlock m a))

-- Use generic instance

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

data instance StorageConfig (DualBlock m a) = DualStorageConfig
  { dualStorageConfigMain :: !(StorageConfig m)
  , dualStorageConfigAux :: !(StorageConfig a)
  }
  deriving Generic

instance
  ( NoThunks (StorageConfig m)
  , NoThunks (StorageConfig a)
  ) =>
  NoThunks (StorageConfig (DualBlock m a))

-- Use generic instance

{-------------------------------------------------------------------------------
  Bridge two ledgers
-------------------------------------------------------------------------------}

-- | Bridge the two ledgers
class
  ( -- Requirements on the main block
    HasHeader m
  , GetHeader m
  , HasHeader (Header m)
  , LedgerSupportsProtocol m
  , HasHardForkHistory m
  , LedgerSupportsMempool m
  , CommonProtocolParams m
  , HasTxId (GenTx m)
  , Show (ApplyTxErr m)
  , Show (LedgerConfig m)
  , -- Requirements on the auxiliary block
    -- No 'LedgerSupportsProtocol' for @a@!
    Typeable a
  , UpdateLedger a
  , LedgerSupportsMempool a
  , Show (ApplyTxErr a)
  , Show (LedgerConfig a)
  , NoThunks (LedgerConfig a)
  , NoThunks (CodecConfig a)
  , NoThunks (StorageConfig a)
  , -- Requirements on the various bridges
    Show (BridgeLedger m a)
  , Eq (BridgeLedger m a)
  , Serialise (BridgeLedger m a)
  , Serialise (BridgeBlock m a)
  , Serialise (BridgeTx m a)
  , Show (BridgeTx m a)
  ) =>
  Bridge m a
  where
  -- | Additional information relating both ledgers
  type BridgeLedger m a :: Type

  -- | Information required to update the bridge when applying a block
  type BridgeBlock m a :: Type

  -- | Information required to update the bridge when applying a transaction
  type BridgeTx m a :: Type

  updateBridgeWithBlock ::
    DualBlock m a ->
    BridgeLedger m a ->
    BridgeLedger m a

  updateBridgeWithTx ::
    Validated (GenTx (DualBlock m a)) ->
    BridgeLedger m a ->
    BridgeLedger m a

{-------------------------------------------------------------------------------
  HasHeader instance
-------------------------------------------------------------------------------}

instance Bridge m a => HasHeader (DualBlock m a) where
  getHeaderFields = getBlockHeaderFields

instance Bridge m a => HasHeader (DualHeader m a) where
  getHeaderFields = castHeaderFields . getHeaderFields . dualHeaderMain

instance Bridge m a => GetPrevHash (DualBlock m a) where
  headerPrevHash = castHash . headerPrevHash . dualHeaderMain

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

type instance BlockProtocol (DualBlock m a) = BlockProtocol m

instance Bridge m a => BlockSupportsProtocol (DualBlock m a) where
  validateView cfg = validateView (dualBlockConfigMain cfg) . dualHeaderMain
  tiebreakerView cfg = tiebreakerView (dualBlockConfigMain cfg) . dualHeaderMain

  projectChainOrderConfig = projectChainOrderConfig . dualBlockConfigMain

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

-- | Both ledger rules threw an error
--
-- We do not verify that the errors agree, merely that they both report /some/
-- error.
--
-- If only /one/ of the two semantics reports an error, we fail with an 'error'
-- (see 'agreeOnError'), rather than a regular chain failure; if this happens,
-- it indicates a bug, and the node should fail (rather than just, for example,
-- reject a block).
data DualLedgerError m a = DualLedgerError
  { dualLedgerErrorMain :: LedgerError m
  , dualLedgerErrorAux :: LedgerError a
  }
  deriving NoThunks via AllowThunk (DualLedgerError m a)

deriving instance
  ( Show (LedgerError m)
  , Show (LedgerError a)
  ) =>
  Show (DualLedgerError m a)
deriving instance
  ( Eq (LedgerError m)
  , Eq (LedgerError a)
  ) =>
  Eq (DualLedgerError m a)

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

data DualLedgerConfig m a = DualLedgerConfig
  { dualLedgerConfigMain :: LedgerConfig m
  , dualLedgerConfigAux :: LedgerConfig a
  }
  deriving NoThunks via AllowThunk (DualLedgerConfig m a)

deriving instance (Show (LedgerConfig m), Show (LedgerConfig a)) => Show (DualLedgerConfig m a)

type instance LedgerCfg (LedgerState (DualBlock m a)) = DualLedgerConfig m a

instance Bridge m a => HasPartialLedgerConfig (DualBlock m a)

instance Bridge m a => GetTip (LedgerState (DualBlock m a)) where
  getTip = castPoint . getTip . dualLedgerStateMain

instance Bridge m a => GetTip (Ticked (LedgerState (DualBlock m a))) where
  getTip = castPoint . getTip . tickedDualLedgerStateMain

-- We only have tables on the main ledger state to be able to compare it to a
-- reference spec implementation which doesn't use tables. The result should be
-- the same.
data instance Ticked (LedgerState (DualBlock m a)) mk = TickedDualLedgerState
  { tickedDualLedgerStateMain :: Ticked (LedgerState m) mk
  , tickedDualLedgerStateAux :: Ticked (LedgerState a) ValuesMK
  , tickedDualLedgerStateBridge :: BridgeLedger m a
  , tickedDualLedgerStateAuxOrig :: LedgerState a ValuesMK
  -- ^ The original, unticked ledger for the auxiliary block
  --
  -- The reason we keep this in addition to the ticked ledger state is that
  -- not every main block is paired with an auxiliary block. When there is
  -- no auxiliary block, the auxiliary ledger state remains unchanged.
  }
  deriving NoThunks via AllowThunk (Ticked (LedgerState (DualBlock m a)) mk)

instance Bridge m a => IsLedger (LedgerState (DualBlock m a)) where
  type LedgerErr (LedgerState (DualBlock m a)) = DualLedgerError m a

  -- \| The dual ledger events are exactly those of the main ledger; it ignores
  -- any possible auxiliary ledger events.
  --
  -- NOTE: This may change. It's unclear whether we want the two ledgers to emit
  -- the same events. And right now we only use the Dual ledger for our tests,
  -- and do so only with the Byron and ByronSpecs ledgers, neither of which have
  -- any events. So we make this easy choice for for now.
  type AuxLedgerEvent (LedgerState (DualBlock m a)) = AuxLedgerEvent (LedgerState m)

  applyChainTickLedgerResult
    evs
    DualLedgerConfig{..}
    slot
    DualLedgerState{..} =
      castLedgerResult ledgerResult <&> \main ->
        TickedDualLedgerState
          { tickedDualLedgerStateMain = main
          , tickedDualLedgerStateAux = applyDiffs dualLedgerStateAux dualLedger
          , tickedDualLedgerStateAuxOrig = dualLedgerStateAux
          , tickedDualLedgerStateBridge = dualLedgerStateBridge
          }
     where
      dualLedger =
        applyChainTick
          evs
          dualLedgerConfigAux
          slot
          (forgetLedgerTables dualLedgerStateAux)
      ledgerResult =
        applyChainTickLedgerResult
          evs
          dualLedgerConfigMain
          slot
          dualLedgerStateMain

applyHelper ::
  Bridge m a =>
  ( ComputeLedgerEvents ->
    LedgerCfg (LedgerState m) ->
    m ->
    Ticked (LedgerState m) ValuesMK ->
    Except (LedgerErr (LedgerState m)) (LedgerResult (LedgerState m) (LedgerState m DiffMK))
  ) ->
  ComputeLedgerEvents ->
  DualLedgerConfig m a ->
  DualBlock m a ->
  Ticked (LedgerState (DualBlock m a)) ValuesMK ->
  Except
    (DualLedgerError m a)
    (LedgerResult (LedgerState (DualBlock m a)) (LedgerState (DualBlock m a) DiffMK))
applyHelper f opts cfg block@DualBlock{..} TickedDualLedgerState{..} = do
  (ledgerResult, aux') <-
    agreeOnError
      DualLedgerError
      ( f
          opts
          (dualLedgerConfigMain cfg)
          dualBlockMain
          tickedDualLedgerStateMain
      , applyMaybeBlock
          opts
          (dualLedgerConfigAux cfg)
          dualBlockAux
          tickedDualLedgerStateAux
          (forgetLedgerTables tickedDualLedgerStateAuxOrig)
      )
  return $
    castLedgerResult ledgerResult <&> \main' ->
      DualLedgerState
        { dualLedgerStateMain = main'
        , dualLedgerStateAux = applyDiffs tickedDualLedgerStateAux aux'
        , dualLedgerStateBridge =
            updateBridgeWithBlock
              block
              tickedDualLedgerStateBridge
        }

instance Bridge m a => ApplyBlock (LedgerState (DualBlock m a)) (DualBlock m a) where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult
    evs
    cfg
    block@DualBlock{..}
    TickedDualLedgerState{..} =
      castLedgerResult ledgerResult <&> \main' ->
        DualLedgerState
          { dualLedgerStateMain = main'
          , dualLedgerStateAux = applyDiffs tickedDualLedgerStateAux auxLedger
          , dualLedgerStateBridge =
              updateBridgeWithBlock
                block
                tickedDualLedgerStateBridge
          }
     where
      auxLedger =
        reapplyMaybeBlock
          evs
          (dualLedgerConfigAux cfg)
          dualBlockAux
          tickedDualLedgerStateAux
          (forgetLedgerTables tickedDualLedgerStateAuxOrig)
      ledgerResult =
        reapplyBlockLedgerResult
          evs
          (dualLedgerConfigMain cfg)
          dualBlockMain
          tickedDualLedgerStateMain

  getBlockKeySets =
    castLedgerTables
      . getBlockKeySets @(LedgerState m)
      . dualBlockMain

data instance LedgerState (DualBlock m a) mk = DualLedgerState
  { dualLedgerStateMain :: LedgerState m mk
  , dualLedgerStateAux :: LedgerState a ValuesMK
  , dualLedgerStateBridge :: BridgeLedger m a
  }
  deriving NoThunks via AllowThunk (LedgerState (DualBlock m a) mk)

instance Bridge m a => UpdateLedger (DualBlock m a)

deriving instance
  ( Bridge m a
  , ShowMK mk
  ) =>
  Show (LedgerState (DualBlock m a) mk)
deriving instance
  ( Bridge m a
  , EqMK mk
  ) =>
  Eq (LedgerState (DualBlock m a) mk)

{-------------------------------------------------------------------------------
  Utilities for working with the extended ledger state
-------------------------------------------------------------------------------}

dualExtValidationErrorMain ::
  ExtValidationError (DualBlock m a) ->
  ExtValidationError m
dualExtValidationErrorMain = \case
  ExtValidationErrorLedger e -> ExtValidationErrorLedger (dualLedgerErrorMain e)
  ExtValidationErrorHeader e -> ExtValidationErrorHeader (castHeaderError e)

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol

  These definitions are asymmetric because the auxiliary block is not involved
  in the consensus protocol, and has no 'LedgerSupportsProtocol' instance.
-------------------------------------------------------------------------------}

instance Bridge m a => HasAnnTip (DualBlock m a) where
  type TipInfo (DualBlock m a) = TipInfo m
  tipInfoHash _ = tipInfoHash (Proxy @m)
  getTipInfo = getTipInfo . dualHeaderMain

instance Bridge m a => BasicEnvelopeValidation (DualBlock m a) where
  expectedFirstBlockNo _ = expectedFirstBlockNo (Proxy @m)
  expectedNextBlockNo _ = expectedNextBlockNo (Proxy @m)
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @m)
  minimumNextSlotNo _ = minimumNextSlotNo (Proxy @m)

instance Bridge m a => ValidateEnvelope (DualBlock m a) where
  type OtherHeaderEnvelopeError (DualBlock m a) = OtherHeaderEnvelopeError m

  additionalEnvelopeChecks cfg ledgerView hdr =
    additionalEnvelopeChecks
      (dualTopLevelConfigMain cfg)
      ledgerView
      (dualHeaderMain hdr)

instance Bridge m a => LedgerSupportsProtocol (DualBlock m a) where
  protocolLedgerView cfg state =
    protocolLedgerView
      (dualLedgerConfigMain cfg)
      (tickedDualLedgerStateMain state)

  ledgerViewForecastAt cfg state =
    ledgerViewForecastAt
      (dualLedgerConfigMain cfg)
      (dualLedgerStateMain state)

instance Bridge m a => HasHardForkHistory (DualBlock m a) where
  type HardForkIndices (DualBlock m a) = HardForkIndices m

  hardForkSummary cfg state =
    hardForkSummary
      (dualLedgerConfigMain cfg)
      (dualLedgerStateMain state)

{-------------------------------------------------------------------------------
  Querying the ledger
-------------------------------------------------------------------------------}

data instance BlockQuery (DualBlock m a) footprint result
  deriving Show

instance
  (Typeable m, Typeable a) =>
  ShowProxy (BlockQuery (DualBlock m a))

-- | Not used in the tests: no constructors
instance Bridge m a => BlockSupportsLedgerQuery (DualBlock m a) where
  answerPureBlockQuery _ = \case {}
  answerBlockQueryLookup _ = \case {}
  answerBlockQueryTraverse _ = \case {}
  blockQueryIsSupportedOnVersion qry _ = case qry of {}

instance SameDepIndex2 (BlockQuery (DualBlock m a)) where
  sameDepIndex2 = \case {}

instance ShowQuery (BlockQuery (DualBlock m a) footprint) where
  showResult = \case {}

-- | Forward to the main ledger
instance Bridge m a => CommonProtocolParams (DualBlock m a) where
  maxHeaderSize = maxHeaderSize . dualLedgerStateMain
  maxTxSize = maxTxSize . dualLedgerStateMain

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

data DualGenTxErr m a = DualGenTxErr
  { dualGenTxErrMain :: ApplyTxErr m
  , dualGenTxErrAux :: ApplyTxErr a
  }

instance
  (Typeable m, Typeable a) =>
  ShowProxy (DualGenTxErr m a)

data instance GenTx (DualBlock m a) = DualGenTx
  { dualGenTxMain :: GenTx m
  , dualGenTxAux :: GenTx a
  , dualGenTxBridge :: BridgeTx m a
  }
  deriving NoThunks via AllowThunk (GenTx (DualBlock m a))

data instance Validated (GenTx (DualBlock m a)) = ValidatedDualGenTx
  { vDualGenTxMain :: Validated (GenTx m)
  , vDualGenTxAux :: Validated (GenTx a)
  , vDualGenTxBridge :: BridgeTx m a
  }
  deriving NoThunks via AllowThunk (Validated (GenTx (DualBlock m a)))

instance
  (Typeable m, Typeable a) =>
  ShowProxy (GenTx (DualBlock m a))

type instance ApplyTxErr (DualBlock m a) = DualGenTxErr m a

instance Bridge m a => LedgerSupportsMempool (DualBlock m a) where
  applyTx
    DualLedgerConfig{..}
    wti
    slot
    DualGenTx{..}
    TickedDualLedgerState{..} = do
      ((main', mainVtx), (aux', auxVtx)) <-
        agreeOnError
          DualGenTxErr
          ( applyTx
              dualLedgerConfigMain
              wti
              slot
              dualGenTxMain
              tickedDualLedgerStateMain
          , applyTx
              dualLedgerConfigAux
              wti
              slot
              dualGenTxAux
              tickedDualLedgerStateAux
          )
      let vtx =
            ValidatedDualGenTx
              { vDualGenTxMain = mainVtx
              , vDualGenTxAux = auxVtx
              , vDualGenTxBridge = dualGenTxBridge
              }
      return
        ( TickedDualLedgerState
            { tickedDualLedgerStateMain = main'
            , tickedDualLedgerStateAux = applyDiffs tickedDualLedgerStateAux aux'
            , tickedDualLedgerStateAuxOrig = tickedDualLedgerStateAuxOrig
            , tickedDualLedgerStateBridge =
                updateBridgeWithTx
                  vtx
                  tickedDualLedgerStateBridge
            }
        , vtx
        )

  reapplyTx
    doDiffs
    DualLedgerConfig{..}
    slot
    tx@ValidatedDualGenTx{..}
    TickedDualLedgerState{..} = do
      (main', aux') <-
        agreeOnError
          DualGenTxErr
          ( reapplyTx
              doDiffs
              dualLedgerConfigMain
              slot
              vDualGenTxMain
              tickedDualLedgerStateMain
          , reapplyTx
              doDiffs
              dualLedgerConfigAux
              slot
              vDualGenTxAux
              tickedDualLedgerStateAux
          )
      return $
        TickedDualLedgerState
          { tickedDualLedgerStateMain = main'
          , tickedDualLedgerStateAux = trackingToValues aux'
          , tickedDualLedgerStateAuxOrig = tickedDualLedgerStateAuxOrig
          , tickedDualLedgerStateBridge =
              updateBridgeWithTx
                tx
                tickedDualLedgerStateBridge
          }

  txForgetValidated vtx =
    DualGenTx
      { dualGenTxMain = txForgetValidated vDualGenTxMain
      , dualGenTxAux = txForgetValidated vDualGenTxAux
      , dualGenTxBridge = vDualGenTxBridge
      }
   where
    ValidatedDualGenTx
      { vDualGenTxMain
      , vDualGenTxAux
      , vDualGenTxBridge
      } = vtx

  getTransactionKeySets =
    castLedgerTables
      . getTransactionKeySets @m
      . dualGenTxMain

instance Bridge m a => TxLimits (DualBlock m a) where
  type TxMeasure (DualBlock m a) = TxMeasure m

  txMeasure DualLedgerConfig{..} TickedDualLedgerState{..} DualGenTx{..} =
    do
      mapExcept (inj +++ id)
      $ txMeasure dualLedgerConfigMain tickedDualLedgerStateMain dualGenTxMain
   where
    inj m = DualGenTxErr m (error "ByronSpec has no tx-too-big error")

  blockCapacityTxMeasure DualLedgerConfig{..} TickedDualLedgerState{..} =
    blockCapacityTxMeasure dualLedgerConfigMain tickedDualLedgerStateMain

-- We don't need a pair of IDs, as long as we can unique ID the transaction
newtype instance TxId (GenTx (DualBlock m a)) = DualGenTxId
  { dualGenTxIdMain :: GenTxId m
  }
  deriving NoThunks via AllowThunk (TxId (GenTx (DualBlock m a)))

instance
  (Typeable m, Typeable a) =>
  ShowProxy (TxId (GenTx (DualBlock m a)))

instance Bridge m a => HasTxId (GenTx (DualBlock m a)) where
  txId = DualGenTxId . txId . dualGenTxMain

deriving instance Bridge m a => Show (GenTx (DualBlock m a))
deriving instance Bridge m a => Show (Validated (GenTx (DualBlock m a)))
deriving instance Bridge m a => Show (DualGenTxErr m a)

deriving instance Show (GenTxId m) => Show (TxId (GenTx (DualBlock m a)))
deriving instance Eq (GenTxId m) => Eq (TxId (GenTx (DualBlock m a)))
deriving instance Ord (GenTxId m) => Ord (TxId (GenTx (DualBlock m a)))

{-------------------------------------------------------------------------------
  Nested contents

  Since we only have a single header, we just delegate to the main block.
-------------------------------------------------------------------------------}

newtype instance NestedCtxt_ (DualBlock m a) f x where
  CtxtDual :: NestedCtxt_ m f x -> NestedCtxt_ (DualBlock m a) f x

deriving instance
  Show (NestedCtxt_ m f x) =>
  Show (NestedCtxt_ (DualBlock m a) f x)

instance
  SameDepIndex (NestedCtxt_ m f) =>
  SameDepIndex (NestedCtxt_ (DualBlock m a) f)
  where
  sameDepIndex (CtxtDual ctxt) (CtxtDual ctxt') =
    sameDepIndex ctxt ctxt'

ctxtDualMain :: NestedCtxt_ (DualBlock m a) f x -> NestedCtxt_ m f x
ctxtDualMain (CtxtDual ctxtMain) = ctxtMain

instance
  HasNestedContent Header m =>
  HasNestedContent Header (DualBlock m a)
  where
  unnest = depPairFirst (mapNestedCtxt CtxtDual) . unnest . dualHeaderMain
  nest = DualHeader . nest . depPairFirst (mapNestedCtxt ctxtDualMain)

instance
  ReconstructNestedCtxt Header m =>
  ReconstructNestedCtxt Header (DualBlock m a)
  where
  reconstructPrefixLen _ =
    -- Account for the outer @encodeListLen 3@
    1 `addPrefixLen` reconstructPrefixLen (Proxy @(Header m))
  reconstructNestedCtxt _ prefix size =
    case reconstructNestedCtxt (Proxy @(Header m)) prefixMain size of
      SomeSecond ctxt -> SomeSecond (mapNestedCtxt CtxtDual ctxt)
   where
    prefixMain = Short.pack . drop 1 . Short.unpack $ prefix

instance
  EncodeDiskDepIx (NestedCtxt Header) m =>
  EncodeDiskDepIx (NestedCtxt Header) (DualBlock m a)
  where
  encodeDiskDepIx ccfg (SomeSecond ctxt) =
    encodeDiskDepIx
      (dualCodecConfigMain ccfg)
      (SomeSecond (mapNestedCtxt ctxtDualMain ctxt))

instance
  EncodeDiskDep (NestedCtxt Header) m =>
  EncodeDiskDep (NestedCtxt Header) (DualBlock m a)
  where
  encodeDiskDep ccfg ctxt =
    encodeDiskDep
      (dualCodecConfigMain ccfg)
      (mapNestedCtxt ctxtDualMain ctxt)

{-------------------------------------------------------------------------------
  HasBinaryBlockInfo
-------------------------------------------------------------------------------}

-- | The binary info just refers to the main block
--
-- This is sufficient, because we never need just the header of the auxiliary.
instance HasBinaryBlockInfo m => HasBinaryBlockInfo (DualBlock m a) where
  getBinaryBlockInfo DualBlock{..} =
    BinaryBlockInfo
      { headerSize = headerSize mainBinaryBlockInfo
      , headerOffset = headerOffset mainBinaryBlockInfo + 1
      }
   where
    mainBinaryBlockInfo :: BinaryBlockInfo
    mainBinaryBlockInfo = getBinaryBlockInfo dualBlockMain

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

-- | 'InspectLedger' just refers to the main block
--
-- 'InspectLedger' is intended to check the ledger state against the node's
-- configuration, and hence takes a full 'TopLevelConfig'. However, we cannot
-- construct that for the auxiliary block, since we have no protocol config
-- for it. We therefore just use the main block.
instance InspectLedger m => InspectLedger (DualBlock m a) where
  type LedgerWarning (DualBlock m a) = LedgerWarning m
  type LedgerUpdate (DualBlock m a) = LedgerUpdate m

  inspectLedger cfg before after =
    map castLedgerEvent $
      inspectLedger
        (dualTopLevelConfigMain cfg)
        (dualLedgerStateMain before)
        (dualLedgerStateMain after)

{-------------------------------------------------------------------------------
  PeerSelection
-------------------------------------------------------------------------------}

instance
  LedgerSupportsPeerSelection m =>
  LedgerSupportsPeerSelection (DualBlock m a)
  where
  getPeers = getPeers . dualLedgerStateMain

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance CannotForge (DualBlock m a) = CannotForge m
type instance ForgeStateInfo (DualBlock m a) = ForgeStateInfo m
type instance ForgeStateUpdateError (DualBlock m a) = ForgeStateUpdateError m

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'applyLedgerBlock' to @Maybe blk@
--
-- Returns state unchanged on 'Nothing'
applyMaybeBlock ::
  UpdateLedger blk =>
  ComputeLedgerEvents ->
  LedgerConfig blk ->
  Maybe blk ->
  TickedLedgerState blk ValuesMK ->
  LedgerState blk EmptyMK ->
  Except (LedgerError blk) (LedgerState blk DiffMK)
applyMaybeBlock _ _ Nothing _ st = return $ st `withLedgerTables` emptyLedgerTables
applyMaybeBlock evs cfg (Just block) tst _ = applyLedgerBlock evs cfg block tst

-- | Lift 'reapplyLedgerBlock' to @Maybe blk@
--
-- See also 'applyMaybeBlock'
reapplyMaybeBlock ::
  UpdateLedger blk =>
  ComputeLedgerEvents ->
  LedgerConfig blk ->
  Maybe blk ->
  TickedLedgerState blk ValuesMK ->
  LedgerState blk EmptyMK ->
  LedgerState blk DiffMK
reapplyMaybeBlock _ _ Nothing _ st = st `withLedgerTables` emptyLedgerTables
reapplyMaybeBlock evs cfg (Just block) tst _ = reapplyLedgerBlock evs cfg block tst

-- | Used when the concrete and abstract implementation should agree on errors
--
-- The abstract-versus-concrete tests from the ledger folks tests precisely
-- this, so if this fails, it indicates a bug somewhere and we error out.
agreeOnError ::
  (Show e, Show e', HasCallStack) =>
  (e -> e' -> err) ->
  (Except e a, Except e' b) ->
  Except err (a, b)
agreeOnError f (ma, mb) =
  case (runExcept ma, runExcept mb) of
    (Left e, Left e') ->
      throwError $ f e e'
    (Left e, Right _) ->
      error $ "agreeOnError: unexpected error " ++ show e
    (Right _, Left e') ->
      error $ "agreeOnError: unexpected error " ++ show e'
    (Right a, Right b) ->
      return (a, b)

{-------------------------------------------------------------------------------
  Serialisation

  For now we just require 'Serialise' for the auxiliary block.
-------------------------------------------------------------------------------}

encodeDualLedgerConfig ::
  (LedgerCfg (LedgerState m) -> Encoding) ->
  (LedgerCfg (LedgerState a) -> Encoding) ->
  DualLedgerConfig m a ->
  Encoding
encodeDualLedgerConfig encodeM encodeA (DualLedgerConfig m a) =
  mconcat
    [ encodeListLen 2
    , encodeM m
    , encodeA a
    ]

decodeDualLedgerConfig ::
  Decoder s (LedgerCfg (LedgerState m)) ->
  Decoder s (LedgerCfg (LedgerState a)) ->
  Decoder s (DualLedgerConfig m a)
decodeDualLedgerConfig decodeM decodeA = do
  enforceSize "DualLedgerConfig" 2
  DualLedgerConfig
    <$> decodeM
    <*> decodeA

encodeDualBlock ::
  (Bridge m a, Serialise a) =>
  (m -> Encoding) ->
  DualBlock m a ->
  Encoding
encodeDualBlock encodeMain DualBlock{..} =
  mconcat
    [ encodeListLen 3
    , encodeMain dualBlockMain
    , encode dualBlockAux
    , encode dualBlockBridge
    ]

decodeDualBlock ::
  (Bridge m a, Serialise a) =>
  Decoder s (Lazy.ByteString -> m) ->
  Decoder s (Lazy.ByteString -> DualBlock m a)
decodeDualBlock decodeMain = do
  enforceSize "DualBlock" 3
  dualBlock
    <$> decodeMain
    <*> decode
    <*> decode
 where
  dualBlock ::
    (Lazy.ByteString -> m) ->
    Maybe a ->
    BridgeBlock m a ->
    (Lazy.ByteString -> DualBlock m a)
  dualBlock conc abst bridge bs = DualBlock (conc bs) abst bridge

encodeDualHeader ::
  (Header m -> Encoding) ->
  Header (DualBlock m a) ->
  Encoding
encodeDualHeader encodeMain DualHeader{..} = encodeMain dualHeaderMain

decodeDualHeader ::
  Decoder s (Lazy.ByteString -> Header m) ->
  Decoder s (Lazy.ByteString -> Header (DualBlock m a))
decodeDualHeader decodeMain =
  dualHeader <$> decodeMain
 where
  dualHeader ::
    (Lazy.ByteString -> Header m) ->
    (Lazy.ByteString -> Header (DualBlock m a))
  dualHeader conc bs = DualHeader (conc bs)

encodeDualGenTx ::
  (Bridge m a, Serialise (GenTx a)) =>
  (GenTx m -> Encoding) ->
  GenTx (DualBlock m a) ->
  Encoding
encodeDualGenTx encodeMain DualGenTx{..} =
  mconcat
    [ encodeListLen 3
    , encodeMain dualGenTxMain
    , encode dualGenTxAux
    , encode dualGenTxBridge
    ]

decodeDualGenTx ::
  (Bridge m a, Serialise (GenTx a)) =>
  Decoder s (GenTx m) ->
  Decoder s (GenTx (DualBlock m a))
decodeDualGenTx decodeMain = do
  enforceSize "DualGenTx" 3
  DualGenTx
    <$> decodeMain
    <*> decode
    <*> decode

encodeDualGenTxId ::
  (GenTxId m -> Encoding) ->
  GenTxId (DualBlock m a) ->
  Encoding
encodeDualGenTxId encodeMain = encodeMain . dualGenTxIdMain

decodeDualGenTxId ::
  Decoder s (GenTxId m) ->
  Decoder s (GenTxId (DualBlock m a))
decodeDualGenTxId decodeMain = DualGenTxId <$> decodeMain

encodeDualGenTxErr ::
  Serialise (ApplyTxErr a) =>
  (ApplyTxErr m -> Encoding) ->
  ApplyTxErr (DualBlock m a) ->
  Encoding
encodeDualGenTxErr encodeMain DualGenTxErr{..} =
  mconcat
    [ encodeListLen 2
    , encodeMain dualGenTxErrMain
    , encode dualGenTxErrAux
    ]

decodeDualGenTxErr ::
  Serialise (ApplyTxErr a) =>
  Decoder s (ApplyTxErr m) ->
  Decoder s (ApplyTxErr (DualBlock m a))
decodeDualGenTxErr decodeMain = do
  enforceSize "DualGenTxErr" 2
  DualGenTxErr
    <$> decodeMain
    <*> decode

encodeDualLedgerState ::
  (Bridge m a, Serialise (LedgerState a ValuesMK)) =>
  (LedgerState m mk -> Encoding) ->
  LedgerState (DualBlock m a) mk ->
  Encoding
encodeDualLedgerState encodeMain DualLedgerState{..} =
  mconcat
    [ encodeListLen 3
    , encodeMain dualLedgerStateMain
    , encode dualLedgerStateAux
    , encode dualLedgerStateBridge
    ]

decodeDualLedgerState ::
  (Bridge m a, Serialise (LedgerState a ValuesMK)) =>
  Decoder s (LedgerState m mk) ->
  Decoder s (LedgerState (DualBlock m a) mk)
decodeDualLedgerState decodeMain = do
  enforceSize "DualLedgerState" 3
  DualLedgerState
    <$> decodeMain
    <*> decode
    <*> decode

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

type instance TxIn (LedgerState (DualBlock m a)) = TxIn (LedgerState m)
type instance TxOut (LedgerState (DualBlock m a)) = TxOut (LedgerState m)

instance CanUpgradeLedgerTables (LedgerState (DualBlock m a)) where
  upgradeTables _ _ = id

instance
  (txout ~ TxOut (LedgerState m), IndexedMemPack (LedgerState m EmptyMK) txout) =>
  IndexedMemPack (LedgerState (DualBlock m a) EmptyMK) txout
  where
  indexedTypeName (DualLedgerState st _ _) = indexedTypeName @(LedgerState m EmptyMK) @txout st
  indexedPackedByteCount (DualLedgerState st _ _) = indexedPackedByteCount st
  indexedPackM (DualLedgerState st _ _) = indexedPackM st
  indexedUnpackM (DualLedgerState st _ _) = indexedUnpackM st

instance
  (Ord (TxIn (LedgerState m)), MemPack (TxIn (LedgerState m)), MemPack (TxOut (LedgerState m))) =>
  SerializeTablesWithHint (LedgerState (DualBlock m a))
  where
  encodeTablesWithHint = defaultEncodeTablesWithHint
  decodeTablesWithHint = defaultDecodeTablesWithHint

instance
  ( Bridge m a
  , NoThunks (TxOut (LedgerState m))
  , NoThunks (TxIn (LedgerState m))
  , Show (TxOut (LedgerState m))
  , Show (TxIn (LedgerState m))
  , Eq (TxOut (LedgerState m))
  , Ord (TxIn (LedgerState m))
  , MemPack (TxIn (LedgerState m))
  ) =>
  HasLedgerTables (LedgerState (DualBlock m a))
  where
  projectLedgerTables DualLedgerState{..} =
    castLedgerTables
      (projectLedgerTables dualLedgerStateMain)

  withLedgerTables DualLedgerState{..} main =
    DualLedgerState
      { dualLedgerStateMain =
          withLedgerTables dualLedgerStateMain $
            castLedgerTables main
      , dualLedgerStateAux = dualLedgerStateAux
      , dualLedgerStateBridge = dualLedgerStateBridge
      }

instance
  ( Bridge m a
  , NoThunks (TxOut (LedgerState m))
  , NoThunks (TxIn (LedgerState m))
  , Show (TxOut (LedgerState m))
  , Show (TxIn (LedgerState m))
  , Eq (TxOut (LedgerState m))
  , Ord (TxIn (LedgerState m))
  , MemPack (TxIn (LedgerState m))
  ) =>
  HasLedgerTables (Ticked (LedgerState (DualBlock m a)))
  where
  projectLedgerTables TickedDualLedgerState{..} =
    castLedgerTables
      (projectLedgerTables tickedDualLedgerStateMain)

  withLedgerTables
    TickedDualLedgerState{..}
    main =
      TickedDualLedgerState
        { tickedDualLedgerStateMain =
            withLedgerTables tickedDualLedgerStateMain $ castLedgerTables main
        , tickedDualLedgerStateAux
        , tickedDualLedgerStateBridge
        , tickedDualLedgerStateAuxOrig
        }

instance
  CanStowLedgerTables (LedgerState m) =>
  CanStowLedgerTables (LedgerState (DualBlock m a))
  where
  stowLedgerTables dls =
    DualLedgerState
      { dualLedgerStateMain = stowLedgerTables dualLedgerStateMain
      , dualLedgerStateAux
      , dualLedgerStateBridge
      }
   where
    DualLedgerState
      { dualLedgerStateMain
      , dualLedgerStateAux
      , dualLedgerStateBridge
      } = dls

  unstowLedgerTables dls =
    DualLedgerState
      { dualLedgerStateMain = unstowLedgerTables dualLedgerStateMain
      , dualLedgerStateAux
      , dualLedgerStateBridge
      }
   where
    DualLedgerState
      { dualLedgerStateMain
      , dualLedgerStateAux
      , dualLedgerStateBridge
      } = dls
