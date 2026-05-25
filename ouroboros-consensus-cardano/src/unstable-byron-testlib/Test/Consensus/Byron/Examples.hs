{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Consensus.Byron.Examples
  ( -- * Setup
    cfg
  , codecConfig
  , leaderCredentials
  , ledgerConfig
  , secParam
  , windowSize

    -- * Examples
  , exampleApplyTxErr
  , exampleChainDepState
  , exampleExtLedgerState
  , exampleGenTx
  , exampleGenTxId
  , exampleHeaderHash
  , exampleHeaderState
  , exampleLedgerState
  , examples
  ) where

import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad.Except (runExcept)
import qualified Control.State.Transition.Extended as STS
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Crypto.DSIGN (SignKeyDSIGN (..))
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Ledger.Conversions (toByronSlotNo)
import Ouroboros.Consensus.Byron.Node (ByronLeaderCredentials (..))
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Network.Block (Serialised (..))
import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC
import qualified Test.Cardano.Chain.Update.Example as CC
import Test.ThreadNet.Infra.Byron.ProtocolInfo (mkLeaderCredentials)
import Test.Util.Serialisation.Examples
  ( Examples (Examples)
  , Labelled
  , labelled
  , unlabelled
  )
import qualified Test.Util.Serialisation.Examples as Examples
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeByronChainDepState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam $ knownNonZeroBounded @2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

cfg :: BlockConfig ByronBlock
cfg =
  ByronConfig
    { byronGenesisConfig = CC.dummyConfig
    , byronProtocolVersion = CC.exampleProtocolVersion
    , byronSoftwareVersion = CC.exampleSoftwareVersion
    }

codecConfig :: CodecConfig ByronBlock
codecConfig = mkByronCodecConfig CC.dummyConfig

ledgerConfig :: LedgerConfig ByronBlock
ledgerConfig = CC.dummyConfig

leaderCredentials :: ByronLeaderCredentials
leaderCredentials =
  mkLeaderCredentials
    CC.dummyConfig
    CC.dummyGeneratedSecrets
    (CoreNodeId 0)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Examples ByronBlock
examples =
  Examples
    { exampleBlock = regularAndEBB exampleBlock exampleEBB
    , exampleSerialisedBlock = regularAndEBB exampleSerialisedBlock exampleSerialisedEBB
    , exampleHeader = regularAndEBB exampleHeader exampleEBBHeader
    , exampleSerialisedHeader = regularAndEBB exampleSerialisedHeader exampleSerialisedEBBHeader
    , exampleHeaderHash = unlabelled exampleHeaderHash
    , exampleGenTx = unlabelled exampleGenTx
    , exampleGenTxId = unlabelled exampleGenTxId
    , exampleApplyTxErr = unlabelled exampleApplyTxErr
    , exampleQuery = unlabelled exampleQuery
    , exampleResult = unlabelled exampleResult
    , exampleAnnTip = unlabelled exampleAnnTip
    , exampleLedgerConfig = unlabelled ledgerConfig
    , exampleLedgerState = unlabelled exampleLedgerState
    , exampleChainDepState = unlabelled exampleChainDepState
    , exampleExtLedgerState = unlabelled exampleExtLedgerState
    , exampleSlotNo = unlabelled exampleSlotNo
    }
 where
  regularAndEBB :: a -> a -> Labelled a
  regularAndEBB regular ebb = labelled [("regular", regular), ("EBB", ebb)]

  exampleQuery = SomeBlockQuery GetUpdateInterfaceState
  exampleResult = SomeResult GetUpdateInterfaceState exampleUPIState

exampleBlock :: ByronBlock
exampleBlock =
  forgeRegularBlock
    cfg
    (BlockNo 1)
    (SlotNo 1)
    (tickByronLedgerState (SlotNo 1) ledgerStateAfterEBB)
    [ValidatedByronTx exampleGenTx]
    (fakeMkIsLeader leaderCredentials)
 where
  -- \| Normally, we'd have to use 'checkIsLeader' to produce this proof.
  fakeMkIsLeader (ByronLeaderCredentials signKey dlgCert _ _) =
    PBftIsLeader
      { pbftIsLeaderSignKey = SignKeyByronDSIGN signKey
      , pbftIsLeaderDlgCert = dlgCert
      }

exampleEBB :: ByronBlock
exampleEBB = forgeEBB cfg (SlotNo 0) (BlockNo 0) GenesisHash

exampleSerialisedBlock :: Serialised ByronBlock
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleSerialisedEBB :: Serialised ByronBlock
exampleSerialisedEBB = Serialised "<EBB>"

exampleHeader :: Header ByronBlock
exampleHeader = getHeader exampleBlock

exampleEBBHeader :: Header ByronBlock
exampleEBBHeader = getHeader exampleEBB

exampleSerialisedHeader :: SerialisedHeader ByronBlock
exampleSerialisedHeader =
  SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtByronRegular 100)) (Serialised "<HEADER>")

exampleSerialisedEBBHeader :: SerialisedHeader ByronBlock
exampleSerialisedEBBHeader =
  SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtByronBoundary 100)) (Serialised "<EBB_HEADER>")

exampleAnnTip :: AnnTip ByronBlock
exampleAnnTip =
  AnnTip
    { annTipSlotNo = SlotNo 37
    , annTipBlockNo = BlockNo 23
    , annTipInfo = TipInfoIsEBB exampleHeaderHash IsNotEBB
    }

exampleChainDepState :: ChainDepState (BlockProtocol ByronBlock)
exampleChainDepState = S.fromList signers
 where
  signers = map (`S.PBftSigner` CC.exampleKeyHash) [1 .. 4]

emptyLedgerState :: LedgerState ByronBlock
emptyLedgerState =
  ByronLedgerState
    { byronLedgerTipBlockNo = Origin
    , byronLedgerState = initState
    , byronLedgerTransition = ByronTransitionInfo Map.empty
    }
 where
  initState :: CC.Block.ChainValidationState
  Right initState =
    runExcept $
      CC.Block.initialChainValidationState ledgerConfig

-- | Pure version of the chain-tick used by Byron's 'IsLedger' instance.
--
-- The instance method @applyChainTickLedgerResult@ lives in @m@ for the
-- benefit of blocks with on-disk tables, but its Byron body does no I/O:
-- it just composes 'CC.applyChainTick' with the ledger-state wrapping.
-- We reproduce that pure composition here so the golden fixtures can be
-- pure values.
tickByronLedgerState ::
  SlotNo -> LedgerState ByronBlock -> Ticked LedgerState ByronBlock
tickByronLedgerState slot ls =
  TickedByronLedgerState
    { tickedByronLedgerState =
        CC.applyChainTick ledgerConfig (toByronSlotNo slot) (byronLedgerState ls)
    , untickedByronLedgerTransition = byronLedgerTransition ls
    }

-- | Apply a block on top of a chain-ticked ledger using the exported pure
-- 'applyByronBlock' helper.
applyByronExample :: SlotNo -> ByronBlock -> LedgerState ByronBlock -> LedgerState ByronBlock
applyByronExample slot blk ledger =
  case runExcept
    ( applyByronBlock
        STS.ValidateAll
        OmitLedgerEvents
        ledgerConfig
        blk
        (tickByronLedgerState slot ledger)
    ) of
    Left err -> error $ "applyByronExample: " <> show err
    Right v -> v

ledgerStateAfterEBB :: LedgerState ByronBlock
ledgerStateAfterEBB = applyByronExample (SlotNo 0) exampleEBB emptyLedgerState

exampleLedgerState :: LedgerState ByronBlock
exampleLedgerState = applyByronExample (SlotNo 1) exampleBlock ledgerStateAfterEBB

exampleHeaderState :: HeaderState ByronBlock
exampleHeaderState = HeaderState (NotOrigin exampleAnnTip) exampleChainDepState

exampleExtLedgerState :: ExtLedgerState ByronBlock
exampleExtLedgerState =
  ExtLedgerState
    { ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

exampleHeaderHash :: ByronHash
exampleHeaderHash = blockHash exampleBlock

exampleGenTx :: GenTx ByronBlock
exampleGenTx = ByronTx CC.exampleTxId (CC.annotateTxAux CC.exampleTxAux)

exampleGenTxId :: TxId (GenTx ByronBlock)
exampleGenTxId = ByronTxId CC.exampleTxId

exampleUPIState :: CC.UPI.State
exampleUPIState = CC.UPI.initialState ledgerConfig

exampleApplyTxErr :: CC.ApplyMempoolPayloadErr
exampleApplyTxErr =
  CC.MempoolTxErr $
    CC.UTxOValidationTxValidationError $
      CC.TxValidationLovelaceError "a" $
        CC.LovelaceOverflow 0

exampleSlotNo :: SlotNo
exampleSlotNo = SlotNo 42
