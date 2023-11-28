{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Shelley.Ledger () where

import qualified Cardano.Ledger.BaseTypes as SL (epochInfoPure)
import qualified Cardano.Ledger.BHeaderView as SL (BHeaderView)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Slotting.EpochInfo
import qualified Control.Exception as Exception
import           Control.Monad.Except
import qualified Control.State.Transition.Extended as STS
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Config (castTopLevelConfig)
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Legacy.Util
import           Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (mkHeaderView)
import           Ouroboros.Consensus.Util.Singletons
import           Ouroboros.Network.Block (mkSerialised)

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

instance (ShelleyBasedEra era, IsLedger (LedgerState (ShelleyBlock proto era)))
      => IsLedger (LedgerState (LegacyBlock (ShelleyBlock proto era))) where

  type LedgerErr (LedgerState (LegacyBlock (ShelleyBlock proto era))) =
          LedgerErr (LedgerState (ShelleyBlock proto era))

  type AuxLedgerEvent (LedgerState (LegacyBlock (ShelleyBlock proto era))) =
          AuxLedgerEvent (LedgerState (ShelleyBlock proto era))

  applyChainTickLedgerResult cfg slotNo (LegacyLedgerState (ShelleyLedgerState{
                                shelleyLedgerTip
                              , shelleyLedgerState
                              , shelleyLedgerTransition
                              })) =
      swizzle appTick <&> \l' -> TickedLegacyLedgerState $
      TickedShelleyLedgerState {
          untickedShelleyLedgerTip      = shelleyLedgerTip
        , tickedShelleyLedgerTransition =
            -- The voting resets each epoch
            if isNewEpoch ei (shelleyTipSlotNo <$> shelleyLedgerTip) slotNo then
              ShelleyTransitionInfo { shelleyAfterVoting = 0 }
            else
              shelleyLedgerTransition
        , tickedShelleyLedgerState      = l'
        , tickedShelleyLedgerTables     = emptyLedgerTables
        }
    where
      globals = shelleyLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfoPure globals

      swizzle (l, events) =
          LedgerResult {
              lrEvents = map ShelleyLedgerEventTICK events
            , lrResult = l
            }

      appTick =
        SL.applyTickOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }
          globals
          shelleyLedgerState
          slotNo

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ( ShelleyCompatible proto era
         , ApplyBlock (LedgerState (ShelleyBlock proto era)) (ShelleyBlock proto era)
         ) => ApplyBlock (LedgerState (LegacyBlock (ShelleyBlock proto era))) (LegacyBlock (ShelleyBlock proto era)) where
  -- Note: in the Shelley ledger, the @CHAIN@ rule is used to apply a whole
  -- block. In consensus, we split up the application of a block to the ledger
  -- into separate steps that are performed together by 'applyExtLedgerState':
  --
  -- + 'applyChainTickLedgerResult': executes the @TICK@ transition
  -- + 'validateHeader':
  --    - 'validateEnvelope': executes the @chainChecks@
  --    - 'updateChainDepState': executes the @PRTCL@ transition
  -- + 'applyBlockLedgerResult': executes the @BBODY@ transition
  --
  applyBlockLedgerResult =
      applyHelper (swizzle ..: appBlk)
    where
      swizzle m =
        withExcept BBodyError m <&> \(l, events) ->
          LedgerResult {
              lrEvents = map ShelleyLedgerEventBBODY events
            , lrResult = l
            }

      -- Apply the BBODY transition using the ticked state
      appBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }

  reapplyBlockLedgerResult =
      runIdentity ..: applyHelper (swizzle ..: reappBlk)
    where
      swizzle m = case runExcept m of
        Left err          ->
          Exception.throw $! ShelleyReapplyException @era err
        Right (l, events) ->
          pure LedgerResult {
              lrEvents = map ShelleyLedgerEventBBODY events
            , lrResult = l
            }

      -- Reapply the BBODY transition using the ticked state
      reappBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
                  asoAssertions = STS.AssertionsOff
                , asoValidation = STS.ValidateNone
                , asoEvents     = STS.EPReturn
                }

  getBlockKeySets = const trivialLedgerTables

applyHelper ::
     forall proto m era. (ShelleyCompatible proto era, Monad m)
  => (   SL.Globals
      -> SL.NewEpochState era
      -> SL.Block (SL.BHeaderView (EraCrypto era)) era
      -> m (LedgerResult
              (LedgerState (LegacyBlock (ShelleyBlock proto era)))
              (SL.NewEpochState era)
           )
     )
  -> LedgerConfig (ShelleyBlock proto era)
  -> LegacyBlock (ShelleyBlock proto era)
  -> Ticked1 (LedgerState (LegacyBlock ((ShelleyBlock proto era)))) ValuesMK
  -> m (LedgerResult
          (LedgerState (LegacyBlock (ShelleyBlock proto era)))
          (LedgerState (LegacyBlock (ShelleyBlock proto era)) DiffMK))
applyHelper f cfg (LegacyBlock blk) (TickedLegacyLedgerState stBefore) = do
    let TickedShelleyLedgerState{
            tickedShelleyLedgerTransition
          , tickedShelleyLedgerState
          } = stBefore

    ledgerResult <-
      f
        globals
        tickedShelleyLedgerState
        ( let b  = shelleyBlockRaw blk
              h' = mkHeaderView (SL.bheader b)
          -- Jared Corduan explains that the " Unsafe " here ultimately only
          -- means the value must not be serialized. We're only passing it to
          -- 'STS.applyBlockOpts', which does not serialize it. So this is a
          -- safe use.
          in SL.UnsafeUnserialisedBlock h' (SL.bbody b)
        )

    return $ ledgerResult <&> \newNewEpochState ->
      LegacyLedgerState $
      ShelleyLedgerState {
          shelleyLedgerTip = NotOrigin ShelleyTip {
              shelleyTipBlockNo = blockNo   blk
            , shelleyTipSlotNo  = blockSlot blk
            , shelleyTipHash    = blockHash blk
            }
        , shelleyLedgerState =
            newNewEpochState
        , shelleyLedgerTransition = ShelleyTransitionInfo {
              shelleyAfterVoting =
                -- We count the number of blocks that have been applied after the
                -- voting deadline has passed.
                (if blockSlot blk >= votingDeadline then succ else id) $
                  shelleyAfterVoting tickedShelleyLedgerTransition
            }
        , shelleyLedgerTables = emptyLedgerTables
        }
  where
    globals = shelleyLedgerGlobals cfg
    swindow = SL.stabilityWindow globals

    ei :: EpochInfo Identity
    ei = SL.epochInfoPure globals

    -- The start of the next epoch is within the safe zone, always.
    startOfNextEpoch :: SlotNo
    startOfNextEpoch = runIdentity $ do
        blockEpoch <- epochInfoEpoch ei (blockSlot blk)
        let nextEpoch = succ blockEpoch
        epochInfoFirst ei nextEpoch

    -- The block must come in strictly before the voting deadline
    -- See Fig 13, "Protocol Parameter Update Inference Rules", of the
    -- Shelley specification.
    votingDeadline :: SlotNo
    votingDeadline = subSlots (2 * swindow) startOfNextEpoch

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

instance ( LedgerSupportsProtocol (ShelleyBlock proto era)
         , ShelleyCompatible proto era
         ) => BlockSupportsLedgerQuery (LegacyBlock (ShelleyBlock proto era)) where
  answerPureBlockQuery cfg (LegacyBlockQuery (q :: BlockQuery (ShelleyBlock proto era) fp result)) ext =
    case (sing :: Sing fp, q) of
      (SQFTraverseTables, GetUTxOByAddress addrs) ->
        flip SL.getFilteredUTxO addrs $ shelleyLedgerState $ getLegacyLedgerState $ ledgerState ext
      (SQFTraverseTables, GetUTxOWhole) ->
        SL.getUTxO $ shelleyLedgerState $ getLegacyLedgerState $ ledgerState ext
      (SQFTraverseTables, GetCBOR q') ->
          mkSerialised (encodeShelleyResult maxBound q')
        $ answerPureBlockQuery cfg (LegacyBlockQuery q') ext
      (SQFLookupTables, GetUTxOByTxIn txins) ->
        flip SL.getUTxOSubset txins . shelleyLedgerState . getLegacyLedgerState . ledgerState $ ext
      (SQFLookupTables, GetCBOR q') ->
        mkSerialised (encodeShelleyResult maxBound q') $
        answerPureBlockQuery cfg (LegacyBlockQuery q') ext
      (SQFNoTables, _) -> answerPureBlockQuery (castExtLedgerCfg castTopLevelConfig cfg) q $
        castExtLedgerState coerce ext
  answerBlockQueryLookup _cfg q _dlv = case q of {}
  answerBlockQueryTraverse _cfg q _dlv = case q of {}
