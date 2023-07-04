{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-- TODO: remove
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Legacy.Convert (
    Convert (..)
  , Convert'
  , convertBlock
  , convertExtLedgerState
  , convertExtLedgerState'
  , convertLedgerConfig
  , convertLedgerError
  , convertLedgerResult
  , convertLedgerState
  , convertLedgerState'
  ) where

import           Data.Coerce
import           Data.Kind
import           Data.SOP.Counting
import           Data.SOP.Functors
import           Data.SOP.Strict
import           Legacy.Byron.Ledger ()
import           Legacy.Cardano.Block
import           Legacy.LegacyBlock
import           Legacy.Shelley.Ledger ()
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.History.Summary
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Unsafe.Coerce

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

convertBlock ::
        CardanoBlock StandardCrypto
     -> LegacyCardanoBlock StandardCrypto
convertBlock = LegacyBlock
             . HardForkBlock
             . OneEraBlock
             . htrans (Proxy @Convert) transOne
             . getOneEraBlock
             . getHardForkBlock
    where
      transOne :: I x -> I (LegacyBlock x)
      transOne = I . LegacyBlock . unI

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

convertLedgerState ::
     LedgerState (CardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
convertLedgerState =
      LegacyLedgerState
    . HardForkLedgerState
    . htrans (Proxy @Convert) transOne
    . hardForkLedgerStatePerEra
  where
    transOne ::
             (Flip LedgerState EmptyMK) x
          -> (Flip LedgerState EmptyMK) (LegacyBlock x)
    transOne = Flip . mkLegacyLedgerState . unFlip

convertLedgerState' ::
     LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (CardanoBlock StandardCrypto) EmptyMK
convertLedgerState' =
      HardForkLedgerState
    . htrans (Proxy @Convert') transOne
    . hardForkLedgerStatePerEra
    . getLegacyLedgerState
  where
    transOne ::
             (Flip LedgerState EmptyMK) (LegacyBlock x)
          -> (Flip LedgerState EmptyMK) x
    transOne = Flip . unmkLegacyLedgerState . unFlip

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

convertExtLedgerState ::
     ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
convertExtLedgerState est = ExtLedgerState {
      ledgerState = convertLedgerState ledgerState
    , headerState = convertHeaderState headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

convertExtLedgerState' ::
     ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK
convertExtLedgerState' est = ExtLedgerState {
      ledgerState = convertLedgerState' ledgerState
    , headerState = convertHeaderState' headerState
    }
  where
    ExtLedgerState{
        ledgerState
      , headerState
      } = est

{-------------------------------------------------------------------------------
  Header state
-------------------------------------------------------------------------------}

convertHeaderState ::
     HeaderState (CardanoBlock StandardCrypto)
  -> HeaderState (LegacyCardanoBlock StandardCrypto)
convertHeaderState hstate = HeaderState {
      headerStateTip = convertAnnTip <$> headerStateTip
    , headerStateChainDep = convertChainDepState headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hstate

convertHeaderState' ::
     HeaderState (LegacyCardanoBlock StandardCrypto)
  -> HeaderState (CardanoBlock StandardCrypto)
convertHeaderState' hstate = HeaderState {
      headerStateTip = convertAnnTip' <$> headerStateTip
    , headerStateChainDep = convertChainDepState' headerStateChainDep
    }
  where
    HeaderState {
        headerStateTip
      , headerStateChainDep
      } = hstate

{-------------------------------------------------------------------------------
  AnnTip
-------------------------------------------------------------------------------}

convertAnnTip ::
     AnnTip (CardanoBlock StandardCrypto)
  -> AnnTip (LegacyCardanoBlock StandardCrypto)
convertAnnTip anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = convertTipInfo annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

convertAnnTip' ::
     AnnTip (LegacyCardanoBlock StandardCrypto)
  -> AnnTip (CardanoBlock StandardCrypto)
convertAnnTip' anntip = AnnTip {
      annTipSlotNo
    , annTipBlockNo
    , annTipInfo = convertTipInfo' annTipInfo
    }
  where
    AnnTip {
        annTipSlotNo
      , annTipBlockNo
      , annTipInfo
      } = anntip

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

convertTipInfo ::
     TipInfo (CardanoBlock StandardCrypto)
  -> TipInfo (LegacyCardanoBlock StandardCrypto)
convertTipInfo =
      OneEraTipInfo
    . htrans (Proxy @Convert) transOne
    . getOneEraTipInfo
  where
    transOne ::
         WrapTipInfo blk
      -> WrapTipInfo (LegacyBlock blk)
    transOne = WrapTipInfo . coerce . unwrapTipInfo

convertTipInfo' ::
     TipInfo (LegacyCardanoBlock StandardCrypto)
  -> TipInfo (CardanoBlock StandardCrypto)
convertTipInfo' =
      OneEraTipInfo
    . htrans (Proxy @Convert') transOne
    . getOneEraTipInfo
  where
    transOne ::
         WrapTipInfo (LegacyBlock blk)
      -> WrapTipInfo blk
    transOne = WrapTipInfo . coerce . unwrapTipInfo

{-------------------------------------------------------------------------------
  Chain dependent state
-------------------------------------------------------------------------------}

convertChainDepState ::
     ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
convertChainDepState  =
    htrans (Proxy @Convert) transOne
  where
    transOne ::
         WrapChainDepState blk
      -> WrapChainDepState (LegacyBlock blk)
    transOne = WrapChainDepState . coerce . unwrapChainDepState

convertChainDepState' ::
     ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
convertChainDepState'  =
    htrans (Proxy @Convert') transOne
  where
    transOne ::
         WrapChainDepState (LegacyBlock blk)
      -> WrapChainDepState blk
    transOne = WrapChainDepState . coerce . unwrapChainDepState


{-------------------------------------------------------------------------------
  Ledger error
-------------------------------------------------------------------------------}

convertLedgerError ::
     LedgerErr (LedgerState (LegacyCardanoBlock StandardCrypto))
  -> LedgerErr (LedgerState (CardanoBlock StandardCrypto))
convertLedgerError (HardForkLedgerErrorFromEra oe) =
    HardForkLedgerErrorFromEra
  . OneEraLedgerError
  . htrans (Proxy @Convert'') coerce
  . getOneEraLedgerError
  $ oe

convertLedgerError (HardForkLedgerErrorWrongEra we) =
    HardForkLedgerErrorWrongEra
  . MismatchEraInfo
  . unsafeCoerce
    -- This should not be problematic because the LedgerEraInfo and
    -- SingleEraInfo are just wrappers over a Text, so everything should be
    -- safely coerced, but SOP complains about not being able to resolve the Top
    -- constraint for every type.
  . getMismatchEraInfo
  $ we

  -- where
  --   convertMismatch :: forall xxs x xs . (xxs ~ x ': xs) =>
  --        Mismatch SingleEraInfo LedgerEraInfo (Map LegacyBlock xxs)
  --     -> Mismatch SingleEraInfo LedgerEraInfo xxs
  --   convertMismatch (ML (SingleEraInfo sei) nslei) =
  --                    ML (SingleEraInfo sei)
  --                       (htrans (Proxy @Top2) coerce nslei)
  --   convertMismatch (MR nssei (LedgerEraInfo (SingleEraInfo lei))) =
  --                    MR (htrans (Proxy @Top2) coerce nssei)
  --                       (LedgerEraInfo (SingleEraInfo lei))
  --   convertMismatch (MS n) = MS $ convertMismatch n

{-------------------------------------------------------------------------------
  Ledger result
-------------------------------------------------------------------------------}


convertLedgerResult ::
     LedgerResult
      (LedgerState (LegacyCardanoBlock StandardCrypto))
      (LedgerState (LegacyCardanoBlock StandardCrypto) DiffMK)
  -> LedgerResult
      (LedgerState (CardanoBlock StandardCrypto))
      (LedgerState (CardanoBlock StandardCrypto) EmptyMK)
convertLedgerResult le =
     LedgerResult {
      lrEvents = map ( OneEraLedgerEvent
                     . htrans (Proxy @Convert'') coerce
                     . getOneEraLedgerEvent
                     ) lrEvents
    , lrResult = convertLedgerState' $ convertMapKind lrResult
    }
 where
   LedgerResult { lrEvents
                , lrResult
                } = le

{-------------------------------------------------------------------------------
  Ledger config
-------------------------------------------------------------------------------}

convertLedgerConfig ::
     LedgerConfig (CardanoBlock StandardCrypto)
  -> LedgerConfig (LegacyCardanoBlock StandardCrypto)
convertLedgerConfig cfg =
  HardForkLedgerConfig {
      hardForkLedgerConfigShape =
        Shape
      . Exactly
      . htrans (Proxy @Top2) (K . unK)
      . getExactly
      . getShape
      $ hardForkLedgerConfigShape
    , hardForkLedgerConfigPerEra =
      PerEraLedgerConfig
      . htrans (Proxy @Convert) (
          WrapPartialLedgerConfig
        . unwrapPartialLedgerConfig
          )
      . getPerEraLedgerConfig
      $ hardForkLedgerConfigPerEra
  }
  where
    HardForkLedgerConfig {
        hardForkLedgerConfigShape
      , hardForkLedgerConfigPerEra
      } = cfg

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

class y ~ LegacyBlock x => Convert x y where
  mkLegacyBlock :: x -> y
  mkLegacyLedgerState :: LedgerState x EmptyMK -> LedgerState y EmptyMK
  unmkLegacyBlock :: y -> x
  unmkLegacyLedgerState :: LedgerState y EmptyMK -> LedgerState x EmptyMK

class Convert x y => Convert' y x

instance Convert x y => Convert' y x

instance Convert x (LegacyBlock x) where
  mkLegacyBlock = LegacyBlock
  mkLegacyLedgerState = LegacyLedgerState
  unmkLegacyBlock = getLegacyBlock
  unmkLegacyLedgerState = getLegacyLedgerState

class (Top x, Top y) => Top2 x y
instance (Top x, Top y) => Top2 x y

class ( Convert' x y
      , LedgerErr (LedgerState (LegacyBlock y)) ~ LedgerErr (LedgerState y)
      , AuxLedgerEvent (LedgerState (LegacyBlock y)) ~ AuxLedgerEvent (LedgerState y)
      ) => Convert'' x y
instance ( Convert' x y
         , LedgerErr (LedgerState (LegacyBlock y)) ~ LedgerErr (LedgerState y)
         , AuxLedgerEvent (LedgerState (LegacyBlock y)) ~ AuxLedgerEvent (LedgerState y)
         ) => Convert'' x y

type Map :: (Type -> Type) -> [Type] -> [Type]
type family Map f xs where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs
