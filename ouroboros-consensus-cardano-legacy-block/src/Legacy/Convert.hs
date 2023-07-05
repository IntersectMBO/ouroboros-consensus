{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Convert between 'CardanoBlock' and 'LegacyCardanoBlock' types.
module Legacy.Convert (
    -- * Classes
    Convert
  , ConvertSym
    -- * Block
  , convertBlock
  , convertBlock'
    -- * Ledger state
  , convertLedgerState
  , convertLedgerState'
    -- * Extended ledger state
  , convertExtLedgerState
  , convertExtLedgerState'
    -- * Header state
  , convertHeaderState
  , convertHeaderState'
    -- * Annotated tip
  , convertAnnTip
  , convertAnnTip'
    -- * Tip info
  , convertTipInfo
  , convertTipInfo'
    -- * Chain dependent state
  , convertChainDepState
  , convertChainDepState'
    -- * Ledger error
  , convertLedgerError
    -- * Ledger result
  , convertLedgerResult
    -- * Ledger config
  , convertLedgerConfig
  ) where

import           Data.Coerce
import           Data.Constraint
import           Data.SOP.Classes
import           Data.SOP.Counting
import           Data.SOP.Functors
import           Data.SOP.Match
import           Data.SOP.Strict
import           Legacy.Cardano
import           Legacy.LegacyBlock
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

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

-- | A relation between types @x@ and @'LegacyBlock' x@. Read: blocks @x@ are
-- related to their legacy version @'LegacyBlock' x@.
class y ~ LegacyBlock x => Convert x y where
-- | This is the only instance for 'Convert' that can and should exist.
instance Convert x (LegacyBlock x)

-- | Symmetry for 'Convert'.
class Convert x y => ConvertSym y x
-- | This is the only instance for 'Convert'' that can and should exist.
instance Convert x y => ConvertSym y x

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

convertBlock ::
        CardanoBlock       StandardCrypto
     -> LegacyCardanoBlock StandardCrypto
convertBlock =
        LegacyBlock
      . HardForkBlock
      . OneEraBlock
      . hcoerce
      . getOneEraBlock
      . getHardForkBlock

convertBlock' ::
        LegacyCardanoBlock StandardCrypto
     -> CardanoBlock       StandardCrypto
convertBlock' =
        HardForkBlock
      . OneEraBlock
      . hcoerce
      . getOneEraBlock
      . getHardForkBlock
      . getLegacyBlock

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

convertLedgerState ::
     LedgerState (CardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
convertLedgerState =
      LegacyLedgerState
    . HardForkLedgerState
    . hcoerce
    . hardForkLedgerStatePerEra

convertLedgerState' ::
     LedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> LedgerState (CardanoBlock StandardCrypto) EmptyMK
convertLedgerState' =
      HardForkLedgerState
    . hcoerce
    . hardForkLedgerStatePerEra
    . getLegacyLedgerState

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
  Annotated tip
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
  Tip info
-------------------------------------------------------------------------------}

convertTipInfo ::
     TipInfo (CardanoBlock StandardCrypto)
  -> TipInfo (LegacyCardanoBlock StandardCrypto)
convertTipInfo = OneEraTipInfo . hcoerce . getOneEraTipInfo

convertTipInfo' ::
     TipInfo (LegacyCardanoBlock StandardCrypto)
  -> TipInfo (CardanoBlock StandardCrypto)
convertTipInfo' = OneEraTipInfo . hcoerce . getOneEraTipInfo

{-------------------------------------------------------------------------------
  Chain dependent state
-------------------------------------------------------------------------------}

convertChainDepState ::
     ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
convertChainDepState = hcoerce

convertChainDepState' ::
     ChainDepState (BlockProtocol (LegacyCardanoBlock StandardCrypto))
  -> ChainDepState (BlockProtocol (CardanoBlock StandardCrypto))
convertChainDepState' = hcoerce

{-------------------------------------------------------------------------------
  Ledger error
-------------------------------------------------------------------------------}

convertLedgerError ::
     LedgerErr (LedgerState (LegacyCardanoBlock StandardCrypto))
  -> LedgerErr (LedgerState (CardanoBlock StandardCrypto))
convertLedgerError (HardForkLedgerErrorFromEra oe) =
      HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . hcoerce
    . getOneEraLedgerError
    $ oe
convertLedgerError (HardForkLedgerErrorWrongEra we) =
      HardForkLedgerErrorWrongEra
    . MismatchEraInfo
    . hcoerce
    . getMismatchEraInfo
    $ we

type instance Same (Mismatch f) = Mismatch f

-- NOTE(jdral): some of this code could technically be optimised using
-- 'unsafeCoerce', but this would sacrifice type safety. For now, this version
-- should be sufficient.
instance (forall x y. LiftedCoercible p p x y)
      => HTrans (Mismatch p) (Mismatch p) where
  htrans ::
       forall proxy c f g xs ys. AllZipN (Prod (Mismatch p)) c xs ys
    => proxy c
    -> (forall x y. c x y => f x -> g y)
    -> Mismatch p f xs
    -> Mismatch p g ys
  htrans p t = \case
      ML fx gy -> ML (coerce fx) $ htrans p t gy
      MR fy gx | Dict <- tailDict -> MR (hcoerce fy) $ t gx
        where
          tailDict :: Dict (AllZip (LiftedCoercible p p) (Tail xs) (Tail ys))
          tailDict = replaceAllZipConstraint (Proxy @c)
      MS m     -> MS $ htrans p t m

  hcoerce ::
       forall f g xs ys. AllZipN (Prod (Mismatch p)) (LiftedCoercible f g) xs ys
    => Mismatch p f xs
    -> Mismatch p g ys
  hcoerce = \case
      ML fx gy -> ML (coerce fx) $ hcoerce gy
      MR fy gx | Dict <- tailDict -> MR (hcoerce fy) $ coerce gx
        where
          tailDict :: Dict (AllZip (LiftedCoercible p p) (Tail xs) (Tail ys))
          tailDict = replaceAllZipConstraint (Proxy @(LiftedCoercible f g))
      MS m     -> MS $ hcoerce m

replaceAllZipConstraint ::
     forall c c' xs ys.
     (AllZip c xs ys, forall x y. c' x y)
  => Proxy c -> Dict (AllZip c' xs ys)
replaceAllZipConstraint _ = go sList sList
  where
    go ::
         forall as bs. AllZip c as bs
      => SList as -> SList bs
      -> Dict (AllZip c' as bs)
    go SNil  SNil  = Dict
    go SCons SCons = case go (sList @(Tail as)) (sList @(Tail bs)) of
        Dict -> Dict

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
convertLedgerResult le = LedgerResult {
      lrEvents = map ( OneEraLedgerEvent
                     . hcoerce
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
convertLedgerConfig cfg = HardForkLedgerConfig {
      hardForkLedgerConfigShape =
          Shape
        . Exactly
        . hcoerce
        . getExactly
        . getShape
        $ hardForkLedgerConfigShape
    , hardForkLedgerConfigPerEra =
          PerEraLedgerConfig
        . hcoerce
        . getPerEraLedgerConfig
        $ hardForkLedgerConfigPerEra
    }
  where
    HardForkLedgerConfig {
        hardForkLedgerConfigShape
      , hardForkLedgerConfigPerEra
      } = cfg
