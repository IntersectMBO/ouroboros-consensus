{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Injecting a transaction from one block type to another
module Ouroboros.Consensus.HardFork.Combinator.InjectTxs
  ( TxsToApply (..)
  , rematchValidatedTxs
  , matchTx
  ) where

import Cardano.Binary (fromCBOR, toCBOR)
import Codec.CBOR.Read
import Codec.CBOR.Write
import Data.Bifunctor (bimap, second)
import Data.ByteString.Lazy
import Data.Functor.Product
import qualified Data.List as L
import qualified Data.SOP as LazySOP
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import qualified Data.SOP.Dict as Dict
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Tele
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Info
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers

-- | How a re-matched transaction must be applied at the ledger tip era.
data TxsToApply a b blk
  = -- | The transaction is already in the tip era; its validation evidence
    -- still holds, so it can be reapplied cheaply.
    ReapplyTxs [(WrapValidatedGenTx blk, a, b)]
  | -- | The transaction was upgraded from an earlier era (serialised there and
    -- decoded here as a plain 'GenTx'), losing its validation evidence, so it
    -- must be fully applied again.
    ApplyTxs [(GenTx blk, a, b)]

-- | Re-match a batch of already-validated transactions against a
-- 'State.HardForkState' whose era might have changed, /without/ translating the
-- transactions across eras.
--
-- The ledger state lives in a single era: the tip of the telescope. Walking the
-- telescope era by era, each transaction that belongs to an era we pass (a @Z@
-- at that point) is serialised to CBOR with that era's 'toCBOR' (after forgetting
-- its validation evidence — we can only encode a 'GenTx'). Once we reach the tip,
-- those bytes are decoded with the tip era's 'fromCBOR', as a 'GenTx':
--
--   * a transaction already in the tip era is kept as-is ('ReapplyTxs');
--
--   * a past-era transaction whose bytes decode in the tip era is upgraded
--     ('ApplyTxs'); it must be fully applied since it lost its evidence;
--
--   * a past-era transaction whose bytes do /not/ decode is untranslatable and
--     is reported back as a 'MismatchEraInfo';
--
--   * a transaction from a /later/ era than the tip (the ledger tip retracted)
--     is reported back as a 'MismatchEraInfo'.
--
-- Reports are built with 'Match.matchNS' from the transaction's original era and
-- the ledger tip era, which is why each transaction carries its original era
-- ('NS' 'SingleEraInfo') alongside its serialised bytes.
rematchValidatedTxs ::
  forall xs a b f.
  All SingleEraBlock xs =>
  -- | How to project a validated transaction
  (forall xs0. Validated (GenTx (HardForkBlock xs0)) -> OneEraValidatedGenTx xs0) ->
  -- | HardForkState
  State.HardForkState f xs ->
  -- | List of transactions to re-match
  [(Validated (GenTx (HardForkBlock xs)), a, b)] ->
  ( [(Validated (GenTx (HardForkBlock xs)), MismatchEraInfo xs)]
  , State.HardForkState (Product f (TxsToApply a b)) xs
  )
rematchValidatedTxs _ hfs [] =
  ([], hmap (\x -> Pair x (ReapplyTxs [])) hfs)
rematchValidatedTxs projectValidatedTx (State.HardForkState tele) vtxs =
  second State.HardForkState $ go Dict.hdicts tele vtxs'
 where
  vtxs' =
    [ (Right ns, hcmap proxySingle singleEraInfo ns, tx, a, b)
    | (tx, a, b) <- vtxs
    , let ns = getOneEraValidatedGenTx $ projectValidatedTx tx
    ]

  -- The ledger era (the tip of the telescope) as an 'NS' inhabited exactly at
  -- the ledger's position.
  ledgerEraNS :: NS LedgerEraInfo xs
  ledgerEraNS = hcmap proxySingle (LedgerEraInfo . singleEraInfo) (Tele.tip tele)

  -- Build the mismatch for a transaction whose era differs from the ledger era.
  -- 'Match.matchNS' positions it correctly (earlier or later than the ledger).
  reject :: NS SingleEraInfo xs -> MismatchEraInfo xs
  reject txEraNS = MismatchEraInfo $ case Match.matchNS txEraNS ledgerEraNS of
    Left mismatch -> mismatch
    Right _ ->
      error
        "rematchValidatedTxs: expected an era mismatch, but the transaction and \
        \ledger eras agreed; this is a bug"

  go ::
    forall f1 xs'.
    LazySOP.NP (Dict.Dict SingleEraBlock) xs' ->
    Tele.Telescope f1 (State.Current f) xs' ->
    [ATx xs a b xs'] ->
    ( [(Validated (GenTx (HardForkBlock xs)), MismatchEraInfo xs)]
    , Tele.Telescope f1 (State.Current (Product f (TxsToApply a b))) xs'
    )
  go LazySOP.Nil t _ = case t of {}
  -- We are passing a past era: serialise the transactions that belong to it
  -- (forgetting their evidence) and forward everything to the next era. The
  -- transform is inlined (rather than a @where@ helper) so that the @Z@ match
  -- stays coupled to the head era brought in scope by the 'Dict' match.
  go (Dict.Dict LazySOP.:* nextDicts) (Tele.TS i st) txs =
    second (Tele.TS i) $
      go
        nextDicts
        st
        [ ( case tx of
              Left bs -> Left bs
              Right (Z t) ->
                Left . toLazyByteString . toCBOR . txForgetValidated . unwrapValidatedGenTx $ t
              Right (S t) -> Right t
          , txEraNS
          , orig
          , a
          , b
          )
        | (tx, txEraNS, orig, a, b) <- txs
        ]
  go (Dict.Dict LazySOP.:* d) (Tele.TZ (State.Current start (st :: f blk))) txs0 =
    second (Tele.TZ . State.Current start . Pair st) $
      Dict.withDict (Dict.all_NP d) $
        go' ([], []) txs0
   where
    -- Survivors are accumulated in a single list, preserving the original order
    -- of the transactions, tagged with how each must be applied: 'Left' = kept
    -- validated (reapply), 'Right' = upgraded (fully apply). By the single-era
    -- invariant the tags are uniform, and 'toTxsToApply' turns them into the
    -- matching 'TxsToApply' constructor.
    go' ::
      All SingleEraBlock xs' =>
      Acc xs a b blk ->
      [ATx xs a b xs'] ->
      ( [(Validated (GenTx (HardForkBlock xs)), MismatchEraInfo xs)]
      , TxsToApply a b blk
      )
    go' (rej, surv) [] = (L.reverse rej, toTxsToApply (L.reverse surv))
    -- A serialised past-era transaction: upgrade it if it decodes in the tip
    -- era, report it as untranslatable otherwise.
    go' (rej, surv) ((Left bs, txEraNS, orig, a, b) : txs) =
      case deserialiseFromBytes fromCBOR bs of
        Left _ -> go' ((orig, reject txEraNS) : rej, surv) txs
        Right (_, t) -> go' (rej, (Right t, a, b) : surv) txs
    -- A transaction already in the tip era: kept as-is.
    go' (rej, surv) ((Right (Z v), _, _, a, b) : txs) =
      go' (rej, (Left v, a, b) : surv) txs
    -- A transaction from a later era: we try to downgrade it.
    go' (rej, surv) ((Right (S tx), txEraNS, orig, a, b) : txs) =
      let bs =
            hcollapse $
              hcmap proxySingle (K . toLazyByteString . toCBOR . txForgetValidated . unwrapValidatedGenTx) tx
       in case deserialiseFromBytes fromCBOR bs of
            Left _ -> go' ((orig, reject txEraNS) : rej, surv) txs
            Right (_, t) -> go' (rej, (Right t, a, b) : surv) txs

    -- Collapse the ordered, tagged survivors into a single 'TxsToApply'. The
    -- fold preserves order; a mix of tags would violate the single-era invariant.
    toTxsToApply ::
      [(Either (WrapValidatedGenTx blk) (GenTx blk), a, b)] -> TxsToApply a b blk
    toTxsToApply = L.foldr step (ReapplyTxs [])
     where
      step (Left v, a, b) (ReapplyTxs vs) = ReapplyTxs ((v, a, b) : vs)
      step (Right t, a, b) (ReapplyTxs []) = ApplyTxs [(t, a, b)]
      step (Right t, a, b) (ApplyTxs ts) = ApplyTxs ((t, a, b) : ts)
      step _ _ =
        error "rematchValidatedTxs: the batch spans more than one era; this is a bug"

type Acc xs a b blk =
  ( [(Validated (GenTx (HardForkBlock xs)), MismatchEraInfo xs)]
  , [(Either (WrapValidatedGenTx blk) (GenTx blk), a, b)]
  )

-- | A transaction being re-matched, as it travels down the telescope.
--
-- The first component is either the CBOR bytes of a transaction from an era we
-- have already passed, or the (era-tagged) validated transaction for the eras we
-- have not reached yet. The second component records the transaction's original
-- era over the whole @xs@, used to report a 'MismatchEraInfo' if untranslatable
-- or from a later era.
type ATx xs a b xs' =
  ( Either ByteString (NS WrapValidatedGenTx xs')
  , NS SingleEraInfo xs
  , Validated (GenTx (HardForkBlock xs))
  , a
  , b
  )

matchTx ::
  All SingleEraBlock xs =>
  NS GenTx xs ->
  State.HardForkState f xs ->
  Either (MismatchEraInfo xs) (State.HardForkState (Product f GenTx) xs)
matchTx tx (State.HardForkState tele) =
  case Match.matchTelescope tx tele of
    -- Transaction and state are in the same era
    Right m -> Right . State.HardForkState . hmap flipCurrAndProd $ m
    -- Transaction and state are in different eras
    Left mm ->
      case State.sequenceHardForkState $ State.HardForkState $ hcmap proxySingle de tele of
        -- Tx failed to deserialise in the target era
        Left _ ->
          Left
            . MismatchEraInfo
            . Match.bihcmap proxySingle singleEraInfo (LedgerEraInfo . singleEraInfo)
            $ mm
        -- Tx deserialised in the target era
        Right n -> Right n
 where
  flipCurrAndProd :: Product GenTx (State.Current f) a -> State.Current (Product f GenTx) a
  flipCurrAndProd (Pair a (State.Current b c)) = State.Current b (Pair c a)

  ser = hcollapse $ hcmap proxySingle (K . toLazyByteString . toCBOR) tx
  de ::
    SingleEraBlock blk =>
    State.Current f blk -> State.Current (Either DeserialiseFailure :.: Product f GenTx) blk
  de (State.Current s st) = case deserialiseFromBytes fromCBOR ser of
    Left err -> State.Current s (Comp $ Left $ err)
    Right (_, v) -> State.Current s (Comp $ Right $ Pair st v)
