{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ <= 906
{-# OPTIONS_GHC -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}
#endif

module Ouroboros.Consensus.Cardano.QueryHF () where

import           Data.SOP.Index
import           Data.SOP.Strict
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Ledger
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.TypeFamilyWrappers

instance CardanoHardForkConstraints c => BlockSupportsHFLedgerQuery (CardanoEras c) where
  answerBlockQueryHFLookup IZ _cfg (q :: BlockQuery ByronBlock QFLookupTables result) _dlv =
    case q of {}
  answerBlockQueryHFLookup idx@(IS IZ) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup idx@(IS (IS IZ)) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup idx@(IS (IS (IS IZ))) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup idx@(IS (IS (IS (IS IZ)))) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup idx@(IS (IS (IS (IS (IS IZ))))) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup idx@(IS (IS (IS (IS (IS (IS IZ)))))) cfg q dlv =
    answerShelleyLookupQueries idx cfg q dlv
  answerBlockQueryHFLookup (IS (IS (IS (IS (IS (IS (IS idx))))))) _cfg _q _dlv =
    case idx of {}

  answerBlockQueryHFTraverse IZ _cfg (q :: BlockQuery ByronBlock QFTraverseTables result) _dlv =
    case q of {}
  answerBlockQueryHFTraverse idx@(IS IZ) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse idx@(IS (IS IZ)) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse idx@(IS (IS (IS IZ))) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse idx@(IS (IS (IS (IS IZ)))) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse idx@(IS (IS (IS (IS (IS IZ))))) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse idx@(IS (IS (IS (IS (IS (IS IZ)))))) cfg q dlv =
    answerShelleyTraversingQueries idx cfg q dlv
  answerBlockQueryHFTraverse (IS (IS (IS (IS (IS (IS (IS idx))))))) _cfg _q _dlv =
    case idx of {}

  -- TODO: remove the indrection through the NS
  queryLedgerGetTraversingFilter IZ (q :: BlockQuery ByronBlock QFTraverseTables result) = case q of {}
  queryLedgerGetTraversingFilter idx@(IS IZ) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter idx@(IS (IS IZ)) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter idx@(IS (IS (IS IZ))) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter idx@(IS (IS (IS (IS IZ)))) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter idx@(IS (IS (IS (IS (IS IZ))))) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter idx@(IS (IS (IS (IS (IS (IS IZ)))))) q = case q of
    GetUTxOByAddress addrs -> \txout -> case toNS txout of
      S (Z (WrapTxOut x)) -> filterGetUTxOByAddressOne addrs x
      S (S (Z (WrapTxOut x))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (Z (WrapTxOut x)))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (Z (WrapTxOut x))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (Z (WrapTxOut x)))))) -> filterGetUTxOByAddressOne addrs x
      S (S (S (S (S (S (Z (WrapTxOut x))))))) -> filterGetUTxOByAddressOne addrs x
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter (IS (IS (IS (IS (IS (IS (IS idx))))))) _ = case idx of {}
