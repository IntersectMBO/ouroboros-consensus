{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the LeiosNotify messages a node sends and receives are logged.
--
-- The fields follow the Leios prototype's, so that tools reading its logs, like
-- the Leios visualizer, read these too.
module Ouroboros.Consensus.Tracing.LeiosNotify
  ( HasLeiosAnnouncement (..)
  ) where

import Cardano.Crypto.Hash (hashToStringAsHex)
import Cardano.Crypto.Leios (LeiosSeatId (..))
import Cardano.Logging
import Data.Aeson (Object, Value (String), (.=))
import qualified Data.List.NonEmpty as NonEmpty
import Data.SOP
import qualified Data.Text as Text
import Network.TypedProtocol.Codec (AnyMessage (..))
import Ouroboros.Consensus.Block (HasHeader, SlotNo (..), blockSlot)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.HardFork.Combinator
  ( HardForkBlock
  , Header (..)
  , OneEraHeader (..)
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..), LeiosProtocol (..))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosPoint (..)
  , LeiosVote (..)
  , RankingBlockHash (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type (LeiosNotify, Message (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (Header (..), ShelleyBlock)
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtocolHeaderSupportsProtocol
  , protocolHeaderView
  )

-- | The endorser block a header announces, read without a block config, which
-- a log formatter has not got.
class HasLeiosAnnouncement blk where
  headerLeiosAnnouncement :: Header blk -> Maybe LeiosAnnouncement

instance HasLeiosAnnouncement ByronBlock where
  headerLeiosAnnouncement _ = Nothing

instance
  (ProtocolHeaderSupportsProtocol proto, LeiosProtocol proto) =>
  HasLeiosAnnouncement (ShelleyBlock proto era)
  where
  headerLeiosAnnouncement =
    getLeiosAnnouncement (Proxy @proto) . protocolHeaderView @proto . shelleyHeaderRaw

instance All HasLeiosAnnouncement xs => HasLeiosAnnouncement (HardForkBlock xs) where
  headerLeiosAnnouncement =
    hcollapse
      . hcmap (Proxy @HasLeiosAnnouncement) (K . headerLeiosAnnouncement)
      . getOneEraHeader
      . getHardForkHeader

instance
  (HasHeader (Header blk), HasLeiosAnnouncement blk) =>
  LogFormatting (AnyMessage (LeiosNotify LeiosPoint (Header blk) LeiosVote))
  where
  forMachine _dtal (AnyMessageAndAgency _stok msg) = case msg of
    MsgLeiosNotificationRequestNext -> kind "MsgLeiosNotificationRequestNext"
    MsgLeiosBlockAnnouncement hdr ->
      mconcat $
        kind "MsgLeiosBlockAnnouncement"
          : case headerLeiosAnnouncement hdr of
            Nothing -> []
            Just MkLeiosAnnouncement{announcementEbHash, announcementEbSize} ->
              [ ebPoint (blockSlot hdr) announcementEbHash
              , "ebBodySize" .= announcementEbSize
              ]
    MsgLeiosBlockOffer (MkLeiosPoint slot ebHash) size ->
      mconcat [kind "MsgLeiosBlockOffer", ebPoint slot ebHash, "ebBytesSize" .= size]
    MsgLeiosBlockTxsOffer (MkLeiosPoint slot ebHash) ->
      mconcat [kind "MsgLeiosBlockTxsOffer", ebPoint slot ebHash]
    MsgLeiosVotes votes ->
      mconcat [kind "MsgLeiosVotes", "votes" .= map vote (NonEmpty.toList votes)]
    MsgDone -> kind "MsgDone"
    MsgQuit -> kind "MsgQuit"
    MsgCanceled -> kind "MsgCanceled"
   where
    kind k = "kind" .= String k

    ebPoint :: SlotNo -> EndorserBlockHash -> Object
    ebPoint (SlotNo slot) ebHash =
      mconcat ["ebSlot" .= slot, "ebHash" .= String (Text.pack (hashToStringAsHex ebHash))]

    vote :: LeiosVote -> Object
    vote MkLeiosVote{voteAnnouncingRbHash, voteSeatId} =
      mconcat
        [ "rbHash" .= String (Text.pack (hashToStringAsHex (unRankingBlockHash voteAnnouncingRbHash)))
        , "voterId" .= leiosSeatIndex voteSeatId
        ]

instance MetaTrace (AnyMessage (LeiosNotify LeiosPoint (Header blk) LeiosVote)) where
  namespaceFor (AnyMessageAndAgency _stok msg) = case msg of
    MsgLeiosNotificationRequestNext -> Namespace [] ["RequestNext"]
    MsgLeiosBlockAnnouncement{} -> Namespace [] ["BlockAnnouncement"]
    MsgLeiosBlockOffer{} -> Namespace [] ["BlockOffer"]
    MsgLeiosBlockTxsOffer{} -> Namespace [] ["BlockTxsOffer"]
    MsgLeiosVotes{} -> Namespace [] ["Votes"]
    MsgDone -> Namespace [] ["Done"]
    MsgQuit -> Namespace [] ["Quit"]
    MsgCanceled -> Namespace [] ["Canceled"]

  severityFor _ _ = Just Debug

  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["RequestNext"]
    , Namespace [] ["BlockAnnouncement"]
    , Namespace [] ["BlockOffer"]
    , Namespace [] ["BlockTxsOffer"]
    , Namespace [] ["Votes"]
    , Namespace [] ["Done"]
    , Namespace [] ["Quit"]
    , Namespace [] ["Canceled"]
    ]
