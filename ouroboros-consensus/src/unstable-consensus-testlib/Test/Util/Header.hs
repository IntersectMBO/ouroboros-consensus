{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Util.Header
  ( -- * Enriching headers with a relative slot time
    attachSlotTime
  , attachSlotTimeToFragment
  , dropTimeFromFragment
  ) where

import Cardano.Slotting.EpochInfo.API (epochInfoSlotToRelativeTime)
import Data.Functor.Identity (runIdentity)
import Data.Maybe.Strict (maybeToStrictMaybe)
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , BlockSupportsProtocol
  , Header
  , SlotNo
  , WithOrigin (..)
  , blockSlot
  )
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( ImmutableEraParams
  , immutableEpochInfo
  )
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.Leios.Protocol (LeiosProtocol (getLeiosLedgerView))
import Ouroboros.Consensus.Protocol.Abstract (LedgerView)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Enriching headers with a relative slot time
-------------------------------------------------------------------------------}

dropTimeFromFragment ::
  AF.HasHeader (Header blk) =>
  AnchoredFragment (HeaderWithTime blk) ->
  AnchoredFragment (Header blk)
dropTimeFromFragment = AF.mapAnchoredFragment hwtHeader

attachSlotTimeToFragment ::
  ( ImmutableEraParams blk
  , BlockSupportsProtocol blk
  , LedgerView (BlockProtocol blk) ~ ()
  ) =>
  TopLevelConfig blk ->
  AnchoredFragment (Header blk) ->
  AnchoredFragment (HeaderWithTime blk)
attachSlotTimeToFragment cfg frag =
  AF.fromOldestFirst (AF.castAnchor (AF.anchor frag)) $
    zipWith (attachSlotTime cfg) predecessorSlots hdrs
 where
  hdrs = AF.toOldestFirst frag

  predecessorSlots =
    AF.anchorToSlotNo (AF.anchor frag) : map (NotOrigin . blockSlot) hdrs

-- | For a block whose ledger view is trivial, and so is the same at every slot
attachSlotTime ::
  forall blk.
  ( ImmutableEraParams blk
  , BlockSupportsProtocol blk
  , LedgerView (BlockProtocol blk) ~ ()
  ) =>
  TopLevelConfig blk ->
  -- | The slot of the header's predecessor
  WithOrigin SlotNo ->
  Header blk ->
  HeaderWithTime blk
attachSlotTime cfg predecessorSlot hdr =
  HeaderWithTime
    { hwtHeader = hdr
    , hwtSlotRelativeTime =
        runIdentity $ epochInfoSlotToRelativeTime ei (blockSlot hdr)
    , hwtPredecessorSlot = predecessorSlot
    , hwtLeiosLedgerView =
        maybeToStrictMaybe $ getLeiosLedgerView (Proxy @(BlockProtocol blk)) ()
    }
 where
  ei = immutableEpochInfo cfg
