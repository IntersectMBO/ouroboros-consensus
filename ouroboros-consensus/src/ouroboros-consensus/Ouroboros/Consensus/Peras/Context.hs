{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Context
  ( -- * Bounded Peras epoch context
    BoundedPerasEpochContext (..)
  , withinEpochContext

    -- * Peras epoch context resolver and handle
  , PerasEpochContextResolver (..)
  , PerasEpochContextNotFoundForRound (..)
  , unsafeResolveRoundNo
  , perasEpochContextResolverBounds
  , PerasEpochContextResolverHandle (..)
  , mockPerasEpochContextResolverHandle
  , withResolvedRoundNo
  , verifyPerasVoteWithHandle
  , verifyPerasCertWithHandle
  , forgePerasVoteIfEligibleWithHandle

    -- * Extracting and resolving Peras epoch contexts from the node state
  , StateSupportsPerasEpochContext (..)
  , dummyPoolDistr
  , mkBoundedPerasEpochContextWith
  , initPerasEpochContextResolverFromUntickedState
  , tickPerasEpochContextResolver

    -- * Time resolution
  , TimeResolutionContext (..)
  , runQueryWithContext
  , runQueryEraIndexedWithContext
  , TimeResolutionContextHandle (..)
  , runQueryWithContextHandle

    -- * Next-epoch detection
  , EpochCrossing (..)
  , DetectNextEpochError
  , isNextEpoch
  )
where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLen
  , decodeListLenOf
  , encodeListLen
  )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError, knownNonZeroCoin)
import Cardano.Ledger.Keys (KeyHash (..), toVRFVerKeyHash)
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Prelude (maybeToEither)
import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Char8 as ByteString
import Data.Data (Proxy (..))
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.SOP (HCollapse (..), K (..))
import Data.SOP.Constraint (All, Top)
import Data.SOP.Index (himap, injectNS)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block.Abstract
  ( EpochNo (..)
  , Point
  , SlotNo
  , WithOrigin (..)
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , IsPerasError (..)
  , IsPerasVote (..)
  , PerasCert
  , PerasEpochContext (..)
  , PerasRoundNo
  , PerasVote
  , PerasVotingCommittee
  , PerasVotingCommitteeInput
  , ValidatedPerasCert
  , ValidatedPerasVote
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PrivateKey)
import Ouroboros.Consensus.Committee.Types (PoolId, TargetCommitteeSize)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import Ouroboros.Consensus.HardFork.History.Qry
  ( EpochToPerasRoundInfo (..)
  , EraIndexed (..)
  , PastHorizonException
  , Qry
  , epochToPerasRoundInfo
  , runQuery
  , runQueryEraIndexed
  , slotToEpoch'
  )
import Ouroboros.Consensus.HeaderValidation
  ( HeaderState
  , Ticked
  , annTipSlotNo
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, LedgerConfig, LedgerState)
import Ouroboros.Consensus.Peras.Params
  ( PerasEnabled
  , PerasParams
  , perasEnabledToMaybe
  , perasTargetCommitteeSize
  , pattern NoPerasEnabled
  , pattern PerasEnabled
  )
import Ouroboros.Consensus.Storage.Serialisation
  ( DecodeDisk (..)
  , EncodeDisk (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( MonadSTM
  , MonadThrow
  , NoThunks (..)
  , throwSTM
  )

-- * Bounded Peras epoch context

-- | A 'PerasEpochContext' that is valid only in a given range of round numbers
data BoundedPerasEpochContext blk
  = BoundedPerasEpochContext
  { startPerasRoundNo :: !PerasRoundNo
  -- ^ Inclusive lower bound
  , endPerasRoundNo :: !PerasRoundNo
  -- ^ Exclusive upper bound
  , epochContext :: !(PerasEpochContext blk)
  -- ^ Epcoh context that is valid within the given bounds
  }

deriving instance
  Show (PerasEpochContext blk) =>
  Show (BoundedPerasEpochContext blk)
deriving instance
  Eq (PerasEpochContext blk) =>
  Eq (BoundedPerasEpochContext blk)
deriving instance
  NoThunks (PerasEpochContext blk) =>
  NoThunks (BoundedPerasEpochContext blk)
deriving instance
  Generic (BoundedPerasEpochContext blk)

instance
  ( Typeable blk
  , FromCBOR (PerasVotingCommittee blk)
  ) =>
  FromCBOR (BoundedPerasEpochContext blk)
  where
  fromCBOR = do
    decodeListLenOf 3
    startPerasRoundNo <- fromCBOR
    endPerasRoundNo <- fromCBOR
    epochContext <- fromCBOR
    pure
      BoundedPerasEpochContext
        { startPerasRoundNo
        , endPerasRoundNo
        , epochContext
        }

instance
  ( Typeable blk
  , ToCBOR (PerasVotingCommittee blk)
  ) =>
  ToCBOR (BoundedPerasEpochContext blk)
  where
  toCBOR
    BoundedPerasEpochContext
      { startPerasRoundNo
      , endPerasRoundNo
      , epochContext
      } =
      encodeListLen 3
        <> toCBOR startPerasRoundNo
        <> toCBOR endPerasRoundNo
        <> toCBOR epochContext

instance
  FromCBOR (BoundedPerasEpochContext blk) =>
  DecodeDisk blk (BoundedPerasEpochContext blk)
  where
  decodeDisk _ccfg = fromCBOR

instance
  ToCBOR (BoundedPerasEpochContext blk) =>
  EncodeDisk blk (BoundedPerasEpochContext blk)
  where
  encodeDisk _ccfg = toCBOR

-- | Check whether a given 'PerasRoundNo' is within the bounds of a
-- 'BoundedPerasEpochContext'.
--
-- Returns the corresponding 'PerasEpochContext' if the round number is within
-- the bounds, or 'Nothing' otherwise.
withinEpochContext ::
  PerasRoundNo ->
  BoundedPerasEpochContext blk ->
  Maybe (PerasEpochContext blk)
withinEpochContext roundNo boundedContext
  | roundNo >= startPerasRoundNo boundedContext
      && roundNo < endPerasRoundNo boundedContext =
      Just (epochContext boundedContext)
  | otherwise =
      Nothing

-- * Peras epoch context resolver (and handle)

-- | A two-epoch window of Peras epoch contexts, which can be used to resolve
-- round numbers into their corresponding Peras contexts.
data PerasEpochContextResolver blk
  = -- | The resolver is in an error state, and cannot resolve any round number.
    --
    -- NOTE: this exists to allow for recoverable errors during resolver
    -- initialisation or ticking caused, e.g., by incorrect parameterization.
    PerasEpochContextResolverError
      !String
  | -- | The resolver has a two-epoch window of Peras epoch contexts, which may
    -- be empty if Peras is not enabled in either of these epochs.
    PerasEpochContextResolver
      -- | Current epoch context
      !(PerasEnabled (BoundedPerasEpochContext blk))
      -- | Previous epoch context
      !(PerasEnabled (BoundedPerasEpochContext blk))

deriving instance
  Show (PerasEpochContext blk) =>
  Show (PerasEpochContextResolver blk)
deriving instance
  Eq (PerasEpochContext blk) =>
  Eq (PerasEpochContextResolver blk)
deriving instance
  NoThunks (PerasEpochContext blk) =>
  NoThunks (PerasEpochContextResolver blk)
deriving instance
  Generic (PerasEpochContextResolver blk)

instance
  ( Typeable blk
  , FromCBOR (PerasVotingCommittee blk)
  ) =>
  FromCBOR (PerasEpochContextResolver blk)
  where
  fromCBOR = do
    len <- decodeListLen
    tag <- fromCBOR @Word8
    case (len, tag) of
      (2, 0) -> do
        reason <- ByteString.unpack <$> fromCBOR
        pure $ PerasEpochContextResolverError reason
      (3, 1) -> do
        curr <- fromCBOR
        prev <- fromCBOR
        pure $ PerasEpochContextResolver curr prev
      _ ->
        fail $
          "PerasEpochContextResolver: unexpected list length and tag: "
            <> show (len, tag)

instance
  ( Typeable blk
  , ToCBOR (PerasVotingCommittee blk)
  ) =>
  ToCBOR (PerasEpochContextResolver blk)
  where
  toCBOR = \case
    PerasEpochContextResolverError reason ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR (ByteString.pack reason)
    PerasEpochContextResolver curr prev ->
      encodeListLen 3
        <> toCBOR (1 :: Word8)
        <> toCBOR curr
        <> toCBOR prev

instance
  FromCBOR (PerasEpochContextResolver blk) =>
  DecodeDisk blk (PerasEpochContextResolver blk)
  where
  decodeDisk _ccfg = fromCBOR

instance
  ToCBOR (PerasEpochContextResolver blk) =>
  EncodeDisk blk (PerasEpochContextResolver blk)
  where
  encodeDisk _ccfg = toCBOR

-- | An error indicating that a 'PerasEpochContext' could not be found for a
-- given 'PerasRoundNo' in a 'PerasEpochContextResolver'.
data PerasEpochContextNotFoundForRound
  = PerasEpochContextNotFoundForRound
      -- | The round number for which the context could not be found.
      !PerasRoundNo
      -- | Detailed reason for the failure.
      !String
  deriving (Eq, Show, Generic, NoThunks, Exception)

-- | Initialise a 'PerasEpochContextResolver' using a bounded context.
--
-- NOTE: regarding why this function uses a hardcoded 'NoPerasEnabled' for the
-- epoch prior to the one being used to initialize this bounded context: please
-- notice that this function is only used in cases where we don't have
-- information about the previous epoch:
--
--  * When creating a new context resolver from scratch, where we don't have any
--    information about Peras for the previous epoch (there might not even be a
--    previous epoch if we are boostrapping from Genesis).
--
--  * When ticking the resolver out of an error state. This could occur due to
--    bad parameterization leading to an error while creating a voting committee
--    for a given epoch.
--
--  * When ticking the resolver more than one epoch at a time, in which case we
--    cannot faitfully build a context for the previous (gap) epoch. This only
--    happens in tests, though.
--
-- In all these cases, using 'NoPerasEnabled' for the previous epoch is the only
-- safe option, although in the future we might want to make each of the cases
-- mentioned above more explicitly representable.
newPerasEpochContextResolver ::
  PerasEnabled (BoundedPerasEpochContext blk) ->
  PerasEpochContextResolver blk
newPerasEpochContextResolver currEpochContext =
  PerasEpochContextResolver
    currEpochContext
    NoPerasEnabled

-- | Advance a 'PerasEpochContextResolver' to the next epoch, given a new
-- bounded context for the next epoch.
--
-- NOTE: this will recover from an error state, but it will leave the previous
-- epoch context disabled.
advancePerasEpochContextResolver ::
  PerasEpochContextResolver blk ->
  PerasEnabled (BoundedPerasEpochContext blk) ->
  PerasEpochContextResolver blk
advancePerasEpochContextResolver resolver newEpochContext =
  case resolver of
    -- The previous resolver is in a valid state => slide the window forward
    PerasEpochContextResolver currEpochContext _prevEpochContext ->
      PerasEpochContextResolver
        newEpochContext
        currEpochContext
    -- The previous resolver is in an error state => re-initialise the window
    PerasEpochContextResolverError{} ->
      newPerasEpochContextResolver
        newEpochContext

-- | Resolve a 'PerasRoundNo' into its corresponding 'PerasEpochContext' using a
-- 'PerasEpochContextResolver'.
--
-- Fails with 'PerasEpochContextNotFoundForRound' if the round number is not
-- within the bounds of either the current or previous epoch context, or if the
-- resolver is in an error state.
--
-- UNSAFE: this resolves against the resolver as-is. A 'PerasEpochContextResolver'
-- only covers a two-epoch window around the slot it was last ticked to, so it
-- must already have been ticked to (around) the round's slot; otherwise
-- resolution may spuriously fail or return a stale context. Prefer
-- 'Ouroboros.Consensus.Ledger.Extended.tickAndResolveRoundNo',
-- which ticks before resolving.
unsafeResolveRoundNo ::
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
unsafeResolveRoundNo resolver roundNo = case resolver of
  PerasEpochContextResolverError reason ->
    Left $
      PerasEpochContextNotFoundForRound roundNo reason
  PerasEpochContextResolver curr prev ->
    case (lookupBounded "current" curr, lookupBounded "previous" prev) of
      -- The round number is within the bounds of the current epoch context
      (Right context, _) ->
        Right context
      -- The round number is within the bounds of the previous epoch context
      (_, Right context) ->
        Right context
      -- The round number is not within the bounds of either epoch context
      (Left reason1, Left reason2) ->
        Left $
          PerasEpochContextNotFoundForRound roundNo (reason1 <> "; " <> reason2)
 where
  lookupBounded name mbContext = do
    boundedContext <-
      maybeToEither
        ("no " <> name <> " epoch context available because Peras isn't enabled")
        (perasEnabledToMaybe mbContext)
    maybeToEither
      ( name
          <> " epoch context available, but roundNo "
          <> show roundNo
          <> " not within "
          <> name
          <> " epoch context bounds ["
          <> show (startPerasRoundNo boundedContext)
          <> ", "
          <> show (endPerasRoundNo boundedContext)
          <> ")"
      )
      (withinEpochContext roundNo boundedContext)

-- | Compute the bounds of the Peras round numbers that are covered by the
-- given 'PerasEpochContextResolver'.
--
-- NOTE: the upper bound is exclusive, thus we return an empty range [0,0) when
-- the resolver doesn't cover any Peras round.
perasEpochContextResolverBounds ::
  PerasEpochContextResolver blk ->
  (PerasRoundNo, PerasRoundNo)
perasEpochContextResolverBounds = \case
  PerasEpochContextResolverError _ ->
    ( 0
    , 0
    )
  PerasEpochContextResolver NoPerasEnabled NoPerasEnabled ->
    ( 0
    , 0
    )
  PerasEpochContextResolver (PerasEnabled curr) NoPerasEnabled ->
    ( startPerasRoundNo curr
    , endPerasRoundNo curr
    )
  PerasEpochContextResolver NoPerasEnabled (PerasEnabled prev) ->
    ( startPerasRoundNo prev
    , endPerasRoundNo prev
    )
  PerasEpochContextResolver (PerasEnabled curr) (PerasEnabled prev) ->
    ( min (startPerasRoundNo curr) (startPerasRoundNo prev)
    , max (endPerasRoundNo curr) (endPerasRoundNo prev)
    )

-- | A handle that resolves a 'PerasRoundNo' into its corresponding
-- 'PerasEpochContext' (or a 'PerasEpochContextNotFoundForRound' error) in 'STM'.
newtype PerasEpochContextResolverHandle m blk
  = PerasEpochContextResolverHandle
  { resolveRoundNo ::
      PerasRoundNo ->
      STM m (Either PerasEpochContextNotFoundForRound (PerasEpochContext blk))
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "PerasEpochContextResolverHandle" (PerasEpochContextResolverHandle m blk)

-- | A mocked 'PerasEpochContextResolverHandle' that always succeeds by
-- resolving every round number to a fixed given (fixed) 'PerasEpochContext'.
mockPerasEpochContextResolverHandle ::
  MonadSTM m =>
  PerasEpochContext blk ->
  PerasEpochContextResolverHandle m blk
mockPerasEpochContextResolverHandle context =
  PerasEpochContextResolverHandle $ \_ -> pure (Right context)

-- | Helper to resolve the epoch context for a given round a pass it to a
-- continuation for further processing.
--
-- NOTE: this function will throw an STM exception if round number cannot be
-- resolved, or if the continuation fails.
withResolvedRoundNo ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , Exception err
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasRoundNo ->
  (PerasEpochContext blk -> Either err a) ->
  STM m a
withResolvedRoundNo PerasEpochContextResolverHandle{resolveRoundNo} roundNo k =
  resolveRoundNo roundNo >>= \case
    Left err -> throwSTM err
    Right context ->
      case k context of
        Left err -> throwSTM err
        Right a -> pure a

-- | Like 'verifyPerasVote', but using a 'PerasEpochContextResolverHandle' to
-- resolve the epoch context for the round number of the given vote.
verifyPerasVoteWithHandle ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasVote blk ->
  STM m (ValidatedPerasVote blk)
verifyPerasVoteWithHandle handle vote =
  withResolvedRoundNo handle (getPerasVoteRound vote) $ \context ->
    verifyPerasVote context vote

-- | Like 'verifyPerasCert', but using a 'PerasEpochContextResolverHandle' to
-- resolve the epoch context for the round number of the given certificate.
verifyPerasCertWithHandle ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasCert blk ->
  STM m (ValidatedPerasCert blk)
verifyPerasCertWithHandle handle cert =
  withResolvedRoundNo handle (getPerasCertRound cert) $ \context ->
    verifyPerasCert context cert

-- | Like 'forgePerasVoteIfEligible', but using a
-- 'PerasEpochContextResolverHandle' to resolve the epoch context for the given
-- round number.
forgePerasVoteIfEligibleWithHandle ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PoolId ->
  PrivateKey (PerasCrypto blk) ->
  PerasRoundNo ->
  Point blk ->
  STM m (Maybe (ValidatedPerasVote blk))
forgePerasVoteIfEligibleWithHandle handle poolId privateKey roundNo point =
  withResolvedRoundNo handle roundNo $ \context ->
    forgePerasVoteIfEligible context poolId privateKey roundNo point

-- * Extracting and resolving Peras epoch contexts from the node state

-- | Type-class for blocks that support constructing a Peras epoch contexts from
-- their corresponding ledger and chain-dep states.
class
  ( HasHardForkHistory blk
  , IsPerasError (PerasError blk) blk
  , Show (PerasError blk)
  , Show (PerasVotingCommittee blk)
  , Eq (PerasVotingCommittee blk)
  , NoThunks (PerasVotingCommittee blk)
  , Typeable (PerasVotingCommittee blk)
  , FromCBOR (PerasVotingCommittee blk)
  , ToCBOR (PerasVotingCommittee blk)
  , Show (PerasEpochContextResolver blk)
  , Eq (PerasEpochContextResolver blk)
  , NoThunks (PerasEpochContextResolver blk)
  , Typeable (PerasEpochContextResolver blk)
  , FromCBOR (PerasEpochContextResolver blk)
  , ToCBOR (PerasEpochContextResolver blk)
  , EncodeDisk blk (PerasEpochContextResolver blk)
  , DecodeDisk blk (PerasEpochContextResolver blk)
  ) =>
  StateSupportsPerasEpochContext blk
  where
  -- | Epoch-dependent information needed to resolve a 'PerasRoundNo' into its
  -- corresponding 'PerasEpochContext'. In practice, this is always an
  -- 'EpochToPerasRoundInfo', with the exception of the 'HardForkBlock', which
  -- additionally takes advantage of the era index returned by
  -- 'runQueryEraIndexed' to be able to dispatch to the correct era-specific
  -- implementation.
  type MaybeEraIndexedEpochToPerasRoundInfo blk :: Type

  -- | Extract a 'EpochToPerasRoundInfo' the opaque
  -- 'MaybeEraIndexedEpochToPerasRoundInfo' of this block.
  fromMaybeEraIndexedEpochToPerasRoundInfo ::
    proxy blk ->
    MaybeEraIndexedEpochToPerasRoundInfo blk ->
    EpochToPerasRoundInfo

  -- | Inject an era-indexed 'EpochToPerasRoundInfo' into an opaque
  -- 'MaybeEraIndexedEpochToPerasRoundInfo' of this block.
  toMaybeEraIndexedEpochToPerasRoundInfo ::
    All Top (HardForkIndices blk) =>
    proxy blk ->
    EraIndexed (HardForkIndices blk) EpochToPerasRoundInfo ->
    MaybeEraIndexedEpochToPerasRoundInfo blk

  -- | Extract the stake distribution from an unticked ledger state.
  getPerasPoolDistrFromLedger ::
    LedgerState blk EmptyMK ->
    PoolDistr

  -- | Extract the stake distribution from a ticked ledger state.
  getPerasPoolDistrFromTickedLedger ::
    Ticked LedgerState blk EmptyMK ->
    PoolDistr

  -- | Extract the epoch nonce from an unticked header state.
  getPerasNonceFromHeaderState ::
    HeaderState blk ->
    Nonce

  -- | Extract the epoch nonce from a ticked header state.
  getPerasNonceFromTickedHeaderState ::
    Ticked (HeaderState blk) ->
    Nonce

  -- | Derive parameters from an unticked concrete ledger state.
  mkPerasParamsFromLedger ::
    LedgerConfig blk ->
    LedgerState blk EmptyMK ->
    Either String (PerasParams blk)

  -- | Derive parameters from a ticked concrete ledger state.
  mkPerasParamsFromTickedLedger ::
    LedgerConfig blk ->
    Ticked LedgerState blk EmptyMK ->
    Either String (PerasParams blk)

  -- | Create a bounded epoch context from a given epoch-to-round info.
  mkBoundedPerasEpochContext ::
    MaybeEraIndexedEpochToPerasRoundInfo blk ->
    PerasParams blk ->
    PoolDistr ->
    Nonce ->
    Either
      (PerasError blk)
      (BoundedPerasEpochContext blk)

-- | A single-pool distribution for blocks that do not support Peras.
--
-- NOTE: this contains a single pool with a stake of 1 to avoid division by zero
-- should this value ever be used in a test context.
dummyPoolDistr :: PoolDistr
dummyPoolDistr =
  PoolDistr
    { unPoolDistr = Map.singleton dummyPoolId dummyPoolStake
    , pdTotalActiveStake = knownNonZeroCoin @1
    }
 where
  dummyPoolId =
    KeyHash
      . Hash.castHash
      . Hash.hashWith id
      . BS8.pack
      $ "peras-mock-pool"
  dummyPoolVrf =
    toVRFVerKeyHash
      . Hash.castHash
      . Hash.hashWith id
      . BS8.pack
      $ "peras-mock-pool-vrf"
  dummyPoolStake =
    IndividualPoolStake
      { individualPoolStake = 1
      , individualTotalPoolStake = compactCoinOrError (Coin 1)
      , individualPoolStakeVrf = dummyPoolVrf
      , individualPoolStakeBls = SNothing
      }

-- | Helper to build a 'BoundedPerasEpochContext' using a function that produces
-- a 'PerasVotingCommitteeInput' from the specific values it needs.
--
-- NOTE: this is useful to define instances for 'StateSupportsPerasEpochContext'
-- where 'mkBoundedPerasEpochContext' is instantiated to either:
--   * @mkBoundedPerasEpochContextWith mkMockPerasVotingCommitteeInput@ for test
--     types with limited Peras support, and
--   * @mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput@ for
--     production types.
mkBoundedPerasEpochContextWith ::
  ( CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , MaybeEraIndexedEpochToPerasRoundInfo blk ~ EpochToPerasRoundInfo
  , IsPerasError (PerasError blk) blk
  ) =>
  ( PoolDistr ->
    Nonce ->
    TargetCommitteeSize ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  ) ->
  MaybeEraIndexedEpochToPerasRoundInfo blk ->
  PerasParams blk ->
  PoolDistr ->
  Nonce ->
  Either
    (PerasError blk)
    (BoundedPerasEpochContext blk)
mkBoundedPerasEpochContextWith
  mkPerasVotingCommitteeInput
  epochToRoundInfo
  params
  poolDistr
  epochNonce = do
    committeeInput <-
      mkPerasVotingCommitteeInput
        poolDistr
        epochNonce
        (perasTargetCommitteeSize params)
    committee <-
      bimap injectVotingCommitteeError id $
        Committee.mkVotingCommittee committeeInput
    pure
      BoundedPerasEpochContext
        { startPerasRoundNo =
            etpriEpochStartPerasRound epochToRoundInfo
        , endPerasRoundNo =
            etpriEpochEndPerasRound epochToRoundInfo
        , epochContext =
            PerasEpochContext
              { pecParams =
                  params
              , pecCommittee =
                  committee
              }
        }

-- | Initialize a 'PerasEpochContextResolver' from a ledger and header state.
--
-- NOTES:
--   1. We may later decide that a 'PerasEpochContextResolver' is always
--      initiated with empty/error value, and rely on the first ticking to
--      properly initialize it. In the current architecture, however, that would
--      work only if the first ticking happens when the previous slot is either
--      'Origin', or a slot from a previous epoch compared to the target slot.
--   2. Given that we have no assumption that (1) would work, we made a
--      polymorphic system where a 'BoundedEpochContext' can be created from
--      either a ticked or unticked ledger+header state
--      (see 'StateSupportsPerasEpochContext'
--      helper classes). This way, a 'PerasEpochContextResolver' can be
--      initialized from unticked 'LedgerState' and 'HeaderState', but then
--      ticked by using the 'Ticked LedgerState' and 'Ticked HeaderState'.
--   3. If in the future we move on to a system where the resolver is always
--      initialized with empty/error value, we can remove the polymorphic system
--      and only create a 'BoundedPerasEpochContext' from 'Ticked LedgerState'
--      and 'Ticked HeaderState'.
initPerasEpochContextResolverFromUntickedState ::
  forall blk.
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  ) =>
  LedgerConfig blk ->
  LedgerState blk EmptyMK ->
  HeaderState blk ->
  PerasEpochContextResolver blk
initPerasEpochContextResolverFromUntickedState
  ledgerConfig
  ledgerState
  headerState =
    case chainTipSlot of
      Origin ->
        newPerasEpochContextResolver NoPerasEnabled
      NotOrigin slotNo ->
        case resolveEpochToRoundInfo slotNo of
          Left err ->
            PerasEpochContextResolverError (show err)
          Right eraIndexedEpochToPerasRoundInfo ->
            createBoundedEpochContextResolverFromUntickedState
              newPerasEpochContextResolver
              eraIndexedEpochToPerasRoundInfo
              ledgerConfig
              ledgerState
              headerState
   where
    chainTipSlot =
      fmap annTipSlotNo (headerStateTip headerState)
    timeResolutionContext =
      TimeResolutionContext ledgerConfig ledgerState
    resolveEpochToRoundInfo slotNo = do
      (epochNo, _) <-
        runQueryWithContext
          timeResolutionContext
          (slotToEpoch' slotNo)
      runQueryEraIndexedWithContext
        timeResolutionContext
        (epochToPerasRoundInfo epochNo)

-- | Tick a 'PerasEpochContextResolver' to a target 'SlotNo'.
--
-- NOTES:
--   1. To avoid circular dependencies, this function uses a deconstructed
--      'ExtLedgerState' instead of the 'ExtLedgerState' itself.
--   2. It doesn't seem to bring much to differentiate a
--      'PerasEpochContextResolver' from a ticked one at type level, since they
--      need to carry exactly the same information. We tried, and it didn't
--      improve readability.
tickPerasEpochContextResolver ::
  forall blk.
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  ) =>
  LedgerConfig blk ->
  -- | The fields needed from the previous 'ExtLedgerState' (before ticking)
  (PerasEpochContextResolver blk, LedgerState blk EmptyMK, HeaderState blk) ->
  -- | Target 'SlotNo' and fields of the 'Ticked ExtLedgerState' ticked to it
  (SlotNo, Ticked LedgerState blk EmptyMK, Ticked (HeaderState blk)) ->
  PerasEpochContextResolver blk
tickPerasEpochContextResolver
  ledgerConfig
  (perasEpochContextResolver, ledgerState, headerState)
  (targetSlot, tickedLedger, tickedHeader) =
    case ( isNextEpoch
             timeResolutionContext
             chainTipSlot
             targetSlot
         ) of
      Left err ->
        error (show err)
      Right SameEpoch ->
        -- No epoch boundary was crossed: keep the current resolver as is.
        perasEpochContextResolver
      Right (NextEpoch eraIndexedEpochToPerasRoundInfo) ->
        -- Exactly one epoch boundary was crossed: advance the two-epoch window
        -- incrementally (the current context becomes the previous one).
        createBoundedEpochContextResolverFromTickedState
          (advancePerasEpochContextResolver perasEpochContextResolver)
          eraIndexedEpochToPerasRoundInfo
          ledgerConfig
          tickedLedger
          tickedHeader
      Right (ManyEpochsCrossed eraIndexedEpochToPerasRoundInfo) ->
        -- More than one epoch boundary was crossed at once, which is only
        -- possible when the chain skips one or more entire epochs (e.g. a
        -- sparse/empty chain, or a node ticking its ledger far ahead of its
        -- tip). The two-epoch window cannot be advanced incrementally
        -- across the gap, so we re-initialise it at the target epoch.
        --
        -- NOTE: this should be an impossible case in production, as we
        -- expect at least one block per epoch. However, we still need to
        -- handle this case in a way that doesn't break the more lenient
        -- test suites, where we might tick the ledger far ahead of its tip.
        -- See: https://github.com/tweag/cardano-peras/issues/260
        createBoundedEpochContextResolverFromTickedState
          newPerasEpochContextResolver
          eraIndexedEpochToPerasRoundInfo
          ledgerConfig
          tickedLedger
          tickedHeader
   where
    chainTipSlot =
      fmap annTipSlotNo (headerStateTip headerState)
    timeResolutionContext =
      TimeResolutionContext ledgerConfig ledgerState

-- | Build a 'PerasEpochContextResolver' from unticked ledger and header states.
--
-- NOTE: the embedding function allows us to share the same logic depending on
-- whether we are creating a fresh epoch context resolver or advancing an
-- existing one.
createBoundedEpochContextResolverFromUntickedState ::
  forall blk.
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  ) =>
  -- | How to embed the (optional) fresh bounded epoch context into a resolver.
  (PerasEnabled (BoundedPerasEpochContext blk) -> PerasEpochContextResolver blk) ->
  EraIndexed (HardForkIndices blk) (PerasEnabled EpochToPerasRoundInfo) ->
  LedgerConfig blk ->
  LedgerState blk EmptyMK ->
  HeaderState blk ->
  PerasEpochContextResolver blk
createBoundedEpochContextResolverFromUntickedState
  embedF
  eraIndexedEpochToRoundInfo
  ledgerConfig
  untickedLedgerState
  untickedHeaderState =
    case collapseEraIndexed eraIndexedEpochToRoundInfo of
      PerasEnabled eiEpochToPerasRoundInfo ->
        case mkPerasParamsFromLedger ledgerConfig untickedLedgerState of
          Left err ->
            PerasEpochContextResolverError err
          Right params ->
            createBoundedEpochCOntextResolver
              embedF
              eiEpochToPerasRoundInfo
              params
              (getPerasPoolDistrFromLedger untickedLedgerState)
              (getPerasNonceFromHeaderState untickedHeaderState)
      NoPerasEnabled ->
        embedF NoPerasEnabled

-- | Build a 'PerasEpochContextResolver' from ticked ledger and header states.
--
-- NOTE: the embedding function allows us to share the same logic depending on
-- whether we are creating a fresh epoch context resolver or advancing an
-- existing one.
createBoundedEpochContextResolverFromTickedState ::
  forall blk.
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  ) =>
  -- | How to embed the (optional) fresh bounded epoch context into a resolver.
  (PerasEnabled (BoundedPerasEpochContext blk) -> PerasEpochContextResolver blk) ->
  EraIndexed (HardForkIndices blk) (PerasEnabled EpochToPerasRoundInfo) ->
  LedgerConfig blk ->
  Ticked LedgerState blk EmptyMK ->
  Ticked (HeaderState blk) ->
  PerasEpochContextResolver blk
createBoundedEpochContextResolverFromTickedState
  embedF
  eraIndexedEpochToRoundInfo
  ledgerConfig
  tickedLedgerState
  tickedHeaderState =
    case collapseEraIndexed eraIndexedEpochToRoundInfo of
      PerasEnabled eiEpochToPerasRoundInfo ->
        case mkPerasParamsFromTickedLedger ledgerConfig tickedLedgerState of
          Left err ->
            PerasEpochContextResolverError err
          Right params ->
            createBoundedEpochCOntextResolver
              embedF
              eiEpochToPerasRoundInfo
              params
              (getPerasPoolDistrFromTickedLedger tickedLedgerState)
              (getPerasNonceFromTickedHeaderState tickedHeaderState)
      NoPerasEnabled ->
        embedF NoPerasEnabled

-- | Create a 'PerasEpochContextResolver' from a given 'EraIndexed' epoch-to-round
createBoundedEpochCOntextResolver ::
  forall blk.
  ( All Top (HardForkIndices blk)
  , StateSupportsPerasEpochContext blk
  ) =>
  -- | How to embed the (optional) fresh bounded epoch context into a resolver.
  (PerasEnabled (BoundedPerasEpochContext blk) -> PerasEpochContextResolver blk) ->
  EraIndexed (HardForkIndices blk) EpochToPerasRoundInfo ->
  PerasParams blk ->
  PoolDistr ->
  Nonce ->
  PerasEpochContextResolver blk
createBoundedEpochCOntextResolver
  embedF
  eraIndexedEpochToPerasRoundInfo
  params
  poolDistr
  epochNonce =
    either
      (PerasEpochContextResolverError . show)
      (embedF . PerasEnabled)
      $ mkBoundedPerasEpochContext
        mbEraIndexedEpochToPerasRoundInfo
        params
        poolDistr
        epochNonce
   where
    mbEraIndexedEpochToPerasRoundInfo =
      toMaybeEraIndexedEpochToPerasRoundInfo
        (Proxy @blk)
        eraIndexedEpochToPerasRoundInfo

-- | Swap the positions of the 'EraIndexed' and 'PerasEnabled' wrappers, so we
-- can directly pattern-match on the 'PerasEnabled' information.
collapseEraIndexed ::
  All Top xs =>
  EraIndexed xs (PerasEnabled a) ->
  PerasEnabled (EraIndexed xs a)
collapseEraIndexed (EraIndexed ns) =
  hcollapse $
    himap
      ( \idx (K pea) ->
          case pea of
            PerasEnabled a ->
              K (PerasEnabled (EraIndexed (injectNS idx (K a))))
            NoPerasEnabled ->
              K NoPerasEnabled
      )
      ns

-- * Time resolution

-- | Data needed to run time-dependent queries.
--
-- NOTE: this is existential on the 'MapKind' of the ledger state, as we really
-- don't care about it for the purpose of time resolution.
data TimeResolutionContext blk where
  TimeResolutionContext ::
    forall blk mk.
    LedgerConfig blk ->
    LedgerState blk mk ->
    TimeResolutionContext blk

-- | Wrapper over 'runQuery' that uses a 'TimeResolutionContext' to build a
-- 'Summary'.
runQueryWithContext ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  Qry a ->
  Either PastHorizonException a
runQueryWithContext (TimeResolutionContext cfg state) qry =
  runQuery qry (hardForkSummary cfg state)

-- | Wrapper over 'runQueryEraIndexed' that uses a 'TimeResolutionContext' to
-- build a 'Summary'.
runQueryEraIndexedWithContext ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  Qry a ->
  Either PastHorizonException (EraIndexed (HardForkIndices blk) a)
runQueryEraIndexedWithContext (TimeResolutionContext cfg state) qry =
  runQueryEraIndexed qry (hardForkSummary cfg state)

-- | A handle to an STM action that returns a 'TimeResolutionContext'.
newtype TimeResolutionContextHandle m blk
  = TimeResolutionContextHandle
  { getTimeResolutionContext :: STM m (TimeResolutionContext blk)
  }

-- | Helper to run a time-dependent query using a 'TimeResolutionContextHandle'.
runQueryWithContextHandle ::
  (HasHardForkHistory blk, MonadSTM m) =>
  TimeResolutionContextHandle m blk ->
  Qry a ->
  STM m (Either PastHorizonException a)
runQueryWithContextHandle handle qry = do
  context <- getTimeResolutionContext handle
  pure (runQueryWithContext context qry)

-- * Next-epoch detection

-- | The outcome of comparing the epoch of a previous slot with the epoch of a
-- target slot when ticking the ledger, discriminated by how many epoch
-- boundaries were crossed. This drives how the Peras epoch-context resolver is
-- updated.
data EpochCrossing a
  = -- | The target slot is in the same epoch as the previous slot: no epoch
    -- boundary was crossed.
    SameEpoch
  | -- | Exactly one epoch boundary was crossed. The resolver can be advanced
    -- incrementally (the current context becomes the previous one).
    NextEpoch !a
  | -- | More than one epoch boundary was crossed at once, i.e. the chain
    -- skipped one or more entire epochs. This is only possible for a
    -- sparse/empty chain, or when ticking the ledger far ahead of its tip. The
    -- two-epoch window cannot be advanced incrementally across the gap, so the
    -- resolver must be re-initialised at the target epoch instead.
    ManyEpochsCrossed !a
  deriving (Show, Functor, Foldable, Traversable)

-- | Errors that can occur when detecting whether a target slot is in the next
-- epoch compared to a previous slot.
data DetectNextEpochError
  = -- | Target slot is past the horizon of the given time resolution context.
    DetectNextEpochPastHorizonError
      PastHorizonException
  | -- | Target slot is in the past compared to the previous slot.
    DetectNextEpochNewSlotInPast
      -- Previous slot.
      !SlotNo
      -- Epoch of the previous slot.
      !EpochNo
      -- Target slot.
      !SlotNo
      -- Epoch of the target slot.
      !EpochNo
  deriving (Show, Exception)

-- | Determine whether a target slot is in the next epoch compared to a previous
-- slot, and if so, how many epoch boundaries were crossed.
isNextEpoch ::
  HasHardForkHistory blk =>
  TimeResolutionContext blk ->
  WithOrigin SlotNo ->
  SlotNo ->
  Either
    DetectNextEpochError
    ( EpochCrossing
        ( EraIndexed
            (HardForkIndices blk)
            (PerasEnabled EpochToPerasRoundInfo)
        )
    )
isNextEpoch context mbPrevSlot nextSlot = do
  resolveEpochCrossing >>= traverse resolvePerasInfoForEpoch
 where
  slotToEpochOrError slot =
    bimap DetectNextEpochPastHorizonError fst $
      runQueryWithContext context $
        slotToEpoch' slot

  resolvePerasInfoForEpoch epochNo =
    bimap DetectNextEpochPastHorizonError id $
      runQueryEraIndexedWithContext context (epochToPerasRoundInfo epochNo)

  resolveEpochCrossing =
    case mbPrevSlot of
      Origin -> do
        nextEpoch <- slotToEpochOrError nextSlot
        if
          | EpochNo 0 <- nextEpoch ->
              Right (NextEpoch nextEpoch)
          | otherwise ->
              Right (ManyEpochsCrossed nextEpoch)
      NotOrigin prevSlot -> do
        prevEpoch <- slotToEpochOrError prevSlot
        nextEpoch <- slotToEpochOrError nextSlot
        if
          | prevEpoch == nextEpoch ->
              Right SameEpoch
          | prevEpoch < nextEpoch ->
              if EpochNo (unEpochNo prevEpoch + 1) == nextEpoch
                then Right (NextEpoch nextEpoch)
                else Right (ManyEpochsCrossed nextEpoch)
          | otherwise ->
              Left
                ( DetectNextEpochNewSlotInPast
                    prevSlot
                    prevEpoch
                    nextSlot
                    nextEpoch
                )
