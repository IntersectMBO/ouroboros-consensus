{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( -- * Value for /each/ era
    PerEraBlockConfig (..)
  , PerEraChainOrderConfig (..)
  , PerEraCodecConfig (..)
  , PerEraConsensusConfig (..)
  , PerEraLedgerConfig (..)
  , PerEraStorageConfig (..)
  , PerEraPerasPrivateKey (..)

    -- * Values for /some/ eras
  , SomeErasCanBeLeader (..)

    -- * Value for /one/ era
  , OneEraApplyTxErr (..)
  , OneEraBlock (..)
  , OneEraCannotForge (..)
  , OneEraEnvelopeErr (..)
  , OneEraForgeStateInfo (..)
  , OneEraForgeStateUpdateError (..)
  , OneEraGenTx (..)
  , OneEraGenTxId (..)
  , OneEraHash (..)
  , OneEraHeader (..)
  , OneEraIsLeader (..)
  , OneEraLedgerError (..)
  , OneEraLedgerEvent (..)
  , OneEraLedgerUpdate (..)
  , OneEraLedgerWarning (..)
  , OneEraReasonForSwitch (..)
  , OneEraTiebreakerView (..)
  , OneEraTentativeHeaderState (..)
  , OneEraTentativeHeaderView (..)
  , OneEraTipInfo (..)
  , OneEraValidateView (..)
  , OneEraValidatedGenTx (..)
  , OneEraValidationErr (..)
  , OneEraPerasVote (..)
  , OneEraPerasCert (..)
  , OneEraPerasError (..)
  , HardForkPerasError (..)
  , OneEraPerasCrypto (..)
  , OneEraPerasVotingCommitteeScheme (..)
  , VotingCommittee (..)

    -- * Serialisation of n-ary sums
  , decodeNS
  , encodeNS

    -- * Value for two /different/ eras
  , EraMismatch (..)
  , MismatchEraInfo (..)
  , mismatchFutureEra
  , mismatchOneEra
  , mkEraMismatch

    -- * Utility
  , getSameValue
  , oneEraBlockHeader
  , oneEraGenTxIdRawHash
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import Codec.Serialise (Serialise (..))
import qualified Codec.Serialise as Serialise
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import Data.Function (on)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Index (Index, hizipWith, injectNS, nsFromIndex, nsToIndex)
import Data.SOP.Match (Mismatch)
import qualified Data.SOP.Match as Match
import Data.SOP.OptNP (NonEmptyOptNP)
import Data.SOP.Strict
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics (Generic)
import GHC.Stack
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Committee.Class (VotingCommittee)
import Ouroboros.Consensus.Committee.Crypto (PrivateKey)
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Info
import Ouroboros.Consensus.HardFork.Combinator.Lifting
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (ShowProxy, allEqual)
import Ouroboros.Consensus.Util.Assert
import Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Serialisation of n-ary sums ('NS')

  Generic CBOR (de)serialisation of an 'NS': a length-2 list holding the 'Word8'
  era index followed by the selected era's payload. Defined here, rather than in
  "Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common" (which re-exports
  them), so that instances sitting upstream of @Common@ -- such as the
  'VotingCommittee' instance below -- can reuse them.
-------------------------------------------------------------------------------}

encodeNS :: SListI xs => NP (f -.-> K Encoding) xs -> NS f xs -> Encoding
encodeNS es ns =
  mconcat
    [ Enc.encodeListLen 2
    , Enc.encodeWord8 $ nsToIndex ns
    , hcollapse $ hzipWith apFn es ns
    ]

decodeNS :: forall xs f s. SListI xs => NP (Decoder s :.: f) xs -> Decoder s (NS f xs)
decodeNS ds = do
  enforceSize "decodeNS" 2
  i <- Dec.decodeWord8
  case nsFromIndex i of
    Nothing -> fail $ "decodeNS: invalid index " ++ show i
    Just ns -> hcollapse $ hizipWith aux ds ns
 where
  aux ::
    Index xs blk ->
    (Decoder s :.: f) blk ->
    K () blk ->
    K (Decoder s (NS f xs)) blk
  aux index (Comp dec) (K ()) = K $ injectNS index <$> dec

{-------------------------------------------------------------------------------
  Value for /each/ era
-------------------------------------------------------------------------------}

newtype PerEraBlockConfig xs = PerEraBlockConfig {getPerEraBlockConfig :: NP BlockConfig xs}
newtype PerEraChainOrderConfig xs = PerEraChainOrderConfig {getPerEraChainOrderConfig :: NP WrapChainOrderConfig xs}
newtype PerEraCodecConfig xs = PerEraCodecConfig {getPerEraCodecConfig :: NP CodecConfig xs}
newtype PerEraConsensusConfig xs = PerEraConsensusConfig {getPerEraConsensusConfig :: NP WrapPartialConsensusConfig xs}
newtype PerEraLedgerConfig xs = PerEraLedgerConfig {getPerEraLedgerConfig :: NP WrapPartialLedgerConfig xs}
newtype PerEraStorageConfig xs = PerEraStorageConfig {getPerEraStorageConfig :: NP StorageConfig xs}
newtype PerEraPerasPrivateKey xs = PerEraPerasPrivateKey {getPerEraPerasPrivateKey :: NP WrapPerasPrivateKey xs}
type instance PrivateKey (OneEraPerasCrypto xs) = PerEraPerasPrivateKey xs

{-------------------------------------------------------------------------------
  Values for /some/ eras

  The reason for using @NonEmptyOptNP f xs@ as opposed to @NP (Maybe :.: f) xs@
  is to maintain the isomorphism between @blk@ and @HardForkBlock '[blk]@ in
  "Ouroboros.Consensus.HardFork.Combinator.Embed.Unary"
-------------------------------------------------------------------------------}

newtype SomeErasCanBeLeader xs = SomeErasCanBeLeader {getSomeErasCanBeLeader :: NonEmptyOptNP WrapCanBeLeader xs}

{-------------------------------------------------------------------------------
  Value for /one/ era
-------------------------------------------------------------------------------}

newtype OneEraApplyTxErr xs = OneEraApplyTxErr {getOneEraApplyTxErr :: NS WrapApplyTxErr xs}
newtype OneEraBlock xs = OneEraBlock {getOneEraBlock :: NS I xs}
newtype OneEraCannotForge xs = OneEraCannotForge {getOneEraCannotForge :: NS WrapCannotForge xs}
newtype OneEraEnvelopeErr xs = OneEraEnvelopeErr {getOneEraEnvelopeErr :: NS WrapEnvelopeErr xs}
newtype OneEraForgeStateInfo xs = OneEraForgeStateInfo {getOneEraForgeStateInfo :: NS WrapForgeStateInfo xs}
newtype OneEraForgeStateUpdateError xs = OneEraForgeStateUpdateError {getOneEraForgeStateUpdateError :: NS WrapForgeStateUpdateError xs}
newtype OneEraGenTx xs = OneEraGenTx {getOneEraGenTx :: NS GenTx xs}
newtype OneEraGenTxId xs = OneEraGenTxId {getOneEraGenTxId :: NS WrapGenTxId xs}
newtype OneEraHeader xs = OneEraHeader {getOneEraHeader :: NS Header xs}
newtype OneEraIsLeader xs = OneEraIsLeader {getOneEraIsLeader :: NS WrapIsLeader xs}
newtype OneEraLedgerError xs = OneEraLedgerError {getOneEraLedgerError :: NS WrapLedgerErr xs}
newtype OneEraLedgerEvent xs = OneEraLedgerEvent {getOneEraLedgerEvent :: NS WrapLedgerEvent xs}
newtype OneEraLedgerUpdate xs = OneEraLedgerUpdate {getOneEraLedgerUpdate :: NS WrapLedgerUpdate xs}
newtype OneEraLedgerWarning xs = OneEraLedgerWarning {getOneEraLedgerWarning :: NS WrapLedgerWarning xs}
newtype OneEraTiebreakerView xs = OneEraTiebreakerView {getOneEraTiebreakerView :: NS WrapTiebreakerView xs}
newtype OneEraTentativeHeaderState xs = OneEraTentativeHeaderState {getOneEraTentativeHeaderState :: NS WrapTentativeHeaderState xs}
newtype OneEraTentativeHeaderView xs = OneEraTentativeHeaderView {getOneEraTentativeHeaderView :: NS WrapTentativeHeaderView xs}
newtype OneEraTipInfo xs = OneEraTipInfo {getOneEraTipInfo :: NS WrapTipInfo xs}
newtype OneEraValidateView xs = OneEraValidateView {getOneEraValidateView :: NS WrapValidateView xs}
newtype OneEraValidatedGenTx xs = OneEraValidatedGenTx {getOneEraValidatedGenTx :: NS WrapValidatedGenTx xs}
newtype OneEraValidationErr xs = OneEraValidationErr {getOneEraValidationErr :: NS WrapValidationErr xs}
newtype OneEraPerasVote xs = OneEraPerasVote {getOneEraPerasVote :: NS WrapPerasVote xs}
newtype OneEraPerasCert xs = OneEraPerasCert {getOneEraPerasCert :: NS WrapPerasCert xs}
newtype OneEraPerasError xs = OneEraPerasError {getOneEraPerasError :: NS WrapPerasError xs}
data HardForkPerasError xs
  = HardForkPerasErrorEraMismatch
  | HardForkPerasErrorOneEraPerasError (OneEraPerasError xs)
  | HardForkPerasErrorConversionError -- Should never be produced in practice, since we dispatch to a concrete era before calling any failible operation
  | HardForkPerasErrorQuorumNotReachedError -- Should never be produced in practice, since we dispatch to a concrete era before calling any failible operation
  | HardForkPerasErrorCommitteeError -- Should never be produced in practice, since we dispatch to a concrete era before calling any failible operation
newtype OneEraPerasCrypto xs = OneEraPerasCrypto {getOneEraPerasCrypto :: NS WrapPerasCrypto xs}
newtype OneEraPerasVotingCommitteeScheme xs = OneEraPerasVotingCommitteeScheme
  {getOneEraPerasVotingCommitteeScheme :: NS WrapPerasVotingCommitteeScheme xs}

newtype instance VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasVotingCommittee
  {getOneEraPerasVotingCommittee :: NS WrapPerasVotingCommittee xs}

deriving via LiftNS WrapPerasVote xs instance CanHardFork xs => Show (OneEraPerasVote xs)
deriving via LiftNS WrapPerasCert xs instance CanHardFork xs => Show (OneEraPerasCert xs)
deriving via LiftNS WrapPerasError xs instance CanHardFork xs => Show (OneEraPerasError xs)
deriving via LiftNS WrapPerasCrypto xs instance CanHardFork xs => Show (OneEraPerasCrypto xs)
deriving via
  LiftNS WrapPerasVotingCommitteeScheme xs
  instance
    CanHardFork xs => Show (OneEraPerasVotingCommitteeScheme xs)
deriving via
  LiftNS WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    Show (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving instance Show (OneEraPerasError xs) => Show (HardForkPerasError xs)

deriving via LiftNS WrapPerasVote xs instance CanHardFork xs => Eq (OneEraPerasVote xs)
deriving via LiftNS WrapPerasCert xs instance CanHardFork xs => Eq (OneEraPerasCert xs)
deriving via LiftNS WrapPerasError xs instance CanHardFork xs => Eq (OneEraPerasError xs)
deriving via LiftNS WrapPerasCrypto xs instance CanHardFork xs => Eq (OneEraPerasCrypto xs)
deriving via
  LiftNS WrapPerasVotingCommitteeScheme xs
  instance
    CanHardFork xs => Eq (OneEraPerasVotingCommitteeScheme xs)
deriving via
  LiftNS WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    Eq (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving instance Eq (OneEraPerasError xs) => Eq (HardForkPerasError xs)

deriving instance Generic (OneEraPerasVote xs)
deriving instance Generic (OneEraPerasCert xs)
deriving instance Generic (OneEraPerasError xs)
deriving instance Generic (OneEraPerasCrypto xs)
deriving instance Generic (OneEraPerasVotingCommitteeScheme xs)
deriving instance
  Generic (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving instance Generic (HardForkPerasError xs)

deriving via
  LiftNamedNS "OneEraPerasVote" WrapPerasVote xs
  instance
    CanHardFork xs => NoThunks (OneEraPerasVote xs)
deriving via
  LiftNamedNS "OneEraPerasCert" WrapPerasCert xs
  instance
    CanHardFork xs => NoThunks (OneEraPerasCert xs)
deriving via
  LiftNamedNS "OneEraPerasError" WrapPerasError xs
  instance
    CanHardFork xs => NoThunks (OneEraPerasError xs)
deriving via
  LiftNamedNS "OneEraPerasCrypto" WrapPerasCrypto xs
  instance
    CanHardFork xs => NoThunks (OneEraPerasCrypto xs)
deriving via
  LiftNamedNS "OneEraPerasVotingCommitteeScheme" WrapPerasVotingCommitteeScheme xs
  instance
    CanHardFork xs => NoThunks (OneEraPerasVotingCommitteeScheme xs)
deriving via
  LiftNamedNS "OneEraPerasVotingCommittee" WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    NoThunks (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving instance NoThunks (OneEraPerasError xs) => NoThunks (HardForkPerasError xs)

instance CanHardFork xs => Exception (OneEraPerasError xs)
instance CanHardFork xs => Exception (HardForkPerasError xs)

instance Typeable xs => ShowProxy (OneEraPerasVote xs)
instance Typeable xs => ShowProxy (OneEraPerasCert xs)

-- Hand-written rather than @deriving via SerialiseNS@: that derivation needs
-- @All (Compose Serialise WrapPerasVotingCommittee) xs@, which GHC cannot solve
-- from the @CanHardFork xs@ (i.e. @All SingleEraBlock xs@) context for an abstract
-- @xs@. We instead build the per-era codecs with @hcpure proxySingle@ -- each era's
-- @Serialise (WrapPerasVotingCommittee blk)@ is reachable from @SingleEraBlock blk@
-- via its @StateSupportsPerasEpochContext@ superclass -- and feed them to 'encodeNS'
-- and 'decodeNS', producing the same wire format a @SerialiseNS@ derivation would.
instance
  CanHardFork xs =>
  Serialise (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
  where
  encode (OneEraPerasVotingCommittee ns) =
    encodeNS (hcpure proxySingle (fn (K . Serialise.encode))) ns
  decode =
    OneEraPerasVotingCommittee
      <$> decodeNS (hcpure proxySingle (Comp Serialise.decode))

{-------------------------------------------------------------------------------
  Hash
-------------------------------------------------------------------------------}

-- | The hash for an era
--
-- This type is special: we don't use an NS here, because the hash by itself
-- should not allow us to differentiate between eras. If it did, the /size/
-- of the hash would necessarily have to increase, and that leads to trouble.
-- So, the type parameter @xs@ here is merely a phantom one, and we just store
-- the underlying raw hash.
newtype OneEraHash (xs :: [k]) = OneEraHash {getOneEraHash :: ShortByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise, NFData, FromCBOR, ToCBOR)

instance Show (OneEraHash xs) where
  show = BSC.unpack . B16.encode . Short.fromShort . getOneEraHash

instance Condense (OneEraHash xs) where
  condense = show

{-------------------------------------------------------------------------------
  OneEraGenTxId
-------------------------------------------------------------------------------}

-- | This instance compares the underlying raw hash ('toRawTxIdHash') of the
-- 'TxId'.
--
-- Note that this means that transactions in different eras can have equal
-- 'TxId's. This should only be the case when the transaction format is
-- backwards compatible from one era to the next.
instance CanHardFork xs => Eq (OneEraGenTxId xs) where
  (==) = (==) `on` oneEraGenTxIdRawHash

-- | See the corresponding 'Eq' instance.
instance CanHardFork xs => Ord (OneEraGenTxId xs) where
  compare = compare `on` oneEraGenTxIdRawHash

{-------------------------------------------------------------------------------
  Value for two /different/ eras
-------------------------------------------------------------------------------}

newtype MismatchEraInfo xs = MismatchEraInfo
  { getMismatchEraInfo :: Mismatch SingleEraInfo LedgerEraInfo xs
  -- ^ Era mismatch
  --
  -- We have an era mismatch between the era of a block/header/tx/query
  -- and the era of the current ledger.
  }

mismatchOneEra :: MismatchEraInfo '[b] -> Void
mismatchOneEra = Match.mismatchOne . getMismatchEraInfo

-- | A mismatch _must_ involve a future era
mismatchFutureEra ::
  SListI xs =>
  MismatchEraInfo (x ': xs) -> NS SingleEraInfo xs
mismatchFutureEra =
  either id (hmap getLedgerEraInfo)
    . Match.mismatchNotFirst
    . getMismatchEraInfo

{-------------------------------------------------------------------------------
  Untyped version of 'MismatchEraInfo'
-------------------------------------------------------------------------------}

-- | Extra info for errors caused by applying a block, header, transaction, or
-- query from one era to a ledger from a different era.
data EraMismatch = EraMismatch
  { ledgerEraName :: !Text
  -- ^ Name of the era of the ledger ("Byron" or "Shelley").
  , otherEraName :: !Text
  -- ^ Era of the block, header, transaction, or query.
  }
  deriving (Eq, Show, Generic)

-- | When a transaction or block from a certain era was applied to a ledger
-- from another era, we get a 'MismatchEraInfo'.
--
-- Given such a 'MismatchEraInfo', return the name of the era of the
-- transaction/block and the name of the era of the ledger.
mkEraMismatch :: SListI xs => MismatchEraInfo xs -> EraMismatch
mkEraMismatch (MismatchEraInfo mismatch) =
  go mismatch
 where
  go :: SListI xs => Mismatch SingleEraInfo LedgerEraInfo xs -> EraMismatch
  go (Match.ML otherEra ledgerEra) =
    EraMismatch
      { ledgerEraName = hcollapse $ hmap (K . ledgerName) ledgerEra
      , otherEraName = otherName otherEra
      }
  go (Match.MR otherEra ledgerEra) =
    EraMismatch
      { ledgerEraName = ledgerName ledgerEra
      , otherEraName = hcollapse $ hmap (K . otherName) otherEra
      }
  go (Match.MS m) = go m

  ledgerName :: LedgerEraInfo blk -> Text
  ledgerName = singleEraName . getLedgerEraInfo

  otherName :: SingleEraInfo blk -> Text
  otherName = singleEraName

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

oneEraBlockHeader :: CanHardFork xs => OneEraBlock xs -> OneEraHeader xs
oneEraBlockHeader =
  OneEraHeader
    . hcmap proxySingle (getHeader . unI)
    . getOneEraBlock

getSameValue ::
  forall xs a.
  (IsNonEmpty xs, Eq a, SListI xs, HasCallStack) =>
  NP (K a) xs ->
  a
getSameValue values =
  case isNonEmpty (Proxy @xs) of
    ProofNonEmpty{} ->
      assertWithMsg allEqualCheck (unK (hd values))
 where
  allEqualCheck :: Either String ()
  allEqualCheck
    | allEqual (hcollapse values) =
        return ()
    | otherwise =
        throwError "differing values across hard fork"

oneEraGenTxIdRawHash :: CanHardFork xs => OneEraGenTxId xs -> ShortByteString
oneEraGenTxIdRawHash =
  hcollapse
    . hcmap proxySingle (K . toRawTxIdHash . unwrapGenTxId)
    . getOneEraGenTxId

{-------------------------------------------------------------------------------
  NoThunks instances
-------------------------------------------------------------------------------}

deriving via
  LiftNamedNP "PerEraBlockConfig" BlockConfig xs
  instance
    CanHardFork xs => NoThunks (PerEraBlockConfig xs)

deriving via
  LiftNamedNP "PerEraCodecConfig" CodecConfig xs
  instance
    CanHardFork xs => NoThunks (PerEraCodecConfig xs)

deriving via
  LiftNamedNP "PerEraConsensusConfig" WrapPartialConsensusConfig xs
  instance
    CanHardFork xs => NoThunks (PerEraConsensusConfig xs)

deriving via
  LiftNamedNP "PerEraLedgerConfig" WrapPartialLedgerConfig xs
  instance
    CanHardFork xs => NoThunks (PerEraLedgerConfig xs)

deriving via
  LiftNamedNP "PerEraStorageConfig" StorageConfig xs
  instance
    CanHardFork xs => NoThunks (PerEraStorageConfig xs)

deriving via
  LiftNamedNS "OneEraEnvelopeErr" WrapEnvelopeErr xs
  instance
    CanHardFork xs => NoThunks (OneEraEnvelopeErr xs)

deriving via
  LiftNamedNS "OneEraGenTx" GenTx xs
  instance
    CanHardFork xs => NoThunks (OneEraGenTx xs)

deriving via
  LiftNamedNS "OneEraGenTxId" WrapGenTxId xs
  instance
    CanHardFork xs => NoThunks (OneEraGenTxId xs)

deriving via
  LiftNamedNS "OneEraHeader" Header xs
  instance
    CanHardFork xs => NoThunks (OneEraHeader xs)

deriving via
  LiftNamedNS "OneEraLedgerError" WrapLedgerErr xs
  instance
    CanHardFork xs => NoThunks (OneEraLedgerError xs)

deriving via
  LiftNamedNS "OneEraTiebreakerView" WrapTiebreakerView xs
  instance
    CanHardFork xs => NoThunks (OneEraTiebreakerView xs)

deriving via
  LiftNamedNS "OneEraTentativeHeaderState" WrapTentativeHeaderState xs
  instance
    CanHardFork xs => NoThunks (OneEraTentativeHeaderState xs)

deriving via
  LiftNamedNS "OneEraTipInfo" WrapTipInfo xs
  instance
    CanHardFork xs => NoThunks (OneEraTipInfo xs)

deriving via
  LiftNamedNS "OneEraValidated" WrapValidatedGenTx xs
  instance
    CanHardFork xs => NoThunks (OneEraValidatedGenTx xs)

deriving via
  LiftNamedNS "OneEraValidationErr" WrapValidationErr xs
  instance
    CanHardFork xs => NoThunks (OneEraValidationErr xs)

deriving via
  LiftNamedMismatch "MismatchEraInfo" SingleEraInfo LedgerEraInfo xs
  instance
    CanHardFork xs => NoThunks (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Other instances
-------------------------------------------------------------------------------}

deriving via LiftNS WrapApplyTxErr xs instance CanHardFork xs => Eq (OneEraApplyTxErr xs)
deriving via LiftNS WrapEnvelopeErr xs instance CanHardFork xs => Eq (OneEraEnvelopeErr xs)
deriving via LiftNS GenTx xs instance CanHardFork xs => Eq (OneEraGenTx xs)
deriving via LiftNS WrapLedgerErr xs instance CanHardFork xs => Eq (OneEraLedgerError xs)
deriving via LiftNS WrapLedgerUpdate xs instance CanHardFork xs => Eq (OneEraLedgerUpdate xs)
deriving via LiftNS WrapLedgerWarning xs instance CanHardFork xs => Eq (OneEraLedgerWarning xs)
deriving via LiftNS WrapTiebreakerView xs instance CanHardFork xs => Eq (OneEraTiebreakerView xs)
deriving via LiftNS WrapTipInfo xs instance CanHardFork xs => Eq (OneEraTipInfo xs)
deriving via LiftNS WrapValidatedGenTx xs instance CanHardFork xs => Eq (OneEraValidatedGenTx xs)
deriving via LiftNS WrapValidationErr xs instance CanHardFork xs => Eq (OneEraValidationErr xs)

deriving via
  LiftNS WrapEnvelopeErr xs
  instance
    CanHardFork xs => Show (OneEraEnvelopeErr xs)
deriving via
  LiftNS WrapForgeStateInfo xs
  instance
    CanHardFork xs => Show (OneEraForgeStateInfo xs)
deriving via
  LiftNS WrapForgeStateUpdateError xs
  instance
    CanHardFork xs => Show (OneEraForgeStateUpdateError xs)
deriving via
  LiftNS WrapLedgerErr xs
  instance
    CanHardFork xs => Show (OneEraLedgerError xs)
deriving via
  LiftNS WrapLedgerUpdate xs
  instance
    CanHardFork xs => Show (OneEraLedgerUpdate xs)
deriving via
  LiftNS WrapLedgerWarning xs
  instance
    CanHardFork xs => Show (OneEraLedgerWarning xs)
deriving via
  LiftNS WrapTentativeHeaderState xs
  instance
    CanHardFork xs => Show (OneEraTentativeHeaderState xs)
deriving via
  LiftNS WrapTentativeHeaderView xs
  instance
    CanHardFork xs => Show (OneEraTentativeHeaderView xs)
deriving via
  LiftNS WrapTipInfo xs
  instance
    CanHardFork xs => Show (OneEraTipInfo xs)
deriving via
  LiftNS WrapValidatedGenTx xs
  instance
    CanHardFork xs => Show (OneEraValidatedGenTx xs)
deriving via
  LiftNS WrapValidationErr xs
  instance
    CanHardFork xs => Show (OneEraValidationErr xs)

deriving instance Show (PartialLedgerConfig xs) => Show (WrapPartialLedgerConfig xs)
deriving via
  LiftNP WrapPartialLedgerConfig xs
  instance
    CanHardFork xs => Show (PerEraLedgerConfig xs)

deriving via
  LiftMismatch SingleEraInfo LedgerEraInfo xs
  instance
    All SingleEraBlock xs => Eq (MismatchEraInfo xs)
deriving via
  LiftMismatch SingleEraInfo LedgerEraInfo xs
  instance
    All SingleEraBlock xs => Show (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Show instances used in tests only
-------------------------------------------------------------------------------}

deriving via LiftNS WrapApplyTxErr xs instance CanHardFork xs => Show (OneEraApplyTxErr xs)
deriving via LiftNS I xs instance CanHardFork xs => Show (OneEraBlock xs)
deriving via LiftNS WrapCannotForge xs instance CanHardFork xs => Show (OneEraCannotForge xs)
deriving via LiftNS GenTx xs instance CanHardFork xs => Show (OneEraGenTx xs)
deriving via LiftNS WrapGenTxId xs instance CanHardFork xs => Show (OneEraGenTxId xs)
deriving via LiftNS Header xs instance CanHardFork xs => Show (OneEraHeader xs)
deriving via LiftNS WrapTiebreakerView xs instance CanHardFork xs => Show (OneEraTiebreakerView xs)
deriving via
  LiftNS WrapReasonForSwitch xs
  instance
    CanHardFork xs => Show (OneEraReasonForSwitch xs)
