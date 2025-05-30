{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Byron.Generators
  ( RegularBlock (..)
  , epochSlots
  , genByronLedgerConfig
  , genByronLedgerState
  , k
  , protocolMagicId
  ) where

import Cardano.Chain.Block
  ( ABlockOrBoundary (..)
  , ABlockOrBoundaryHdr (..)
  , ChainValidationState (..)
  , cvsPreviousHash
  )
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as API
import Cardano.Chain.Common
  ( Address
  , BlockCount (..)
  , CompactAddress
  , KeyHash
  , Lovelace
  )
import qualified Cardano.Chain.Delegation as CC.Del
import qualified Cardano.Chain.Delegation.Validation.Activation as CC.Act
import qualified Cardano.Chain.Delegation.Validation.Interface as CC.DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as CC.Sched
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Genesis as CC.Genesis
import Cardano.Chain.Slotting
  ( EpochNumber
  , EpochSlots (..)
  , SlotNumber
  )
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.Update.Validation.Registration as CC.Reg
import Cardano.Crypto
  ( ProtocolMagicId (..)
  , RequiresNetworkMagic (..)
  )
import Cardano.Crypto.Hashing (Hash)
import Cardano.Crypto.Signing
import qualified Cardano.Crypto.Wallet as Wallet
import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Cardano.Ledger.Binary (decCBOR, encCBOR)
import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Protocol
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.HeaderValidation (AnnTip (..))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import Ouroboros.Network.SizeInBytes
import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.Genesis.Gen as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Slotting.Gen as CC
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as UG
import qualified Test.Cardano.Crypto.Gen as CC
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip
  ( Coherent (..)
  , WithVersion (..)
  )
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Matches that from the 'CC.dummyConfig'
k :: SecurityParam
k = SecurityParam $ knownNonZeroBounded @10

-- | Matches that from the 'CC.dummyConfig'
epochSlots :: EpochSlots
epochSlots = EpochSlots 100

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

instance Arbitrary CC.Genesis.Config where
  arbitrary =
    CC.Genesis.Config
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CC.Genesis.GenesisData where
  arbitrary =
    CC.Genesis.GenesisData
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CC.Genesis.GenesisKeyHashes where
  arbitrary = CC.Genesis.GenesisKeyHashes <$> arbitrary

instance Arbitrary CC.Genesis.GenesisDelegation where
  arbitrary =
    (CC.Genesis.mkGenesisDelegation <$> arbitrary)
      `suchThatMap` (either (const Nothing) Just)

instance Arbitrary (CC.Del.ACertificate ()) where
  arbitrary =
    CC.Del.signCertificate
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary SafeSigner where
  arbitrary = do
    seed <- BS.pack <$> sequence (replicate 32 arbitrary)
    passPhrase <- BS.pack <$> sequence (replicate passphraseLength arbitrary)
    let xprv = Wallet.generate seed passPhrase
    return $ SafeSigner (SigningKey xprv) (PassPhrase (fromString (BSC8.unpack passPhrase)))

instance Arbitrary VerificationKey where
  arbitrary =
    either (error . show) id . parseFullVerificationKey
      <$> (T.pack . BSC8.unpack . B64.encode <$> arbitraryKey)
   where
    -- The key must be 64 bytes
    arbitraryKey = BS.pack <$> sequence (replicate 64 arbitrary)

instance Arbitrary CC.Genesis.GenesisNonAvvmBalances where
  arbitrary = CC.Genesis.GenesisNonAvvmBalances <$> arbitrary

instance Arbitrary Address where
  arbitrary = hedgehog CC.genAddress

instance Arbitrary Lovelace where
  arbitrary = hedgehog CC.genLovelace

instance Arbitrary CC.Genesis.GenesisAvvmBalances where
  arbitrary = CC.Genesis.GenesisAvvmBalances <$> arbitrary

instance Arbitrary CompactRedeemVerificationKey where
  arbitrary = hedgehog CC.genCompactRedeemVerificationKey

instance Arbitrary BlockCount where
  arbitrary = hedgehog CC.genBlockCount

instance Arbitrary RequiresNetworkMagic where
  arbitrary = hedgehog CC.genRequiresNetworkMagic

instance Arbitrary ProtocolMagicId where
  arbitrary = hedgehog CC.genProtocolMagicId

instance Arbitrary CC.UTxO.UTxOConfiguration where
  arbitrary = CC.UTxO.UTxOConfiguration <$> arbitrary

instance Arbitrary CompactAddress where
  arbitrary = hedgehog CC.genCompactAddress

-- | A 'ByronBlock' that is never an EBB.
newtype RegularBlock = RegularBlock {unRegularBlock :: ByronBlock}
  deriving (Eq, Show)

instance Arbitrary RegularBlock where
  arbitrary =
    RegularBlock . annotateByronBlock epochSlots
      <$> hedgehog (CC.genBlock protocolMagicId epochSlots)

instance Arbitrary ByronBlock where
  arbitrary = getCoherent <$> arbitrary

instance Arbitrary (Coherent ByronBlock) where
  arbitrary =
    Coherent
      <$> frequency
        [ (3, genBlock)
        , (1, genBoundaryBlock)
        ]
   where
    genBlock :: Gen ByronBlock
    genBlock = unRegularBlock <$> arbitrary
    genBoundaryBlock :: Gen ByronBlock
    genBoundaryBlock =
      mkByronBlock epochSlots . ABOBBoundary . API.reAnnotateBoundary protocolMagicId
        <$> hedgehog (CC.genBoundaryBlock)

instance Arbitrary (Header ByronBlock) where
  arbitrary =
    frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
   where
    genHeader :: Gen (Header ByronBlock)
    genHeader = do
      blockSize <- SizeInBytes <$> arbitrary
      flip (mkByronHeader epochSlots) blockSize
        . ABOBBlockHdr
        . API.reAnnotateUsing
          (CC.Block.encCBORHeader epochSlots)
          (CC.Block.decCBORAHeader epochSlots)
        <$> hedgehog (CC.genHeader protocolMagicId epochSlots)

    genBoundaryHeader :: Gen (Header ByronBlock)
    genBoundaryHeader = do
      blockSize <- SizeInBytes <$> arbitrary
      flip (mkByronHeader epochSlots) blockSize
        . ABOBBoundaryHdr
        . API.reAnnotateUsing
          (CC.Block.encCBORABoundaryHeader protocolMagicId)
          CC.Block.decCBORABoundaryHeader
        <$> hedgehog CC.genBoundaryHeader

instance Arbitrary (Hash a) where
  arbitrary = coerce <$> hedgehog CC.genTextHash

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> arbitrary

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (GenTx ByronBlock) where
  arbitrary =
    fromMempoolPayload . API.reAnnotateUsing encCBOR decCBOR
      <$> hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId ByronBlock) where
  arbitrary =
    oneof
      [ ByronTxId <$> hedgehog CC.genTxId
      , ByronDlgId <$> hedgehog genCertificateId
      , ByronUpdateProposalId <$> hedgehog (UG.genUpId protocolMagicId)
      , ByronUpdateVoteId <$> hedgehog genUpdateVoteId
      ]
   where
    genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
    genUpdateVoteId = CC.genAbstractHash (UG.genVote protocolMagicId)

instance Arbitrary API.ApplyMempoolPayloadErr where
  arbitrary =
    oneof
      [ API.MempoolTxErr <$> hedgehog CC.genUTxOValidationError
      , API.MempoolDlgErr <$> hedgehog CC.genError
      -- TODO there is no generator for
      -- Cardano.Chain.Update.Validation.Interface.Error and we can't write one
      -- either because the different Error types it wraps are not exported.
      -- , MempoolUpdateProposalErr <$> arbitrary
      -- , MempoolUpdateVoteErr     <$> arbitrary
      ]

instance Arbitrary (SomeBlockQuery (BlockQuery ByronBlock)) where
  arbitrary = pure $ SomeBlockQuery GetUpdateInterfaceState

instance Arbitrary EpochNumber where
  arbitrary = hedgehog CC.genEpochNumber

instance Arbitrary SlotNumber where
  arbitrary = hedgehog CC.genSlotNumber

instance Arbitrary CC.Update.ApplicationName where
  arbitrary = hedgehog UG.genApplicationName

instance Arbitrary CC.Update.SystemTag where
  arbitrary = hedgehog UG.genSystemTag

instance Arbitrary CC.Update.InstallerHash where
  arbitrary = hedgehog UG.genInstallerHash

instance Arbitrary CC.Update.ProtocolVersion where
  arbitrary = hedgehog UG.genProtocolVersion

instance Arbitrary CC.Update.ProtocolParameters where
  arbitrary = hedgehog UG.genProtocolParameters

instance Arbitrary CC.Update.SoftwareVersion where
  arbitrary = hedgehog UG.genSoftwareVersion

instance Arbitrary CC.Reg.ProtocolUpdateProposal where
  arbitrary =
    CC.Reg.ProtocolUpdateProposal
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CC.Reg.SoftwareUpdateProposal where
  arbitrary =
    CC.Reg.SoftwareUpdateProposal
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CC.Reg.ApplicationVersion where
  arbitrary =
    CC.Reg.ApplicationVersion
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CC.UPI.State where
  arbitrary =
    CC.UPI.State
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure mempty -- TODO CandidateProtocolUpdate's constructor is not exported
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure mempty -- TODO Endorsement is not exported
      <*> arbitrary

instance Arbitrary CC.Genesis.GenesisHash where
  arbitrary = CC.Genesis.GenesisHash <$> arbitrary

instance Arbitrary CC.UTxO.UTxO where
  arbitrary =
    oneof
      [ hedgehog CC.genUTxO
      , -- We would sometimes like to run tests using a smaller (or even empty)
        -- UTxO, but 'genUTxO' generates a UTxO without depending on the QC size
        -- parameter. The probability of generating smaller (or empty) UTxOs is
        -- therefore low.
        CC.UTxO.fromList
          <$> listOf ((,) <$> hedgehog CC.genTxIn <*> hedgehog CC.genTxOut)
      ]

instance Arbitrary CC.Act.State where
  arbitrary =
    CC.Act.State
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CC.Sched.ScheduledDelegation where
  arbitrary =
    CC.Sched.ScheduledDelegation
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CC.Sched.State where
  arbitrary =
    CC.Sched.State
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CC.DI.State where
  arbitrary =
    CC.DI.State
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CC.Block.ChainValidationState where
  arbitrary =
    CC.Block.ChainValidationState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ByronNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ByronNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CC.Del.Map where
  arbitrary = CC.Del.fromList <$> arbitrary

instance Arbitrary ByronTransition where
  arbitrary = ByronTransitionInfo . Map.fromList <$> arbitrary

instance Arbitrary (LedgerState ByronBlock mk) where
  arbitrary = ByronLedgerState <$> arbitrary <*> arbitrary <*> arbitrary

-- | Generator for a Byron ledger state in which the tip of the ledger given by
-- `byronLedgerTipBlockNo` is consistent with the chain validation state, i.e., if there is no
-- previous block, the ledger tip wil be `Origin`.
genByronLedgerState :: Gen (LedgerState ByronBlock EmptyMK)
genByronLedgerState = do
  chainValidationState <- arbitrary
  ledgerTransition <- arbitrary
  ledgerTipBlockNo <- genLedgerTipBlockNo chainValidationState
  pure $
    ByronLedgerState
      { byronLedgerTipBlockNo = ledgerTipBlockNo
      , byronLedgerState = chainValidationState
      , byronLedgerTransition = ledgerTransition
      }
 where
  genLedgerTipBlockNo ChainValidationState{cvsPreviousHash} =
    case cvsPreviousHash of
      Left _ -> pure Origin
      Right _ -> NotOrigin <$> arbitrary

instance ZeroableMK mk => Arbitrary (LedgerTables (LedgerState ByronBlock) mk) where
  arbitrary = pure emptyLedgerTables

genByronLedgerConfig :: Gen Byron.Config
genByronLedgerConfig = hedgehog $ CC.genConfig protocolMagicId

instance Arbitrary (TipInfoIsEBB ByronBlock) where
  arbitrary = TipInfoIsEBB <$> arbitrary <*> elements [IsEBB, IsNotEBB]

instance Arbitrary (AnnTip ByronBlock) where
  arbitrary =
    AnnTip
      <$> (SlotNo <$> arbitrary)
      <*> (BlockNo <$> arbitrary)
      <*> arbitrary

instance Arbitrary (PBftState PBftByronCrypto) where
  arbitrary = do
    slots <- choose (0, 10)
    keys <- replicateM 3 arbitrary
    let signers = zipWith PBftState.PBftSigner (map SlotNo [0 .. slots]) (cycle keys)
    return $ PBftState.fromList signers

instance Arbitrary (SomeResult ByronBlock) where
  arbitrary = SomeResult GetUpdateInterfaceState <$> arbitrary

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have to be careful about headers with ByronNodeToNodeVersion1,
-- where we will have a fake block size hint.
instance Arbitrary (WithVersion ByronNodeToNodeVersion (Header ByronBlock)) where
  arbitrary = do
    version <- arbitrary
    hdr <- arbitrary
    let hdr' = case version of
          ByronNodeToNodeVersion1 ->
            hdr{byronHeaderBlockSizeHint = fakeByronBlockSizeHint}
          ByronNodeToNodeVersion2 ->
            hdr
    return (WithVersion version hdr')

instance Arbitrary (WithVersion ByronNodeToNodeVersion (SomeSecond (NestedCtxt Header) ByronBlock)) where
  arbitrary = do
    version <- arbitrary
    size <- case version of
      ByronNodeToNodeVersion1 -> return fakeByronBlockSizeHint
      ByronNodeToNodeVersion2 -> SizeInBytes <$> arbitrary
    ctxt <-
      elements
        [ SomeSecond . NestedCtxt $ CtxtByronRegular size
        , SomeSecond . NestedCtxt $ CtxtByronBoundary size
        ]
    return (WithVersion version ctxt)
