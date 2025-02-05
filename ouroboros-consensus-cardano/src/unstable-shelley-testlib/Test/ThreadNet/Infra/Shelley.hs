{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ThreadNet.Infra.Shelley (
    CoreNode (..)
  , CoreNodeKeyInfo (..)
  , DecentralizationParam (..)
  , KesConfig (..)
  , coreNodeKeys
  , genCoreNode
  , incrementMinorProtVer
  , initialLovelacePerCoreNode
  , mkCredential
  , mkEpochSize
  , mkGenesisConfig
  , mkKesConfig
  , mkKeyHash
  , mkKeyHashVrf
  , mkKeyPair
  , mkLeaderCredentials
  , mkMASetDecentralizationParamTxs
  , mkProtocolShelley
  , mkSetDecentralizationParamTxs
  , mkVerKey
  , networkId
  , tpraosSlotLength
  ) where

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignKeyDSIGN,
                     seedSizeDSIGN)
import           Cardano.Crypto.KES (KESAlgorithm (..), UnsoundPureSignKeyKES,
                     seedSizeKES, unsoundPureDeriveVerKeyKES,
                     unsoundPureGenKeyKES)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Seed as Cardano.Crypto
import           Cardano.Crypto.VRF (SignKeyVRF, deriveVerKeyVRF, genKeyVRF,
                     seedSizeVRF)
import qualified Cardano.Ledger.Allegra.Scripts as SL
import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.BaseTypes (boundRational)
import           Cardano.Ledger.BaseTypes.NonZero (nonZeroOr)
import           Cardano.Ledger.Hashes (EraIndependentTxBody,
                     HashAnnotated (..), SafeHash, hashAnnotated)
import qualified Cardano.Ledger.Keys as LK
import qualified Cardano.Ledger.Mary.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Val as SL
import           Cardano.Protocol.Crypto (Crypto, KES, VRF, hashVerKeyVRF)
import           Cardano.Protocol.TPraos.OCert
                     (OCert (ocertKESPeriod, ocertN, ocertSigma, ocertVkHot))
import qualified Cardano.Protocol.TPraos.OCert as SL (KESPeriod, OCert (OCert),
                     OCertSignable (..))
import           Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.ListMap (ListMap (ListMap))
import qualified Data.ListMap as ListMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (maybeToStrictMaybe)
import           Data.Ratio (denominator, numerator)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Lens.Micro
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (PraosCanBeLeader (PraosCanBeLeader),
                     praosCanBeLeaderColdVerKey, praosCanBeLeaderOpCert,
                     praosCanBeLeaderSignKeyVRF)
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger (GenTx (..),
                     ShelleyBasedEra, ShelleyBlock, ShelleyCompatible,
                     mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike
import           Quiet (Quiet (..))
import qualified Test.Cardano.Ledger.Core.KeyPair as TL (KeyPair (..),
                     mkWitnessesVKey)
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Gen
import           Test.Cardano.Ledger.Shelley.Utils (unsafeBoundRational)
import           Test.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time (dawnOfTime)

{-------------------------------------------------------------------------------
  The decentralization parameter
-------------------------------------------------------------------------------}

-- | A suitable value for the @d@ protocol parameter
--
-- In the range @0@ to @1@, inclusive. Beware the misnomer: @0@ means fully
-- decentralized, and @1@ means fully centralized.
newtype DecentralizationParam =
    DecentralizationParam {decentralizationParamToRational :: Rational }
  deriving (Eq, Generic, Ord)
  deriving (Show) via (Quiet DecentralizationParam)

-- | A fraction with denominator @10@ and numerator @0@ to @10@ inclusive
instance Arbitrary DecentralizationParam where
  arbitrary = do
      let d = 10
      n <- choose (0, d)
      pure $ DecentralizationParam $ fromInteger n / fromInteger d

{-------------------------------------------------------------------------------
  Important constants
-------------------------------------------------------------------------------}

tpraosSlotLength :: SlotLength
tpraosSlotLength = slotLengthFromSec 2

{-------------------------------------------------------------------------------
  CoreNode secrets/etc
-------------------------------------------------------------------------------}

data CoreNode c = CoreNode {
      cnGenesisKey  :: !(SignKeyDSIGN LK.DSIGN)
    , cnDelegateKey :: !(SignKeyDSIGN LK.DSIGN)
      -- ^ Cold delegate key. The hash of the corresponding verification
      -- (public) key will be used as the payment credential.
    , cnStakingKey  :: !(SignKeyDSIGN LK.DSIGN)
      -- ^ The hash of the corresponding verification (public) key will be
      -- used as the staking credential.
    , cnVRF         :: !(SignKeyVRF   (VRF c))
    , cnKES         :: !(UnsoundPureSignKeyKES (KES c))
    , cnOCert       :: !(SL.OCert        c)
    }

data CoreNodeKeyInfo c = CoreNodeKeyInfo
  { cnkiKeyPair
      ::  ( TL.KeyPair 'SL.Payment
          , TL.KeyPair 'SL.Staking
          )
  , cnkiCoreNode ::
      ( TL.KeyPair 'SL.Genesis
      , Gen.AllIssuerKeys c 'SL.GenesisDelegate
      )
  }

coreNodeKeys :: CoreNode c -> CoreNodeKeyInfo c
coreNodeKeys CoreNode{cnGenesisKey, cnDelegateKey, cnStakingKey} =
    CoreNodeKeyInfo {
        cnkiCoreNode =
          ( mkKeyPair cnGenesisKey
          , Gen.AllIssuerKeys
            { Gen.aikCold        = mkKeyPair cnDelegateKey
              -- 'CoreNodeKeyInfo' is used for all sorts of generators, not
              -- only transaction generators. To generate transactions we
              -- don't need all these keys, hence the 'error's.
            , Gen.aikVrf         = error "vrf used while generating transactions"
            , Gen.aikHot         = error "hot used while generating transactions"
            , Gen.aikColdKeyHash = error "hk used while generating transactions"
            }
          )
      , cnkiKeyPair = (mkKeyPair cnDelegateKey, mkKeyPair cnStakingKey)
      }

genCoreNode ::
     forall c. PraosCrypto c
  => SL.KESPeriod
  -> Gen (CoreNode c)
genCoreNode startKESPeriod = do
    genKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @LK.DSIGN))
    delKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @LK.DSIGN))
    stkKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @LK.DSIGN))
    vrfKey <- genKeyVRF   <$> genSeed (seedSizeVRF   (Proxy @(VRF   c)))
    kesKey <- unsoundPureGenKeyKES <$> genSeed (seedSizeKES   (Proxy @(KES   c)))
    let kesPub = unsoundPureDeriveVerKeyKES kesKey
        sigma  = LK.signedDSIGN
          delKey
          (SL.OCertSignable kesPub certificateIssueNumber startKESPeriod)
    let ocert = SL.OCert {
            ocertVkHot     = kesPub
          , ocertN         = certificateIssueNumber
          , ocertKESPeriod = startKESPeriod
          , ocertSigma     = sigma
          }
    return CoreNode {
        cnGenesisKey  = genKey
      , cnDelegateKey = delKey
      , cnStakingKey  = stkKey
      , cnVRF         = vrfKey
      , cnKES         = kesKey
      , cnOCert       = ocert
      }
  where
    certificateIssueNumber = 0

    genBytes :: Integral a => a -> Gen BS.ByteString
    genBytes nbBytes = BS.pack <$> vectorOf (fromIntegral nbBytes) arbitrary

    genSeed :: Integral a => a -> Gen Cardano.Crypto.Seed
    genSeed = fmap mkSeedFromBytes . genBytes

mkLeaderCredentials :: CoreNode c -> ShelleyLeaderCredentials c
mkLeaderCredentials CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    ShelleyLeaderCredentials {
        shelleyLeaderCredentialsInitSignKey = cnKES
      , shelleyLeaderCredentialsCanBeLeader = PraosCanBeLeader {
          praosCanBeLeaderOpCert     = cnOCert
        , praosCanBeLeaderColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , praosCanBeLeaderSignKeyVRF = cnVRF
        }
      , shelleyLeaderCredentialsLabel       = "ThreadNet"
      }

{-------------------------------------------------------------------------------
  KES configuration
-------------------------------------------------------------------------------}

-- | Currently @'maxEvolutions' * 'slotsPerEvolution'@ is the max number of
-- slots the test can run without needing new ocerts.
--
-- TODO This limitation may be lifted by PR #2107, see
-- <https://github.com/IntersectMBO/ouroboros-network/issues/2107>.
data KesConfig = KesConfig
  { maxEvolutions     :: Word64
  , slotsPerEvolution :: Word64
  }

-- | A 'KesConfig' that will not require more evolutions than this test's crypto
-- allows.
mkKesConfig ::
     forall proxy c. Crypto c
  => proxy c -> NumSlots -> KesConfig
mkKesConfig _ (NumSlots t) = KesConfig
    { maxEvolutions
    , slotsPerEvolution = divCeiling t maxEvolutions
    }
  where
    maxEvolutions = fromIntegral $ totalPeriodsKES (Proxy @(KES c))

    -- | Like 'div', but rounds-up.
    divCeiling :: Integral a => a -> a -> a
    divCeiling n d = q + min 1 r
      where
        (q, r) = quotRem n d

{-------------------------------------------------------------------------------
  TPraos node configuration
-------------------------------------------------------------------------------}

-- | The epoch size, given @k@ and @f@.
--
-- INVARIANT: @10 * k / f@ must be a whole number.
mkEpochSize :: SecurityParam -> Rational -> EpochSize
mkEpochSize (SecurityParam k) f =
    if r /= 0 then error "10 * k / f must be a whole number" else
    EpochSize q
  where
    n = numerator   f
    d = denominator f

    (q, r) = quotRem (10 * k * fromInteger d) (fromInteger n)

-- | Note: a KES algorithm supports a particular max number of KES evolutions,
-- but we can configure a potentially lower maximum for the ledger, that's why
-- we take it as an argument.
mkGenesisConfig ::
     forall c. PraosCrypto c
  => ProtVer   -- ^ Initial protocol version
  -> SecurityParam
  -> Rational  -- ^ Initial active slot coefficient
  -> DecentralizationParam
  -> Word64
     -- ^ Max Lovelace supply, must be >= #coreNodes * initialLovelacePerCoreNode
  -> SlotLength
  -> KesConfig
  -> [CoreNode c]
  -> ShelleyGenesis
mkGenesisConfig pVer k f d maxLovelaceSupply slotLength kesCfg coreNodes =
    assertWithMsg checkMaxLovelaceSupply $
    ShelleyGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = dawnOfTime
    , sgNetworkMagic          = 0
    , sgNetworkId             = networkId
    , sgActiveSlotsCoeff      = unsafeBoundRational f
    , sgSecurityParam         = nonZeroOr (maxRollbacks k) $ error "The security parameter cannot be zero."
    , sgEpochLength           = mkEpochSize k f
    , sgSlotsPerKESPeriod     = slotsPerEvolution kesCfg
    , sgMaxKESEvolutions      = maxEvolutions     kesCfg
    , sgSlotLength            = SL.toNominalDiffTimeMicroWithRounding $ getSlotLength slotLength
    , sgUpdateQuorum          = quorum
    , sgMaxLovelaceSupply     = maxLovelaceSupply
    , sgProtocolParams        = pparams
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = ListMap.fromMap initialFunds
    , sgStaking               = initialStake
    }
  where
    checkMaxLovelaceSupply :: Either String ()
    checkMaxLovelaceSupply
      | maxLovelaceSupply >=
        fromIntegral (length coreNodes) * initialLovelacePerCoreNode
      = return ()
      | otherwise
      = throwError $ unwords [
            "Lovelace supply ="
          , show maxLovelaceSupply
          , "but must be at least"
          , show (fromIntegral (length coreNodes) * initialLovelacePerCoreNode)
          ]

    quorum :: Word64
    quorum = nbCoreNodes `min` ((nbCoreNodes `div` 2) + 1)
      where
        nbCoreNodes = fromIntegral (length coreNodes)

    pparams :: SL.PParams ShelleyEra
    pparams = SL.emptyPParams
      & SL.ppDL               .~
          unsafeBoundRational (decentralizationParamToRational d)
      & SL.ppMaxBBSizeL       .~ 10000 -- TODO
      & SL.ppMaxBHSizeL       .~ 1000 -- TODO
      & SL.ppProtocolVersionL .~ pVer

    coreNodesToGenesisMapping ::
         Map (SL.KeyHash 'SL.Genesis) SL.GenDelegPair
    coreNodesToGenesisMapping  = Map.fromList
      [ let
          gkh :: SL.KeyHash 'SL.Genesis
          gkh = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey

          gdpair :: SL.GenDelegPair
          gdpair = SL.GenDelegPair
              (SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey)
              (hashVerKeyVRF @c $ deriveVerKeyVRF cnVRF)

        in (gkh, gdpair)
      | CoreNode { cnGenesisKey, cnDelegateKey, cnVRF } <- coreNodes
      ]

    initialFunds :: Map SL.Addr SL.Coin
    initialFunds = Map.fromList
      [ (addr, coin)
      | CoreNode { cnDelegateKey, cnStakingKey } <- coreNodes
      , let addr = SL.Addr networkId
                           (mkCredential cnDelegateKey)
                           (SL.StakeRefBase (mkCredential cnStakingKey))
            coin = SL.Coin $ fromIntegral initialLovelacePerCoreNode
      ]

    -- In this initial stake, each core node delegates its stake to itself.
    initialStake :: ShelleyGenesisStaking
    initialStake = ShelleyGenesisStaking
      { sgsPools = ListMap
          [ (pk, pp)
          | pp@SL.PoolParams { ppId = pk } <- Map.elems coreNodeToPoolMapping
          ]
        -- The staking key maps to the key hash of the pool, which is set to the
        -- "delegate key" in order that nodes may issue blocks both as delegates
        -- and as stake pools.
      , sgsStake = ListMap
          [ ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnStakingKey
            , SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            )
          | CoreNode {cnDelegateKey, cnStakingKey} <- coreNodes
          ]
      }
      where
        coreNodeToPoolMapping ::
             Map (SL.KeyHash 'SL.StakePool) SL.PoolParams
        coreNodeToPoolMapping = Map.fromList [
              ( SL.hashKey . SL.VKey . deriveVerKeyDSIGN $ cnStakingKey
              , SL.PoolParams
                { SL.ppId = poolHash
                , SL.ppVrf = vrfHash
                  -- Each core node pledges its full stake to the pool.
                , SL.ppPledge = SL.Coin $ fromIntegral initialLovelacePerCoreNode
                , SL.ppCost = SL.Coin 1
                , SL.ppMargin = minBound
                  -- Reward accounts live in a separate "namespace" to other
                  -- accounts, so it should be fine to use the same address.
                , SL.ppRewardAccount = SL.RewardAccount networkId $ mkCredential cnDelegateKey
                , SL.ppOwners = Set.singleton poolOwnerHash
                , SL.ppRelays = Seq.empty
                , SL.ppMetadata = SL.SNothing
                }
              )
            | CoreNode { cnDelegateKey, cnStakingKey, cnVRF } <- coreNodes
              -- The pool and owner hashes are derived from the same key, but
              -- use different hashing schemes
            , let poolHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let poolOwnerHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let vrfHash = hashVerKeyVRF @c $ deriveVerKeyVRF cnVRF
            ]

mkProtocolShelley ::
     forall m c.
     (IOLike m, PraosCrypto c, ShelleyCompatible (TPraos c) ShelleyEra)
  => ShelleyGenesis
  -> SL.Nonce
  -> ProtVer
  -> CoreNode c
  -> ( ProtocolInfo (ShelleyBlock (TPraos c) ShelleyEra)
     , m [BlockForging m (ShelleyBlock (TPraos c) ShelleyEra)]
     )
mkProtocolShelley genesis initialNonce protVer coreNode =
    protocolInfoShelley
      genesis
      ProtocolParamsShelleyBased {
          shelleyBasedInitialNonce      = initialNonce
        , shelleyBasedLeaderCredentials = [mkLeaderCredentials coreNode]
        }
      protVer
{-------------------------------------------------------------------------------
  Necessary transactions for updating the 'DecentralizationParam'
-------------------------------------------------------------------------------}

incrementMinorProtVer :: SL.ProtVer -> SL.ProtVer
incrementMinorProtVer (SL.ProtVer major minor) = SL.ProtVer major (succ minor)

mkSetDecentralizationParamTxs ::
     forall c. (ShelleyBasedEra ShelleyEra)
  => [CoreNode c]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (ShelleyBlock (TPraos c) ShelleyEra)]
mkSetDecentralizationParamTxs coreNodes pVer ttl dNew =
    (:[]) $
    mkShelleyTx $
    SL.mkBasicTx body & SL.witsTxL .~ witnesses
  where
    -- The funds touched by this transaction assume it's the first transaction
    -- executed.
    scheduledEpoch :: EpochNo
    scheduledEpoch = EpochNo 0


    witnesses :: SL.TxWits ShelleyEra
    witnesses = SL.mkBasicTxWits & SL.addrTxWitsL .~ signatures

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: Set (SL.WitVKey 'SL.Witness)
    signatures =
        TL.mkWitnessesVKey
          (hashAnnotated body)
          [ TL.KeyPair (SL.VKey vk) sk
          | cn <- coreNodes
          , let sk = cnDelegateKey cn
          , let vk = deriveVerKeyDSIGN sk
          ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody ShelleyEra
    body = SL.mkBasicTxBody
         & SL.inputsTxBodyL  .~ Set.singleton (fst touchCoins)
         & SL.outputsTxBodyL .~ Seq.singleton (snd touchCoins)
         & SL.ttlTxBodyL     .~ ttl
         & SL.updateTxBodyL .~ SL.SJust update

    -- Every Shelley transaction requires one input.
    --
    -- We use the input of the first node, but we just put it all right back.
    --
    -- ASSUMPTION: This transaction runs in the first slot.
    touchCoins :: (SL.TxIn, SL.TxOut ShelleyEra)
    touchCoins = case coreNodes of
        []   -> error "no nodes!"
        cn:_ ->
            ( SL.initialFundsPseudoTxIn addr
            , SL.ShelleyTxOut addr coin
            )
          where
            addr = SL.Addr networkId
                (mkCredential (cnDelegateKey cn))
                (SL.StakeRefBase (mkCredential (cnStakingKey cn)))
            coin = SL.Coin $ fromIntegral initialLovelacePerCoreNode

    -- One replicant of the parameter update per each node.
    update :: SL.Update ShelleyEra
    update =
        flip SL.Update scheduledEpoch $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $ deriveVerKeyDSIGN $ cnGenesisKey cn
          , SL.emptyPParamsUpdate
            & SL.ppuDL .~ (maybeToStrictMaybe $
                           boundRational $
                           decentralizationParamToRational dNew)
            & SL.ppuProtocolVersionL .~ SL.SJust pVer
          )
        | cn <- coreNodes
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

initialLovelacePerCoreNode :: Word64
initialLovelacePerCoreNode = 1000000

mkCredential :: SignKeyDSIGN LK.DSIGN -> SL.Credential r
mkCredential = SL.KeyHashObj . mkKeyHash

mkKeyHash :: SignKeyDSIGN LK.DSIGN -> SL.KeyHash r
mkKeyHash = SL.hashKey . mkVerKey

mkVerKey :: SignKeyDSIGN LK.DSIGN -> SL.VKey r
mkVerKey = SL.VKey . deriveVerKeyDSIGN

mkKeyPair :: SignKeyDSIGN LK.DSIGN -> TL.KeyPair r
mkKeyPair sk = TL.KeyPair { vKey = mkVerKey sk, sKey = sk }

mkKeyHashVrf :: forall c r. Crypto c => SignKeyVRF (VRF c) -> LK.VRFVerKeyHash (r :: LK.KeyRoleVRF)
mkKeyHashVrf = hashVerKeyVRF @c . deriveVerKeyVRF

networkId :: SL.Network
networkId = SL.Testnet

{-------------------------------------------------------------------------------
  Temporary Workaround
-------------------------------------------------------------------------------}

-- | TODO This is a copy-paste-edit of 'mkSetDecentralizationParamTxs'
--
-- Our current plan is to replace all of this infrastructure with the ThreadNet
-- rewrite; so we're minimizing the work and maintenance here for now.
mkMASetDecentralizationParamTxs ::
     forall proto era.
     ( ShelleyBasedEra era
     , SL.AllegraEraTxBody era
     , SL.ShelleyEraTxBody era
     , SL.AtMostEra AlonzoEra era
     )
  => [CoreNode (ProtoCrypto proto)]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (ShelleyBlock proto era)]
mkMASetDecentralizationParamTxs coreNodes pVer ttl dNew =
    (:[]) $
    mkShelleyTx $
    SL.mkBasicTx body & SL.witsTxL .~ witnesses
  where
    -- The funds touched by this transaction assume it's the first transaction
    -- executed.
    scheduledEpoch :: EpochNo
    scheduledEpoch = EpochNo 0

    witnesses :: SL.TxWits era
    witnesses = SL.mkBasicTxWits & SL.addrTxWitsL .~ signatures

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: Set (SL.WitVKey 'SL.Witness)
    signatures =
        TL.mkWitnessesVKey
          (eraIndTxBodyHash' body)
          [ TL.KeyPair (SL.VKey vk) sk
          | cn <- coreNodes
          , let sk = cnDelegateKey cn
          , let vk = deriveVerKeyDSIGN sk
          ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody era
    body = SL.mkBasicTxBody
      & SL.inputsTxBodyL .~ inputs
      & SL.outputsTxBodyL .~ outputs
      & SL.vldtTxBodyL .~ vldt
      & SL.updateTxBodyL .~ update'
      where
        inputs   = Set.singleton (fst touchCoins)
        outputs  = Seq.singleton (snd touchCoins)
        vldt     = SL.ValidityInterval {
                   invalidBefore    = SL.SNothing
                 , invalidHereafter = SL.SJust ttl
                 }
        update'  = SL.SJust update

    -- Every Shelley transaction requires one input.
    --
    -- We use the input of the first node, but we just put it all right back.
    --
    -- ASSUMPTION: This transaction runs in the first slot.
    touchCoins :: (SL.TxIn, SL.TxOut era)
    touchCoins = case coreNodes of
        []   -> error "no nodes!"
        cn:_ ->
            ( SL.initialFundsPseudoTxIn addr
            , SL.mkBasicTxOut addr coin
            )
          where
            addr = SL.Addr networkId
                (mkCredential (cnDelegateKey cn))
                (SL.StakeRefBase (mkCredential (cnStakingKey cn)))
            coin = SL.inject $ SL.Coin $ fromIntegral initialLovelacePerCoreNode

    -- One replicant of the parameter update per each node.
    update :: SL.Update era
    update =
        flip SL.Update scheduledEpoch $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $ deriveVerKeyDSIGN $ cnGenesisKey cn
          , SL.emptyPParamsUpdate
            & SL.ppuDL .~ (maybeToStrictMaybe $
                           boundRational $
                           decentralizationParamToRational dNew)
            & SL.ppuProtocolVersionL .~ SL.SJust pVer
          )
        | cn <- coreNodes
        ]

eraIndTxBodyHash' ::
     HashAnnotated body EraIndependentTxBody
  => body
  -> SafeHash
       EraIndependentTxBody
eraIndTxBodyHash' = coerce . hashAnnotated
