{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.HardFork.Combinator.Node.Peras
  (
  ) where

import Data.Proxy (Proxy (..))
import Data.SOP (K (..), NS (..), I (I), NP ((:*)))
import Data.SOP.Strict (HCollapse (..), hcmap)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..), Point (..), HeaderHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , IsPerasVote (..)
  , PerasCert
  , PerasCertForgeErr (..)
  , PerasCertValidationErr (..)
  , PerasVote
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , lookupPerasVoteStake
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
  ( SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraHash (..)
  , OneEraPerasCert (..)
  , OneEraPerasVote (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock (..))
import Ouroboros.Consensus.HardFork.Combinator.Block ()
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId (..)
  , PerasVoteTarget (..)
  , stakeAboveThreshold
  )
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapPerasCert (..)
  , WrapPerasVote (..)
  )
import Ouroboros.Consensus.Peras.Vote qualified as Base
import Ouroboros.Consensus.Peras.Cert qualified as Base
import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor.Compose (Compose)

{-------------------------------------------------------------------------------
  Peras
-------------------------------------------------------------------------------}

sameEraTraverse :: Traversable t => t (NS f xs) -> Maybe (NS (Compose t f) xs)
sameEraTraverse = undefined

newtype ZipFunctor f g x = ZipFunctor { unZipFunctor :: (f x, g x) }
type family LZip xs ys where
  LZip '[] _ = '[]
  LZip _ '[] = '[]
  LZip (x ': xs) (y ': ys) = (x, y) ': LZip xs ys

sameEraZip :: NS f xs -> NS g ys -> Maybe (NS (ZipFunctor f g) (LZip xs ys))
sameEraZip = undefined

type instance HeaderHash (NS I '[]) = NS I '[]
type instance HeaderHash (NS I (x ': xs)) = AppendNS (HeaderHash x) (HeaderHash (NS I xs))

instance BlockSupportsPeras x => BlockSupportsPeras (NS I '[x]) where
  type PerasCfg (NS I '[x]) = NP I '[PerasCfg x]
  type PerasCert (NS I '[x]) = NS I '[PerasCert x]
  type ValidatedPerasCert (NS I '[x]) = NS I '[ValidatedPerasCert x]
  type PerasVote (NS I '[x]) = NS I '[PerasVote x]
  type ValidatedPerasVote (NS I '[x]) = NS I '[ValidatedPerasVote x]
  type PerasCertValidationErr (NS I '[x]) = NS I '[PerasCertValidationErr x]
  type PerasCertForgeErr (NS I '[x]) = NS I '[PerasCertForgeErr x]
  type PerasVoteValidationErr (NS I '[x]) = NS I '[PerasVoteValidationErr x]
  type PerasVoteForgeErr (NS I '[x]) = NS I '[PerasVoteForgeErr x]

  validatePerasCert (I params :* _) (Z (I cert)) = bimap (Z . I) (Z . I) $ validatePerasCert params cert
  validatePerasCert _params (S impossible) = case impossible of {}

  forgePerasCert (I (params :: PerasCfg x) :* _) roundNo block votes = case (do
      votes' <- sameEraTraverse votes
      case block of
        GenesisPoint -> Just (GenesisPoint, votes')
        BlockPoint s (h :: NS I '[HeaderHash x]) -> case sameEraZip votes' h of
          
          
           (fmap (BlockPoint s)) <$> 
    ) of
    Just (Z (votes' :: [ValidatedPerasVote x])) ->
      let res :: Either (PerasCertForgeErr x) (ValidatedPerasCert x) = forgePerasCert params roundNo block votes' in
      bimap (Z . I) (Z . I) res
    Just (S impossible) -> case impossible of {}
    Nothing -> error "forgePerasCert: votes from multiple eras" -- TODO: replace with concrete forge error


  validatePerasVote _params _distr vote =
    case vote of {}

  forgePerasVote _params _roundNo _block =
    undefined

type family AppendNS x nsixs where
  AppendNS x (NS I xs) = NS I (x ': xs)

type family AppendNP x npixs where
  AppendNP x (NP I xs) = NP I (x ': xs)

instance (BlockSupportsPeras x,
 BlockSupportsPeras (NS I (x' ': xs))) => BlockSupportsPeras (NS I (x ': (x' ': xs))) where
  type PerasCfg (NS I (x ': (x' ': xs))) = AppendNP (PerasCfg x) (PerasCfg (NP I (x' ': xs)))
  type PerasCert (NS I (x ': (x' ': xs))) = AppendNS (PerasCert x) (PerasCert (NS I (x' ': xs)))
  type PerasVote (NS I (x ': (x' ': xs))) = AppendNS (PerasVote x) (PerasVote (NS I (x' ': xs)))
  type PerasCertValidationErr (NS I (x ': (x' ': xs))) = AppendNS (PerasCertValidationErr x) (PerasCertValidationErr (NS I (x' ': xs)))
  type PerasCertForgeErr (NS I (x ': (x' ': xs))) = AppendNS (PerasCertForgeErr x) (PerasCertForgeErr (NS I (x' ': xs)))
  type PerasVoteValidationErr (NS I (x ': (x' ': xs))) = AppendNS (PerasVoteValidationErr x) (PerasVoteValidationErr (NS I (x' ': xs)))
  type PerasVoteForgeErr (NS I (x ': (x' ': xs))) = AppendNS (PerasVoteForgeErr x) (PerasVoteForgeErr (NS I (x' ': xs)))

  validatePerasCert params cert = undefined


instance CanHardFork xs => IsPerasCert (HardForkBlock xs) (OneEraPerasCert xs) where
  getPerasCertRound (OneEraPerasCert hcert) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasCertRound
            . unwrapPerasCert
        )
      $ hcert

  getPerasCertBoostedBlock (OneEraPerasCert hcert) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . injectHardForkPoint
            . getPerasCertBoostedBlock
            . unwrapPerasCert
        )
      $ hcert

instance CanHardFork xs => IsPerasVote (HardForkBlock xs) (OneEraPerasVote xs) where
  getPerasVoteRound (OneEraPerasVote hvote) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasVoteRound
            . unwrapPerasVote
        )
      $ hvote

  getPerasVoteVoterId (OneEraPerasVote hvote) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasVoteVoterId
            . unwrapPerasVote
        )
      $ hvote

  getPerasVoteTarget (OneEraPerasVote hvote) =
    PerasVoteTarget
      { pvtRoundNo =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteRound
                  . unwrapPerasVote
              )
            $ hvote
      , pvtBlock =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . injectHardForkPoint
                  . pvtBlock
                  . getPerasVoteTarget
                  . unwrapPerasVote
              )
            $ hvote
      }

  getPerasVoteId (OneEraPerasVote hvote) =
    PerasVoteId
      { pviRoundNo =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteRound
                  . unwrapPerasVote
              )
            $ hvote
      , pviVoterId =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteVoterId
                  . unwrapPerasVote
              )
            $ hvote
      }

-- | Inject a 'Point' from a single era into a 'Point' of the hard fork block.
injectHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point blk ->
  Point (HardForkBlock xs)
injectHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s h ->
    BlockPoint s (OneEraHash (toShortRawHash (Proxy @blk) h))
