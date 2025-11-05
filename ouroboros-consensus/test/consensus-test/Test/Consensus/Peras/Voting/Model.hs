{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Peras.Voting.Model
  ( isPerasVotingAllowed
  ) where

import Ouroboros.Consensus.Block.Abstract
  ( SlotNo (..)
  , WithOrigin (..)
  , succWithOrigin
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , onPerasRoundNo
  )
import Ouroboros.Consensus.Peras.Params
  ( PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Peras.Voting (PerasVotingView (..))

-- | A simplified model of the Peras voting rules, used to compare against the
-- real implementation. The main difference is that this model computes the
-- result of the predicate directly over the inputs, rather than using the
-- 'Pred' combinators to produce evidence in either direction.
--
-- NOTE: this predicate could be lifted directly from the agda specification.
isPerasVotingAllowed ::
  HasPerasCertRound a =>
  PerasVotingView a ->
  ( -- Should we vote according to the model?
    Bool
  , -- The individual voting rules that were applied
    (Bool, Bool, Bool, Bool)
  )
isPerasVotingAllowed
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertSeen
    , latestCertOnChain
    , certRoundStart
    , arrivalSlot
    , candidateExtendsCert
    } =
    ( vr1a && vr1b || vr2a && vr2b
    , (vr1a, vr1b, vr2a, vr2b)
    )
   where
    vr1a =
      vr1a1 && vr1a2
    vr1a1 =
      currRoundNo == succWithOrigin (getPerasCertRound <$> latestCertSeen)
    vr1a2 =
      case latestCertSeen of
        NotOrigin cert ->
          arrivalSlot cert <= certRoundStart cert + _X
        Origin -> True
    vr1b =
      case latestCertSeen of
        NotOrigin cert -> candidateExtendsCert cert
        Origin -> True
    vr2a =
      (succWithOrigin (getPerasCertRound <$> latestCertSeen) - 1 + _R)
        <= currRoundNo

    vr2b =
      case latestCertOnChain of
        NotOrigin cert ->
          (currRoundNo > getPerasCertRound cert)
            && (currRoundNo `rmod` _K == getPerasCertRound cert `rmod` _K)
        Origin -> currRoundNo `rmod` _K == _K - 1

    _X =
      SlotNo $
        unPerasCertArrivalThreshold $
          perasCertArrivalThreshold $
            perasParams
    _R =
      PerasRoundNo $
        unPerasIgnoranceRounds $
          perasIgnoranceRounds $
            perasParams
    _K =
      PerasRoundNo $
        unPerasCooldownRounds $
          perasCooldownRounds $
            perasParams

    rmod = onPerasRoundNo mod
