module Learning.ManyHypothesis where

import Planning.PDDL
import Planning.PDDL.Logic()
import Learning

data HypBox =
  forall hyp. (DomainHypothesis hyp PDDLDomain PDDLProblem ActionSpec, Show hyp) => HypBox hyp

instance Show HypBox where
  show (HypBox hyp) = show hyp

newtype ManyHypothesis = ManyHypothesis [HypBox] deriving (Show)

instance DomainHypothesis ManyHypothesis PDDLDomain PDDLProblem ActionSpec where
      update (ManyHypothesis (hyps)) domain trans  =
        let updater dom t (HypBox hyp)  = HypBox (update hyp dom t)
         in ManyHypothesis (  map ((updater domain trans)) hyps )
      adjustDomain (ManyHypothesis hyps) dom  =
          let folder (HypBox hyp) d = adjustDomain hyp d
           in foldl (flip folder) dom hyps

      fromDomain _ = error "Cant make a ManyHypothesis from a domain, as Hypothesises are unknown"
--
-- initialHypothesis :: PDDLDomain ->  OptimisticHypothesis
-- initialHypothesis dom =  OptimisticHypothesis
--                             ( Pre.initialPreDomainHyp dom
--                             , Eff.initialHypothesis dom)
