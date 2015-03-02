module Learning.ManyHypothesis where

import Planning.PDDL
import Planning.PDDL.Logic()
import Learning

data HypBox =
  forall hyp. (DomainHypothesis hyp PDDLDomain PDDLProblem ActionSpec) => HypBox [hyp]

newtype ManyHypothesis = ManyHypothesis [HypBox]

instance DomainHypothesis ManyHypothesis PDDLDomain PDDLProblem ActionSpec where
      update (ManyHypothesis (hyps)) domain trans  =
        let updater dom t hyp  =  update hyp dom t
         in ManyHypothesis (  map ((updater domain trans)) hyps )
      adjustDomain (ManyHypothesis hyps) dom  = foldl (flip adjustDomain) dom hyps

      fromDomain _ = error "Cant make a ManyHypothesis from a domain, as Hypothesises are unknown"
--
-- initialHypothesis :: PDDLDomain ->  OptimisticHypothesis
-- initialHypothesis dom =  OptimisticHypothesis
--                             ( Pre.initialPreDomainHyp dom
--                             , Eff.initialHypothesis dom)
