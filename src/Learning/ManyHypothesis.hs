module Learning.ManyHypothesis where

import Planning.PDDL
import Planning.PDDL.Logic ()
import Planning.Loggable

import qualified Learning.SchemaLearning as Lrn

import Data.Typeable

data HypBox =
  forall hyp. ( Lrn.DomainHypothesis hyp PDDLDomain PDDLProblem ActionSpec
              , Show hyp
              , Eq hyp
              , Typeable hyp
              ) => HypBox hyp

instance Show HypBox where
    show (HypBox hyp) = show hyp

instance Eq HypBox where
    HypBox a == HypBox b =
        case cast a of Just a' -> a' == b
                       Nothing -> False

newtype ManyHypothesis = ManyHypothesis [HypBox] deriving (Show, Eq)

instance Loggable ManyHypothesis where
    logg = undefined
    chart = undefined

instance Lrn.DomainHypothesis ManyHypothesis PDDLDomain PDDLProblem ActionSpec where
      update (ManyHypothesis hyps) domain trans  =
        let updater dom t (HypBox hyp)  = HypBox (Lrn.update hyp dom t)
         in ManyHypothesis (map (updater domain trans) hyps )
      adjustDomain (ManyHypothesis hyps) dom  =
          let folder (HypBox hyp) d = Lrn.adjustDomain hyp d
           in foldl (flip folder) dom hyps

      fromDomain _ = error "Cant make a ManyHypothesis from a domain, as Hypothesises are unknown"
