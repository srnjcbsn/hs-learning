module Environments.Sokoban.Runner where

import Planning.FastDownward
import PDDL.Type
import PDDL.Logic hiding (applyAction)
import Learning.OptPrecondLearn
import Learning.OptEffectLearn
import Environments.Sokoban.Sokoban
import Environments.Sokoban.PDDL

visualize :: SokobanPDDL -> IO ()
visualize = undefined

runSokoban :: ExtPlanner ep => ep
           -> Domain
           -> [Problem]
           -> DomainHypothesis
           -> PreDomainHypothesis
           -> SokobanPDDL
           -> Problem
           -> Plan
           -> [IO ()]
runSokoban planner domain (p:ps) effectHyp precondHyp env curP curPlan =
  let s = probInitialState p
      uptDom = (`domainFromKnowledge` effectHyp) . (`domainFromPrecondHypothesis` precondHyp)
      actions =
        do
          maybePlan <-  if null curPlan
                        then makePlan planner (uptDom domain) curP
                        else return $ Just curPlan
          case maybePlan of
            Just (act:acts) ->
              let maybeEnv' = applyAction env act
                  s' = do env' <- maybeEnv'
                          return $ toState env'
                  trans = (s, act, s')
                  effectHyp' = updateDomainHyp domain effectHyp trans
                  precondHyp' = updatePreDomainHyp domain precondHyp trans

              in return undefined
            Nothing -> return undefined


   in undefined
runSokoban _ _ [] _ _ _ _ _= []
