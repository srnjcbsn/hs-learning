module Learning where

import PDDL.Type
import qualified Learning.OptEffectLearn as Eff
import qualified Learning.OptPrecondLearn as Pre
import qualified Environment as Env
import Environment (Environment)
import Learning.OptPrecondLearn (PreDomainHypothesis)
import Learning.OptEffectLearn (DomainHypothesis)
import Data.Maybe
import Planning
import Control.Monad

refine  :: ExternalPlanner ep
        => ep
        -> Domain
        -> Problem
        -> PreDomainHypothesis
        -> DomainHypothesis
        -> Maybe Plan
        -> IO (Maybe Plan)
refine planner domain problem precondHyp effectHyp maybeCurPlan =
    let uptDom = (`Eff.domainFromKnowledge` effectHyp)
               . (`Pre.domainFromPrecondHypothesis` precondHyp)
     in if isNothing maybeCurPlan
        then makePlan planner (uptDom domain) problem
        else return maybeCurPlan

learn :: Domain
      -> Problem
      -> Transition
      -> PreDomainHypothesis
      -> DomainHypothesis
      -> (PreDomainHypothesis, DomainHypothesis)
learn domain prob trans preHyp effHyp  =
  let (s, act, s') = trans
      effectHyp' = Eff.updateDomainHyp domain effHyp trans
      precondHyp' = Pre.updatePreDomainHyp domain preHyp trans
   in (precondHyp', effectHyp')

perform :: (Environment env)
        => Domain
        -> Problem
        -> env
        -> Maybe Plan
        -> Either (env,Transition,Plan) Bool
perform domain problem env (Just fullPlan@(action:restPlan)) =
  let trans s' = (probState problem, action, s')
   in case Env.applyAction env action of
        Just env' -> Left (env', trans $ Just $ Env.toState env', restPlan)
        Nothing -> Left (env, trans Nothing, fullPlan)
perform domain problem env (Just []) = Right True
perform domain problem env Nothing = Right False

run :: (ExternalPlanner ep, Environment env)
    => ep
    -> Domain
    -> Problem
    -> env
    -> PreDomainHypothesis
    -> DomainHypothesis
    -> Maybe Plan
    -> IO (Either (env, PreDomainHypothesis, DomainHypothesis, Maybe Plan) Bool)
run planner domain problem env preHyp effHyp plan =
  do plan' <- refine planner domain problem preHyp effHyp plan
     case perform domain problem env plan' of
       Left (env', trans, plan'') ->
        let (preHyp', effHyp') = learn domain problem trans preHyp effHyp
         in return $ Left (env', preHyp', effHyp', Just plan'')
       Right ans -> return $ Right ans

runnerVisualized :: (ExternalPlanner ep, Environment env)
                 => ep
                 -> (env -> IO ())
                 -> Domain
                 -> Problem
                 -> env
                 -> PreDomainHypothesis
                 -> DomainHypothesis
                 -> Maybe Plan
                 -> IO ()
runnerVisualized planr visual dom prob env preHyp effHyp plan =
  do
    res <- run planr dom prob env preHyp effHyp plan
    case res of
      Left (env', preHyp', effHyp', plan') ->
        let prob' = prob { probState = Env.toState env'}
         in do
           visual env'
           runnerVisualized planr visual dom prob' env' preHyp' effHyp' plan'
      Right True ->
        unless   (Env.isGoalReached prob env)
               $ error "Planner says problem is solved, environment does not"
      Right False -> error "Cannot solve problem, planner found no plan"
