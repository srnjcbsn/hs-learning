module Learning where

import           Control.Monad
import           Data.Maybe
import           Environment              (Environment)
import qualified Environment              as Env
import           Learning.OptEffectLearn  (DomainHypothesis)
import qualified Learning.OptEffectLearn  as Eff
import           Learning.OptPrecondLearn (PreDomainHypothesis)
import qualified Learning.OptPrecondLearn as Pre
import           PDDL
import           Planning

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
      -> Transition
      -> PreDomainHypothesis
      -> DomainHypothesis
      -> (PreDomainHypothesis, DomainHypothesis)
learn domain trans preHyp effHyp  =
  let effectHyp' = Eff.updateDomainHyp domain effHyp trans
      precondHyp' = Pre.updatePreDomainHyp domain preHyp trans
   in (precondHyp', effectHyp')

perform :: (Environment env)
        => env
        -> Maybe Plan
        -> Either (env, Transition, Maybe Plan) Bool
perform env (Just fullPlan@(action:restPlan)) =
  let trans s' = (Env.toState env, action, s')
   in case Env.applyAction env action of
        Just env' -> Left (env', trans $ Just $ Env.toState env', Just restPlan)
        Nothing -> Left (env, trans Nothing, Nothing)
perform _ (Just []) = Right True
perform _ Nothing = Right False

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
     case perform env plan' of
       Left (env', trans, plan'') ->
        let (preHyp', effHyp') = learn domain trans preHyp effHyp
         in return $ Left (env', preHyp', effHyp', plan'')
       Right ans -> return $ Right ans

runnerVisualized :: (ExternalPlanner ep, Environment env)
                 => ep
                 -> (env -> IO ())
                 -> (Maybe Plan -> IO ())
                 -> Domain
                 -> Problem
                 -> env
                 -> PreDomainHypothesis
                 -> DomainHypothesis
                 -> Maybe Plan
                 -> IO ()
runnerVisualized planr visual logger dom prob env preHyp effHyp plan =
  do
    logger plan
    res <- run planr dom prob env preHyp effHyp plan
    case res of
      Left (env', preHyp', effHyp', plan') ->
        let prob' = prob { probState = Env.toState env'}
         in do
           visual env'
           runnerVisualized planr visual logger dom prob' env' preHyp' effHyp' plan'
      Right True ->
        unless   (Env.isGoalReached prob env)
               $ error "Planner says problem is solved, environment does not"
      Right False -> error "Cannot solve problem, planner found no plan"
