module Learning where

import           Control.Monad
import           Data.Maybe
import           Text.Show.Pretty

import           Environment              (Environment)
import qualified Environment              as Env
import           Learning.OptEffectLearn  (DomainHypothesis)
import qualified Learning.OptEffectLearn  as Eff
import           Learning.OptPrecondLearn (PreDomainHypothesis)
import qualified Learning.OptPrecondLearn as Pre
import           Planning.PDDL
import           Planning
import Data.Map ((!))
import Debug.Trace

refine  :: ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec
        => ep
        -> PDDLDomain
        -> PDDLProblem
        -> PreDomainHypothesis
        -> DomainHypothesis
        -> Maybe Plan
        -> IO (Maybe Plan)
refine planner domain problem precondHyp effectHyp maybeCurPlan =
    let uptDom = (`Eff.domainFromKnowledge` effectHyp)
               . (`Pre.domainFromPrecondHypothesis` precondHyp)
     in if isNothing maybeCurPlan
        then do p' <- makePlan planner (uptDom domain) problem
                return p'

        else return maybeCurPlan

learn :: PDDLDomain
      -> Transition
      -> PreDomainHypothesis
      -> DomainHypothesis
      -> (PreDomainHypothesis, DomainHypothesis)
learn domain trans@(_,act@(name,_),_) preHyp effHyp  =
  let effectHyp' = Eff.updateDomainHyp domain effHyp trans
      precondHyp' = Pre.updatePreDomainHyp domain preHyp trans
   in trace ("learned(" ++ show act ++ "): " ++ (ppShow $ precondHyp' ! name))
            (precondHyp', effectHyp')

perform :: (Environment env)
        => env
        -> Maybe Plan
        -> Either (env, Transition, Maybe Plan) Bool
perform env (Just (action:restPlan)) =
  let trans s' = (Env.toState env, action, s')
   in case Env.applyAction env action of
        Just env' -> Left (env', trans $ Just $ Env.toState env', Just restPlan)
        Nothing -> Left (env, trans Nothing, Nothing)
perform _ (Just []) = Right True
perform _ Nothing = Right False

run :: (ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec, Environment env)
    => ep
    -> PDDLDomain
    -> PDDLProblem
    -> env
    -> PreDomainHypothesis
    -> DomainHypothesis
    -> Maybe Plan
    -> IO (Either (env, PreDomainHypothesis, DomainHypothesis, Maybe Plan) Bool)
run planner domain problem env preHyp effHyp plan =
  do plan' <- refine planner domain problem preHyp effHyp plan
     case trace ("running: refine returned plan: " ++ (show plan'))
                (perform env plan') of
       Left (env', trans@(_,act,s'), plan'') ->
        let (preHyp', effHyp') = learn domain trans preHyp effHyp
         in do --putStrLn $ "running: did action " ++ (ppShow act)
               --putStrLn $ "running: new state: " ++ (ppShow s')
               return $ Left (env', preHyp', effHyp', plan'')
       Right ans -> return $ Right ans

runnerVisualized :: (ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec, Environment env)
                 => ep
                 -> (env -> IO ())
                 -> (Maybe Plan -> IO ())
                 -> PDDLDomain
                 -> PDDLProblem
                 -> env
                 -> PreDomainHypothesis
                 -> DomainHypothesis
                 -> Maybe Plan
                 -> IO (env)
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
        if (isSolved prob (Env.toState env))
        then return env
        else error "Planner says problem is solved, environment does not"
      Right False -> error "Cannot solve problem, planner found no plan"
