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
import Planning.Viewing

import Data.Map ((!))
import Data.Set (Set)
import Debug.Trace
import System.IO
import qualified Data.Set as Set
import Data.List(find)

refine  :: (BoundedPlanner ep, ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec)
        => ep
        -> PDDLDomain
        -> PDDLProblem
        -> Maybe Plan
        -> Int
        -> [Action]
        -> IO (Maybe Plan, [Action])
refine planner domain problem maybeCurPlan bound applications = outp where
    planner' = setBound planner bound
    s = initialState problem
    outp | isNothing maybeCurPlan && bound == 1 =
             case applications of
                (act : rest) -> return (Just [act], rest)
                []           -> refine planner domain problem maybeCurPlan 1
                                $ allApplicableActions domain problem s
         | isNothing maybeCurPlan =
            liftM (flip (,) applications) $ makePlan planner' domain problem

         | otherwise =  return (maybeCurPlan, applications)

learn :: PDDLDomain
      -> Transition
      -> PreDomainHypothesis
      -> DomainHypothesis
      -> (PreDomainHypothesis, DomainHypothesis)
learn domain trans@(_,act@(name,_),_) preHyp effHyp  =
  let effectHyp' = Eff.updateDomainHyp domain effHyp trans
      precondHyp' = Pre.updatePreDomainHyp domain preHyp trans
   in (precondHyp', effectHyp')
        --trace ("learned(" ++ show act ++ "): " ++ (ppShow $ precondHyp' ! name))
        --    (precondHyp', effectHyp')

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

run :: (BoundedPlanner ep, ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec, Environment env)
    => ep
    -> View env
    -> PDDLDomain
    -> PDDLProblem
    -> env
    -> PreDomainHypothesis
    -> DomainHypothesis
    -> Maybe Plan
    -> Int
    -> [Action]
    -> IO (Either (env, PreDomainHypothesis, DomainHypothesis, Maybe Plan, Int, [Action]) Bool)
run planner view oldDomain problem env preHyp effHyp plan bound apps =
  let uptDom = (`Eff.domainFromKnowledge` effHyp)
             . (`Pre.domainFromPrecondHypothesis` preHyp)
      newDom = uptDom oldDomain
  in do (plan', apps') <- refine planner newDom problem plan bound apps
        case perform env plan' of
         Left (env', trans@(s,act,s'), plan'') ->
          let planState = apply newDom s act
              planStateIsSameAsNewState = fromMaybe False
                                        $ liftM2 (==) planState  s'
              planIsDone = fromMaybe False (liftM null plan'')
              planRunning = fromMaybe False (liftM ((> 0) . length ) plan'')
              newBound | planStateIsSameAsNewState && planIsDone  = bound * 2
                       | planStateIsSameAsNewState && planRunning = bound
                       | isJust s' && bound == 1                  = 2
                       | otherwise                                = 1

              (preHyp', effHyp') = learn newDom trans preHyp effHyp
              newPlan | isNothing s' = Nothing
                      | bound == 1 && planIsDone = Nothing
                      | otherwise = plan''

              apps'' | planStateIsSameAsNewState = []
                     | otherwise                 = apps'
           in do --putStrLn $ "running: did action " ++ (ppShow act)
                 --putStrLn $ "running: new state: " ++ (ppShow s')
                 --hFlush stdout
                 actionPerformed view act (isJust s')
                 return $ Left (env', preHyp', effHyp', newPlan, newBound, apps'')
         Right ans -> return $ Right ans

runnerVisualized :: (BoundedPlanner ep, ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec, Environment env)
                 => ep
                --  -> (env -> IO ())
                --  -> (Maybe Plan -> IO ())
                 -> View env
                 -> PDDLDomain
                 -> PDDLProblem
                 -> env
                 -> PreDomainHypothesis
                 -> DomainHypothesis
                 -> Maybe Plan
                 -> IO env
runnerVisualized planr view dom prob env preHyp effHyp plan =
  let solved sp senv = isSolved sp (Env.toState senv)
      runv rprob renv rpreHyp reffHyp rplan bound apps =
        if solved rprob renv
        then return renv
        else
          do planMade view plan
             res <- run planr view dom rprob renv rpreHyp reffHyp rplan bound apps
             case res of
               Left (env', preHyp', effHyp', plan', bound', apps') ->
                 let prob' = prob { probState = Env.toState env'}
                  in do
                      envChanged view env'
                      runv prob' env' preHyp' effHyp' plan' bound' apps'
               Right True -> error "Planner says problem is solved, environment does not"
               Right False -> error "Cannot solve problem, planner found no plan"
    in envChanged view env >> runv prob env preHyp effHyp plan 1 []
