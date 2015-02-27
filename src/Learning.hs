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
import System.IO



refine  :: (BoundedPlanner ep, ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec)
        => ep
        -> PDDLDomain
        -> PDDLProblem
        -> Maybe Plan
        -> Int
        -> IO (Maybe Plan)
refine planner domain problem maybeCurPlan bound =
    let planner' = setBound planner bound
        s = initialState problem
        outp | isNothing maybeCurPlan && bound == 1 =
                case allApplicableActions domain problem s of
                  act:_ -> return $ Just [act]
                  [] -> return Nothing
             | isNothing maybeCurPlan =
                -- do putStrLn $ "planning: "
                -- putStrLn $ "using domain: " ++ (ppShow domain)
                -- putStrLn $ "using problem: " ++ (ppShow problem)
                -- hFlush stdout
                makePlan planner' domain problem
             | otherwise =  return maybeCurPlan
     in outp

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
    -> PDDLDomain
    -> PDDLProblem
    -> env
    -> PreDomainHypothesis
    -> DomainHypothesis
    -> Maybe Plan
    -> Int
    -> IO (Either (env, PreDomainHypothesis, DomainHypothesis, Maybe Plan, Int) Bool)
run planner oldDomain problem env preHyp effHyp plan bound =
  let uptDom = (`Eff.domainFromKnowledge` effHyp)
             . (`Pre.domainFromPrecondHypothesis` preHyp)
      newDom = uptDom oldDomain
  in do plan' <- refine planner newDom problem plan bound
        case perform env plan' of
         Left (env', trans@(s,act,s'), plan'') ->
          let planState = apply newDom s act
              planStateIsSameAsNewState = fromMaybe False
                                        $ liftM2 (==) planState  s'
              planIsDone = fromMaybe False (liftM null plan'')
              planRunning = fromMaybe False (liftM ((> 0) . length ) plan'')
              newBound | planStateIsSameAsNewState && planIsDone =  bound * 2
                       | planStateIsSameAsNewState && planRunning = bound
                       | otherwise = 1
              (preHyp', effHyp') = learn newDom trans preHyp effHyp
              newPlan | isNothing s' = Nothing
                      | isSolved problem (Env.toState env) = Just []
                      | otherwise = plan''
           in do putStrLn $ "running: did action " ++ (ppShow act)
                 putStrLn $ "running: new state: " ++ (ppShow s')
                 hFlush stdout
                 return $ Left (env', preHyp', effHyp', newPlan, newBound)
         Right ans -> return $ Right ans

runnerVisualized :: (BoundedPlanner ep, ExternalPlanner ep PDDLDomain PDDLProblem ActionSpec, Environment env)
                 => ep
                 -> (env -> IO ())
                 -> (Maybe Plan -> IO ())
                 -> PDDLDomain
                 -> PDDLProblem
                 -> env
                 -> PreDomainHypothesis
                 -> DomainHypothesis
                 -> Maybe Plan
                 -> IO env
runnerVisualized planr visual logger dom prob env preHyp effHyp plan =
  let solved sp senv = isSolved sp (Env.toState senv)
      runv rprob renv rpreHyp reffHyp rplan bound =
        if solved rprob renv
        then return renv
        else
          do logger plan
             res <- run planr dom rprob renv rpreHyp reffHyp rplan bound
             case res of
               Left (env', preHyp', effHyp', plan', bound') ->
                 let prob' = prob { probState = Env.toState env'}
                  in do
                      visual env'
                      runv prob' env' preHyp' effHyp' plan' bound'
               Right True ->
                 if bound > 1
                 then error "Planner says problem is solved, environment does not"
                 else runv rprob renv rpreHyp reffHyp rplan bound
               Right False -> error "Cannot solve problem, planner found no plan"
    in runv prob env preHyp effHyp plan 1
