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
        -> Set Action
        -> IO (Maybe Plan, Set Action)
refine planner domain problem maybeCurPlan bound triedActs =
    let planner' = setBound planner bound
        s = initialState problem
        outp | isNothing maybeCurPlan && bound == 1 =
                case allApplicableActions domain problem s of
                  acts@(_:_) ->
                    let found = find (not . flip Set.member triedActs) acts
                    in case found of
                      Just act -> return  (Just [act], Set.insert act triedActs)
                      Nothing -> return (Nothing, triedActs)
                  [] -> return (Nothing, triedActs)
             | isNothing maybeCurPlan =
                -- do putStrLn $ "planning: "
                -- putStrLn $ "using domain: " ++ (ppShow domain)
                -- putStrLn $ "using problem: " ++ (ppShow problem)
                -- hFlush stdout
                liftM (flip (,) triedActs) $ makePlan planner' domain problem
             | otherwise =  return (maybeCurPlan, triedActs)
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
    -> Set Action
    -> IO (Either (env, PreDomainHypothesis, DomainHypothesis, Maybe Plan, Int, Set Action) Bool)
run planner oldDomain problem env preHyp effHyp plan bound triedActions =
  let uptDom = (`Eff.domainFromKnowledge` effHyp)
             . (`Pre.domainFromPrecondHypothesis` preHyp)
      newDom = uptDom oldDomain
  in do (plan',triedActions') <- refine planner newDom problem plan bound triedActions
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
                      | bound == 1 && planIsDone = Nothing
                      | otherwise = plan''
           in do putStrLn $ "running: did action " ++ (ppShow act)
                 putStrLn $ "running: new state: " ++ (ppShow s')
                 hFlush stdout
                 return $ Left (env', preHyp', effHyp', newPlan, newBound, triedActions')
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
      runv rprob renv rpreHyp reffHyp rplan bound tried =
        if solved rprob renv
        then return renv
        else
          do logger plan
             res <- run planr dom rprob renv rpreHyp reffHyp rplan bound tried
             case res of
               Left (env', preHyp', effHyp', plan', bound', tried') ->
                 let prob' = prob { probState = Env.toState env'}
                  in do
                      visual env'
                      runv prob' env' preHyp' effHyp' plan' bound' tried'
               Right True -> error "Planner says problem is solved, environment does not"
               Right False -> error "Cannot solve problem, planner found no plan"
    in runv prob env preHyp effHyp plan 1 Set.empty
