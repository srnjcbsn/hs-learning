module Learning where

import           Control.Monad
import           Data.Maybe
--import           Text.Show.Pretty

import           Environment              (Environment)
import qualified Environment              as Env
import           Planning
import Planning.Viewing

--import Data.Map ((!))
--import Data.Set (Set)
--import Debug.Trace
--import System.IO
--import qualified Data.Set as Set
--import Data.List(find)

class Domain dom p as => DomainHypothesis dh dom p as | dh -> dom p as where
    update :: dh -> dom -> Transition -> dh
    adjustDomain :: dh -> dom -> dom
    fromDomain :: dom -> dh

class Domain d p as => LearningDomain d p as | d -> p as where
   learn :: d -> Transition -> d


refine  :: ( BoundedPlanner ep
           , Domain dom prob as
           , Problem prob
           , ExternalPlanner ep dom prob as
           )
        => ep
        -> dom
        -> prob
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

run :: ( BoundedPlanner ep
       , LearningDomain dom prob as
       , ExternalPlanner ep dom prob as
       , Environment env)
    => ep
    -> View env
    -> dom
    -> prob
    -> env
    -> Maybe Plan
    -> Int
    -> [Action]
    -> IO (Either (env, dom, Maybe Plan, Int, [Action]) Bool)
run planner view oldDomain problem  env plan bound apps =
  let
  in do (plan',apps') <- refine planner oldDomain problem plan bound apps
        case perform env plan' of
         Left (env', trans@(s,act,s'), plan'') ->
          let planState = apply oldDomain s act
              planStateIsSameAsNewState = fromMaybe False
                                        $ liftM2 (==) planState  s'
              planIsDone = fromMaybe False (liftM null plan'')
              planRunning = fromMaybe False (liftM ((> 0) . length ) plan'')
              newBound | planStateIsSameAsNewState && planIsDone  = bound * 2
                       | planStateIsSameAsNewState && planRunning = bound
                       | isJust s' && bound == 1                  = 2
                       | otherwise                                = 1

              newDom = learn oldDomain trans
              newPlan | isNothing s' = Nothing
                      | bound == 1 && planIsDone = Nothing
                      | otherwise = plan''

              apps'' | planStateIsSameAsNewState = []
                     | otherwise                 = apps'
           in do --putStrLn $ "running: did action " ++ (ppShow act)
                 --putStrLn $ "running: new state: " ++ (ppShow s')
                 --hFlush stdout
                 actionPerformed view act (isJust s')
                 return $ Left (env', newDom, newPlan, newBound, apps'')
         Right ans -> return $ Right ans

runnerVisualized :: ( BoundedPlanner ep
                    , LearningDomain dom prob as
                    , Problem prob
                    , ExternalPlanner ep dom prob as
                    , Environment env)
                 => ep
                 -> View env
                 -> dom
                 -> prob
                 -> env
                 -> Maybe Plan
                 -> IO env
runnerVisualized planr view dom prob env plan =
  let solved sp senv = isSolved sp (Env.toState senv)
      runv rdom rprob renv rplan bound tried =
        if solved rprob renv
        then return renv
        else
          do planMade view plan
             res <- run planr view rdom rprob renv rplan bound tried
             case res of
               Left (env', dom', plan', bound', tried') ->
                 let prob' = setInitialState prob (Env.toState env')
                  in do
                      envChanged view env'
                      runv  dom' prob' env' plan' bound' tried'
               Right True -> error "Planner says problem is solved, environment does not"
               Right False -> error "Cannot solve problem, planner found no plan"
    in envChanged view env >> runv dom prob env plan 1 []


newtype (Domain dom p as, DomainHypothesis dh dom p as) =>
      LearningDomain' dom dh p as =  LearningDomain' (dom, dh)

instance (Domain dom p as, DomainHypothesis dh dom p as)
         => Domain (LearningDomain' dom dh p as) p as where
    actionSpecification  (LearningDomain' (dom, _)) = actionSpecification dom
    actions              (LearningDomain' (dom, _)) = actions dom
    apply                (LearningDomain' (dom, _)) = apply dom
    allApplicableActions (LearningDomain' (dom, _)) = allApplicableActions dom

instance (Domain dom p as, DomainHypothesis dh dom p as) =>
        LearningDomain (LearningDomain' dom dh p as) p as where
    learn (LearningDomain' (dom, dh)) trans =
      let dh'  = update dh dom trans
          dom' = adjustDomain dh' dom
       in LearningDomain' (dom', dh')

toLearningDomain :: ( Domain domain p as
                    , DomainHypothesis domainHypothesis domain p as)
                 => domainHypothesis
                 -> domain
                 -> LearningDomain' domain domainHypothesis p as
toLearningDomain dHyp dom = LearningDomain' (dom, dHyp)
