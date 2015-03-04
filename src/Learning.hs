module Learning where

import           Control.Monad
import           Data.Maybe
--import           Text.Show.Pretty

import           Environment              (Environment)
import qualified Environment              as Env
import           Planning
import Planning.Viewing

import Data.Map (Map)
import qualified Data.Map as Map
--import Data.Maybe(isNothing)
--import Data.Set (Set)
--import Debug.Trace
--import System.IO
--import qualified Data.Set as Set
--import Data.List(find)

class (Domain dom p as, Eq dh) => DomainHypothesis dh dom p as | dh -> dom p as where
    update :: dh -> dom -> Transition -> dh
    adjustDomain :: dh -> dom -> dom
    fromDomain :: dom -> dh

class (Domain d p as, Show d) => LearningDomain d p as | d -> p as where
   learn :: d -> Transition -> d

type Finder = Map State [Action]
type FinderResult = (Maybe Action, Finder)

isStateExplored :: Finder -> State -> Bool
isStateExplored finder state = fromMaybe False
                               $ liftM null (Map.lookup state finder)

findAction :: (State -> [Action]) -> Finder -> State -> (Maybe Action, Finder)
findAction actsFunc finder state =
    let acts = Map.lookup state finder
     in case acts of
          Just (act:rest) -> (Just act, Map.insert state rest finder)
          Just [] -> (Nothing, finder)
          Nothing -> findAction actsFunc
                                (Map.insert state (actsFunc state) finder)

                                state
emptyFinder :: Finder
emptyFinder = Map.empty

isMEq :: Eq a => Maybe a -> a -> Bool
isMEq m v = fromMaybe False $ liftM (== v) m

refine  :: ( BoundedPlanner ep
           , Domain dom prob as
           , Problem prob
           , ExternalPlanner ep dom prob as
           )
        => ep
        -> dom
        -> prob
        -> Maybe Plan
        -> Maybe Int
        -> Finder
        -> IO (Maybe Plan, Finder)
refine planner domain problem maybeCurPlan bound finder = outp where
    planner' = setBound planner bound
    s = initialState problem
    actGen = allApplicableActions domain problem
    --sExplored = isStateExplored finder s
    (finderAct, finder') = findAction actGen finder s
    outp | isJust finderAct && isMEq bound 1 =
               return (Just [fromJust finderAct], finder')
         | isNothing maybeCurPlan =
            liftM (flip (,) finder) $ makePlan planner' domain problem
         | otherwise =  return (maybeCurPlan, finder)


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

runRound  :: ( BoundedPlanner ep
             , LearningDomain dom prob as
             , ExternalPlanner ep dom prob as
             , Environment env)
          => ep
          -> View env
          -> dom
          -> prob
          -> env
          -> Maybe Plan
          -> Maybe Int
          -> Finder
          -> IO (Either (env, dom, Maybe Plan, Maybe Int, Finder) Bool)
runRound planner view oldDomain problem  env plan bound apps =
  let
  in do (plan',apps') <- refine planner oldDomain problem plan bound apps
        case perform env plan' of
         Left (env', trans@(s,act,s'), plan'') ->
          let planState = apply oldDomain s act
              psIsSameAsNs = fromMaybe False
                                        $ liftM2 (==) planState  s'

              planIsDone = fromMaybe False (liftM null plan'')
              planRunning = fromMaybe False (liftM ((> 0) . length ) plan'')
              newBound | psIsSameAsNs && planIsDone  = liftM (2 *) bound
                       | psIsSameAsNs && planRunning = bound
                       | isJust s' && isMEq bound 1  = Just 2
                       | otherwise                   = Just 1

              newDom = learn oldDomain trans
              newPlan | isNothing s' = Nothing
                      | planIsDone = Nothing
                      | otherwise = plan''
           in do actionPerformed view act (isJust s')
                 return $ Left (env', newDom, newPlan, newBound, apps')
         Right ans -> return $ Right ans

runEpisode :: ( BoundedPlanner ep
              , LearningDomain dom prob as
              , Problem prob
              , ExternalPlanner ep dom prob as
              , Environment env)
           => ep
           -> View env
           -> dom
           -> prob
           -> env
           -> Maybe Int
           -> IO (Maybe env, dom)
runEpisode planr view dom prob env startBound =
  let solved sp senv = isSolved sp (Env.toState senv)
      runv rdom rprob renv rplan bound tried =
        if solved rprob renv
        then return (Just renv, rdom)
        else
          do planMade view rplan
             res <- runRound planr view rdom rprob renv rplan bound tried
             case res of
               Left (env', dom', plan', bound', tried') ->
                 let prob' = setInitialState prob (Env.toState env')
                  in do
                      envChanged view env'
                      --putStrLn ("New bound: " ++ show bound')
                      runv  dom' prob' env' plan' bound' tried'
               Right True -> error "Planner says problem is solved, environment does not"
               Right False -> return (Nothing, rdom)
    in envChanged view env >> runv dom prob env Nothing startBound emptyFinder

runUntilSolved :: ( BoundedPlanner ep
                  , Eq dom
                  , LearningDomain dom prob as
                  , Problem prob
                  , ExternalPlanner ep dom prob as
                  , Environment env)
               => ep
               -> View env
               -> dom
               -> prob
               -> env
               -> IO (env,dom)
runUntilSolved planner view domain prob environment =
  let run' bound dom env =
        do res <- runEpisode planner view dom prob env bound
           case res of
            (Just doneEnv, newDom) -> return (doneEnv, newDom)
            (Nothing, newDom) | newDom == dom -> error "Environment is unsolvable"
            (Nothing, newDom) -> run' Nothing newDom environment
   in run' (Just 1) domain environment

newtype (Domain dom p as, DomainHypothesis dh dom p as) =>
      LearningDomain' dom dh p as =  LearningDomain' (dom, dh)
        deriving (Show, Eq)

instance (Domain dom p as, DomainHypothesis dh dom p as)
         => Domain (LearningDomain' dom dh p as) p as where
    actionSpecification  (LearningDomain' (dom, _)) = actionSpecification dom
    actions              (LearningDomain' (dom, _)) = actions dom
    apply                (LearningDomain' (dom, _)) = apply dom
    allApplicableActions (LearningDomain' (dom, _)) = allApplicableActions dom

instance (Domain dom p as, Show dh, Show dom, DomainHypothesis dh dom p as) =>
        LearningDomain (LearningDomain' dom dh p as) p as where
    learn (LearningDomain' (dom, dh)) trans =
      let dh'  = update dh dom trans
          dom' = adjustDomain dh' dom
       in LearningDomain' (dom', dh')
    -- tellExploredAll (LearningDomain' (dom, dh)) s  =
    --  let dh'  = ignoreState dh s
    --      dom' = adjustDomain dh' dom
    --  in LearningDomain' (dom', dh')

toLearningDomain :: ( Domain domain p as
                    , DomainHypothesis domainHypothesis domain p as)
                 => domainHypothesis
                 -> domain
                 -> LearningDomain' domain domainHypothesis p as
toLearningDomain dHyp dom = LearningDomain' (adjustDomain dHyp dom, dHyp)
