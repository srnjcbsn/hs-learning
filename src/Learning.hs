module Learning where

import           Control.Monad
import Control.Monad.Writer
import           Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import           Text.Show.Pretty

import           Environment              (Environment)
import qualified Environment              as Env
import           Planning
import Planning.Viewing
import qualified Learning.SchemaLearning as Lrn

import Data.Map (Map)
import qualified Data.Map as Map

type Finder = Map State [Action]
type FinderResult = (Maybe Action, Finder)

isStateExplored :: Finder -> State -> Bool
isStateExplored finder state = fromMaybe False
                               $ liftM null (Map.lookup state finder)

findWithRem :: (a -> Bool) -> [a] -> Maybe (a, [a])
findWithRem prd (h:r) | prd h = Just (h, r)
                      | otherwise = findWithRem prd r
findWithRem _ [] = Nothing

findAction :: (Action -> Bool)
           -> (State -> [Action])
           -> Finder
           -> State
           -> (Maybe Action, Finder)
findAction isAppAble actsFunc finder state =
    let acts = Map.lookup state finder
        filtered = liftM (findWithRem isAppAble) acts
     in case filtered of
          Just (Just (act, rest)) -> (Just act, Map.insert state rest finder)
          Just Nothing -> (Nothing, finder)
          Nothing -> findAction isAppAble actsFunc
                                (Map.insert state (actsFunc state) finder)
                                state

emptyFinder :: Finder
emptyFinder = Map.empty

isMEq :: Eq a => Maybe a -> a -> Bool
isMEq m v = Just v == m --fromMaybe False $ liftM (== v) m

refine  :: ( BoundedPlanner ep
           , Domain dom prob as
           , Problem prob
           , ExternalPlanner ep dom prob as
           , Environment env
           )
        => ep
        -> View env
        -> dom
        -> prob
        -> Maybe Plan
        -> Maybe Int
        -> Finder
        -> IO (Maybe Plan, Finder)
refine planner view domain problem maybeCurPlan bound finder = outp where
    planner' = setBound planner bound
    s = initialState problem
    actGen = allApplicableActions domain problem
    --sExplored = isStateExplored finder s
    (finderAct, finder') = findAction (isActionApplicable domain s) actGen finder s
    outp | isMEq bound 1 && isJust finderAct =
               return (Just [fromJust finderAct], finder')
         | isNothing maybeCurPlan = do
            plan <- makePlan planner' domain problem
            planMade view plan
            return (plan,finder)
         | otherwise =  return (maybeCurPlan, finder)


perform :: (Environment env)
        => env
        -> Maybe Plan
        -> Either (env, Lrn.Transition, Maybe Plan) Bool
perform env (Just (action : rest))
    | s == s'   = Left (env', t, Nothing)
    | otherwise = Left (env', t, Just rest)
    where s    = Env.toState env
          env' = Env.applyAction env action
          s'   = Env.toState env'
          t    = (s, action, s')

perform _ (Just []) = Right True
perform _ Nothing   = Right False

runRound  :: ( BoundedPlanner ep
             , Lrn.LearningDomain dom prob as
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
  in do (plan',apps') <- refine planner view oldDomain problem plan bound apps
        case perform env plan' of
         Left (env', trans@(s,act,s'), plan'') ->
          let planState = apply oldDomain s act
            --   psIsSameAsNs = fromMaybe False
            --                             $ liftM2 (==) planState  s'
              psIsSameAsNs = Just s' == planState
              -- psIsNotSameAsNs = fromMaybe False
              --                           $ liftM2 (/=) planState  s'
              planIsDone = fromMaybe False (liftM null plan'')
              --planRunning = fromMaybe False (liftM ((> 0) . length ) plan'')
              newBound | planIsDone  = liftM (2 *) bound
                       -- psIsSameAsNs && planRunning = bound
                       | psIsSameAsNs                = bound
                       | s /= s' && isMEq bound 1    = Just 2
                       | otherwise                   = Just 1

              newDom = Lrn.learn oldDomain trans
              newPlan | s == s'    = Nothing
                      | planIsDone = Nothing
                      | otherwise = plan''

              showEnv | s /= s'   = envChanged view env'
                      | otherwise = return ()

           in do
             showEnv
             --envChanged view env'
             actionPerformed view act (s /= s')
             return $ Left (env', newDom, newPlan, newBound, apps')
         Right ans -> return $ Right ans

runEpisode :: ( BoundedPlanner ep
              , Lrn.LearningDomain dom prob as
              , Problem prob
              , ExternalPlanner ep dom prob as
              , Environment env)
           => ep
           -> View env
           -> dom
           -> prob
           -> env
           -> Maybe Int
           -> IO (Maybe env, dom, Maybe Int)
runEpisode planr view dom prob env startBound =
  let solved sp senv = isSolved sp (Env.toState senv)
      runv rdom rprob renv rplan bound tried =
        if solved rprob renv
        then return (Just renv, rdom, bound)
        else
          do res <- runRound planr view rdom rprob renv rplan bound tried
             case res of
               Left (env', dom', plan', bound', tried') ->
                 let prob' = setInitialState prob (Env.toState env')
                  in runv  dom' prob' env' plan' bound' tried'
               Right True -> error "Planner says problem is solved, environment does not"
               Right False -> return (Nothing, rdom, bound)
    in envChanged view env >> runv dom prob env Nothing startBound emptyFinder

runUntilSolved :: ( BoundedPlanner ep
                  , Eq dom
                  , Lrn.LearningDomain dom prob as
                  , Problem prob
                  , ExternalPlanner ep dom prob as
                  , Environment env)
               => ep
               -> View env
               -> dom
               -> prob
               -> env
               -> (IO (env, dom))
runUntilSolved planner view domain prob environment =
  let run' bound dom env =
        do res <- runEpisode planner view dom prob env bound
           case res of
            (Just doneEnv, newDom, _) -> return (doneEnv, newDom)
            (Nothing, newDom, newBound) | isNothing newBound && newDom == dom ->
                error ("Environment is unsolvable, final dom:"
                      ++ ppShow newDom)
            (Nothing, newDom, _) -> run' Nothing newDom environment
   in run' (Just 1) domain environment

newtype (Domain dom p as, Lrn.DomainHypothesis dh dom p as) =>
      LearningDomain' dom dh p as = LearningDomain' (dom, dh)
        deriving (Show, Eq)

instance (Domain dom p as, Lrn.DomainHypothesis dh dom p as)
         => Domain (LearningDomain' dom dh p as) p as where
    actionSpecification  (LearningDomain' (dom, _)) = actionSpecification dom
    actions              (LearningDomain' (dom, _)) = actions dom
    apply                (LearningDomain' (dom, _)) = apply dom
    allApplicableActions (LearningDomain' (dom, _)) = allApplicableActions dom

instance (Domain dom p as, Show dh, Show dom, Lrn.DomainHypothesis dh dom p as) =>
        Lrn.LearningDomain (LearningDomain' dom dh p as) p as where
    learn (LearningDomain' (dom, dh)) trans =
      let dh'  = Lrn.update dh dom trans
          dom' = Lrn.adjustDomain dh' dom
       in LearningDomain' (dom', dh')
    -- tellExploredAll (LearningDomain' (dom, dh)) s  =
    --  let dh'  = ignoreState dh s
    --      dom' = adjustDomain dh' dom
    --  in LearningDomain' (dom', dh')

toLearningDomain :: ( Domain domain p as
                    , Lrn.DomainHypothesis domainHypothesis domain p as)
                 => domainHypothesis
                 -> domain
                 -> LearningDomain' domain domainHypothesis p as
toLearningDomain dHyp dom = LearningDomain' (Lrn.adjustDomain dHyp dom, dHyp)
