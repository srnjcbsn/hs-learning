module Environments.Sokoban.Runner where

import System.Console.ANSI
import Control.Monad (forM_)
import Data.Map (assocs, keys)
import Text.Show.Pretty (ppShow)
import Data.Maybe

import Planning.FastDownward
import PDDL.Type
import PDDL.Logic hiding (applyAction)
import Learning.OptPrecondLearn
import Learning.OptEffectLearn
import Environments.Sokoban.Sokoban
import Environments.Sokoban.PDDL
import PDDL.Environment (Environment)
import qualified PDDL.Environment as Env
setCursorPosition' :: Integer -> Integer -> IO ()
setCursorPosition' x y =
    setCursorPosition (fromIntegral x) (fromIntegral y)

goalSymbol, sokobanSymbol :: Char
goalSymbol = 'X'
sokobanSymbol = 'S'

tileSymbol :: Tile -> Char
tileSymbol Clear   = '+'
tileSymbol (Box _) = '#'

showAt :: Coord -> Char -> IO ()
showAt (Coord (x, y)) s =
    setCursorPosition' x y >> putChar s

visTile :: Coord -> Tile -> IO ()
visTile c t = showAt c $ tileSymbol t

visGoal :: Coord -> IO ()
visGoal c = showAt c goalSymbol

visSokoban :: Coord -> IO ()
visSokoban c = showAt c sokobanSymbol

visSeparator :: Int -> Int -> IO ()
visSeparator width height = do
    setCursorPosition 0 (height + 1)
    putStrLn $ replicate width '='

-- | Prints a textual listing of the state below the map
visState :: World -> IO ()
visState world' = do
    visSeparator width height
    setCursorPosition 0 (height + 3)
    putStrLn (ppShow world')
    where width  = maximum $ map xCoord coords
          height = maximum $ map yCoord coords
          coords = keys $ coordMap world'

visualize :: SokobanPDDL -> IO ()
visualize pddl = do
    clearScreen
    forM_ tileCoords (uncurry visTile)
    forM_ goalCoords visGoal
    visSokoban sokoCoord
    visState (world pddl)
    where tileCoords = assocs $ (coordMap . world) pddl
          goalCoords = (goals . world) pddl
          sokoCoord  = (sokoban . world) pddl

runSokoban :: (ExtPlanner ep, Environment env)
           => ep
           -> Domain
           -> Problem
           -> env
           -> DomainHypothesis
           -> PreDomainHypothesis
           -> Maybe Plan
           -> IO ()
runSokoban planner domain problem env effectHyp precondHyp maybeCurPlan =
  let s = toState env
      uptDom = (`domainFromKnowledge` effectHyp) . (`domainFromPrecondHypothesis` precondHyp)
      actions =
        do
          maybePlan <-  if isNothing maybeCurPlan
                        then makePlan planner
                                (uptDom domain)
                                ( problem { probInitialState = s} )
                        else return maybeCurPlan
          case maybePlan of
            Just (act:restPlan) ->
              let maybeEnv' = applyAction env act
                  maybeS' = do env' <- maybeEnv'
                               return $ toState env'
                  trans = (s, act, maybeS')
                  effectHyp' = updateDomainHyp domain effectHyp trans
                  precondHyp' = updatePreDomainHyp domain precondHyp trans
              in case maybeEnv' of
                  Just env' ->
                    do visualize env'
                       return $ Just (env',effectHyp',precondHyp', problem, Just restPlan)
                  Nothing -> return $ Just (env,effectHyp',precondHyp', problem, Just restPlan)
            Just [] -> return Nothing
            Nothing -> undefined


   in do
     outp <- actions
     case outp of
      Just (env',effectHyp',precondHyp', ps', restPlan) ->
        undefined--runSokoban planner domain ps' effectHyp' precondHyp' env' restPlan
