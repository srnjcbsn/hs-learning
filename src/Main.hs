module Main where

import           System.Console.ANSI
import           System.Directory                         (removeFile)

import           ActionViewer
import           Environment
import           Environment.Sokoban.ConsoleView          (visualize)
import           Environment.Sokoban.PDDL
import qualified Environment.Sokoban.Samples.SimpleSample as SS
import qualified Environment.Sokoban.Samples.WikiSample   as WS
import           Environment.Sokoban.SokobanDomain
import           Learning
import           Learning.OptEffectLearn
import           Learning.OptPrecondLearn
import           Planning.Planner.FastDownward
import           Graph.Search.Astar as Astar
import           System.IO.Error
import Planning
import Planning.PDDL
logPath = "./log.log"

data Astar = Astar

instance ExternalPlanner Astar PDDLDomain PDDLProblem ActionSpec where
    makePlan _ d p = return $ Astar.search (PDDLGraph (d,p)) (initialState p)

main :: IO ()
main = do
    catchIOError (removeFile logPath) (\_ -> return ())
    clearScreen
    setTitle "SOKOBAN!"
    runnerVisualized astar vis (logAction logPath) dom prob sokoEnv iniPreDomHyp iniEffDomHyp Nothing
    where
        vis _ = return () :: IO ()
        sokoWorld = SS.world
        sokoEnv = fromWorld sokoWorld
        dom = sokobanDomain
        prob = toProblem sokoWorld
        astar = Astar
        -- fd = mkFastDownard dom prob
        iniPreDomHyp = initialPreDomainHyp dom
        iniEffDomHyp = initialHypothesis dom
