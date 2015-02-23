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

logPath = "./log.log"

main :: IO ()
main = do
    removeFile logPath
    clearScreen
    setTitle "SOKOBAN!"
    runnerVisualized fd vis (logAction logPath) dom prob sokoEnv iniPreDomHyp iniEffDomHyp Nothing
    where
        vis _ = return () :: IO ()
        sokoWorld = SS.world
        sokoEnv = fromWorld sokoWorld
        dom = sokobanDomain
        prob = toProblem sokoWorld
        fd = mkFastDownard dom prob
        iniPreDomHyp = initialPreDomainHyp dom
        iniEffDomHyp = initialHypothesis dom
