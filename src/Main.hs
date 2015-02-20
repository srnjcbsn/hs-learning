module Main where

import System.Directory (removeFile)
import           System.Console.ANSI

import           Environment
import           Environment.Sokoban.ConsoleView (visualize)
import           Environment.Sokoban.PDDL
import qualified Environment.Sokoban.Samples.WikiSample as WS
import qualified Environment.Sokoban.Samples.SimpleSample as SS
import           Environment.Sokoban.SokobanDomain
import           Learning
import           Planning.FastDownward
import Learning.OptEffectLearn
import Learning.OptPrecondLearn
import ActionViewer

logPath = "./log.log"

main :: IO ()
main = do
    removeFile logPath
    clearScreen
    setTitle "SOKOBAN!"
    runnerVisualized fd visualize (logAction logPath) dom prob sokoEnv iniPreDomHyp iniEffDomHyp Nothing
    where

        sokoWorld = SS.world
        sokoEnv = fromWorld sokoWorld
        dom = sokobanDomain
        prob = toProblem sokoWorld
        fd = mkFastDownard dom prob
        iniPreDomHyp = initialPreDomainHyp dom
        iniEffDomHyp = initialHypothesis dom
