module Main where

import           System.Console.ANSI
import           System.Directory                         (removeFile)

import           Data.Map                                 ((!))
import Planning.Viewing
import           Data.Maybe
import           Environment                              as Env
import           Environment.Sokoban.SokobanView          
import           Environment.Sokoban.PDDL
import qualified Environment.Sokoban.Samples.SimpleSample as SS
import qualified Environment.Sokoban.Samples.LargeSample  as LS
import qualified Environment.Sokoban.Samples.WikiSample   as WS
import           Environment.Sokoban.SokobanDomain
import           Graph.Search.Astar                       as Astar
import           Learning
import           Learning.OptEffectLearn
import           Learning.OptPrecondLearn
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic
import           Planning.PDDL.Samples.SimpleBox
import           Planning.Planner.FastDownward
import           System.IO.Error
import           Text.Show.Pretty

logPath = "./log.log"

data Astar = Astar Int

instance BoundedPlanner Astar where
  setBound (Astar _) = Astar

instance ExternalPlanner Astar PDDLDomain PDDLProblem ActionSpec where
    makePlan (Astar bound) d p = return $ Astar.searchBounded (PDDLGraph (d,p)) (initialState p) bound

toFormula :: PDDLDomain -> PreDomainHypothesis -> [(String, Formula Argument)]
toFormula dom dHyp =
  map (\as -> (asName as, constructPrecondFormula (dHyp ! asName as)))
                                    $ dmActionsSpecs dom


main :: IO ()
main = do
    catchIOError (removeFile logPath) (\_ -> return ())
    clearScreen
    setTitle "SOKOBAN!"
    putStrLn (ppShow $ initialState prob)
    putStrLn (ppShow (toFormula dom iniPreDomHyp))
    --outp <- runn env iniPreDomHyp iniEffDomHyp Nothing
    -- case outp of
    --   Left (env',preHyp,_,_) ->
    --     let as = fromJust $ actionSpecification dom "move-v"
    --         as' = constructPrecondSchema (preHyp ! "move-v") as
    --       in putStr (ppShow (groundPreconditions as' ["b0x1", "b0x0"]))
    fenv <- runv env iniPreDomHyp iniEffDomHyp Nothing
    putStr (ppShow fenv)
    return ()
    --outp2 <- continue outp
    --printOut outp
    --printOut outp2
    where
        sokoWorld = WS.world
        sokoEnv = fromWorld sokoWorld
        --runn = run astar dom prob
        runv = runnerVisualized astar (sokobanView "log.log") dom prob
        -- continue outp =
        --   case outp of
        --     Left (env',preHyp,effHyp,plan) -> runn env' preHyp effHyp plan
        --     Right eror -> error ("stopped " ++ show eror)
        -- printOut outp =
        --   case outp of
        --    Left (env,preHyp,effHyp,_) ->
        --     do putStrLn ("preconds: " ++ (ppShow effHyp))
        --    Right True -> putStrLn ("Success")
        --    Right False -> putStrLn ("failed")
        env = sokoEnv
        dom = sokobanDomain
        prob = toProblem sokoWorld
        --env = SBEnvironment (initialState prob, dom)
        astar = Astar 1
        -- fd = mkFastDownard dom prob
        iniPreDomHyp = initialPreDomainHyp dom
        iniEffDomHyp = initialHypothesis dom
