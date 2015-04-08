
module Main where



import           Environment                              as Env
import           Environment.Sokoban.PDDL
import qualified Environment.Sokoban.Samples.BigSample    as BS
import qualified Environment.Sokoban.Samples.LargeSample  as LS
import qualified Environment.Sokoban.Samples.SimpleSample as SS
import qualified Environment.Sokoban.Samples.WikiSample   as WS
import           Environment.Sokoban.SokobanDomain
import           Environment.Sokoban.SokobanView
import           Graph.Search.Astar                       as Astar
import           Learning
import           Learning.PDDL
import           Learning.PDDL.NonConditionalKnowledge
import           Learning.PDDL.OptimisticStrategy
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic
import           Planning.Planner.FastDownward
import           Planning.Viewing

import           Data.Map                                 ((!))
import           System.Console.ANSI
import           System.Directory                         (removeFile)
import           System.IO.Error
import           Text.Show.Pretty
logPath = "./log.log"

data Astar = Astar (Maybe Int)

instance BoundedPlanner Astar where
  setBound (Astar _) = Astar

instance ExternalPlanner Astar PDDLDomain PDDLProblem ActionSpec where
    makePlan (Astar bound) d p =
      case bound of
        Just b -> return $ Astar.searchBounded (PDDLGraph (d,p)) (initialState p) b
        Nothing -> return $ Astar.search (PDDLGraph (d,p)) (initialState p)

-- Inquirable uni question info
instance Inquirable SokobanPDDL PDDLProblem PDDLInfo where
    inquire _ _ = return Nothing

main :: IO ()
main = do
    catchIOError (removeFile logPath) (\_ -> return ())
    clearScreen
    setTitle "SOKOBAN!"
    -- putStrLn (ppShow $ initialState prob)



    (knl', world') <- scientificMethod sokoView optStrat initKnl ssEnv ssProb
    (knl'', world'') <- scientificMethod sokoView optStrat knl' lsEnv lsProb
    (knl''', world''') <- scientificMethod sokoView optStrat knl'' bsEnv bsProb
    -- putStrLn (ppShow fenv)
    -- putStrLn (ppShow dom''')
    -- writeFile "sokoDom.pddl" $ writeDomain dom
    -- writeFile "sokoProb.pddl" $ writeProblem wsProb
    return ()
    where
        sokoView = sokobanView logPath

        optStrat = OptimisticStrategy (Astar Nothing, Nothing)
        bsWorld = BS.world
        bsEnv = fromWorld bsWorld
        bsProb = toProblem bsWorld

        lsWorld = LS.world
        lsEnv = fromWorld lsWorld
        lsProb = toProblem lsWorld

        ssWorld = SS.world
        ssEnv = fromWorld ssWorld
        ssProb = toProblem ssWorld

        wsWorld = WS.world
        wsEnv = fromWorld wsWorld
        wsProb = toProblem wsWorld

        initKnl  = initialKnowledge dom (Env.toState ssEnv)
        dom = sokobanDomain
        astar = Astar Nothing
