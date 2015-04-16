
module Main where

import           Data.TupleSet                            (TupleSet)
import qualified Data.TupleSet                            as TSet
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
import qualified Learning.PDDL.EffectKnowledge            as Eff
import           Learning.PDDL.Experiment
import           Learning.PDDL.NonConditionalKnowledge
import           Learning.PDDL.OptimisticStrategy
import qualified Learning.PDDL.PreconditionKnowledge      as Pre
import           Planning
import           Planning.PDDL
import Charting

import           Control.Monad                            (unless)
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import qualified Data.Set                                 as Set
import           System.Console.ANSI
import           System.Directory                         (removeFile)
import           System.IO.Error
import           Text.Show.Pretty

instance Inquirable SokobanPDDL PDDLProblem (PDDLInfo SokobanPDDL) where
    inquire _ _ = return Nothing

deltaKnl :: (Pre.PreKnowledge, Eff.EffectKnowledge)
         -> (Pre.PreKnowledge, Eff.EffectKnowledge)
         -> (Hyp Argument, Hyp Argument)
deltaKnl (pk1, ek1) (pk2, ek2) =
    ( deltaHyp (pkHyp pk1) (pkHyp pk2)
    , deltaHyp (ekHyp ek1) (ekHyp ek2)
    )

actKnl :: SokoSimStep -> (Hyp Argument, Hyp Argument)
actKnl step = (pkHyp p, ekHyp e) where
    domKnl = domainKnowledge $ ssKnl step
    act = lastAction step
    (p, e) = case Map.lookup (aName act) domKnl of
               Just k -> k
               Nothing -> error "ERROR"

learned :: SokoSimStep -> SokoSimStep -> (Hyp Argument, Hyp Argument)
learned prev latest = deltaKnl (actKnl prev) (actKnl latest) where
   domKnl = domainKnowledge . ssKnl
   act = lastAction latest
   actKnl s = case Map.lookup (aName act) (domKnl s) of
                Just k -> k
                Nothing -> error "ERROR message"

showWorld :: [SokoSimStep] -> IO ()
showWorld (step : _) = visualize $ ssWorld step
showWorld [] = return ()

showLearned :: [SokoSimStep] -> IO ()
showLearned (step : prev : _) =  print (Set.size (posKnown precs'))
                              >> print (Set.size (negKnown precs'))
                              >> print (Set.size (posKnown effs'))
                              >> print (Set.size (negKnown effs'))
                              >> posPrecMessage >> negPrecMessage
                              >> posEffMessage >> negEffMessage
                              >> nPosPrecMessage >> nNegPrecMessage
                              >> nPosEffMessage >> nNegEffMessage where
    (precs, effs) = learned prev step
    (precs', effs') = actKnl step
    baseMessage = "The following predicates have been proven to be "
    message set str = unless (Set.null set)
                    $ putStrLn $ baseMessage ++ str ++ ppShow set

    posPrecMessage = message (posKnown precs) "positive preconditions: "
    negPrecMessage = message (negKnown precs) "negative preconditions: "
    posEffMessage = message (posKnown effs) "positive effects: "
    negEffMessage = message (negKnown effs) "negative effects: "

    nPosPrecMessage = message (posUnknown precs) "NOT pos prec: "
    nNegPrecMessage = message (negUnknown precs) "NOT neg prec: "
    nPosEffMessage  = message (posUnknown effs) "NOT pos effs: "
    nNegEffMessage  = message (negUnknown effs) "NOT neg effs: "
showLearned _ = return ()

showAct :: [SokoSimStep] -> IO ()
showAct (step : _) = putStrLn $ "executed action " ++ show (lastAction step)
showAct _ = return ()

showBound :: [SokoSimStep] -> IO ()
showBound (step : _) = print b where
    (OptimisticStrategy (_, b)) = ssStrat step
showBound [] = return ()

writeSim :: [SokoSimStep] -> IO ()
writeSim steps =  showAct steps
               >> showWorld steps
               >> showLearned steps
               >> showBound steps
-- writeSim [] = return ()

main :: IO ()
main = do
    catchIOError (removeFile logPath) (\_ -> return ())
    clearScreen
    setTitle "SOKOBAN!"

    -- putStrLn (ppShow $ initialState prob)
    hist <- runAll writeSim optStrat initKnl [ (ssEnv, ssProb)
                                            --  , (lsEnv, lsProb)
                                            --  , (bsEnv, bsProb)
                                             ]
    chartKnowledge hist
    -- hist <- scientificMethod writeSim optStrat initKnl ssEnv ssProb
    -- (knl'', world'') <- scientificMethod emptyIO optStrat knl' lsEnv lsProb
    -- (knl''', world''') <- scientificMethod emptyIO optStrat knl'' bsEnv bsProb
    -- putStrLn (ppShow fenv)
    -- putStrLn (ppShow dom''')
    -- writeFile "sokoDom.pddl" $ writeDomain dom
    -- writeFile "sokoProb.pddl" $ writeProblem wsProb
    return ()
    where
        logPath = "./log.log"

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
