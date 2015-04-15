{-# LANGUAGE NoMonomorphismRestriction #-}

module Charting where

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

import           Control.Monad                            (unless)
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import qualified Data.Set                                 as Set
import           System.Console.ANSI
import           System.Directory                         (removeFile)
import           System.IO.Error
import           Text.Show.Pretty

import Diagrams.Backend.SVG
import Diagrams.Prelude

data Astar = Astar (Maybe Int) deriving Show

instance BoundedPlanner Astar where
  setBound (Astar _) = Astar

instance ExternalPlanner Astar PDDLDomain PDDLProblem ActionSpec where
  makePlan (Astar bound) d p =
    case bound of
      Just b -> return $ Astar.searchBounded (PDDLGraph (d,p)) (initialState p) b
      Nothing -> return $ Astar.search (PDDLGraph (d,p)) (initialState p)

type SokoSimStep = SimStep (OptimisticStrategy Astar SokobanPDDL)
                           SokobanPDDL
                           PDDLProblem
                           (PDDLKnowledge SokobanPDDL)
                           (PDDLExperiment SokobanPDDL)
                           (PDDLInfo SokobanPDDL)

lastAction :: SokoSimStep -> Action
lastAction step =
    case transitions (ssInfo step) of
        ((_, act, _) : _) -> act
        [] -> error "lastAction: Empty transition list in PDDLInfo."

extractKnowledge :: SokoSimStep -> (Hyp Argument, Hyp Argument)
extractKnowledge step = (pkHyp (fst actKnl), ekHyp (snd actKnl))  where
    domKnl = (domainKnowledge . ssKnl)
    act = lastAction step
    actKnl = case Map.lookup (aName act) (domKnl step) of
               Just k -> k
               Nothing -> error "ERROR message"

chartSingleKnowledge :: SokoSimStep -> Diagram SVG R2
chartSingleKnowledge step = rect 0.3 knsRatio # fc green where
    knsRatio = fromIntegral ((Set.size . posKnown) (fst $ extractKnowledge step))
             / fromIntegral (Set.size preds)
    preds = allPredsForAction (pddlDomain (ssKnl step)) act
    act = aName (lastAction step)

chartKnowledge :: [SokoSimStep] -> IO ()
chartKnowledge (step : _) = renderSVG "chart.svg" sizeSpec c where
    sizeSpec = mkSizeSpec (Just 100) (Just 100)
    c = chartSingleKnowledge step
