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
import qualified Learning.PDDL							  as PDDL
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
import qualified Debug.Trace                              as Debug
import           System.Console.ANSI
import           System.Directory                         (removeFile)
import           System.IO.Error
import           Text.Show.Pretty

-- import           Diagrams.Backend.SVG
-- import           Diagrams.Prelude

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
                           (PDDL.PDDLInfo SokobanPDDL)

lastAction :: SokoSimStep -> Action
lastAction step =
    case PDDL.transitions (ssInfo step) of
        ((_, act, _) : _) -> act
        [] -> error "lastAction: Empty transition list in PDDLInfo."

extractKnowledge :: SokoSimStep -> (PDDL.Knowledge Argument, PDDL.Knowledge Argument)
extractKnowledge step = (PDDL.knlFromPk (fst actKnl), PDDL.knlFromEk (snd actKnl))  where
    domKnl = (domainKnowledge . ssKnl)
    act = lastAction step
    actKnl = case Map.lookup (aName act) (domKnl step) of
               Just k -> k
               Nothing -> error "ERROR message"

-- chartSingleKnowledge :: SokoSimStep -> Diagram SVG R2
-- chartSingleKnowledge step = rect 0.3 knsRatio # fc green where
--     knsRatio = fromIntegral ((Set.size . posKnown) (fst $ extractKnowledge step))
--              / fromIntegral (Set.size preds)
--     preds = allPredsForAction (pddlDomain (ssKnl step)) act
--     act = aName (lastAction step)

hypRatio :: Int -> PDDL.Knowledge Argument -> ChartKnl
hypRatio universe h = ( (ratio (PDDL.posKnown h), ratio (PDDL.posUnknown h))
                      , (ratio (PDDL.negKnown h), ratio (PDDL.negUnknown h))
                      )
    where ratio set = fromIntegral (Set.size set) / fromIntegral universe

toRatio :: PDDLDomain
        -> String
        -> (Pre.PreKnowledge, Eff.EffectKnowledge)
        -> (ChartKnl, ChartKnl)
toRatio dom name (pk, ek) =
    let n = Set.size $ allPredsForAction dom name
    in (hypRatio n (PDDL.knlFromPk pk), hypRatio n (PDDL.knlFromEk ek))

toRatios :: PDDLKnowledge e -> Map String (ChartKnl, ChartKnl)
toRatios knl = Map.mapWithKey (toRatio dom) dk where
    dk = domainKnowledge knl
    dom = pddlDomain knl

-- | (Known, Unknown)
type RatioKnl = (Double, Double)

-- | (Positive, Negative)
type ChartKnl = (RatioKnl, RatioKnl)

knowledgeRatios :: [SokoSimStep] -> Map String [(ChartKnl, ChartKnl)]
knowledgeRatios (step : rest) = Map.unionWith (++) ratioMap nextMap where
    knl = ssKnl step
    dk = domainKnowledge knl
    dom = pddlDomain knl
    ratioMap = Map.map (:[]) $ Map.mapWithKey (toRatio dom) dk
    nextMap = knowledgeRatios rest
knowledgeRatios [] = Map.empty

-- chartKnowledge :: [SokoSimStep] -> IO ()
-- chartKnowledge steps =
--     let ratios = Map.toList $ knowledgeRatios steps
--         size = mkSizeSpec (Just 100) (Just 100)
--         chartPrecs, chartEffs :: (String, [(ChartKnl, ChartKnl)]) -> IO ()
--         chartPrecs (name, ls) = renderSVG (name ++ "Precs.svg")
--                                           size
--                                           (chartRatios $ map (fst . fst) ls)
--
--         chartEffs (name, ls) = renderSVG (name ++ "Effects.svg")
--                                          size
--                                          (chartRatios $ map (fst . snd) ls)
--
--         chartRatios :: [RatioKnl] -> Diagram SVG R2
--         chartRatios rs = hcat $ map chartRatio rs
--         chartRatio (r1, r2) = if r2 > 0 then Debug.traceShow r1 $ rect 0.1 (r1 * 100) # fc green -- rect 0.1 r2 # fc green
--                               else rect 0.1 (r1 * 100) # fc green
--     in mapM_ (chartPrecs >> chartEffs) ratios
    -- renderSVG "chart.svg" sizeSpec c where
    -- sizeSpec = mkSizeSpec (Just 100) (Just 100)
    -- c = chartSingleKnowledge step
