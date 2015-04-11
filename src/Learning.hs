module Learning
    ( Knowledge (..)
    , Experiment (..)
    , Strategy (..)
    , Inquirable (..)
    , SimStep (..)
    , scientificMethod
    , runAll
    ) where

import           Control.Monad (foldM, liftM)
import           Data.Maybe

class Knowledge knl info question | knl -> info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool

class Show exp => Experiment exp world info | exp -> world info  where
    conduct :: exp -> world -> IO (world, info)

class Experiment exp world info => Strategy strat world question knl exp info | strat -> exp knl question where
    design :: strat -> question -> knl -> IO( Maybe (exp, strat) )
    update :: strat -> exp -> info -> strat

class Inquirable uni question info | question -> info where
    inquire :: uni -> question -> IO (Maybe info)

data ( Strategy s w q k e i
     , Experiment e w i
     , Knowledge k i q
     ) => SimStep s w q k e i = SimStep
    { ssExp   :: e
    , ssInfo  :: i
    , ssStrat :: s
    , ssWorld :: w
    , ssKnl   :: k
    }

runAll :: ( Strategy strat world question knl exp info
          , Experiment exp world info
          , Inquirable world question info
          , Knowledge knl info question
          )
       => ([SimStep strat world question knl exp info] -> IO ())
       -> strat
       -> knl
       -> [(world, question)]
       -> IO ([SimStep strat world question knl exp info])
runAll logger strat knl quests = foldM runner [] quests where
    runner steps (w, q) = scientificMethod' steps logger strat (knl' steps) w q
    knl' [] = knl
    knl' (step : _) = ssKnl step

scientificMethod :: ( Strategy strat world question knl exp info
                    , Experiment exp world info
                    , Inquirable world question info
                    , Knowledge knl info question
                    )
                 => ([SimStep strat world question knl exp info] -> IO ())
                 -> strat
                 -> knl
                 -> world
                 -> question
                 -> IO ([SimStep strat world question knl exp info])
scientificMethod = scientificMethod' []

scientificMethod' :: ( Strategy strat world question knl exp info
                     , Experiment exp world info
                     , Inquirable world question info
                     , Knowledge knl info question
                     )
        => [SimStep strat world question knl exp info]
        -> ([SimStep strat world question knl exp info] -> IO ())
        -> strat
        -> knl
        -> world
        -> question
        -> IO ([SimStep strat world question knl exp info])
scientificMethod' sss logger strat knl world quest = do
    ms <- updateKnowledge strat knl world quest
    case ms of
        Nothing -> return []
        Just ss | canAnswer (ssKnl ss) quest -> do
                    logger (ss : sss)
                    return (ss : sss)
                | otherwise -> do
                    logger (ss : sss)
                    scientificMethod' (ss : sss) logger (ssStrat ss) (ssKnl ss) (ssWorld ss) quest

updateKnowledge :: ( Strategy strat world question knl exp info
                   , Experiment exp world info
                   , Inquirable world question info
                   , Knowledge knl info question
                   )
                => strat
                -> knl
                -> world
                -> question
                -> IO (Maybe (SimStep strat world question knl exp info))
updateKnowledge strat knowledge world question = do
    information <- inquire world question
    let knowledge' = fromMaybe knowledge (liftM (knowledge `analyze`)  information )
    dres <- design strat question knowledge'
    case dres of
        Nothing -> return Nothing
        Just (experiment, strat') -> do
           (world', testdata) <- conduct experiment world
           let strat'' = update strat' experiment testdata
           let knowledge'' = analyze knowledge' testdata
           return $ Just $ SimStep experiment testdata strat'' world' knowledge''
