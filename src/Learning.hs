module Learning where

import           Control.Monad
import           Data.Maybe

class Knowledge knl info question | knl -> info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool

class Experiment exp world info | exp -> world info  where
    conduct :: exp -> world -> IO (world, info)

class Experiment exp world info => Strategy strat world question knl exp info | strat -> exp knl where
    design :: strat -> question -> knl -> IO( Maybe (exp, strat) )

class Inquirable uni question info | question -> info where
    inquire :: uni -> question -> IO (Maybe info)


scientificMethod :: ( Strategy strat world question knl exp info
                    , Experiment exp world info
                    , Inquirable world question info
                    , Knowledge knl info question )
                 => strat -> knl -> world -> question -> IO (knl, world)
scientificMethod strat knowledge world  question  =
  do information <- inquire world question
     let knowledge' = fromMaybe knowledge (liftM (knowledge `analyze`)  information )  -- undefined --liftM (analyze knowledge) information
     dres <- design strat question knowledge'
     case dres of
      Just (experiment,strat') -> do
       (world', testdata) <- conduct experiment world
       let knowledge'' = analyze knowledge' testdata

       if canAnswer knowledge'' question
       then return (knowledge'', world')
       else scientificMethod strat' knowledge'' world' question
      Nothing -> return (knowledge', world)
