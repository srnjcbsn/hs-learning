module Learning2 where

import           Control.Monad
import           Data.Maybe
import           Text.Show.Pretty

import           Environment              (Environment)
import qualified Environment              as Env
import           Planning
import Planning.Viewing
import qualified Learning.SchemaLearning as Lrn

import Data.Map (Map)
import qualified Data.Map as Map

class Knowledge knl info question | knl -> info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool

class Experiment exp world info | exp -> world info  where
    conduct :: exp -> world -> IO (info, world)

class Experiment exp world info => Strategy strat world knl exp info | strat -> exp knl where
    design :: strat -> knl -> exp


class Inquirable uni question info | question -> info where
    inquire :: uni -> question -> IO (Maybe info)


scientificMethod :: ( Strategy strat world knl exp info
                    , Experiment exp world info
                    , Inquirable world question info
                    , Knowledge knl info question )
                 => world -> strat -> knl -> question -> IO knl
scientificMethod world strat knowledge question  =
  do information <- inquire world question
     let knowledge' = fromMaybe knowledge (liftM (knowledge `analyze`)  information )  -- undefined --liftM (analyze knowledge) information
     let loop w knl =
          do let experiment = design strat knl
             (testdata, w') <- conduct experiment w
             let knl' = analyze knl testdata
             if canAnswer knl' question
             then return knl'
              else loop w' knl'
      in loop world knowledge'











      ---fuck søren
