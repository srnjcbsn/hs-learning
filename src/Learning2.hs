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

class Knowledge knl info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool

class Experiment exp world info where
    conduct :: exp -> world -> IO info

class (Experiment exp world info) => Strategy world knl exp info where
    design :: strat -> knl -> exp


class Inquirable uni question info where
    inquire :: uni -> question -> IO (Maybe info)


answered = undefined

scientificMethod :: ( Strategy world knl exp info
                    , Experiment exp world info
                    , Inquirable world question info)
                 => world -> strat -> knl -> question -> IO (knl)
scientificMethod world strat knowledge question  =
  do information <- inquire world question
     let knowledge' = liftM (analyze knowledge) information
     let loop knl =
          do experiment <- design strat knl
             testdata <- conduct world experiment
             let knl' = analyze knl testdata
             if answered knl' question
             then knl'
             else loop knl'
      in loop >> knowledge'
