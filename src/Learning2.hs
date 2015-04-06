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

type Transition = (State, Action, State)

class Hypthesis hyp where
    update :: hyp -> Action -> hyp

class Experiment exp where
    next :: exp -> (Action, exp)
    --validate ::

class (Experiment exp, Hypthesis hyp) => Strategy hyp exp where
    --form :: hypthesis -> exp

class Inquirable i q a where
    inquire :: i -> q -> IO (Maybe a)


conduct world experiment = undefined
form = undefined
analyze = undefined
interpret = undefined

--scientificMethod ::
scientificMethod world strat knowledge question  =
  do information <- inquire world question
     let loop knl =
          do (hypthesis, experiment) <- form strat knl information
             testdata <- conduct world experiment
             let knl' = analyze knl testdata
             if interpret knl' question
             then knl'
             else loop knl'
      in loop knowledge
