module Planning.Viewing where

import Planning
import           Environment
import           Learning.ManyHypothesis
-- import           Learning.SchemaLearning
import qualified Planning                as P
import           Planning.PDDL

import           Control.Monad.Writer
import           Data.Map                (Map)

-- instance Loggable HypBox where
--     logg (HypBox hyp) = logg hyp

-- instance Loggable ManyHypothesis where
--     logg (ManyHypothesis hbs) = concatMap logg hbs

data Environment env => SimState env =
    SimState { transition :: Transition
             , envState   :: env
             , hyp        :: ManyHypothesis
             , step       :: Int
             }

data Environment env => Log env = Log [SimState env]

log :: Environment env => SimState env -> Log env -> Log env
log s (Log l) = Log (s : l)

data View e = View { actionPerformed :: P.Action -> Bool -> IO ()
                   , planMade        :: Maybe P.Plan -> IO ()
                   , envChanged      :: e -> IO ()
                   }
