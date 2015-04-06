module Planning.Viewing where

import           Environment
import           Learning.SchemaLearning
import qualified Planning                as P
import           Planning.PDDL

import           Control.Monad.Writer
import           Data.Map                (Map)

class Loggable l where
    logg :: l -> IO ()


data (Environment env, LearningDomain dom prob as) => SimState env dom prob as =
    SimState { transition :: Transition
             , envState   :: env
             , lDom       :: dom
             , step       :: Int
             }

data (Environment env, LearningDomain dom prob as) => Log env dom prob as =
    Log [SimState env dom prob as]

log :: (Environment env, LearningDomain dom prob as)
    => SimState env dom prob as
    -> Log env dom prob as
    -> Log env dom prob as
log s (Log l) = Log (s : l)

data View e = View { actionPerformed :: P.Action -> Bool -> IO ()
                   , planMade        :: Maybe P.Plan -> IO ()
                   , envChanged      :: e -> IO ()
                   }
