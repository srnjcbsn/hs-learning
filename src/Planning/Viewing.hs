module Planning.Viewing where

import           qualified Planning as P
import Learning.PDDL

import Control.Monad.State

data View e = View { actionPerformed :: P.Action -> Bool -> IO ()
                   , planMade        :: Maybe P.Plan -> IO ()
                   , envChanged      :: e -> IO ()
                   , preHypChanged   :: State DomainHyp DomainHyp
                   }
