module Planning where

import           PDDL.Type

class ExternalPlanner ep where
    makePlan :: ep -> Domain -> Problem -> IO (Maybe Plan)
