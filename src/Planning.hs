module Planning where

import           PDDL

class ExternalPlanner ep where
    makePlan :: ep -> Domain -> Problem -> IO (Maybe Plan)
