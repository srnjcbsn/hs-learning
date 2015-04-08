module Learning.PDDL.Experiment where

import Data.List (mapAccumL)
import Environment
import Learning
import qualified Learning.PDDL as Lrn
import           Planning

updateEnv :: Environment env => env -> Action -> (env, Transition)
updateEnv env act = (env', t) where
    env' = applyAction env act
    t = (toState env, act, toState env')

newtype Environment env => PDDLExperiment env = PDDLExperiment Plan

instance Environment env => Experiment (PDDLExperiment env) env Lrn.PDDLInfo where
    conduct (PDDLExperiment acts) env = return $ mapAccumL updateEnv env acts
