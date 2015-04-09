module Learning.PDDL.Experiment where

import Environment
import Learning
import Planning.PDDL
import qualified Learning.PDDL as Lrn
import           Planning
import qualified Planning.PDDL.Logic as L ()

updateEnv :: Environment env
          => env
          -> PDDLDomain
          -> Int
          -> Plan
          -> (env, Lrn.PDDLInfo env)
updateEnv env dom n (act : rest)
    | hs == Just s' = (env'', Lrn.PDDLInfo (t : trans) (env'' : envs) n')
    | otherwise     = (env', Lrn.PDDLInfo [t] [env'] n)
    where env' = applyAction env act
          s = toState env
          s' = toState env'
          hs = apply dom s act
          t = (s, act, s')
          (env'', Lrn.PDDLInfo trans envs n') = updateEnv env' dom (n + 1) rest

updateEnv env _ n [] = (env, Lrn.PDDLInfo [] [env] n)

data Environment env => PDDLExperiment env = PDDLExperiment Plan PDDLDomain
    deriving Show

instance Environment env => Experiment (PDDLExperiment env) env (Lrn.PDDLInfo env) where
    conduct (PDDLExperiment plan dom) env = return newEnv where
        (e, Lrn.PDDLInfo ts envs n) = updateEnv env dom 0 plan
        newEnv = (e, Lrn.PDDLInfo (reverse ts) (reverse envs) n)
