module Learning.PDDL.Experiment where

import           Environment
import           Learning
import qualified Learning.PDDL       as Lrn
import           Planning
import           Planning.PDDL
import qualified Planning.PDDL.Logic as L ()

updateEnv :: Environment env
          => env
          -> PDDLDomain
          -> PDDLEnvSpec
          -> Int
          -> Plan
          -> (env, Lrn.PDDLInfo env)
updateEnv env dom envSpec n (act : rest)
    | hs == Just s' = (env'', Lrn.PDDLInfo (t : trans) (env'' : envs) n')
    | otherwise     = (env', Lrn.PDDLInfo [t] [env'] n)
    where env' = applyAction env act
          s = toState env
          s' = toState env'
          prob = envSpecToProblem envSpec
          hs = apply dom prob s act
          t = (s, act, s')
          (env'', Lrn.PDDLInfo trans envs n') = updateEnv env' dom envSpec (n + 1) rest

updateEnv env _ _ n [] = (env, Lrn.PDDLInfo [] [env] n)

data Environment env => PDDLExperiment env = PDDLExperiment Plan PDDLDomain PDDLEnvSpec
    deriving Show

instance Environment env => Experiment (PDDLExperiment env) env (Lrn.PDDLInfo env) where
    conduct (PDDLExperiment plan dom spec) env = return newEnv where
        (e, Lrn.PDDLInfo ts envs n) = updateEnv env dom spec 0 plan
        newEnv = (e, Lrn.PDDLInfo (reverse ts) (reverse envs) n)
