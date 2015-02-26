module Environment where
import Planning.PDDL

class Environment env where
  toState :: env -> State
  fromProblem :: PDDLProblem -> env
  applyAction :: env -> Action -> Maybe env
