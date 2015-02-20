module Environment where
import PDDL.Type

class Environment env where
  toState :: env -> State
  fromProblem :: Problem -> env
  applyAction :: env -> Action -> Maybe env
  isGoalReached :: Problem -> env -> Bool
