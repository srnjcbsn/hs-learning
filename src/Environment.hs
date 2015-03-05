module Environment where
--import Planning.PDDL
import Planning

class Environment env where
  toState :: env -> State
  applyAction :: env -> Action -> Maybe env
