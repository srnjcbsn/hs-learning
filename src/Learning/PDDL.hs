module Learning.PDDL where

-- import           Logic.Formula
import qualified Planning      as P
-- import           Planning.PDDL
import Environment
-- import           Data.TupleSet (TupleSet)
-- import qualified Data.TupleSet as TSet
--
-- import           Control.Monad
-- import           Data.Map      (Map)
-- import           Data.Set      (Set)
-- import qualified Data.Set      as Set

data Environment env => PDDLInfo env = PDDLInfo
    { transitions :: [P.Transition]
    , states      :: [env]
    , stepsDone   :: Int
    } deriving Show
