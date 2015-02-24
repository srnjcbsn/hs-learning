module Planning.Planner.BFS where

import           Planning

import           Control.Monad (replicateM)
import           Data.Set      (Set, (\\))
import qualified Data.Set      as Set
import qualified Data.Queue as Queue
import           Data.Queue (Queue)

type Node = (State, [Action])

applications :: (ActionSpecification as, Problem p) => as -> p -> [Action]
applications as problem =
    map ((,) (name as)) $ replicateM (arity as) (objects problem)

applicableActions :: (ActionSpecification as, Problem p)
                  => as -> p -> State -> [Action]
applicableActions as problem state =
    filter (isApplicable as state . aArgs) $ applications as problem

toNode :: Domain d a => d -> Node -> Action -> Node
toNode dom (s, rest) act = (apply dom s act, act : rest)

isExplored :: Set State -> Node -> Bool
isExplored explored (s, _) = s `Set.member` explored

search :: (Domain d a, Problem p) => d -> p -> Maybe Plan
search dom p =
    do (_, as) <- bfs (Set.fromList [initialState p]) (Queue.fromList [(initialState p, [])])
       return $ reverse as

    where bfs :: Set State -> Queue Node -> Maybe Node
          bfs explored queue = do
            (n@(s,_), queue') <- Queue.dequeue queue
            let actions' as = applicableActions as p s
                apActs      = concatMap actions' (actions dom)
                uExpl       = filter (not . isExplored explored)
                            $ map (toNode dom n) apActs
                res | s `Set.member` explored = bfs explored queue'
                    | isSolved p s = Just n
                    | otherwise = bfs (Set.insert s explored) (foldl Queue.enqueue queue' uExpl)
             in res
