module Planning.Planner.BFS where

import           Planning

import           Data.Set      (Set)
import qualified Data.Set      as Set
import qualified Data.Queue as Queue
import           Data.Queue (Queue)
-- import Debug.Trace
-- import Text.Show.Pretty

data BFS = BFS

instance (Domain d p as, Problem p) => ExternalPlanner BFS d p as where
    makePlan _ d p = return $ search d p

type Node = (State, [Action])


toNode :: Domain d p a => d -> Node -> Action -> Node
toNode dom (s, rest) act = case apply dom s act of
                            Just s' -> (s', act : rest)
                            Nothing -> error $ "BFS: Inconsistency (action " ++ show act ++
                                             "should be applicable, but apply" ++
                                             "returned nothing)"

isExplored :: Set State -> Node -> Bool
isExplored explored (s, _) = s `Set.member` explored

-- trace' :: Show s => String -> s -> s
-- trace' str val = val
    --trace (str ++ ppShow val) val

maxLength :: Set State -> Int
maxLength s | Set.null s = 0
            | otherwise  = Set.findMax $ Set.map Set.size s

search :: (Domain d p a, Problem p) => d -> p -> Maybe Plan
search dom p =
    do (_, as) <- bfs Set.empty (Queue.fromList [(initialState p, [])])
       return $ reverse as

    where bfs :: Set State -> Queue Node -> Maybe Node
          bfs explored queue = do
            (n@(s, _), queue') <- Queue.dequeue queue
            let actions' as = applicableActions  p s as
                apActs      = concatMap actions' (actions dom)
                uExpl       = filter (not . isExplored explored)
                            $ map (toNode dom n) apActs
                res | s `Set.member` explored = bfs explored queue'
                    | isSolved p s = Just n
                    | otherwise    = bfs (Set.insert s explored) (foldl Queue.enqueue queue' uExpl)
             in res
