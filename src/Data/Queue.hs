module Data.Queue (enqueue, dequeue, fromList, toList, Queue) where

newtype Queue a = Queue ([a], [a])
  deriving (Eq, Read, Show)

enqueue  :: Queue a -> a -> Queue a
enqueue (Queue (ins, outs)) item = Queue (item:ins, outs)

dequeue  :: Queue a -> Maybe (a,  Queue a)
dequeue (Queue ([], [])) = Nothing
dequeue (Queue (ins, [])) = dequeue (Queue ([], reverse ins))
dequeue (Queue (ins , item:rest)) = Just (item, Queue(ins, rest))

fromList :: [a] -> Queue a
fromList l = Queue([], l)

toList :: Queue a -> [a]
toList (Queue (l1, l2)) = l1 ++ l2
