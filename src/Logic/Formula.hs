module Logic.Formula
    ( Predicate (..)
    , predName
    , predArgs
    , predArity
    ) where

type Name = String

data Predicate a = Predicate Name [a] deriving (Eq, Ord, Show)

predArity :: Predicate a -> Int
predArity = length . predArgs

instance Functor Predicate where
    fmap f (Predicate n as) = Predicate n (fmap f as)

predName :: Predicate a -> Name
predName (Predicate n _) = n

predArgs :: Predicate a -> [a]
predArgs (Predicate _ as) = as
