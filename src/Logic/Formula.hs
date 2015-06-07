module Logic.Formula
    ( Predicate (..)
    , Literal (..)
    , signAs
    , atom
    , predName
    , predArgs
    , predArity
    , LitPred
    ) where

type Name = String

data Predicate a = Predicate Name [a] deriving (Eq, Ord, Show)

type LitPred a = Literal (Predicate a)

predArity :: Predicate a -> Int
predArity = length . predArgs

instance Functor Predicate where
    fmap f (Predicate n as) = Predicate n (fmap f as)

predName :: Predicate a -> Name
predName (Predicate n _) = n

predArgs :: Predicate a -> [a]
predArgs (Predicate _ as) = as

data Literal a = Pos a
               | Not a
               deriving (Eq, Ord, Show)

instance Functor Literal where
    fmap f (Pos a) = Pos (f a)
    fmap f (Not a) = Not (f a)

-- | Pack 'b' into a literal with same sign as 'a'
signAs :: b -> Literal a -> Literal b
signAs b = fmap (const b)

-- | Extract the atom of a 'Literal', throwing aeay the sign
atom :: Literal a -> a
atom (Pos a) = a
atom (Not a) = a
