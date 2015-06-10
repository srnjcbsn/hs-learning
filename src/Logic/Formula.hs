module Logic.Formula
    ( Predicate (..)
    , Literal (..)
    , signAs
    , signs
    , flipSign
    , atom
    , predName
    , predArgs
    , predArity
    , LitPred
    ) where

type Name = String

data Predicate a = Predicate Name [a] deriving (Eq, Ord, Show)

type LitPred a = Literal (Predicate a)

instance Functor Predicate where
    fmap f (Predicate n as) = Predicate n (fmap f as)

predName :: Predicate a -> Name
predName (Predicate n _) = n

predArgs :: Predicate a -> [a]
predArgs (Predicate _ as) = as

predArity :: Predicate a -> Int
predArity = length . predArgs

data Literal a = Pos a
               | Neg a
               deriving (Eq, Ord, Show)

instance Functor Literal where
    fmap f (Pos a) = Pos (f a)
    fmap f (Neg a) = Neg (f a)

flipSign :: Literal a -> Literal a
flipSign (Pos a) = Neg a
flipSign (Neg a) = Pos a

-- | Pack 'b' into a literal with same sign as 'a'
--
-- >>> "p" `signAs` (Neg "q")
-- Neg "p"
signAs :: b -> Literal a -> Literal b
signAs b = fmap (const b)

-- | A flipped version of 'signAs'
--
-- >>> (Pos "p") `signs` "q"
-- Pos "q"
signs :: Literal a -> b -> Literal b
signs = flip signAs

-- | Extract the atom of a 'Literal', throwing away the sign
atom :: Literal a -> a
atom (Pos a) = a
atom (Neg a) = a
