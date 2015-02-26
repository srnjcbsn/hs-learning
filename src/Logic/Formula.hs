module Logic.Formula
    ( Predicate (..)
    , Formula (..)
    , predName
    , predArgs
    , mapNegate
    , conjunction
    , evaluateCWA
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Name = String

data Predicate a = Predicate Name [a] deriving (Eq, Ord, Show)

instance Functor Predicate where
    fmap f (Predicate n as) = Predicate n (fmap f as)

predName :: Predicate a -> Name
predName (Predicate n _) = n

predArgs :: Predicate a -> [a]
predArgs (Predicate _ as) = as

data Formula a
    = Pred (Predicate a)
    | Neg  (Formula a)
    | Con  [Formula a]
    deriving (Eq, Ord, Show)

instance Functor Formula where
    fmap f (Pred p) = Pred $ fmap f p
    fmap f (Neg n)  = Neg  $ fmap f n
    fmap f (Con fs) = Con  $ fmap (fmap f) fs

evaluateCWA :: Ord a => Formula a -> Set (Predicate a) -> Bool
evaluateCWA (Pred p) s = Set.member p s
evaluateCWA (Neg f)  s = not $ evaluateCWA f s
evaluateCWA (Con fs) s = all (`evaluateCWA` s) fs

-- flatten :: Formula -> Formula
-- flatten (Con ((Con f) : fs)) = map flatten f `conjunction` map flatten fs
-- flatten (Neg (Con fs))       = Con (map (Neg . flatten) fs)
-- flatten f                    = f

-- | Negate a 'Formula'. If the given 'Formula' is a conjunction, the contained
--   'Formula'e are negated recursively.
mapNegate :: (Eq a, Ord a, Show a) => Formula a -> Formula a
mapNegate (Con fs) = Con $ map Neg fs
mapNegate (Neg f)  = f
mapNegate f        = Neg f

-- | Forms a 'Conjunction' from two 'Formula'e.
conjunction :: (Eq a, Ord a, Show a) => Formula a -> Formula a -> Formula a
conjunction (Con c1) (Con c2)       = Con (c1 ++ c2)
conjunction p@(Pred _) (Con c) = Con (p : c)
conjunction (Con c) p@(Pred _) = Con (p : c)
conjunction (Neg f1) f2 = Neg (conjunction f1 f2)
conjunction f1 (Neg f2) = Neg (conjunction f1 f2)
conjunction f1 f2 = Con [f1, f2]
