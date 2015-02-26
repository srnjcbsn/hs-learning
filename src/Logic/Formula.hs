module Logic.Formula
    ( Predicate (..)
    , Formula (..)
    , predName
    , predArgs
    , negateF
    , conjunction
    ) where

type Name = String

data Predicate a = Predicate Name [a] deriving (Eq, Ord, Show)

predName :: Predicate a -> Name
predName (Predicate n _) = n

predArgs :: Predicate a -> [a]
predArgs (Predicate _ as) = as

data (Eq a, Show a, Ord a) => Formula a
    = Pred (Predicate a)
    | Neg  (Formula a)
    | Con  [Formula a]
    deriving (Eq, Ord, Show)

-- flatten :: Formula -> Formula
-- flatten (Con ((Con f) : fs)) = map flatten f `conjunction` map flatten fs
-- flatten (Neg (Con fs))       = Con (map (Neg . flatten) fs)
-- flatten f                    = f

-- | Negate a 'Formula'. If the given 'Formula' is a conjunction, the contained
--   'Formula'e are negated recursively.
negateF :: (Eq a, Ord a, Show a) => Formula a -> Formula a
negateF (Con fs)   = Con $ map negateF fs
negateF p@(Pred _) = Neg p
negateF (Neg f)    = f

-- | Forms a 'Conjunction' from two 'Formula'e.
conjunction :: (Eq a, Ord a, Show a) => Formula a -> Formula a -> Formula a
conjunction (Con c1) (Con c2)       = Con (c1 ++ c2)
conjunction p@(Pred _) (Con c) = Con (p : c)
conjunction (Con c) p@(Pred _) = Con (p : c)
conjunction (Neg f1) f2 = Neg (conjunction f1 f2)
conjunction f1 (Neg f2) = Neg (conjunction f1 f2)
conjunction f1 f2 = Con [f1, f2]
