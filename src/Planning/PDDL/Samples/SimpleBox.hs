module Planning.PDDL.Samples.SimpleBox where
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Logic.Formula
import           Planning.PDDL

inside :: String
inside = "inside"
outside :: String
outside = "outside"

insideSpec :: Name -> PredicateSpec
insideSpec pn = Predicate inside [(pn, baseType)]

outsideSpec :: Name -> PredicateSpec
outsideSpec pn = Predicate outside [(pn, baseType)]

pInside :: a -> Predicate a
pInside t = Predicate "inside" [t]

pOutside :: a -> Predicate a
pOutside t = Predicate "outside" [t]

nInside :: Name -> Predicate Term
nInside = pInside . TName

nOutside:: Name -> Predicate Term
nOutside = pOutside . TName



a :: String
a = "?a"

ar :: Term
ar = TVar a

putIn :: ActionSpec
putIn = ActionSpec
    { asName = "put-in"
    , asParas = [a]
    , asPrecond = gPos $ pOutside ar
    , asEffect = EAnd [eNeg $ pOutside ar, ePos $ pInside ar]
    , asConstants = []
    , asTypes = Map.empty
    }
takeOut :: ActionSpec
takeOut = ActionSpec
    { asName = "take-out"
    , asParas = [a]
    , asPrecond = gPos $ pInside ar
    , asEffect = EAnd [eNeg $ pInside ar, ePos $ pOutside ar]
    , asConstants = []
    , asTypes = Map.empty
    }

sBDomain :: PDDLDomain
sBDomain = PDDLDomain
    { dmName = "SimpleBox"
    , dmPredicates = [insideSpec a, outsideSpec a]
    , dmActionsSpecs = [putIn, takeOut]
    , dmConstants = []
    , dmTypes = []
    }

sBProblem :: PDDLProblem
sBProblem = PDDLProblem
    { probName = "put-into-box"
    , probObjs = ["A", "B"]
    , probDomain = "SimpleBox"
    , probState = Set.fromList [pInside "B", pOutside "A"]
    , probGoal = GAnd [ gPos $ nInside "A", gPos $ nOutside "B"]
    , probTypes = Map.empty
    }
newtype SBEnvironment =  SBEnvironment (State, PDDLDomain)

-- instance Environment SBEnvironment where
--   toState (SBEnvironment (s,_)) = s
--   applyAction (SBEnvironment (s,dom)) action =
--     do s' <- apply dom s action
--        return $ SBEnvironment (s', dom)
