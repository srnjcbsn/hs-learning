module Planning.PDDL.Samples.SimpleBox where
import Planning.PDDL.Logic()
import Planning.PDDL
import Logic.Formula
import qualified Data.Set as Set
import qualified Data.Map as Map
import Environment
import Planning

inside = "inside"
outside = "outside"

insideSpec a = Predicate inside [(a, baseType)]
outsideSpec a = Predicate outside [(a, baseType)]

pInside a = Predicate "inside" [a]
fInside = Pred . pInside

pOutside a = Predicate "outside" [a]
fOutside = Pred . pOutside

a = "?a"
ar = Ref a

putIn = ActionSpec
    { asName = "put-in"
    , asParas = [a]
    , asPrecond = fOutside ar
    , asEffect = Con [Neg $ fOutside ar,  fInside ar]
    , asConstants = []
    , asTypes = Map.empty
    }

takeOut = ActionSpec
    { asName = "take-out"
    , asParas = [a]
    , asPrecond = fInside ar
    , asEffect = Con [Neg $ fInside ar, fOutside ar]
    , asConstants = []
    , asTypes = Map.empty
    }

sBDomain = PDDLDomain
    { dmName = "SimpleBox"
    , dmPredicates = [insideSpec a, outsideSpec a]
    , dmActionsSpecs = [putIn, takeOut]
    , dmConstants = []
    , dmTypes = []
    }

sBProblem = PDDLProblem
    { probName = "put-into-box"
    , probObjs = ["A", "B"]
    , probDomain = "SimpleBox"
    , probState = Set.fromList [pInside "B", pOutside "A"]
    , probGoal = Con [fInside "A", fOutside "B"]
    , probTypes = Map.empty
    }
newtype SBEnvironment =  SBEnvironment (State, PDDLDomain)

-- instance Environment SBEnvironment where
--   toState (SBEnvironment (s,_)) = s
--   applyAction (SBEnvironment (s,dom)) action =
--     do s' <- apply dom s action
--        return $ SBEnvironment (s', dom)
