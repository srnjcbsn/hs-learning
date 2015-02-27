module Planning.PDDL.Samples.SimpleBox where
import Planning.PDDL.Logic()
import Planning.PDDL
import Logic.Formula
import qualified Data.Set as Set
import qualified Data.Map as Map
import Environment
import Planning
inside a = Predicate "inside" [a]
pInside = Pred . inside

outside a = Predicate "outside" [a]
pOutside = Pred . outside


a = "?a"
ar = Ref a

putIn = ActionSpec
    { asName = "put-in"
    , asParas = [a]
    , asPrecond = pOutside ar
    , asEffect = Con [Neg $ pOutside ar,  pInside ar]
    , asConstants = []
    , asTypes = Map.empty
    }

takeOut = ActionSpec
    { asName = "take-out"
    , asParas = [a]
    , asPrecond = pInside ar
    , asEffect = Con [Neg $ pInside ar, pOutside ar]
    , asConstants = []
    , asTypes = Map.empty
    }

sBDomain = PDDLDomain
    { dmName = "SimpleBox"
    , dmPredicates = [inside a, outside a]
    , dmActionsSpecs = [putIn, takeOut]
    , dmConstants = []
    , dmTypes = []
    }

sBProblem = PDDLProblem
    { probName = "put-into-box"
    , probObjs = ["A", "B"]
    , probDomain = "SimpleBox"
    , probState = Set.fromList [inside "B", outside "A"]
    , probGoal = Con [pInside "A", pOutside "B"]
    , probTypes = Map.empty
    }
newtype SBEnvironment =  SBEnvironment (State, PDDLDomain)

-- instance Environment SBEnvironment where
--   toState (SBEnvironment (s,_)) = s
--   applyAction (SBEnvironment (s,dom)) action =
--     do s' <- apply dom s action
--        return $ SBEnvironment (s', dom)
