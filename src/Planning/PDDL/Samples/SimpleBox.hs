module Planning.PDDL.Samples.SimpleBox where

import Planning.PDDL
import Logic.Formula

import qualified Data.Map as Map

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
