module Planning.PDDL.Samples.SimpleBox where

import Planning.PDDL
import Logic.Formula

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
    }

takeOut = ActionSpec
    { asName = "take-out"
    , asParas = [a]
    , asPrecond = pInside ar
    , asEffect = Con [Neg $ pInside ar, pOutside ar]
    }

sBDomain = PDDLDomain
    { dmName = "SimpleBox"
    , dmPredicates = [inside a, outside a]
    , dmActionsSpecs = [putIn, takeOut]
    , dmConstants = []
    }
