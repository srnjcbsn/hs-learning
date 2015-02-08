module PDDL.Samples.SimpleBox where

import PDDL.Type

inside a = ("inside", [a])
pInside = Predicate . inside

outside a = ("outside", [a])
pOutside = Predicate . outside


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

sBDomain = Domain
    { dmName = "SimpleBox"
    , dmPredicates = [inside a, outside a]
    , dmActionsSpecs = [putIn, takeOut]
    , dmConstants = []
    }
