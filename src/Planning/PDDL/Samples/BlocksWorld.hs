-- | Adapted from http://reason.cs.uiuc.edu/filter/filter/blocks-world-exper/blocks-domain.pddl
module Planning.PDDL.Samples.BlocksWorld where

import qualified Data.Map      as Map
import           Logic.Formula
import           Planning.PDDL

clear :: a -> Predicate a
clear o   = Predicate "clear" [o]

onTable :: a -> Predicate a
onTable b = Predicate "on-table" [b]

armEmpty:: Predicate a
armEmpty  = Predicate "arm-empty" []

holding:: a -> Predicate a
holding b = Predicate "holding" [b]

on:: a -> a -> Predicate a
on b o    = Predicate "on" [b, o]


ob :: String
ob = "?ob"

obr :: Term
obr = TVar ob

uob:: String
uob = "?uob"

uobr :: Term
uobr = TVar uob


pickUp :: ActionSpec
pickUp = ActionSpec
    { asName    = "move"
    , asParas   = [ob]
    , asPrecond = GAnd [ gPos $ clear obr
                       , gPos $ onTable obr
                       , gPos armEmpty
                       ]
    , asEffect  = EAnd [ ePos $ holding obr
                       , eNeg $ clear obr
                       , eNeg $ onTable obr
                       , eNeg   armEmpty
                       ]
    , asConstants = []
    , asTypes = Map.empty
    }

putDown :: ActionSpec
putDown = ActionSpec
    { asName    = "putdown"
    , asParas   = [ob]
    , asPrecond = gPos $ holding obr
    , asEffect  = EAnd [ ePos $ clear obr
                       , ePos   armEmpty
                       , ePos $ onTable obr
                       , eNeg $ holding obr
                       ]
    , asConstants = []
    , asTypes = Map.empty
    }

stack :: ActionSpec
stack = ActionSpec
    { asName = "stack"
    , asParas = [ob, uob]
    , asPrecond = GAnd [ gPos $ clear uobr
                       , gPos $ holding obr
                       ]

    , asEffect  = EAnd [ ePos armEmpty
                       , ePos $ clear obr
                       , ePos $ on obr uobr
                       , eNeg $ clear uobr
                       , eNeg $ holding obr
                       ]
    , asConstants = []
    , asTypes = Map.empty
    }

unStack :: ActionSpec
unStack = ActionSpec
    { asName = "unstack"
    , asParas = [ob, uob]
    , asPrecond = GAnd [ gPos $ on obr uobr
                       , gPos $ clear obr
                       , gPos armEmpty
                       ]
    , asEffect  = EAnd [ ePos $ holding obr
                       , ePos $ clear uobr
                       , eNeg $ on obr uobr
                       , eNeg $ clear obr
                       , eNeg armEmpty
                       ]
    , asConstants = []
    , asTypes = Map.empty
    }
