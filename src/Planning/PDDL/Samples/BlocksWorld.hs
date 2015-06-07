-- | Adapted from http://reason.cs.uiuc.edu/filter/filter/blocks-world-exper/blocks-domain.pddl
module Planning.PDDL.Samples.BlocksWorld where

import Planning.PDDL
import Logic.Formula

clear o   = Predicate "clear" [o]
onTable b = Predicate "on-table" [b]
armEmpty  = Predicate "arm-empty" []
holding b = Predicate "holding" [b]
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
    , asEffect  = Con [ Pred $ holding obr
                      , Neg $ Pred $ clear obr
                      , Neg $ Pred $ onTable obr
                      , Neg $ Pred $ armEmpty
                      ]
    }

putDown = ActionSpec
    { asName    = "putdown"
    , asParas   = [ob]
    , asPrecond = Pred $ holding obr
    , asEffect  = Con [ Pred $ clear obr
                      , Pred  armEmpty
                      , Pred $ onTable obr
                      , Neg $ Pred $ holding obr
                      ]
    }

stack = ActionSpec
    { asName = "stack"
    , asParas = [ob, uob]
    , asPrecond = Con [ Pred $ clear uobr
                      , Pred $ holding obr
                      ]

    , asEffect  = Con [ Pred armEmpty
                      , Pred $ clear obr
                      , Pred $ on obr uobr
                      , Neg $ Pred $ clear uobr
                      , Neg $ Pred $ holding obr
                      ]
    }

unStack = ActionSpec
    { asName = "unstack"
    , asParas = [ob, uob]
    , asPrecond = Con [ Pred $ on obr uobr
                      , Pred $ clear obr
                      , Pred armEmpty
                      ]
    , asEffect  = Con [ Pred $ holding obr
                      , Pred $ clear uobr
                      , Neg $ Pred $ on obr uobr
                      , Neg $ Pred $ clear obr
                      , Neg $ Pred $ armEmpty
                      ]
    }
