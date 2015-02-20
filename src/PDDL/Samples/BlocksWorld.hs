-- | Adapted from http://reason.cs.uiuc.edu/filter/filter/blocks-world-exper/blocks-domain.pddl
module PDDL.Samples.BlocksWorld where

import PDDL



clear o   = ("clear", [o])
onTable b = ("on-table", [b])
armEmpty  = ("arm-empty", [])
holding b = ("holding", [b])
on b o    = ("on", [b, o])

ob = "?ob"
obr = Ref ob

uob = "?uob"
uobr = Ref uob


pickUp :: ActionSpec
pickUp = ActionSpec
    { asName    = "move"
    , asParas   = [ob]
    , asPrecond = Con [ Predicate $ clear obr
                      , Predicate $ onTable obr
                      , Predicate armEmpty
                      ]
    , asEffect  = Con [ Predicate $ holding obr
                      , Neg $ Predicate $ clear obr
                      , Neg $ Predicate $ onTable obr
                      , Neg $ Predicate $ armEmpty
                      ]
    }

putDown = ActionSpec
    { asName    = "putdown"
    , asParas   = [ob]
    , asPrecond = Predicate $ holding obr
    , asEffect  = Con [ Predicate $ clear obr
                      , Predicate  armEmpty
                      , Predicate $ onTable obr
                      , Neg $ Predicate $ holding obr
                      ]
    }

stack = ActionSpec
    { asName = "stack"
    , asParas = [ob, uob]
    , asPrecond = Con [ Predicate $ clear uobr
                      , Predicate $ holding obr
                      ]

    , asEffect  = Con [ Predicate armEmpty
                      , Predicate $ clear obr
                      , Predicate $ on obr uobr
                      , Neg $ Predicate $ clear uobr
                      , Neg $ Predicate $ holding obr
                      ]
    }

unStack = ActionSpec
    { asName = "unstack"
    , asParas = [ob, uob]
    , asPrecond = Con [ Predicate $ on obr uobr
                      , Predicate $ clear obr
                      , Predicate armEmpty
                      ]
    , asEffect  = Con [ Predicate $ holding obr
                      , Predicate $ clear uobr
                      , Neg $ Predicate $ on obr uobr
                      , Neg $ Predicate $ clear obr
                      , Neg $ Predicate $ armEmpty
                      ]
    }
