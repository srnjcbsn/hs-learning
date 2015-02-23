module Environment.Sokoban.SokobanDomain where

import Planning.PDDL
import Planning.PDDL.Logic

fluentPredicate :: PredicateSpec -> [Name] -> FluentPredicate
fluentPredicate (name, _) ns = (name, map Ref ns)

mkActionSpec :: Name
             -> [Name]
             -> ([Name] -> Formula)
             -> ([Name] -> Formula)
             -> ActionSpec
mkActionSpec name paras conds effs =
    ActionSpec { asName    = name
               , asParas   = paras
               , asPrecond = conds paras
               , asEffect  = effs paras
               }
con :: [(PredicateSpec, [Name])] -> Formula
con = Con . map (Predicate . uncurry fluentPredicate)

negCon :: [(PredicateSpec, [Name])] -> Formula
negCon = Con . map (Neg . Predicate . uncurry fluentPredicate)

hAdj, vAdj, sokobanAt, at, atGoal, clear, goal, notGoal :: PredicateSpec
hAdj      = ("hAdj", ["from", "to"])
vAdj      = ("vAdj", ["from", "to"])
sokobanAt = ("sokobanAt", ["l"])
at        = ("at", ["c", "l"])
atGoal    = ("atGoal", ["c"])
clear     = ("clear", ["l"])
goal      = ("goal", ["l"])
notGoal   = ("notGoal", ["l"])

moveParas, pushParas :: [Name]
moveParas = ["from", "to"]
pushParas = ["c", "sokoban", "from", "to"]

predicate :: PredicateSpec -> [Name] -> Formula
predicate ps paras = Predicate $ fluentPredicate ps paras

moveCond :: PredicateSpec -> [Name] -> Formula
moveCond adjPred [from, to] =
    con [ (sokobanAt, [from])
        , (adjPred, [from, to])
        , (clear, [to])
        ]

pushCond :: PredicateSpec -> PredicateSpec -> [Name] -> Formula
pushCond adjPred goalPred [c, soko, from, to] =
    con [ (sokobanAt, [from])
        , (at, [c, from])
        , (adjPred, [soko, from])
        , (adjPred, [from, to])
        , (clear, [to])
        , (goalPred, [to])
        ]

moveEff :: [Name] -> Formula
moveEff [from, to] = poss `conjunction` negateF negs where
    poss = con [ (sokobanAt, [to])
               , (clear, [from])
               ]

    negs = con [ (sokobanAt, [from])
               , (clear, [to])
               ]

pushEffShared :: [Name] -> Formula
pushEffShared [c, soko, from, to] = poss `conjunction` negateF negs where
    poss = con [ (sokobanAt, [from])
               , (at, [c, to])
               , (clear, [soko])
               ]

    negs = con [ (sokobanAt, [soko])
               , (at, [c, from])
               , (clear, [to])
               ]

pushEffGoal :: [Name] -> Formula
pushEffGoal paras@(c : _) = pushEffShared paras `conjunction` gPred
    where gPred = Predicate (fluentPredicate atGoal [c])

pushEffNotGoal :: [Name] -> Formula
pushEffNotGoal paras@(c : _) = pushEffShared paras `conjunction` ngPred
    where ngPred = Neg $ Predicate (fluentPredicate atGoal [c])

moveH, moveV, pushH, pushV, pushHGoal, pushVGoal :: ActionSpec
moveH     = mkActionSpec "move-h" moveParas (moveCond hAdj) moveEff
moveV     = mkActionSpec "move-v" moveParas (moveCond vAdj) moveEff
pushH     = mkActionSpec "push-h" pushParas (pushCond hAdj notGoal) pushEffNotGoal
pushV     = mkActionSpec "push-v" pushParas (pushCond vAdj notGoal) pushEffNotGoal
pushHGoal = mkActionSpec "push-h-goal" pushParas (pushCond hAdj goal) pushEffGoal
pushVGoal = mkActionSpec "push-v-goal" pushParas (pushCond vAdj goal) pushEffGoal

sokobanDomain :: PDDLDomain
sokobanDomain = PDDLDomain
    { dmName = "sokobanDom"
    , dmPredicates = [hAdj, vAdj, sokobanAt, at, atGoal, clear, goal, notGoal]
    , dmActionsSpecs = [moveH, moveV, pushH, pushV, pushHGoal, pushVGoal]
    , dmConstants = []
    }
