module Environment.Sokoban.SokobanDomain where

import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Logic.Formula
import           Planning
import           Planning.PDDL
-- import           Planning.PDDL.Logic ()

fluentPredicate :: PredicateSpec -> [Name] -> FluentPredicate
fluentPredicate (Predicate pname _) ns = Predicate pname $ map Ref ns

groundedPredicate :: PredicateSpec -> [Name] -> GroundedPredicate
groundedPredicate (Predicate pname _) = Predicate pname

-- mkActionSpec :: Name
--              -> [Name]
--              -> Map Name Type
--              -> ([Name] -> Formula Argument)
--              -> ([Name] -> Formula Argument)
--              -> ActionSpec
-- mkActionSpec aname paras t conds effs =
--     ActionSpec { asConstants  = []
--                , asName    = aname
--                , asParas   = paras
--                , asPrecond = conds paras
--                , asEffect  = effs paras
--                , asTypes   = t
--                }

mkActionSpec :: Name
             -> [Variable]
             -> Map Name Type
             -> ([Variable] -> GoalDesc)
             -> ([Variable] -> Effect)
             -> ActionSpec
mkActionSpec aname paras t conds effs =
    ActionSpec { asConstants  = []
               , asName    = aname
               , asParas   = paras
               , asPrecond = conds paras
               , asEffect  = effs paras
               , asTypes   = t
               }

con :: [(PredicateSpec, [Name])] -> GoalDesc
con = GAnd . map (G . Pos . uncurry fluentPredicate)

conGrounded :: [(PredicateSpec, [Name])] -> Formula Name
conGrounded = Con . map (Pred . uncurry groundedPredicate)

negCon :: [(PredicateSpec, [Name])] -> Formula Argument
negCon = Con . map (Neg . Pred . uncurry fluentPredicate)

negConGrounded :: [(PredicateSpec, [Name])] -> Formula Name
negConGrounded = Con . map (Neg . Pred . uncurry groundedPredicate)

hAdj, vAdj, sokobanAt, at, atGoal, clear, goal, notGoal :: PredicateSpec
hAdj      = Predicate "hAdj" $ zip ["from", "to"] $ repeat locType
vAdj      = Predicate "vAdj" $ zip ["from", "to"] $ repeat locType
sokobanAt = Predicate "sokobanAt" [("l", locType)]
at        = Predicate "at" [("c", crateType), ("l", locType)]
atGoal    = Predicate "atGoal" [("c", crateType)]
clear     = Predicate "clear" [("l", locType)]
goal      = Predicate "goal" [("l", locType)]
notGoal   = Predicate "notGoal" [("l", locType)]

moveParas, pushParas :: [Name]
moveParas = ["from", "to"]
pushParas = ["c", "sokoban", "from", "to"]

crateType, locType :: Type
crateType = "crate"
locType = "location"

moveTypes, pushTypes :: Map Name Type
moveTypes = Map.fromList $ zip moveParas [locType, locType]
pushTypes = Map.fromList $ zip pushParas [crateType, locType, locType, locType]

-- predicate :: PredicateSpec -> [Variable] -> Formula Argument
-- predicate ps paras = Pred $ fluentPredicate ps paras

wrongArityError :: Show a => String -> [a] -> Int -> String
wrongArityError n args ar = n ++ " called with wrong arity. Arguments: "
                            ++ show args ++ " but arity is " ++ show ar

moveCond :: PredicateSpec -> [Variable] -> GoalDesc
moveCond adjPred [from, to] =
    con [ (sokobanAt, [from])
        , (adjPred, [from, to])
        , (clear, [to])
        ]
moveCond _ args = error $ wrongArityError "moveCond" args 2

pushCondNotGoal :: PredicateSpec  -> [Name] -> Formula Argument
pushCondNotGoal adjPred [c, soko, from, to] = poss `conjunction` mapNegate negs where
    poss = con  [ (sokobanAt, [soko])
                , (at, [c, from])
                , (adjPred, [soko, from])
                , (adjPred, [from, to])
                , (clear, [to])
                ]
    negs = con  [ (goal, [to])
                ]
pushCondNotGoal _ args = error $ wrongArityError "pushCondNotGoal" args 4

pushCondGoal :: PredicateSpec  -> [Name] -> Formula Argument
pushCondGoal adjPred  [c, soko, from, to] =
    con [ (sokobanAt, [soko])
        , (at, [c, from])
        , (adjPred, [soko, from])
        , (adjPred, [from, to])
        , (clear, [to])
        , (goal, [to])
        ]
pushCondGoal _ args = error $ wrongArityError "pushCondGoal" args 4

moveEff :: [Name] -> Formula Argument
moveEff [from, to] = poss `conjunction` mapNegate negs where
    poss = con [ (sokobanAt, [to])
               , (clear, [from])
               ]

    negs = con [ (sokobanAt, [from])
               , (clear, [to])
               ]
moveEff args = error $ wrongArityError "moveEff" args 2

pushEffShared :: [Name] -> Formula Argument
pushEffShared [c, soko, from, to] = poss `conjunction` mapNegate negs where
    poss = con
            [ (sokobanAt, [from])
            , (at, [c, to])
            , (clear, [soko])
            ]

    negs = con
            [ (sokobanAt, [soko])
            , (at, [c, from])
            , (clear, [to])
            ]
pushEffShared args = error $ wrongArityError "pushEffShared" args 4

pushEffGoal :: [Name] -> Formula Argument
pushEffGoal paras@(c : _) = pushEffShared paras `conjunction` gPred
    where gPred = Pred (fluentPredicate atGoal [c])
pushEffGoal args = error $ wrongArityError "pushEffGoal" args 4

pushEffNotGoal :: [Name] -> Formula Argument
pushEffNotGoal paras@(c : _) = pushEffShared paras `conjunction` ngPred
    where ngPred = Neg $ Pred (fluentPredicate atGoal [c])
pushEffNotGoal args = error $ wrongArityError "pushEffNotGoal" args 4

moveH, moveV, pushH, pushV, pushHGoal, pushVGoal :: ActionSpec
moveH     = mkActionSpec "move-h" moveParas moveTypes (moveCond hAdj) moveEff
moveV     = mkActionSpec "move-v" moveParas moveTypes (moveCond vAdj) moveEff
pushH     = mkActionSpec "push-h" pushParas pushTypes (pushCondNotGoal hAdj ) pushEffNotGoal
pushV     = mkActionSpec "push-v" pushParas pushTypes (pushCondNotGoal vAdj ) pushEffNotGoal
pushHGoal = mkActionSpec "push-h-goal" pushParas pushTypes (pushCondGoal hAdj ) pushEffGoal
pushVGoal = mkActionSpec "push-v-goal" pushParas pushTypes (pushCondGoal vAdj ) pushEffGoal

sokobanDomain :: PDDLDomain
sokobanDomain = PDDLDomain
    { dmName = "sokobanDom"
    , dmPredicates = [hAdj, vAdj, sokobanAt, at, atGoal, clear, goal]
    , dmActionsSpecs = [moveH, moveV, pushH, pushV, pushHGoal, pushVGoal]
    , dmConstants = []
    , dmTypes = []
    }
