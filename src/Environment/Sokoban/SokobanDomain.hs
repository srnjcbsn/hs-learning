module Environment.Sokoban.SokobanDomain where

import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic ()

fluentPredicate :: PredicateSpec -> [Name] -> FluentPredicate
fluentPredicate (Predicate name _) ns = Predicate name $ map Ref ns

groundedPredicate :: PredicateSpec -> [Name] -> GroundedPredicate
groundedPredicate (Predicate name _) = Predicate name

mkActionSpec :: Name
             -> [Name]
             -> Map Name Type
             -> ([Name] -> Formula Argument)
             -> ([Name] -> Formula Argument)
             -> ActionSpec
mkActionSpec name paras t conds effs =
    ActionSpec { asConstants  = []
               , asName    = name
               , asParas   = paras
               , asPrecond = conds paras
               , asEffect  = effs paras
               , asTypes   = t
               }
con :: [(PredicateSpec, [Name])] -> Formula Argument
con = Con . map (Pred . uncurry fluentPredicate)

conGrounded :: [(PredicateSpec, [Name])] -> Formula Name
conGrounded = Con . map (Pred . uncurry groundedPredicate)

negCon :: [(PredicateSpec, [Name])] -> Formula Argument
negCon = Con . map (Neg . Pred . uncurry fluentPredicate)

negConGrounded :: [(PredicateSpec, [Name])] -> Formula Name
negConGrounded = Con . map (Neg . Pred . uncurry groundedPredicate)

hAdj, vAdj, sokobanAt, at, atGoal, clear, goal, notGoal :: PredicateSpec
hAdj      = Predicate "hAdj" ["from", "to"]
vAdj      = Predicate "vAdj" ["from", "to"]
sokobanAt = Predicate "sokobanAt" ["l"]
at        = Predicate "at" ["c", "l"]
atGoal    = Predicate "atGoal" ["c"]
clear     = Predicate "clear" ["l"]
goal      = Predicate "goal" ["l"]
notGoal   = Predicate "notGoal" ["l"]

moveParas, pushParas :: [Name]
moveParas = ["from", "to"]
pushParas = ["c", "sokoban", "from", "to"]

crateType, locType :: Type
crateType = "crate"
locType = "location"

moveTypes, pushTypes :: Map Name Type
moveTypes = Map.fromList $ zip moveParas [locType, locType]
pushTypes = Map.fromList $ zip pushParas [crateType, locType, locType, locType]

predicate :: PredicateSpec -> [Name] -> Formula Argument
predicate ps paras = Pred $ fluentPredicate ps paras

moveCond :: PredicateSpec -> [Name] -> Formula Argument
moveCond adjPred [from, to] =
    con [ (sokobanAt, [from])
        , (adjPred, [from, to])
        , (clear, [to])
        ]

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

pushCondGoal :: PredicateSpec  -> [Name] -> Formula Argument
pushCondGoal adjPred  [c, soko, from, to] =
    con [ (sokobanAt, [soko])
        , (at, [c, from])
        , (adjPred, [soko, from])
        , (adjPred, [from, to])
        , (clear, [to])
        , (goal, [to])
        ]

moveEff :: [Name] -> Formula Argument
moveEff [from, to] = poss `conjunction` mapNegate negs where
    poss = con [ (sokobanAt, [to])
               , (clear, [from])
               ]

    negs = con [ (sokobanAt, [from])
               , (clear, [to])
               ]

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

pushEffGoal :: [Name] -> Formula Argument
pushEffGoal paras@(c : _) = pushEffShared paras `conjunction` gPred
    where gPred = Pred (fluentPredicate atGoal [c])

pushEffNotGoal :: [Name] -> Formula Argument
pushEffNotGoal paras@(c : _) = pushEffShared paras `conjunction` ngPred
    where ngPred = Neg $ Pred (fluentPredicate atGoal [c])

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
