module Environment.Sokoban.PDDL where

import           Environment.Sokoban hiding (Object)
import           PDDL

import           Data.Char                    (isDigit)
import           Data.List                    (partition)
import qualified Data.List                    as List
import           Data.Map                     (Map, member, (!))
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe,catMaybes)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

data SokobanPDDL = SokobanPDDL
    { world           :: World
    , locMap          :: Map Location Coord
    , persistentState :: Set GroundedPredicate
    }

data Adj = Adj (Object, Object) deriving Eq
type Structure = ([Adj], [Adj])

type Location = Object
type Crate = Object

hAdjName, vAdjName, atGoalName :: String
hAdjName = "hAdj"
vAdjName = "vAdj"
atGoalName = "atGoal"

pSokobanAt :: Location -> GroundedPredicate
pSokobanAt loc = ("sokobanAt", [loc])

pAt :: Crate -> Location -> GroundedPredicate
pAt crate loc = ("at", [crate, loc])

pHAdj :: Location -> Location -> GroundedPredicate
pHAdj loc loc2 = (hAdjName, [loc, loc2])

pVAdj :: Location -> Location -> GroundedPredicate
pVAdj loc loc2 = (vAdjName, [loc, loc2])

pAtGoal :: Crate -> GroundedPredicate
pAtGoal crate = (atGoalName, [crate])

pClear :: Location -> GroundedPredicate
pClear loc = ("clear", [loc])

pGoal :: Location -> GroundedPredicate
pGoal loc = ("goal", [loc])

pNotGoal :: Location -> GroundedPredicate
pNotGoal loc = ("notGoal", [loc])

directionFromObjs :: SokobanPDDL -> Location -> Location -> Direction
directionFromObjs pddlw from to =
  let fromPos = locMap pddlw ! from
      toPos = locMap pddlw ! to
   in case toPos - fromPos of
        Coord (0, 1) -> UpDir
        Coord (0, -1) -> DownDir
        Coord (1, 0) -> RightDir
        Coord (-1, 0) -> LeftDir
        _ -> error $  "cannot get direction (fromPos: "
                   ++ show fromPos
                   ++ ", toPos: "
                   ++ show toPos
                   ++ ")"

applyFromLoc :: SokobanPDDL -> Location -> Location -> SokobanPDDL
applyFromLoc pddlw from to =
    pddlw { world = move (world pddlw) $ directionFromObjs pddlw from to }
    --   case move (world pddlw) $ directionFromObjs pddlw from to of
    --     Just world' -> Just pddlw { world = world' }
    --     Nothing -> Nothing

-- type Assert = (World -> Bool)
-- asserts :: [World -> Bool]

locToCoord :: SokobanPDDL -> Location -> Coord
locToCoord pddl loc = locMap pddl ! loc

locationExists :: Location -> SokobanPDDL -> Bool
locationExists loc pddl = member loc (locMap pddl)

sokobanAt :: Location -> SokobanPDDL -> Bool
sokobanAt loc pddl =
    locMap pddl ! loc == (sokoban . world $ pddl)

hAdj, vAdj :: Location -> Location -> SokobanPDDL -> Bool
hAdj a b pddl =
    case (locMap pddl ! a) - (locMap pddl ! b) of
        Coord (-1, 0) -> True
        Coord (1 , 0) -> True
        _             -> False

vAdj a b pddl =
    case (locMap pddl ! a) - (locMap pddl ! b) of
        Coord (0, -1) -> True
        Coord (0,  1) -> True
        _             -> False

tileClear :: Location -> Assertion
tileClear loc pddl =
    case coordMap (world pddl) ! coord of
        Clear -> not $ sokobanAt loc pddl
        Box _ -> False
    where coord = locMap pddl ! loc

crateAt :: Crate -> Location -> Assertion
crateAt c loc pddl =
    case (coordMap . world) pddl ! (locMap pddl ! loc) of
        Box c' -> c == c'
        _      -> False

goal :: Location -> Assertion
goal loc pddl = (locMap pddl ! loc) `elem` (goals . world) pddl

notGoal :: Location -> Assertion
notGoal loc pddl = not $ goal loc pddl

type Assertion = SokobanPDDL -> Bool

asserts :: SokobanPDDL -> [SokobanPDDL -> Bool] -> Bool
asserts pddl = all (\f -> f pddl)


applyAction :: SokobanPDDL -> Action -> Maybe SokobanPDDL
applyAction pddlw ("move-h", [from, to])
    | asserts pddlw [sokobanAt from, tileClear to, hAdj from to] =
        Just $ applyFromLoc pddlw from to
    | otherwise = Nothing

applyAction pddlw ("move-v", [from, to])
    | asserts pddlw [sokobanAt from, tileClear to, vAdj from to] =
        Just $ applyFromLoc pddlw from to
    | otherwise = Nothing

applyAction pddlw ("push-h", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = Just $ applyFromLoc pddlw soko cLoc
    | otherwise = Nothing
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , hAdj soko cLoc
                           , hAdj cLoc toLoc
                           , tileClear toLoc
                           , notGoal toLoc
                           ]

applyAction pddlw ("push-v", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = Just $ applyFromLoc pddlw soko toLoc
    | otherwise = Nothing
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , vAdj soko cLoc
                           , vAdj cLoc toLoc
                           , tileClear toLoc
                           , notGoal toLoc
                           ]

applyAction pddlw ("push-h-goal", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = Just $ applyFromLoc pddlw soko toLoc
    | otherwise = Nothing
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , hAdj soko cLoc
                           , hAdj cLoc toLoc
                           , tileClear toLoc
                           , goal toLoc
                           ]

applyAction pddlw ("push-v-goal", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = Just $ applyFromLoc pddlw soko toLoc
    | otherwise = Nothing
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , vAdj soko cLoc
                           , vAdj cLoc toLoc
                           , tileClear toLoc
                           , goal toLoc
                           ]

applyAction _ act = error ("Unknown action: " ++ show act)

isStructurePred :: GroundedPredicate -> Bool
isStructurePred (name, _)
    | name == vAdjName = True
    | name == hAdjName = True
    | otherwise        = False

parseLocation :: Location -> Coord
parseLocation ('b' : n) = Coord (x, y) where
    (xStr, rest) = span isDigit n
    x = read xStr
    y = read $ tail rest
parseLocation n = error $ "Location " ++ n ++ " could not be parsed."

writeLocation :: Coord -> Location
writeLocation (Coord (x, y)) = ('b' : show x) ++ ('x' : show y)

statePred :: GroundedPredicate -> Maybe StatePred
statePred ("goal", [a]) = Just $ Goal a
statePred ("at", [c, l]) = Just $ CrateLoc c l
statePred ("sokobanAt", [l]) = Just $ SokobanLoc l
-- statePred (name, [a, b])
--     | name == hAdjName || name == vAdjName = Structure (min a b) (max a b)
statePred p = Nothing -- error $ "Could not understand predicate " ++ show p


data StatePred = Structure Location Location
               | CrateLoc Crate Location
               | SokobanLoc Location
               | Goal Location

fromState :: State -> SokobanPDDL
fromState state = pddl where
    (structPreds, rest) = partition isStructurePred $ Set.toList state

    goalPreds    = [ gp  | gp@("goal", _) <- rest ]
    notGoalPreds = [ ngp | ngp@("notGoal", _) <- rest ]

    coordMap = Map.fromList
             $ Set.toList
             $ Set.map (\n -> (n, parseLocation n))
             $ Set.unions
             $ map (\(_, args) -> Set.fromList args) structPreds

    dataList = mapMaybe statePred rest
    -- If this is an empty list, we throw an error, as we wouldn't be able
    -- to go on without knowing the location of Sokoban:
    sokoLoco = coordMap ! head [ l | SokobanLoc l <- dataList ]
    crates   = [ (coordMap ! l, Box c) | CrateLoc c l <- dataList ]
    goals    = [ coordMap ! g | Goal g <- dataList ]

    -- Set all tiles as 'Clear'
    clearTiles = Map.fromList $ zip (Map.elems coordMap) (repeat Clear)
    -- Overwrite the tiles that contain crates
    tileMap  = Map.fromList crates `Map.union` clearTiles

    world = World
        { coordMap = tileMap
        , sokoban  = sokoLoco
        , goals    = goals
        }

    pddl = SokobanPDDL
        { world = world
        , locMap = coordMap
        , persistentState = Set.fromList $ structPreds ++ goalPreds ++ notGoalPreds
        }

worldPreds :: World -> Set GroundedPredicate
worldPreds world = Set.insert sokLoc tilePreds where

    sokLoc = pSokobanAt $ writeLocation $ sokoban world
    -- The tilemap minus the tile sokoban is standing on
    tileMap' = Map.delete (sokoban world) (coordMap world)
    tilePreds = Set.fromList $ map (uncurry tilePred) $ Map.toList tileMap'

    tilePred :: Coord -> Tile -> GroundedPredicate
    tilePred coord Clear = pClear $ writeLocation coord
    tilePred coord (Box n)
        | coord `elem` goals world = pAtGoal n
        | otherwise = pAt n $ writeLocation coord

toState :: SokobanPDDL -> State
toState pddl =
     worldPreds (world pddl) `Set.union` persistentState pddl


adjTiles:: World -> Coord -> ([(Coord,Tile)], [(Coord,Tile)])
adjTiles w coord =
  let vAdj = [Coord (0,1),Coord (0,-1)]
      hAdj = [Coord (1,0),Coord (-1,0)]
      absHAdj = List.map (+ coord) hAdj
      absVAdj = List.map (+ coord) vAdj
      f x = (x, Map.lookup x $ coordMap w)
      tilesH = List.map f absHAdj
      tilesV = List.map f absVAdj
      tOnly tiles = [(c,t) | (c,Just t) <- tiles]
   in (tOnly tilesH, tOnly tilesV)

fromWorld :: World -> SokobanPDDL
fromWorld w =
  let cm = coordMap w
      addAdjsToSet coord _ allPreds =
          let (hAdjs, vAdjs) = adjTiles w coord
              cLoc = writeLocation coord
              toLocs l = [writeLocation c | (c,_) <- l]
              hlocs = toLocs hAdjs
              vlocs = toLocs vAdjs
              vPreds = List.map (pVAdj cLoc) vlocs
              hPreds = List.map (pHAdj cLoc) hlocs
           in Set.union allPreds (Set.fromList $ vPreds ++ hPreds)
      persistAdjs = Map.foldWithKey addAdjsToSet Set.empty cm
      persistGoals = List.map (pGoal . writeLocation) $ goals w
      locs = Map.fromList [(writeLocation c, c) | (c, _) <- Map.toList cm]
      persist = Set.union persistAdjs (Set.fromList persistGoals)
   in SokobanPDDL { world = w, locMap = locs, persistentState = persist}

toProblem :: World -> Problem
toProblem pWorld =
    let crateObjs = [ t | Box t <- Map.elems (coordMap pWorld) ]
        structObjs = map writeLocation $ Map.keys (coordMap pWorld)
        goalPred c = Predicate (atGoalName, [Const c])
        goalsF = Con $ map goalPred crateObjs
    in Problem
        { probName   = "sokoban"
        , probObjs   = crateObjs ++ structObjs
        , probDomain = "sokoban"
        , probState  = (toState . fromWorld) pWorld
        , probGoal   = goalsF
        }
