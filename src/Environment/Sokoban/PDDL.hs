module Environment.Sokoban.PDDL where

import qualified Environment         as Env
import           Environment.Sokoban hiding (Object)
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Learning
import qualified Learning.PDDL       as PDDL

import           Data.Char           (isDigit)
import           Data.List           (partition)
import qualified Data.List           as List
import           Data.Map            (Map, member, (!))
import qualified Data.Map            as Map
import           Data.Maybe          (mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
data SokobanPDDL = SokobanPDDL
    { world           :: World
    , locMap          :: Map Location Coord
    , persistentState :: Set GroundedPredicate
    } deriving (Show)

instance Inquirable SokobanPDDL PDDLProblem (PDDL.PDDLInfo SokobanPDDL) where
    inquire _ _ = return Nothing

instance Env.Environment SokobanPDDL where
    toState     = toState
    --fromProblem = fromState . probState
    applyAction = applyAction

data Adj = Adj (Object, Object) deriving Eq
type Structure = ([Adj], [Adj])

type Location = Object
type Crate = Object

crateType, locType :: Type
crateType = "crate"
locType = "location"

hAdjName, vAdjName, atGoalName :: String
hAdjName = "hAdj"
vAdjName = "vAdj"
atGoalName = "atGoal"

pSokobanAt :: Location -> GroundedPredicate
pSokobanAt loc = Predicate "sokobanAt" [loc]

pAt :: Crate -> Location -> GroundedPredicate
pAt crate loc = Predicate "at" [crate, loc]

pHAdj :: Location -> Location -> GroundedPredicate
pHAdj loc loc2 = Predicate hAdjName [loc, loc2]

pVAdj :: Location -> Location -> GroundedPredicate
pVAdj loc loc2 = Predicate vAdjName [loc, loc2]

pAtGoal :: Crate -> GroundedPredicate
pAtGoal crate = Predicate atGoalName [crate]

pClear :: Location -> GroundedPredicate
pClear loc = Predicate "clear" [loc]

pGoal :: Location -> GroundedPredicate
pGoal loc = Predicate "goal" [loc]

pNotGoal :: Location -> GroundedPredicate
pNotGoal loc = Predicate "notGoal" [loc]

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

locToCoord :: SokobanPDDL -> Location -> Coord
locToCoord pddl loc = locMap pddl ! loc

locationExists :: Location -> SokobanPDDL -> Bool
locationExists loc pddl = member loc (locMap pddl)

sokobanAt :: Location -> SokobanPDDL -> Bool
sokobanAt loc pddl = locationExists loc pddl &&
    locMap pddl ! loc == (sokoban . world $ pddl)

location :: SokobanPDDL -> Location -> Coord
location pddl loc = case Map.lookup loc (locMap pddl) of
                          Just c -> c
                          Nothing -> error $ "Tried to look up location "
                                             ++ loc ++ " which does not exist."

hAdj, vAdj :: Location -> Location -> SokobanPDDL -> Bool
hAdj a b pddl = locationExists a pddl && locationExists b pddl &&
    case location pddl a - location pddl b of
        Coord (-1, 0) -> True
        Coord (1 , 0) -> True
        _             -> False

vAdj a b pddl = locationExists a pddl && locationExists b pddl &&
    case location pddl a - location pddl b of
        Coord (0, -1) -> True
        Coord (0,  1) -> True
        _             -> False

tileClear :: Location -> Assertion
tileClear loc pddl = locationExists loc pddl &&
    case coord (world pddl) coord' of
        Clear -> not $ sokobanAt loc pddl
        Box _ -> False
    where coord' = location pddl loc

crateAt :: Crate -> Location -> Assertion
crateAt c loc pddl = locationExists loc pddl &&
    case coord (world pddl) (location pddl loc)  of -- ! (locMap pddl ! loc) of
        Box c' -> c == c'
        _      -> False

goal :: Location -> Assertion
goal loc pddl = (locMap pddl ! loc) `elem` (goals . world) pddl

notGoal :: Location -> Assertion
notGoal loc pddl = locationExists loc pddl && not (goal loc pddl)

type Assertion = SokobanPDDL -> Bool

asserts :: SokobanPDDL -> [SokobanPDDL -> Bool] -> Bool
asserts pddl = all (\f -> f pddl)

applyAction :: SokobanPDDL -> Action -> SokobanPDDL
applyAction pddlw ("move-h", [from, to])
    | asserts pddlw [sokobanAt from, tileClear to, hAdj from to] =
        applyFromLoc pddlw from to
    | otherwise = pddlw

applyAction pddlw ("move-v", [from, to])
    | asserts pddlw [sokobanAt from, tileClear to, vAdj from to] =
        applyFromLoc pddlw from to
    | otherwise = pddlw

applyAction pddlw ("push-h", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = applyFromLoc pddlw soko cLoc
    | otherwise = pddlw
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , hAdj soko cLoc
                           , hAdj cLoc toLoc
                           , tileClear toLoc
                           , notGoal toLoc
                           ]

applyAction pddlw ("push-v", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = applyFromLoc pddlw soko cLoc
    | otherwise = pddlw
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , vAdj soko cLoc
                           , vAdj cLoc toLoc
                           , tileClear toLoc
                           , notGoal toLoc
                           ]

applyAction pddlw ("push-h-goal", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = applyFromLoc pddlw soko cLoc
    | otherwise = pddlw
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , hAdj soko cLoc
                           , hAdj cLoc toLoc
                           , tileClear toLoc
                           , goal toLoc
                           ]

applyAction pddlw ("push-v-goal", [c, soko, cLoc, toLoc])
    | asserts pddlw conditions = applyFromLoc pddlw soko cLoc
    | otherwise = pddlw
        where conditions = [ sokobanAt soko
                           , crateAt c cLoc
                           , vAdj soko cLoc
                           , vAdj cLoc toLoc
                           , tileClear toLoc
                           , goal toLoc
                           ]

applyAction _ act = error ("Unknown action: " ++ show act)

isStructurePred :: GroundedPredicate -> Bool
isStructurePred (Predicate pname _)
    | pname == vAdjName = True
    | pname == hAdjName = True
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
statePred (Predicate "goal" [a])      = Just $ Goal a
statePred (Predicate "at" [c, l])     = Just $ CrateLoc c l
statePred (Predicate "sokobanAt" [l]) = Just $ SokobanLoc l
statePred _ = Nothing

data StatePred = Structure Location Location
               | CrateLoc Crate Location
               | SokobanLoc Location
               | Goal Location

fromState :: State -> SokobanPDDL
fromState state = pddl where
    (structPreds, rest) = partition isStructurePred $ Set.toList state

    goalPreds    = [ gp  | gp@(Predicate "goal" _) <- rest ]
    notGoalPreds = [ ngp | ngp@(Predicate "notGoal" _) <- rest ]

    cMap = Map.fromList
         $ Set.toList
         $ Set.map (\n -> (n, parseLocation n))
         $ Set.unions
         $ map (\(Predicate _ args) -> Set.fromList args) structPreds

    dataList = mapMaybe statePred rest
    -- If this is an empty list, we throw an error, as we wouldn't be able
    -- to go on without knowing the location of Sokoban:
    sokoLoco = cMap ! head [ l | SokobanLoc l <- dataList ]
    crates   = [ (cMap ! l, Box c) | CrateLoc c l <- dataList ]
    goals'   = [ cMap ! g | Goal g <- dataList ]

    -- Set all tiles as 'Clear'
    clearTiles = Map.fromList $ zip (Map.elems cMap) (repeat Clear)
    -- Overwrite the tiles that contain crates
    tileMap  = Map.fromList crates `Map.union` clearTiles

    w = World
        { coordMap = tileMap
        , sokoban  = sokoLoco
        , goals    = goals'
        }

    pddl = SokobanPDDL
        { world = w
        , locMap = cMap
        , persistentState = Set.fromList $ structPreds ++ goalPreds ++ notGoalPreds
        }

worldPreds :: World -> Set GroundedPredicate
worldPreds pworld = Set.insert sokLoc tilePreds where

    sokLoc = pSokobanAt $ writeLocation $ sokoban pworld
    -- The tilemap minus the tile sokoban is standing on
    tileMap' = Map.delete (sokoban pworld) (coordMap pworld)
    tilePreds = Set.fromList $ concatMap (uncurry tilePred) $ Map.toList tileMap'

    tilePred :: Coord -> Tile -> [GroundedPredicate]
    tilePred c Clear = [pClear $ writeLocation c]
    tilePred c (Box n)
        | c `elem` goals pworld = [pAtGoal n, pAt n $ writeLocation c]
        | otherwise = [pAt n $ writeLocation c]

toState :: SokobanPDDL -> State
toState pddl =
     worldPreds (world pddl) `Set.union` persistentState pddl


adjTiles:: World -> Coord -> ([(Coord,Tile)], [(Coord,Tile)])
adjTiles w c =
  let vAdjC = [Coord (0,1), Coord (0,-1)]
      hAdjC = [Coord (1,0), Coord (-1,0)]
      absHAdj = List.map (+ c) hAdjC
      absVAdj = List.map (+ c) vAdjC
      f x = (x, Map.lookup x $ coordMap w)
      tilesH = List.map f absHAdj
      tilesV = List.map f absVAdj
      tOnly tiles = [(c', t) | (c', Just t) <- tiles]
   in (tOnly tilesH, tOnly tilesV)

fromWorld :: World -> SokobanPDDL
fromWorld w =
  let cm = coordMap w
      addAdjsToSet c _ allPreds =
          let (hAdjs, vAdjs) = adjTiles w c
              cLoc = writeLocation c
              toLocs l = [writeLocation c' | (c', _) <- l]
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

toProblem :: World -> PDDLProblem
toProblem pWorld =
    let crateObjs = [ t | Box t <- Map.elems (coordMap pWorld) ]
        structObjs = map writeLocation $ Map.keys (coordMap pWorld)
        goalPred c = GLit . Pos $ Predicate atGoalName [TName c]
        goalsF = GAnd $ map goalPred crateObjs
        boxMap = zip crateObjs (repeat crateType)
        structMap = zip structObjs (repeat locType)
    in PDDLProblem
        { probName   = "sokobanProb"
        , probObjs   = crateObjs ++ structObjs
        , probDomain = "sokobanDom"
        , probState  = (toState . fromWorld) pWorld
        , probGoal   = goalsF
        , probTypes  = Map.fromList $ boxMap ++ structMap
        }
