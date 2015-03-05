module Environment.Sokoban.SokobanView where

import           Control.Concurrent
import           Data.Char
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Environment.Sokoban
import           Environment.Sokoban.PDDL
import           Planning
import           Planning.Viewing
import           System.Console.ANSI
import           System.IO                (hFlush, stdout)
import           Text.Show.Pretty

goalSymbol, sokobanSymbol, boxSymbol, clearSymbol, boxWithGoalSymbol, emptySymbol :: Char
goalSymbol = 'O'
sokobanSymbol = 'S' -- chr 128515
boxSymbol = 'B' -- chr 9744
clearSymbol = '·'
boxWithGoalSymbol = 'X'
emptySymbol = '+' -- chr 9608

tileSymbol :: Tile -> Char
tileSymbol Clear   = clearSymbol
tileSymbol (Box _) = boxSymbol

vis :: Int -> String -> String
vis width str = take width str ++ "\n" ++ vis width (drop width str)

visTile :: Map Coord Char -> Coord -> String
visTile m c | xCoord c == 0 = ['\n', t]
            | otherwise = [t]
                where t = fromMaybe emptySymbol $ Map.lookup c m

visualize :: SokobanPDDL -> IO ()
visualize pddl =
    do clearScreen
       putStrLn worldStr
       hFlush stdout
       threadDelay 2000
    where
    goalFunc s | s == boxSymbol = boxWithGoalSymbol
               | otherwise      = goalSymbol
    w = world pddl
    tileMap = Map.map tileSymbol (coordMap w)
    upd symb m k = Map.adjust symb k m
    tileMap' = foldl (upd goalFunc) tileMap (goals w)
    tileMap'' = upd (const sokobanSymbol) tileMap' (sokoban w)
    coords = Map.keys $ coordMap w
    width = fromIntegral $ maximum $ map xCoord coords
    height = fromIntegral $ maximum $ map yCoord coords
    coords' = [Coord (x, y) | y <- [0 .. height], x <- [0 .. width]]
    worldStr = concatMap (visTile tileMap'') coords'

onActionPerformed :: FilePath -> Action -> Bool -> IO ()
onActionPerformed file action True =
    appendFile file $ "Action " ++ ppShow action ++ " succeeded.\n"
    -- putStrLn ("Action " ++ ppShow action ++ " succeeded.\n") >> hFlush stdout
onActionPerformed file action False =
    appendFile file $ "Action " ++ ppShow action ++ " failed.\n"
    -- putStrLn ("Action " ++ ppShow action ++ " failed.\n") >> hFlush stdout
onPlanMade :: FilePath -> Maybe Plan -> IO ()
onPlanMade file (Just p) =
    appendFile file $ "Plan found: " ++ ppShow p ++ "\n"
onPlanMade file Nothing =
    appendFile file "Failed to find plan.\n"

sokobanView :: FilePath -> View SokobanPDDL
sokobanView file = View { actionPerformed = onActionPerformed file
                        , planMade = onPlanMade file
                        , envChanged = -- \_ -> return ()
                                       visualize
                        }
