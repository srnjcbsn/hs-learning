module Environment.Sokoban.SokobanView where

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

goalSymbol, sokobanSymbol :: Char
goalSymbol = 'X'
sokobanSymbol = 'S'

tileSymbol :: Tile -> Char
tileSymbol Clear   = '+'
tileSymbol (Box _) = '#'

vis :: Int -> String -> String
vis width str = take width str ++ "\n" ++ vis width (drop width str)

visTile :: Map Coord Char -> Coord -> String
visTile m c | xCoord c == 0 = ['\n', t]
            | otherwise = [t]
                where t = fromMaybe ' ' $ Map.lookup c m

visualize :: SokobanPDDL -> IO ()
visualize pddl =
    do clearScreen
       putStrLn worldStr
       hFlush stdout
    where
    w = world pddl
    tileMap = Map.map tileSymbol (coordMap w)
    upd symb m k = Map.adjust symb k m
    tileMap' = foldl (upd (const goalSymbol)) tileMap (goals w)
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
    --putStrLn ("Action " ++ ppShow action ++ " failed.\n") >> hFlush stdout
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
