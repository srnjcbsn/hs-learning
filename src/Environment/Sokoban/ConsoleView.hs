module Environment.Sokoban.ConsoleView where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           System.Console.ANSI

import           Environment.Sokoban
import           Environment.Sokoban.PDDL

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
visualize pddl = clearScreen >> putStrLn worldStr where
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
