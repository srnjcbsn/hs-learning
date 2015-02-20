module Environment.Sokoban.ConsoleView where

import           Control.Monad                (forM_)
import           Data.Map                     (assocs, keys)
import           System.Console.ANSI
import           Text.Show.Pretty             (ppShow)


import           Environment.Sokoban.PDDL
import           Environment.Sokoban

setCursorPosition' :: Integer -> Integer -> IO ()
setCursorPosition' x y =
    setCursorPosition (fromIntegral x) (fromIntegral y)

goalSymbol, sokobanSymbol :: Char
goalSymbol = 'X'
sokobanSymbol = 'S'

tileSymbol :: Tile -> Char
tileSymbol Clear   = '+'
tileSymbol (Box _) = '#'

showAt :: Coord -> Char -> IO ()
showAt (Coord (x, y)) s =
    setCursorPosition' x y >> putChar s

visTile :: Coord -> Tile -> IO ()
visTile c t = showAt c $ tileSymbol t

visGoal :: Coord -> IO ()
visGoal c = showAt c goalSymbol

visSokoban :: Coord -> IO ()
visSokoban c = showAt c sokobanSymbol

-- | Print a seperator beneath the map.
visSeparator :: Int -> Int -> IO ()
visSeparator width height = do
    setCursorPosition 0 (height + 1)
    putStrLn $ replicate width '='

-- | Prints a textual listing of the state below the map
visState :: World -> IO ()
visState world' = do
    visSeparator width height
    setCursorPosition 0 (height + 3)
    putStrLn (ppShow world')
    where width  = maximum $ map xCoord coords
          height = maximum $ map yCoord coords
          coords = keys $ coordMap world'

-- | Displays the state of the sokkoban environment in the console.
--   Draws a map of the sokoban world, and pretty prints the state
--   below it.
visualize :: SokobanPDDL -> IO ()
visualize pddl = do
    clearScreen
    forM_ tileCoords (uncurry visTile)
    forM_ goalCoords visGoal
    visSokoban sokoCoord
    visState (world pddl)
    where tileCoords = assocs $ (coordMap . world) pddl
          goalCoords = (goals . world) pddl
          sokoCoord  = (sokoban . world) pddl
