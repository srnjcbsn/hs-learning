module Environments.Sokoban.Runner where

import System.Console.ANSI
import Control.Monad (forM_)
import Data.Map (assocs, keys)
import Text.Show.Pretty (ppShow)


import Planning.FastDownward
import PDDL.Type
import Learning.OptPrecondLearn
import Environments.Sokoban.Sokoban
import Environments.Sokoban.PDDL

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

runSokoban :: Domain -> Problem -> IO ()
runSokoban = undefined
