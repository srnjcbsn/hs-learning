module Planning.Viewing where

import           Planning
import           Planning.PDDL

import Text.Show.Pretty
import System.IO

data View = View { actionPerformed :: Action -> Bool -> IO ()
                 , planMade :: Maybe Plan -> IO ()
                 }

onActionPerformed :: FilePath -> Action -> Bool -> IO ()
onActionPerformed file action True =
    appendFile file $ "Action " ++ ppShow action ++ " succeeded.\n"
onActionPerformed file action False =
    appendFile file $ "Action " ++ ppShow action ++ " failed.\n"

onPlanMade :: FilePath -> Maybe Plan -> IO ()
onPlanMade file (Just p) =
    appendFile file $ "Plan found: " ++ ppShow p ++ "\n"
onPlanMade file Nothing =
    appendFile file "Failed to find plan.\n"

defaultView :: FilePath -> View
defaultView file = View { actionPerformed = onActionPerformed file
                        , planMade = onPlanMade file
                        }
