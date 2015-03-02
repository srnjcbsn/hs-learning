module Planning.Viewing where

import           Planning

data View e = View { actionPerformed :: Action -> Bool -> IO ()
                   , planMade        :: Maybe Plan -> IO ()
                   , envChanged      :: e -> IO ()
                   }
