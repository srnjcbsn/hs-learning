module Planning.Viewing where

import           qualified Planning as P

data View e = View { actionPerformed :: P.Action -> Bool -> IO ()
                   , planMade        :: Maybe P.Plan -> IO ()
                   , envChanged      :: e -> IO ()
                   }
