module ActionViewer where

import PDDL
import Text.Show.Pretty

logAction :: FilePath -> Maybe Plan -> IO ()
logAction fp plan = --appendFile fp (ppShow act)
    case plan of
        Just (act : _) -> appendFile fp (ppShow act ++ "\n")
        Nothing -> appendFile fp "Nothing\n"
