module ActionViewer where

import PDDL
import Text.Show.Pretty
import System.Directory

logAction :: FilePath -> Maybe Plan -> IO ()
logAction fp plan = do --appendFile fp (ppShow act)
    cwd' <- getCurrentDirectory
    putStrLn $ "logger: " ++ cwd'
    putStrLn $ "logger: " ++ ppShow plan
    case plan of
        Just [] -> appendFile fp "Goal reached"
        Just (act : _) -> appendFile fp (ppShow act ++ "\n")
        Nothing -> appendFile fp "Nothing\n"
