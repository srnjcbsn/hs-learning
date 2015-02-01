module FastDownward where

import PDDL.Type
import PDDL.Parser

domainToFile :: Domain -> String -> IO ()
domainToFile domain path = do
    writeFile path (writeDomain domain)

domainFromFile :: FilePath -> IO Domain
domainFromFile path = do
    cont   <- readFile path
    domain <- case doParse parseDomain cont of
                Left err  -> error $ show err
                Right val -> return val

    return domain
