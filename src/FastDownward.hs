module FastDownward where

import System.Process
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Directory
import System.FilePath.Posix

import PDDL.Type
import PDDL.Parser

sasFilePath = "output.sas"
outputFilePath = "output"

searchArgs searchAlgo planFile =
    [ "--internal-plan-file", planFile
    , "--search", searchAlgo
    ]

domainToFile :: Domain -> String -> IO ()
domainToFile domain path =
    writeFile path (writeDomain domain)

domainFromFile :: FilePath -> IO Domain
domainFromFile path = do
    cont   <- readFile path
    case doParse parseDomain cont of
         Left err  -> error $ show err
         Right val -> return val

fastDownWard :: FilePath        -- ^ The path to the fast-downward program
             -> FilePath        -- ^ Path to a .pddl file desribing a domain
             -> FilePath        -- ^ Path to a .pddl file describing a problem
             -> String          -- ^ a template for the name of the temp dir
             -> IO (Maybe Plan) -- ^ The plan constructed by fd,
                                -- or 'Nothing' if no plan could be found
fastDownWard fd dom prob temp =
    withTempDirectory "." temp mkPlan
    where args tmp = [ "--run-all"
                   , "--plan-file", tmp </> "plan"
                   , dom, prob
                   , "--search", "astar(blind())"
                   ]
          mkPlan tmp = do
            _ <- callProcess fd $ args tmp
            parsePlan $ tmp </> "plan"


-- | Invokes the fast-downward translator with the file paths given as arguments.
--   The first file path should point to a '.pddl' file containing a domain
--   description, and the second to a '.pddl' file describing a corresponding
--   problem. This will place a file called "output.sas" in the current
--   directory.
translate :: FilePath -> FilePath -> IO ()
translate domainFile problemFile =
    callProcess "fd/translate" [domainFile, problemFile]

-- | Invoke the fast-downward preprocessor. This will look for the file
--   "output.sas" in the current directory (as produced by the fast-downward)
--   translation step). If this file cannot be found, an IOError is thrown.
--   If successfull, a file named "output" is placed in the current directory,
--   which can be fed to the fast-downward search program.
preprocess :: IO ()
preprocess = do
    sas <- openFile sasFilePath ReadMode
    _ <- createProcess (proc "fd/preprocess" []) {std_in = UseHandle sas}
    return ()

-- | Invoke the fast-downward planner with the given search algorithm.
--
-- > search searchAlgo planFile
--
-- will invoke the planner with 'searchAlgo' and write the produced
-- plan (if any) to 'planFile'. This function will look for a file named
-- "output" in the current directory (as produced in the fast-downward
-- preprocessing step). If this file cannot be found, an 'IOError' is thrown.
search :: String -> FilePath -> IO ()
search searchAlgo planFile = do
    outp <- openFile outputFilePath ReadMode
    _ <- createProcess (proc "fd/search"  $ searchArgs searchAlgo planFile)
         {std_in = UseHandle outp}
    return ()

-- | Parses a plan in a given file. Returns 'Nothing' if the file does not exists,
--   and raises an 'IOError' if the file could not be parsed, propagating the
--   parser error.
parsePlan :: FilePath -> IO (Maybe Plan)
parsePlan planFile =
    (readFile planFile >>= parsePlan') `catchIOError` errHandler
      where parsePlan' str =
              case doParse plan str of
              Left perr   -> ioError $ parseErr perr
              Right plan' -> return $ Just plan'

            parseErr :: ParseError -> IOError
            parseErr err = userError $ "Failed to parse plan: \n" ++ show err

            errHandler :: IOError -> IO (Maybe Plan)
            errHandler e
                | isDoesNotExistError e = return Nothing
                | otherwise = ioError e
