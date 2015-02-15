module FastDownward where

import System.Process
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Directory
import System.FilePath.Posix

import PDDL.Type
import PDDL.Parser

-- | A record describing how the fast-downard program should be called.
data FastDownward = FastDownWard
    { fdPath     :: FilePath -- ^ The path to the fast-downward program
    , domFile    :: FilePath -- ^ Path to a .pddl file desribing a domain
    , probFile   :: FilePath -- ^ Path to a .pddl file describing a problem
    , searchAlgo :: String   -- ^ The search algorithm to be used by the fast-
                             --   downward program. See the fast-downward
                             --   documentation for valid values.
    , planName   :: String   -- ^ The name of the plan file to be produced
                             --   by fast-downward
    }

-- | Constructs a list of cammand line arguments to invoke fast-downward with.
fdArgs :: FastDownward
       -> FilePath -- ^ The directory in which the temporary files (and the
                   --   plan file) should be kept.
       -> [String]
fdArgs fd tmp =
    [ "--run-all"
    , "--plan-file", tmp </> planName fd
    , domFile fd, probFile fd
    , "--search", searchAlgo fd
    ]

-- makePlan :: Domain -> Problem -> IO (Maybe Plan)
-- makePlan =

domainToFile :: Domain -> String -> IO ()
domainToFile domain path =
    writeFile path (writeDomain domain)

domainFromFile :: FilePath -> IO Domain
domainFromFile path = do
    cont   <- readFile path
    case doParse parseDomain cont of
         Left err  -> error $ show err
         Right val -> return val

-- | Given a description of the fast downward invocation, creates a temporary
--   directory (in the current directory) in which the output from fast-downard
--   is stored.
--   After the plan output by fd has been parsed, the temporary directory is
--   removed (including the plan file).
fastDownward :: FastDownward
             -> String          -- ^ a template for the name of the temp dir
             -> IO (Maybe Plan) -- ^ The plan constructed by fd,
                                -- or 'Nothing' if no plan could be found
fastDownward fd temp =
    withTempDirectory "." temp mkPlan
    where mkPlan tmp = do
            _ <- callProcess (fdPath fd) $ fdArgs fd tmp
            parsePlan $ tmp </> planName fd

-- TODO: If the file can be read, but not parsed, print the contents
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
