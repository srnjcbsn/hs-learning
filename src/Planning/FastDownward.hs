module Planning.FastDownward where

import           System.FilePath.Posix
import           System.IO.Error
import           System.IO.Temp
import           System.Process

import           PDDL.Parser
import           PDDL
import           Planning

-- | A record describing how the fast-downard program should be called.
data FastDownward = FastDownward
    { fdPath     :: FilePath -- ^ The path to the fast-downward program
    , workDir    :: FilePath -- ^ The directory in which fast-downward should
                             --   be executed.
    , domFile    :: FilePath -- ^ Path to a .pddl file desribing a domain
    , probFile   :: FilePath -- ^ Path to a .pddl file describing a problem
    , searchAlgo :: String   -- ^ The search algorithm to be used by the fast-
                             --   downward program. See the fast-downward
                             --   documentation for valid values.
    , planName   :: String   -- ^ The name of the plan file to be produced
                             --   by fast-downward
    }

instance ExternalPlanner FastDownward where
    makePlan = makePlan'

-- | construct a default 'FastDownward' record, where
--
-- * the path to the fast-downward program is @fd/fast-downward.py@
--
-- * the working directory is the current directory,
--
-- * the name of the domain and problem files are generated from their
--   respective names
--
-- * the search algorithm is @astar(blind())@
--
-- * the name of the generated plan is "plan"
mkFastDownard :: Domain -> Problem -> FastDownward
mkFastDownard dom prob =
    FastDownward { fdPath = "fd/fast-downward.py"
                 , workDir = "."
                 , domFile = dmName dom ++ ".pddl"
                 , probFile = probName prob ++ ".pddl"
                 , searchAlgo = "astar(blind())"
                 , planName = "plan"
                 }

-- | Constructs a list of cammand line arguments to invoke fast-downward with.
--   The argugments are of the form
--
-- > --run-all --plan-file <PLANFILE> <DOMAINFILE> <PROBLEMFILE> --search <SEARCHALGO>
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

-- | Write the given domain and problem specifications to the files specified in
--   in the given 'FastDownward' record, and call 'fastDownward'.
makePlan' :: FastDownward -> Domain -> Problem -> IO (Maybe Plan)
makePlan' fd dom prob = do
    writeFile (domFile fd) (writeDomain dom)
    writeFile (probFile fd) (writeProblem prob)
    fastDownward fd "plan"
    -- let domFile = dmName dom
    --     probFile = probName prob
    --     fd = FastDownward fdPath domFile probFile searchAlgo "plan"
    -- in do
    --     writeFile (dmName dom) (writeDomain dom)
    --     writeFile (probName prob) (writeProblem prob)
    --     fastDownward fd temp


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
             -> String          -- ^ A template for the name of the temp dir
             -> IO (Maybe Plan) -- ^ The plan constructed by fd,
                                -- or 'Nothing' if no plan could be found
fastDownward fd temp =
    withTempDirectory (workDir fd) temp mkPlan
    where mkPlan tmp = do
            _ <- createProcess (proc (fdPath fd) $ fdArgs fd tmp)
                {cwd = Just (workDir fd)} -- set the working directory
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
