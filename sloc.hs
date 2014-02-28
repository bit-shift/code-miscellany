import Control.Monad (when)
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)

data Flags = ShowHelp | KSloc | Quiet
    deriving (Eq, Ord, Show)
data SourceType = PythonSource | HaskellSource | ShellSource | Plaintext | GuessType
    deriving (Eq, Show)
data FileWithType = TypedFile FilePath SourceType (Maybe String)
    deriving (Eq, Show)
data FileOrStdin = File FilePath | Stdin
-- types for caching file contents
type FileCache = Map FilePath String
type CachingIO = StateT FileCache IO

--
-- GENERAL FUNCTIONS USED IN MULTIPLE SECTIONS

startsWith prefix@(p:ps) s@(c:cs) = if c == p
                                    then startsWith cs ps
                                    else False
startsWith ""            _        = True
startsWith _             _        = False

--
-- COMMAND-LINE HANDLING

-- XXX: write usage for *this* program!
putUsage progname = do
    putStrLn ("Usage: " ++ progname ++ " [OPTION] [FILE...]")
    putStrLn "Prints line, word, or char (with or without newlines) counts"
    putStrLn "for each FILE. With no FILE, or with \"-\", standard input is"
    putStrLn "counted instead. If no count type option is given, defaults to"
    putStrLn "word counts."
    putStrLn "The options below may be used:"
    putStrLn "  -c            print character counts (excluding newlines)"
    putStrLn "  -C            print character counts (including newlines)"
    putStrLn "  -l            print line counts"
    putStrLn "  -w            print word counts"
    putStrLn "  -h, --help    print this help message, and exit"

parseArgs (x:xs) curType curDefaultType files flags
    | x == "--python"   = parseArgs xs PythonSource PythonSource files flags
    | x == "--python1"  = parseArgs xs PythonSource curDefaultType files flags
    | x == "--haskell"  = parseArgs xs HaskellSource HaskellSource files flags
    | x == "--haskell1" = parseArgs xs HaskellSource curDefaultType files flags
    | x == "--shell"    = parseArgs xs ShellSource ShellSource files flags
    | x == "--shell1"   = parseArgs xs ShellSource curDefaultType files flags
    | x == "--text"     = parseArgs xs Plaintext Plaintext files flags
    | x == "--text1"    = parseArgs xs Plaintext curDefaultType files flags
    | x == "--auto"     = parseArgs xs GuessType GuessType files flags
    | x == "--auto1"    = parseArgs xs GuessType curDefaultType files flags
    | x == "-q"         = parseArgs xs curType curDefaultType files (Set.insert Quiet flags)
    | x == "-v"         = parseArgs xs curType curDefaultType files (Set.delete Quiet flags)
    | x == "-k"         = parseArgs xs curType curDefaultType files (Set.insert KSloc flags)
    | x == "-l"         = parseArgs xs curType curDefaultType files (Set.delete KSloc flags)
    | x == "-h"         = ([], curType, Set.singleton ShowHelp)
    | x == "--help"     = ([], curType, Set.singleton ShowHelp)
    | otherwise         = parseArgs xs curDefaultType curDefaultType ((TypedFile x curType Nothing):files) flags
parseArgs [] curType _ files flags = (reverse files, curType, flags)

parseArgsWithDefaults args = parseArgs args GuessType GuessType [] Set.empty

--
-- FILETYPE GUESSING

--- INDIVIDUAL GUESSERS
guessByExtension (TypedFile path _ _) =
    let splitName = splitOn "." path
    in if length splitName == 1
       then GuessType
       else case last splitName of
        "py"       -> PythonSource
        "hs"       -> HaskellSource
        "sh"       -> ShellSource
        "bash"     -> ShellSource
        "zsh"      -> ShellSource
        "txt"      -> Plaintext
        "rst"      -> Plaintext
        "md"       -> Plaintext
        "mkd"      -> Plaintext
        "markdown" -> Plaintext
        "textile"  -> Plaintext
        _          -> GuessType

guessByShebang (TypedFile _ _ Nothing)         = GuessType
guessByShebang (TypedFile _ _ (Just contents)) =
    case lines contents of
        []  -> GuessType
        l:_ -> if not (startsWith "#!" l)
               then GuessType
               else let splitShebang = splitOn "/" l
                    in if length splitShebang == 1
                       then GuessType
                       else let finalPart = last splitShebang
                                progname = if startsWith "env " finalPart
                                           then drop 4 finalPart
                                           else finalPart
                            in case progname of
                                "sh"         -> ShellSource
                                "bash"       -> ShellSource
                                "zsh"        -> ShellSource
                                "python"     -> PythonSource
                                "python2"    -> PythonSource
                                "python3"    -> PythonSource  -- never seen this hardcoded, but I guess it's probably been done
                                "runghc"     -> HaskellSource
                                "runhaskell" -> HaskellSource
                                _            -> GuessType

--- PUTTING THE GUESSERS TOGETHER
applyGuessers (g:gs) f defaultType = case g f of
                                        GuessType -> applyGuessers gs f defaultType
                                        filetype  -> filetype
applyGuessers []     _ defaultType = defaultType

guessType f@(TypedFile path _ (Just contents)) = applyGuessers [guessByShebang, guessByExtension] f Plaintext
guessType (TypedFile path _ Nothing) = error "guessType should never be called on an unread file."

finalizeType f@(TypedFile path GuessType contents) = do
    let guessedType = guessType f
    putStrLn ("NOTE: filetype of '" ++ path ++ "' not provided, guessed " ++ (languageName guessedType))
    return (TypedFile path guessedType contents)
    where languageName PythonSource = "Python"
          languageName HaskellSource = "Haskell"
          languageName ShellSource = "shell (bash/etc.)"
          languageName Plaintext = "plain text"
finalizeType f@(TypedFile _ _ _) = return f

--
-- SOURCE-LINE FILTERING

--- HELPERS
applyAll fs x = [f x | f <- fs]

filterNone ps (x:xs) = if or (applyAll ps x)
                       then filterNone ps xs
                       else x:(filterNone ps xs)
filterNone ps []     = []

stripLeadingWhitespace s@(c:cs) = if c == ' ' || c == '\t'
                                  then stripLeadingWhitespace cs
                                  else s
stripLeadingWhitespace ""       = ""

--- LINE TESTS
isAllWhitespace = null . stripLeadingWhitespace

isHashComment = (startsWith "#") . stripLeadingWhitespace

isHaskellComment = (startsWith "--") . stripLeadingWhitespace

--- SOURCE-LINE FILTERS
sourceLines PythonSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines HaskellSource = filterNone [null, isAllWhitespace, isHaskellComment]

sourceLines ShellSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines Plaintext = filterNone [null, isAllWhitespace]

sourceLines _ = id

--
-- LINE COUNTING

countSloc (TypedFile path filetype Nothing) = (path, 0)
countSloc (TypedFile path filetype (Just contents)) = (path, length ls)
    where ls = sourceLines filetype (lines contents)

prettyCounts counts = let biggestN (n:ns) acc = if n > acc
                                                then biggestN ns n
                                                else biggestN ns acc
                          biggestN []     acc = acc
                          numWidth = length (show (biggestN (map snd counts) 0))
                          padNum n w = (take (w - (length (show n))) (repeat ' ')) ++ (show n)
                          formatLine w (path, n) = (padNum n w) ++ " " ++ path
                      in map (formatLine numWidth) counts

--
-- READING FILES

fileOrStdinFromPath path | path == "-" = Stdin
                         | otherwise   = File path

readFileOrStdin Stdin           = getContents
readFileOrStdin (File filename) = readFile filename

readTypedFile (TypedFile path filetype _) = do
    let input = fileOrStdinFromPath path
    contents <- readFileOrStdin input
    length contents `seq` (return (TypedFile path filetype (Just contents)))

cachedReadTypedFile :: FileWithType -> CachingIO FileWithType
cachedReadTypedFile f@(TypedFile path filetype _) = do 
    cachedContents <- State.gets (Map.lookup path)
    case cachedContents of
        (Just contents) -> return (TypedFile path filetype (Just contents))
        Nothing         -> do f@(TypedFile path _ contents) <- State.lift $ readTypedFile f
                              case contents of
                                (Just contents) -> State.modify (Map.insert path contents)
                                Nothing         -> error ("Failed to read '" ++ path ++ "'")
                              return f

readTypedFiles :: [FileWithType] -> CachingIO [FileWithType]
readTypedFiles fs = do
    mapM cachedReadTypedFile fs

--
-- PULL IT ALL TOGETHER

main = do
    args <- getArgs
    let (rawFiles, lastType, flags) = parseArgsWithDefaults args
    let files = if (rawFiles == []) && (not (Set.member ShowHelp flags))
                then [TypedFile "-" lastType Nothing]
                else rawFiles
    when (Set.member ShowHelp flags) $ getProgName >>= putUsage
    (filesRead, _) <- runStateT (readTypedFiles files) Map.empty
    finalFiles <- mapM finalizeType filesRead
    mapM putStrLn (prettyCounts (map countSloc finalFiles))
