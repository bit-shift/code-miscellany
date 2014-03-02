import Control.Monad (when)
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)

data Flags = ShowHelp | HumanSizes | Quiet
    deriving (Eq, Ord, Show)
data SourceType = PythonSource | PerlSource | HaskellSource | ShellSource
    | Plaintext | GuessType deriving (Eq, Show)
data PathWithType = TypedPath FilePath SourceType
    deriving (Eq, Show)
data FileWithType = TypedFile PathWithType String
    deriving (Eq, Show)
data FileOrStdin = File FilePath | Stdin
data Args = Args [PathWithType] SourceType (Set Flags)
data CountedFile = CountedFile FilePath Int
data ShowCountedFile = ShowCountedFile FilePath String
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

putUsage progname = do
    putStrLn ("Usage: " ++ progname ++ " [OPTION...] [[TYPE] FILE...]")
    putStrLn "Prints source line of code (SLOC) counts for each FILE, with"
    putStrLn "blank lines and comments ignored based on the provided TYPE"
    putStrLn "of a file, or its guessed type if --auto[1] is used, or no"
    putStrLn "type is provided."
    putStrLn "All language options may be used with or without a 1 at the"
    putStrLn "end, i.e. --python or --python1, with the first form marking"
    putStrLn "all following files as that type (until another type is"
    putStrLn "given), and the second marking *only* the file immediately"
    putStrLn "following it."
    putStrLn "The following type options (the single-file variants are"
    putStrLn "omitted for brevity) are available:"
    putStrLn "  --python      Python source"
    putStrLn "  --perl        Perl source"
    putStrLn "  --haskell     Haskell source"
    putStrLn "  --shell       shell (bash, zsh, etc.) script"
    putStrLn "  --text        plain text (count all non-blank lines)"
    putStrLn "  --auto        attempt to guess type"
    putStrLn "Additionally, the following general options may be used:"
    putStrLn "  -q            don't print filetype guesses"
    putStrLn "  -v            print filetype guesses"
    putStrLn "  -H            print large line counts with human-friendly"
    putStrLn "                  suffixes (k, M, G, etc.)"
    putStrLn "  -l            print exact line counts"
    putStrLn "  -h, --help    print this help message, and exit"

parseArgs (x:xs) curType curDefaultType files flags
    | x == "--python"   = parseArgs xs PythonSource PythonSource files flags
    | x == "--python1"  = parseArgs xs PythonSource curDefaultType files flags
    | x == "--perl"     = parseArgs xs PerlSource PerlSource files flags
    | x == "--perl1"    = parseArgs xs PerlSource curDefaultType files flags
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
    | x == "-H"         = parseArgs xs curType curDefaultType files (Set.insert HumanSizes flags)
    | x == "-l"         = parseArgs xs curType curDefaultType files (Set.delete HumanSizes flags)
    | x == "-h"         = Args [] curType (Set.singleton ShowHelp)
    | x == "--help"     = Args [] curType (Set.singleton ShowHelp)
    | otherwise         = parseArgs xs curDefaultType curDefaultType ((TypedPath x curType):files) flags
parseArgs [] curType _ files flags = Args (reverse files) curType flags

parseArgsWithDefaults args = parseArgs args GuessType GuessType [] Set.empty

--
-- FILETYPE GUESSING

--- INDIVIDUAL GUESSERS
guessByExtension (TypedFile (TypedPath path _) _) =
    let splitName = splitOn "." path
    in if length splitName == 1
       then GuessType
       else case last splitName of
        "py"       -> PythonSource
        "pl"       -> PerlSource
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

guessByShebang (TypedFile _ contents) =
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
                                "perl"       -> PerlSource
                                "runghc"     -> HaskellSource
                                "runhaskell" -> HaskellSource
                                _            -> GuessType

--- PUTTING THE GUESSERS TOGETHER
applyGuessers (g:gs) defaultType f = case g f of
                                        GuessType -> applyGuessers gs defaultType f
                                        filetype  -> filetype
applyGuessers []     defaultType _ = defaultType

guessType = applyGuessers [guessByShebang, guessByExtension] Plaintext

finalizeType quiet f@(TypedFile (TypedPath path GuessType) contents) = do
    let guessedType = guessType f
    when (not quiet) $ putStrLn ("NOTE: filetype of '" ++ path ++ "' not provided, guessed " ++ (languageName guessedType))
    return (TypedFile (TypedPath path guessedType) contents)
    where languageName PythonSource = "Python"
          languageName PerlSource = "Perl"
          languageName HaskellSource = "Haskell"
          languageName ShellSource = "shell (bash/etc.)"
          languageName Plaintext = "plain text"
          languageName GuessType = error "type shouldn't be unguessed after being guessed!"
finalizeType _ f@(TypedFile _ _) = return f

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

sourceLines PerlSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines HaskellSource = filterNone [null, isAllWhitespace, isHaskellComment]

sourceLines ShellSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines Plaintext = filterNone [null, isAllWhitespace]

sourceLines _ = id

--
-- LINE COUNTING

countSloc (TypedFile (TypedPath path filetype) contents) = CountedFile path (length ls)
    where ls = sourceLines filetype (lines contents)

scaleDown n =
    let scaleDownInner n steps = if n >= 1000.0 && steps < 8
                                 then scaleDownInner (n / 1000.0) (steps + 1)
                                 else (n, steps)
    in scaleDownInner n 0

showCount humanSizes (CountedFile path n) =
    let sizeStr = if not humanSizes
                  then show n
                  else let (scaledN, scaleSteps) = scaleDown (fromIntegral n)
                           scaleSuffix = case scaleSteps of
                                            0 -> ""
                                            1 -> "k"
                                            2 -> "M"
                                            3 -> "G"
                                            4 -> "T"
                                            5 -> "P"
                                            6 -> "E"
                                            7 -> "Z"
                                            8 -> "Y"
                                            _ -> error "Shouldn't happen."
                           roundedN = (fromInteger (round (scaledN * 10)) / 10.0)
                           fullyRoundN = round roundedN
                       in if roundedN == (fromInteger fullyRoundN)  -- xyz.0
                          then (show fullyRoundN) ++ scaleSuffix
                          else (show roundedN) ++ scaleSuffix
    in ShowCountedFile path sizeStr

prettyCounts humanSizes counts =
    let shownCounts = map (showCount humanSizes) counts
        longestN (n:ns) acc = let len = length n
                              in if len > acc
                                 then longestN ns len
                                 else longestN ns acc
        longestN []     acc = acc
        getNumLines (ShowCountedFile _ n) = n
        numWidth = longestN (map getNumLines shownCounts) 0
        padNum n w = (take (w - (length n)) (repeat ' ')) ++ n
        formatLine w (ShowCountedFile path n) = (padNum n w) ++ " " ++ path
    in map (formatLine numWidth) shownCounts

--
-- READING FILES

fileOrStdinFromPath path | path == "-" = Stdin
                         | otherwise   = File path

readFileOrStdin Stdin           = getContents
readFileOrStdin (File filename) = readFile filename

readTypedFile p@(TypedPath path filetype) = do
    let input = fileOrStdinFromPath path
    contents <- readFileOrStdin input
    return (TypedFile p contents)

cachedReadTypedFile :: PathWithType -> CachingIO FileWithType
cachedReadTypedFile p@(TypedPath path filetype) = do
    cachedContents <- State.gets (Map.lookup path)
    case cachedContents of
        (Just contents) -> return (TypedFile p contents)
        Nothing         -> do f@(TypedFile _ contents) <- State.lift $ readTypedFile p
                              State.modify (Map.insert path contents)
                              return f

readTypedFiles :: [PathWithType] -> CachingIO [FileWithType]
readTypedFiles fs = do
    mapM cachedReadTypedFile fs

--
-- PULL IT ALL TOGETHER

main = do
    args <- getArgs
    let (Args rawFiles lastType flags) = parseArgsWithDefaults args
    let files = if (rawFiles == []) && (not (Set.member ShowHelp flags))
                then [TypedPath "-" lastType]
                else rawFiles
    if Set.member ShowHelp flags
    then getProgName >>= putUsage
    else do
        (filesRead, _) <- runStateT (readTypedFiles files) Map.empty
        finalFiles <- mapM (finalizeType (Set.member Quiet flags)) filesRead
        mapM_ putStrLn (prettyCounts (Set.member HumanSizes flags) (map countSloc finalFiles))
