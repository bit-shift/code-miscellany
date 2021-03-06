module Main (main)
where

import Control.Applicative ((<*>))
import Control.Monad (when)
import Control.Monad.State (StateT, runStateT, unless)
import qualified Control.Monad.State as State
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)

data Flags = ShowHelp | HumanSizes | Quiet | ShowTotal | HideIndividual
    deriving (Eq, Ord, Show)
data SourceType = PythonSource | PerlSource | HaskellSource | ShellSource
    | Plaintext | GuessType deriving (Eq, Show)
data PathWithType = TypedPath FilePath SourceType
    deriving (Eq, Show)
data FileWithType = TypedFile PathWithType String
    deriving (Eq, Show)
data FileOrStdin = File FilePath | Stdin
data Args = Args [PathWithType] SourceType (Set Flags)
data CountedFile = CountedFile { cfPath :: FilePath, cfCount :: Int }
data ShowCountedFile = ShowCountedFile FilePath String
-- types for caching file contents
type FileCache = Map FilePath String
type CachingIO = StateT FileCache IO

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
    putStrLn "  -v            print filetype guesses (DEFAULT)"
    putStrLn "  -t            print summary (total line count)"
    putStrLn "  -T            don't print summary (DEFAULT)"
    putStrLn "  -s            print summary only (implies -t)"
    putStrLn "  -a            print line counts for all files (DEFAULT)"
    putStrLn "  -H            print large line counts with human-friendly"
    putStrLn "                  suffixes (k, M, G, etc.)"
    putStrLn "  -l            print exact line counts (DEFAULT)"
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
    | x == "-t"         = parseArgs xs curType curDefaultType files (Set.insert ShowTotal flags)
    | x == "-T"         = parseArgs xs curType curDefaultType files (Set.delete ShowTotal flags)
    | x == "-s"         = parseArgs xs curType curDefaultType files (Set.insert ShowTotal
                                                                   . Set.insert HideIndividual
                                                                   $ flags)
    | x == "-a"         = parseArgs xs curType curDefaultType files (Set.delete HideIndividual flags)
    | x == "-H"         = parseArgs xs curType curDefaultType files (Set.insert HumanSizes flags)
    | x == "-l"         = parseArgs xs curType curDefaultType files (Set.delete HumanSizes flags)
    | x == "-h"         = Args [] curType (Set.singleton ShowHelp)
    | x == "--help"     = Args [] curType (Set.singleton ShowHelp)
    | otherwise         = parseArgs xs curDefaultType curDefaultType (TypedPath x curType : files) flags
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
        l:_ -> if not ("#!" `isPrefixOf` l)
               then GuessType
               else let splitShebang = splitOn "/" l
                    in if length splitShebang == 1
                       then GuessType
                       else let finalPart = last splitShebang
                                progname = if "env " `isPrefixOf` finalPart
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

finalizeTypes (f@(TypedFile (TypedPath path GuessType) contents):fs) =
    let guessedType = guessType f
        finalizedFile = TypedFile (TypedPath path guessedType) contents
    in finalizedFile : finalizeTypes fs
finalizeTypes (f:fs) = f : finalizeTypes fs
finalizeTypes [] = []

diffLists (x:xs) (y:ys) = if x == y
                          then diffLists xs ys
                          else (x, y) : diffLists xs ys
diffLists _      _      = []

describeGuesses (TypedFile (TypedPath path filetype) _ : gs) =
    let languageName = case filetype of
            PythonSource  -> "Python"
            PerlSource    -> "Perl"
            HaskellSource -> "Haskell"
            ShellSource   -> "shell (bash/etc.)"
            Plaintext     -> "plain text"
            GuessType     -> error "type shouldn't be unguessed after being guessed!"
        desc = "filetype of '" ++ path ++ "' not provided, guessed " ++ languageName
    in desc : describeGuesses gs
describeGuesses [] = []

--
-- SOURCE-LINE FILTERING

--- HELPERS
filterNone ps (x:xs) = if or $ ps <*> [x]
                       then filterNone ps xs
                       else x : filterNone ps xs
filterNone ps []     = []

--- LINE TESTS
isAllWhitespace = all isSpace

isHashComment = ("#" `isPrefixOf`) . dropWhile isSpace

isHaskellComment = ("--" `isPrefixOf`) . dropWhile isSpace

--- SOURCE-LINE FILTERS
sourceLines PythonSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines PerlSource = filterNone [null, isAllWhitespace, isHashComment]

sourceLines HaskellSource = filterNone [null, isAllWhitespace, isHaskellComment]
                          . stripMultilineComments False
    where stripMultilineComments False (l:ls) =
            if "{-" `isPrefixOf` dropWhile isSpace l
            then stripMultilineComments True (l:ls)
            else l : stripMultilineComments False ls
          stripMultilineComments True  (l:ls) =
            if "-}" `isInfixOf` l
            then let l' = concat . drop 1 . splitOn "-}" $ l
                 in stripMultilineComments False (l':ls)
            else stripMultilineComments True ls
          stripMultilineComments _     _      = []

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
                       in if roundedN == fromInteger fullyRoundN  -- xyz.0
                          then show fullyRoundN ++ scaleSuffix
                          else show roundedN ++ scaleSuffix
    in ShowCountedFile path sizeStr

prettyCounts fHumanSizes fTotal fOnlyTotal counts =
    let totalCount = (++ " total lines") . show . sum . map cfCount $ counts
        shownCounts = map (showCount fHumanSizes) counts
        longestN (n:ns) acc = let len = length n
                              in longestN ns (if len > acc then len else acc)
        longestN []     acc = acc
        getNumLines (ShowCountedFile _ n) = n
        numWidth = longestN (map getNumLines shownCounts) 0
        padNum n w = replicate (w - length n) ' ' ++ n
        formatLine w (ShowCountedFile path n) = padNum n w ++ " " ++ path
    in (if fOnlyTotal
        then []
        else map (formatLine numWidth) shownCounts)
       ++
       (if fTotal
        then if fOnlyTotal
             then [totalCount]
             else ["", totalCount]
        else [])

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
readTypedFiles = mapM cachedReadTypedFile

--
-- PULL IT ALL TOGETHER

main = do
    args <- getArgs
    let (Args rawFiles lastType flags) = parseArgsWithDefaults args
    let files = if null rawFiles && not (Set.member ShowHelp flags)
                then [TypedPath "-" lastType]
                else rawFiles
    if Set.member ShowHelp flags
    then getProgName >>= putUsage
    else do
        (filesRead, _) <- runStateT (readTypedFiles files) Map.empty
        let finalFiles = finalizeTypes filesRead
        unless (Set.member Quiet flags) $ do
            let guesses = map snd (diffLists filesRead finalFiles)
            mapM_ (putStrLn . ("NOTE: " ++)) (describeGuesses guesses)
        let [fHumanSizes, fTotal, fOnlyTotal] = map (flip Set.member flags)
                                              $ [HumanSizes, ShowTotal, HideIndividual]
        mapM_ putStrLn (prettyCounts fHumanSizes fTotal fOnlyTotal (map countSloc finalFiles))
