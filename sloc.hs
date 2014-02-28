import Control.Monad (when)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)

data Flags = ShowHelp | KSloc | Quiet
    deriving (Eq, Ord, Show)
data SourceType = PythonSource | HaskellSource | ShellSource | Plaintext | GuessType
    deriving (Eq, Show)
data FileWithType = TypedFile FilePath SourceType
    deriving (Eq, Show)

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
    | otherwise         = parseArgs xs curDefaultType curDefaultType ((TypedFile x curType):files) flags
parseArgs [] curType _ files flags = (reverse files, curType, flags)

parseArgsWithDefaults args = parseArgs args GuessType GuessType [] Set.empty

--
-- FILETYPE GUESSING

guessType (TypedFile path _) = Plaintext

finalizeType f@(TypedFile path GuessType) = do
    let guessedType = guessType f
    putStrLn ("NOTE: filetype of '" ++ path ++ "' not provided, guessed " ++ (languageName guessedType))
    return (TypedFile path guessedType)
    where languageName PythonSource = "Python"
          languageName HaskellSource = "Haskell"
          languageName ShellSource = "shell (bash/etc.)"
          languageName Plaintext = "plain text"
finalizeType f@(TypedFile _ _) = return f

--
-- PULL IT ALL TOGETHER

main = do
    args <- getArgs
    let (rawFiles, lastType, flags) = parseArgsWithDefaults args
    let files = if (rawFiles == []) && (not (Set.member ShowHelp flags))
                then [TypedFile "-" lastType]
                else rawFiles
    when (Set.member ShowHelp flags) $ getProgName >>= putUsage
    finalFiles <- mapM finalizeType files
    mapM (putStrLn . show) finalFiles
