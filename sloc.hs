import Control.Monad (when)
import System.Environment (getArgs, getProgName)

data SourceType = PythonSource | HaskellSource | ShellSource | Plaintext | GuessType
    deriving (Eq, Show)
data FileWithType = TypedFile FilePath SourceType
    deriving (Eq, Show)

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

parseArgs (x:xs) curType files
    | x == "--python"  = parseArgs xs PythonSource files
    | x == "--haskell" = parseArgs xs HaskellSource files
    | x == "--shell"   = parseArgs xs ShellSource files
    | x == "--text"    = parseArgs xs Plaintext files
    | x == "--auto"    = parseArgs xs GuessType files
    | x == "-h"        = ([], curType, True)
    | x == "--help"    = ([], curType, True)
    | otherwise        = parseArgs xs curType ((TypedFile x curType):files)
parseArgs [] curType files = (reverse files, curType, False)

main = do
    args <- getArgs
    let (rawFiles, lastType, showHelp) = parseArgs args GuessType []
    let files = if (rawFiles == []) && (not showHelp)
                then [TypedFile "-" lastType]
                else rawFiles
    when showHelp $ getProgName >>= putUsage
    mapM (putStrLn . show) files
