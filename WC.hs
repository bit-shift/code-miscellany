import Control.Monad (when)
import System.Environment (getArgs, getProgName)

data CountMode = CountLines | CountChars | CountAllChars | CountWords | ShowHelp
    deriving Eq

data FileOrStdin = File FilePath | Stdin

parseArgs (x:xs) mode files | x == "-l" = parseArgs xs CountLines files
                            | x == "-c" = parseArgs xs CountChars files
                            | x == "-C" = parseArgs xs CountAllChars files
                            | x == "-w" = parseArgs xs CountWords files
                            | x == "-h" = (ShowHelp, [])
                            | otherwise = parseArgs xs mode (x:files)
parseArgs [] mode files = (mode, reverse files)

putUsage progname = do
    putStrLn ("Usage: " ++ progname ++ " [OPTION] [FILE...]")
    putStrLn "Prints line, word, or char (with or without newlines) counts"
    putStrLn "for each FILE. With no FILE, or with \"-\", standard input is"
    putStrLn "counted instead. If no count type option is given, defaults to"
    putStrLn "word counts."
    putStrLn "The options below may be used:"
    putStrLn "  -c    print character counts (excluding newlines)"
    putStrLn "  -C    print character counts (including newlines)"
    putStrLn "  -l    print line counts"
    putStrLn "  -w    print word counts"
    putStrLn "  -h    print this help message, and exit"

fileOrStdinFromPath path | path == "-" = Stdin
                         | otherwise   = File path

readFileOrStdin Stdin           = getContents
readFileOrStdin (File filename) = readFile filename

printableName Stdin = "<stdin>"
printableName (File filename) = filename

putFileCount mode path = do
    let inputSource = fileOrStdinFromPath path
    input <- readFileOrStdin inputSource
    case mode of
        CountLines    -> putLength inputSource (lines input)
        CountChars    -> putLength inputSource (concat (lines input))
        CountAllChars -> putLength inputSource input
        CountWords    -> putLength inputSource (words input)
        where putLength f l = putStrLn ((printableName f) ++ ": " ++ (show (length l)))

main = do
    args <- getArgs
    let (mode, rawFiles) = parseArgs args CountWords []
    let files = if (rawFiles == []) && (mode /= ShowHelp) then ["-"]
                                                          else rawFiles
    when (mode == ShowHelp) $ getProgName >>= putUsage
    mapM (putFileCount mode) files
