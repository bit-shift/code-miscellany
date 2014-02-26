import Control.Monad (liftM)
import System.Environment (getArgs)

pairs (x:y:rest) = (x, y) : pairs rest
pairs _ = []

genericsounder (phrase, num) = \x -> if (mod x num == 0) then phrase else ""

genericsound soundspecs x = let soundfns = map genericsounder soundspecs
                                soundstr = concat [f x | f <- soundfns]
                            in  if (soundstr == "") then (show x) else soundstr

readSoundSpec (phrase, num) = (phrase, read num)

soundspecsFromArgs args = map readSoundSpec (pairs args)

main = do
    rawsoundspecs <- (liftM soundspecsFromArgs) getArgs
    let soundspecs = if (rawsoundspecs == [])
                     then [("Fizz", 3), ("Buzz", 5)]
                     else rawsoundspecs
    mapM (putStrLn . (genericsound soundspecs)) [1..100]
