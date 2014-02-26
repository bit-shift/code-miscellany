genericsounder (phrase, num) = \x -> if (mod x num == 0) then phrase else ""

genericsound soundspecs x = let soundfns = map genericsounder soundspecs
                                soundstr = foldl1 (++) $ (`map` soundfns) ($ x)
                            in  if (soundstr == "") then (show x) else soundstr

fizzbuzz = genericsound [("Fizz", 3), ("Buzz", 5)]

main = mapM (putStrLn . fizzbuzz) [1..100]
