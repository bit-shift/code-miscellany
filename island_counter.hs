import Control.Monad
import Control.Monad.State (State, StateT, lift, runStateT)
import qualified Control.Monad.State as S
import System.IO

type FinishedZones = Int
type ExtraModifier = Int
type Casualties = Int
type Combats = Int
data CounterData = CounterData FinishedZones ExtraModifier Casualties Combats
type Counter a = StateT CounterData IO a

prompt = "set mod: +N, finish zone(s): ^N, finished combat: everything else> "
initialCounter = (CounterData 0 0 0 0)

increaseFinish n (CounterData fz em c cm) = CounterData (min 6 (fz + n)) em c cm
getFinish (CounterData fz _ _ _) = fz

setModifier n (CounterData fz _ c cm) = CounterData fz n c cm
getModifier (CounterData _ em _ _) = em

increaseCasualties n (CounterData fz em c cm) = CounterData fz em (min 1000 (c + n)) cm
getCasualties (CounterData _ _ c _) = c

increaseCombats (CounterData fz em c cm) = CounterData fz em c (cm + 1)
getCombats (CounterData _ _ _ cm) = cm

loop :: Counter ()
loop = forever $ do
    lift $ putStr prompt
    lift $ hFlush stdout
    input <- lift getLine
    case input of
        ('+':n) -> do let newmod = read n
                      S.modify (setModifier newmod)
                      lift $ putStrLn ("Extra modifier set to +" ++ show newmod)
        ('^':n) -> do let completions = if null n
                                        then 1
                                        else read n
                      S.modify (increaseFinish completions)
                      fin <- S.gets getFinish
                      lift $ putStrLn (show fin ++ " zones finished")
        _       -> do fin <- S.gets getFinish
                      modifier <- S.gets getModifier
                      prevCasualties <- S.gets getCasualties
                      S.modify (increaseCasualties ((2 ^ fin) + modifier))
                      S.modify increaseCombats
                      casualties <- S.gets getCasualties
                      combats <- S.gets getCombats
                      lift $ putStrLn ((show casualties) ++ " casualties, " ++ (show combats) ++ " combats")
                      when (prevCasualties < 64 && casualties >= 64) $ lift $ putStrLn "unlock 1"
                      when (prevCasualties < 192 && casualties >= 192) $ lift $ putStrLn "unlock 2"
                      when (prevCasualties < 458 && casualties >= 458) $ lift $ putStrLn "unlock 3"
                      when (prevCasualties < 1000 && casualties == 1000) $ lift $ putStrLn "unlock HQ!"
    lift $ putStrLn ""

main = runStateT loop initialCounter
