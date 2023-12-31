module Task20 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (digitToInt, toUpper)

data Pulse = High | Low deriving (Show, Eq)
data Type = FlipFlop Bool | Conjunction [(ModuleName, Pulse)] | Broadcast | Button deriving (Show, Eq)

type ModuleName = String
type Module = (ModuleName, Type, [ModuleName])

type Message = (ModuleName, Pulse, ModuleName) 

main :: IO ()
main = do
    --handle <- openFile "test20.txt" ReadMode
    handle <- openFile "task20.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = lines contents
        modules' = map parseModule parsed_data
        modules = ("button", Button, ["broadcaster"]) : updateConjunctionOutputs modules'
        nTimes  = propageteFullNTimes modules 1000
        res = (\(_,cl, ch) -> cl*ch) nTimes
        -------------- part 2 (note: need to modify input, see at the bottom)-----------------------------
        --res2 = propageteTillRxStart modules

    print res
    --print res2
    hClose handle

------------------ parse ----------------
parseModule :: String -> Module
parseModule str = (name, type_, outputs)
    where
        spl1 = splitOn " -> " str
        name' = head spl1
        outputs = splitOn ", " $ last spl1
        type_ | head name' == '%' = FlipFlop False
              | head name' == '&' = Conjunction []
              | name' == "broadcaster" = Broadcast
        name | type_ == Broadcast = name'
             | otherwise = tail name'
        inputs = tail $ tail $ splitOn " " str

getInputsToModule :: [Module] -> ModuleName -> [ModuleName]
getInputsToModule ms name = map (\(n,_,_) -> n) $ filter (\(_,_,out) -> name `elem` out) ms

updateConjunctionOutputs :: [Module] -> [Module]
updateConjunctionOutputs moduls = helper moduls where
    helper :: [Module] -> [Module]
    helper [] = []
    helper (m@(name, Conjunction inputs, outputs):ms) = (name, Conjunction $ zip newInputs (repeat Low), outputs) : helper ms where
        newInputs = getInputsToModule moduls name
    helper (m:ms) = m : helper ms

updateMem :: [(ModuleName, Pulse)] -> ModuleName -> Pulse -> [(ModuleName, Pulse)]
updateMem [] _ _ = error "Not found"
updateMem ((name, p):xs) name2 p2 | name == name2 = (name, p2) : xs
                                  | otherwise = (name, p) : updateMem xs name2 p2

------------------ propagate pulse ----------------
proccessPulse :: Message -> Module -> (Module, [Message])
proccessPulse (_, High, _) m@(_, FlipFlop _, _) = (m,[])
proccessPulse (_, Low, _)  (name, FlipFlop b, outputs) = ((name, FlipFlop newb, outputs), map (\out -> (name, outPulse, out)) outputs) where
    newb = not b
    outPulse = if newb then High else Low

proccessPulse (from, p, _) (name, Conjunction inp, outputs) = ((name, Conjunction newP, outputs), map (\out -> (name, outPulse, out)) outputs) where
    newP = updateMem inp from p
    outPulse = if all (\(_,p) -> p == High) newP then Low else High

proccessPulse (_, p, _) m@(name, Broadcast, outputs) = (m, map (\out -> (name, p, out)) outputs)

proccessPulse (_, p, _) m@(name, Button, outputs) = (m, [(name, Low, "broadcaster")])

pushTheButton :: Message
pushTheButton = ("button", Low, "broadcaster")

propagateMessage :: [Module] -> Message -> ([Module], [Message])
--propagateMessage [] msg@(_,_,recieverName) = error $ "module not found for msg: " ++ show msg
propagateMessage [] _ = ([],[])
propagateMessage (m@(name,_,_):ms) msg@(_,p, recieverName) | name /= recieverName = (\(modules, outmsg) -> (m:modules, outmsg) ) (propagateMessage ms msg)
                                                           | otherwise = (updModule:ms, outsmg) where
                                                            (updModule, outsmg) = proccessPulse msg m


-- inModules -> inputMessages -> (output modules, LowCount, HighCount)
propageteFull :: [Module] -> [Message] -> ([Module], Int, Int)
propageteFull inMod msgs = helper (inMod, 0, 0) msgs where
    helper :: ([Module], Int, Int) -> [Message] -> ([Module], Int, Int)
    helper res [] = res
    helper (modules, lowCount, highCount) (m@(_,p,_):msgs) = helper (updModules, updLowCount, updHighCount) (msgs ++ outMsgs) where
        (updModules, outMsgs) = propagateMessage modules m
        updLowCount  = if p == Low  then lowCount  + 1 else lowCount
        updHighCount = if p == High then highCount + 1 else highCount


propageteFullNTimes :: [Module] -> Int -> ([Module], Int, Int)
propageteFullNTimes inMod n = helper (inMod, 0, 0) n where
    helper :: ([Module], Int, Int) -> Int -> ([Module], Int, Int)
    helper res 0 = res
    helper (modules, lc, hc) n = helper (updModules, lc + nlc, hc + nhc) (n-1) where
        (updModules, nlc, nhc) = propageteFull modules [pushTheButton]

------------------ part 2 ------------------------------------------------------------

propagateMessage2 :: [Module] -> Message -> ([Module], [Message])
propagateMessage2 [] (_, Low, "rx") = ([], [("END", Low, "END")])
propagateMessage2 [] _ = ([],[])
propagateMessage2 (m@(name,_,_):ms) msg@(_,p, recieverName) | name /= recieverName = (\(modules, outmsg) -> (m:modules, outmsg) ) (propagateMessage2 ms msg)
                                                           | otherwise = (updModule:ms, outsmg) where
                                                            (updModule, outsmg) = proccessPulse msg m


-- inModules -> inputMessages -> (output modules, Stop?)
propageteOncePart2 :: [Module] -> [Message] -> ([Module], Bool)
propageteOncePart2 inMod msgs = helper inMod msgs where
    helper :: [Module] -> [Message] -> ([Module], Bool)
    helper modules [] = (modules, False)
    helper modules (("END", Low, "END"):_) = (modules, True)
    helper modules (m@(_,p,_):msgs) = helper updModules (msgs ++ outMsgs) where
        (updModules, outMsgs) = propagateMessage2 modules m

propageteTillRxStart :: [Module] -> Int
propageteTillRxStart inMod = (\(_,_,count) -> count) $ helper (inMod, False, 0) where
    helper :: ([Module], Bool, Int) -> ([Module], Bool, Int)
    helper r@(_, True, _) = r
    helper (modules, _, n) = helper (updModules, b, n+1) where
        (updModules, b) = propageteOncePart2 modules [pushTheButton]

-- test on different inputs
-- in my case I have one & modul to rx
-- &hp -> rx. 

--So try leaving only one input to hp (I have 4 of them)
-- &rf -> hp (4021)
-- &vq -> hp (3917)
-- &sn -> hp (3967)
-- &sr -> hp (3923)

-- We need to find when their cycles allign
-- All primes -> multiply to find LCM