module Task14 where

import System.IO
import Data.List
import Data.List.Split


main :: IO ()
main = do
    --handle <- openFile "test14.txt" ReadMode
    handle <- openFile "task14.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents
        toTheLeft = transpose grid
        movedToTheLeft = map rollBoulderToTheLeft toTheLeft
        calcLoad = map calculateLeftLoad movedToTheLeft
--------------------part 2------------------------------------------------
        nSkip = 200
        skipCycles = iterate makeCycleRoll grid !! nSkip
        cycleLen = tryFindCycle skipCycles
        findSameAsEnd = (1000000000 - nSkip) `rem` cycleLen
        endCycle = iterate makeCycleRoll skipCycles !! findSameAsEnd
        endToTheLeft = transpose endCycle
        calcLoad2 = map calculateLeftLoad endToTheLeft
    --print "--------grid---------"
    --mapM_  print grid
    print $ cycleLen
    print $ findSameAsEnd
    print $ calcLoad2
    print $ sum calcLoad2
    hClose handle

tryFindCycle :: [String] -> Int
tryFindCycle startGrid = helper (makeCycleRoll startGrid) 1 where
    helper :: [String] -> Int -> Int
    helper arr counter | counter > 1000 = error "No cycle found in 1000 steps"
                       | arr == startGrid = counter
                       | otherwise = helper (makeCycleRoll arr) (counter + 1)

makeCycleRoll :: [String] -> [String]
makeCycleRoll arr = map reverse move4 where
    northL = transpose arr
    move1  = map rollBoulderToTheLeft northL
    westL  = transpose move1
    move2  = map rollBoulderToTheLeft westL
    southL = map reverse $ transpose move2
    move3  = map rollBoulderToTheLeft southL
    eastL  = map reverse $ transpose $ map reverse move3
    move4  = map rollBoulderToTheLeft eastL


rollBoulderToTheLeft :: String -> String
rollBoulderToTheLeft str = helper str [] [] [] where
    helper :: String -> String -> String -> String -> String
    helper []       bufferB bufferEmpty res = reverse (bufferEmpty ++ bufferB ++ res)
    helper ('#':xs) bufferB bufferEmpty res = helper xs [] [] ("#" ++ bufferEmpty ++ bufferB ++ res)
    helper ('O':xs) bufferB bufferEmpty res = helper xs ('O':bufferB) bufferEmpty res
    helper ('.':xs) bufferB bufferEmpty res = helper xs bufferB ('.':bufferEmpty) res
    helper (_:xs) _ _ _ = error "Invalid input"

calculateLeftLoad :: String -> Int
calculateLeftLoad str = helper str 0 where
    helper :: String -> Int -> Int
    helper []       res = res
    helper ('O':xs) res = helper xs (res + length xs + 1)
    helper ( _ :xs) res = helper xs res