module Task3 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Grid = [String]
type Row = String
type Coge = (Int, [(Int, Int)])

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)

main :: IO ()
main = do
    --handle <- openFile "test3.txt" ReadMode
    handle <- openFile "task3.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents

        --res1 = map (\rowInd -> proceedRow grid rowInd) [0..(length grid - 1)]
        res2 = map (\rowInd -> proceedRow2 grid rowInd) [0..(length grid - 1)]
        res22 = map (\(x, [a]) -> (x,a)) $ mySort $ filter (not . null . snd) $ concat res2
        res222 = mergeCoges res22
        res2222 = map (\(xs, _) -> product xs) res222
    print grid
    print $ sum res2222
    --print $ sum $ concat res1
    hClose handle

mergeCoges :: [(Int, (Int, Int))] -> [([Int], (Int, Int))]
mergeCoges xs = helper xs [] where
    helper :: [(Int, (Int, Int))] -> [([Int], (Int, Int))] -> [([Int], (Int, Int))]
    helper [] res = res
    helper [_] res = res
    helper ((n1, (x1, y1)):(n2, (x2, y2)):xs) res | x1==x2 && y1==y2 = helper xs (([n1, n2], (x1, y1)):res)
                                                  | otherwise = helper ((n2, (x2, y2)):xs) res

proceedRow :: Grid -> Int -> [Int]
proceedRow gr row = helper gr row 0 [] where
    helper :: Grid -> Int -> Int -> [Int] -> [Int]
    helper gr rowIn colIn res = if colIn >= length (gr!!rowIn) then res else (case readNumberAt gr rowIn colIn of 
        Just (n,l) -> helper gr rowIn (colIn + l) (if hasNonDotSymbolNear gr rowIn colIn (colIn + l - 1) then n:res else res) 
        Nothing -> helper gr rowIn (colIn + 1) res)

proceedRow2 :: Grid -> Int -> [Coge]
proceedRow2 gr row = helper gr row 0 [] where
    helper :: Grid -> Int -> Int -> [Coge] -> [Coge]
    helper gr rowIn colIn res = if colIn >= length (gr!!rowIn) then res else (case readNumberAt gr rowIn colIn of 
        Just (n,l) -> helper gr rowIn (colIn + l) ((n, getNearbyCoges gr rowIn colIn (colIn + l - 1)):res) 
        Nothing -> helper gr rowIn (colIn + 1) res)

getNearbyCoges :: Grid -> Int -> Int -> Int -> [(Int, Int)]
getNearbyCoges gr row colSt colEnd = checkHoriL ++ checkHoriR ++ checkTopN ++ checkBottonN where
    helper :: Row -> Int -> Int -> [Int]
    helper rowGr colS colE = (filter (\col -> (rowGr !! col) == '*') [max colS 0 .. min colE (length rowGr - 1)])
    checkTopN    = if row > 0                                          then zip [row-1, row-1 ..] $ helper (gr !! (row-1)) (colSt-1) (colEnd+1) else []
    checkBottonN = if row < (length gr - 1)                            then zip [row+1, row+1 ..] $ helper (gr !! (row+1)) (colSt-1) (colEnd+1) else []
    checkHoriL   = if colSt > 0 && ((gr !! row) !! (colSt -1) == '*')  then [(row, colSt-1)]               else []
    checkHoriR   = if colEnd < (length (gr !! row) - 1) && ((gr !! row) !! (colEnd+1) == '*') then      [(row, colEnd+1)]                         else []


hasNonDotSymbolNear :: Grid -> Int -> Int -> Int -> Bool
hasNonDotSymbolNear gr row colSt colEnd = checkHoriL || checkHoriR || checkTopN || checkBottonN where
    helper :: Row -> Int -> Int -> Bool
    helper rowGr colS colE = (any (\col -> (rowGr !! col) /= '.') [max colS 0 .. min colE (length rowGr - 1)])
    checkTopN = if row > 0 then helper (gr !! (row-1)) (colSt-1) (colEnd+1) else False
    checkBottonN = if row < (length gr - 1) then helper (gr !! (row+1)) (colSt-1) (colEnd+1) else False
    checkHoriL = if colSt > 0 then (gr !! row) !! (colSt-1) /= '.' else False
    checkHoriR = if colEnd < (length (gr !! row) - 1) then (gr !! row) !! (colEnd+1) /= '.' else False

hasNonDotSymbolNear' :: Grid -> Int -> Int -> Int -> (Bool, Bool, Bool, Bool)
hasNonDotSymbolNear' gr row colSt colEnd = (checkHoriL, checkHoriR, checkTopN, checkBottonN) where
    helper :: Row -> Int -> Int -> Bool
    helper rowGr colS colE = (any (\col -> (rowGr !! col) /= '.') [max colS 0 .. min colE (length rowGr - 1)])
    checkTopN = if row > 0 then helper (gr !! (row-1)) (colSt-1) (colEnd+1) else False
    checkBottonN = if row < (length gr - 1) then helper (gr !! (row+1)) (colSt-1) (colEnd+1) else False
    checkHoriL = if colSt > 0 then (gr !! row) !! (colSt-1) /= '.' else False
    checkHoriR = if colEnd < (length (gr !! row) - 1) then (gr !! row) !! (colEnd+1) /= '.' else False

readNumberAt :: Grid -> Int -> Int -> Maybe (Int, Int)
readNumberAt gr row col = if number /= "" then Just ((read number :: Int), length number) else Nothing where
    number = getnumber gr row col ""

getnumber :: Grid -> Int -> Int -> String -> String
getnumber gr row col str = if col <= (length (gr !! row) - 1) && isDigit (gr !! row !! col) then getnumber gr row (col + 1) (str ++ [gr !! row !! col]) else str
