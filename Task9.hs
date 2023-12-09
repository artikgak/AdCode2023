module Task9 where

import System.IO
import Data.List

main :: IO ()
main = do
    --handle <- openFile "test9.txt" ReadMode
    handle <- openFile "task9.txt" ReadMode
    contents <- hGetContents handle
    let data1 = lines contents
        sequnces = map (map read . words) data1 :: [[Int]]
        predictions = map predict sequnces
    print predictions
    print $ foldl1 (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) predictions
    hClose handle

predict :: [Int] -> (Int, Int)
predict xs = (foldl1 (flip (-)) fistElems, foldl1 (+) lastElems) where
    allSeq = getAllSequnces [xs]
    lastElems = map last allSeq
    fistElems = map head allSeq

nextSequnce :: [Int] -> [Int]
nextSequnce xs = zipWith (-) (tail xs) xs

getAllSequnces :: [[Int]] -> [[Int]]
getAllSequnces xs = if all (==0) nextSeq then xs else getAllSequnces (nextSeq:xs) where
    nextSeq = nextSequnce (head xs)