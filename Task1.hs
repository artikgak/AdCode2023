module Task1 where

import System.IO
import Data.List

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "task1.txt" ReadMode
    contents <- hGetContents handle
    print $ lines contents
    let parsed = lines contents
        
        --res1 = (map (\x -> [head x, last x])) parsed
        res1 = findFLNumbers parsed
        res = map (\x -> 10 * (head x) + (last x)) res1
    print  $ sum res
    hClose handle

parseLines :: [String] -> [String]
parseLines xs = map (filter (\x -> x `elem` "0123456789")) xs

findFLNumbers :: [String] -> [[Int]]
findFLNumbers xs = map (reverse . helper) xs  where
    helper :: String -> [Int]
    helper str = helper2 str [] where
        helper2 :: String -> [Int] -> [Int]
        helper2 [] res = res
        helper2 arr@(x:xs) res | x `elem` "0123456789" = helper2 xs ((read [x]::Int) : res)
                           | isPrefixOf "one" arr   = helper2 xs (1:res)    
                           | isPrefixOf "two" arr   = helper2 xs (2:res) 
                           | isPrefixOf "three" arr = helper2 xs (3:res) 
                           | isPrefixOf "four" arr  = helper2 xs (4:res) 
                           | isPrefixOf "five" arr  = helper2 xs (5:res) 
                           | isPrefixOf "six" arr   = helper2 xs (6:res) 
                           | isPrefixOf "seven" arr = helper2 xs (7:res) 
                           | isPrefixOf "eight" arr = helper2 xs (8:res) 
                           | isPrefixOf "nine" arr  = helper2 xs (9:res) 
                           | otherwise = helper2 xs res
