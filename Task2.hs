module Task2 where

import System.IO
import Data.List
import Data.List.Split

type Set = [(String, Int)]
type Game = (Int, [Set])

main :: IO ()
main = do
    --handle <- openFile "test2.txt" ReadMode
    handle <- openFile "task2.txt" ReadMode
    contents <- hGetContents handle
    let parsed = parseLines $ lines contents
        res1 = map (toGame) parsed
        res2 = filter checkCondition res1
    --print  $ res1
    --print  $ sum $ map fst res2
    print $ sum $ map getPowerSet res1
    hClose handle

parseLines :: [String] -> [[String]]
parseLines xs = map (\x -> splitOn " " x) xs

pairElements :: [a] -> [(a, a)]
pairElements [] = []
pairElements [_] = [] -- If there's an odd number of elements, ignore the last one
pairElements (x:y:xs) = (x, y) : pairElements xs

-- Example usage:
-- pairElements [1,2,3,4,5,6] will result in [(1,2),(3,4),(5,6)]

preproccess :: [String] -> [[String]]
preproccess xs = helper xs [] [] where
    helper :: [String] -> [String] -> [[String]] -> [[String]]
    helper [] [] res2 = res2
    helper [] res1 res2 = res1 : res2
    helper (x:xs) res1 res2 | last x == ';' = helper xs [] ((x:res1) : res2)
                            | otherwise = helper xs (x:res1) res2

toGame :: [String] -> Game
toGame (_:gid:str) = (read $ (init gid)::Int, prep3) where
    prep = preproccess str
    prep2 = map pairElements prep
    prep3 = map (\e -> map (\(x,y) -> (if last x == ',' || last x == ';' then init x else x, read y::Int)) e) prep2

shortenGame :: Game -> Game
shortenGame (gid, xss) = (gid, map (\xs -> helper xs []) xss) where
    helper :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
    helper [] res = res
    helper (x:xs) res | (fst x) `elem` (map fst res) = helper xs (map (\y -> if (fst y) == (fst x) then (fst x, (snd x) + (snd y)) else y) res)
                      | otherwise = helper xs (x:res)

checkCondition :: Game -> Bool
checkCondition (_, xss) = all checkConditionSet xss

--the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes
checkConditionSet :: Set -> Bool
checkConditionSet xs = length xs <= 3 && red && green && blue where
    red' = filter (\x -> fst x == "red") xs
    red = null red' || (snd . head) red' <= 12
    green' = filter (\x -> fst x == "green") xs
    green = null green' || (snd . head) green' <= 13
    blue' = filter (\x -> fst x == "blue") xs
    blue = null blue' || (snd . head) blue' <= 14

getPowerSet :: Game -> Int
getPowerSet (_, xss) = helper (0,0,0) (concat xss) where
    helper :: (Int,Int,Int) -> Set -> Int
    helper (r,g,b) (("red", n):xs) = helper (max r n,g,b) xs
    helper (r,g,b) (("green", n):xs) = helper (r,max g n,b) xs
    helper (r,g,b) (("blue", n):xs) = helper (r,g,max b n) xs
    helper (r,g,b) [] = r * g * b