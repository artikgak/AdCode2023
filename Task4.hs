module Task4 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Card = ([Int], [Int])

main :: IO ()
main = do
    --handle <- openFile "test4.txt" ReadMode
    handle <- openFile "task4.txt" ReadMode
    contents <- hGetContents handle
    let cards = lines contents
        preproccess1 = map (\x -> map (splitOn " ") x) $ map (splitOn "|") cards
        preproccess2 = map (\[a,b] -> (drop 2 $ filter (not . null) a , filter (not . null) b)) preproccess1
        sorted = map (\(a,b) -> (sort $ (map (\x -> read x::Int) a), sort $ (map (\x -> read x::Int) b))) preproccess2
        matches = map findMatch sorted
        p2 = part2 (zip [1, 1 ..] matches)
    print $ sum $ map toScore matches -- part 1
    print $ sum $ map fst p2 -- part 2
    hClose handle

-- (total n-cards, n-matches)
part2 :: [(Int,Int)] -> [(Int,Int)]
part2 [] = []
part2 ((c, m):xs) = (c, m) : (part2 uxs) where
    uxs = (map (\(c', n') -> (c' + c, n')) (take m xs)) ++ (drop m xs)

toScore :: Int -> Int
toScore n = if n == 0 then 0 else 2^(n-1) where

findMatch :: Card -> Int
findMatch (a, b) = helper a b 0 where
    helper :: [Int] -> [Int] -> Int -> Int
    helper [] _ n = n
    helper _ [] n = n
    helper (x:xs) (y:ys) n | x == y = helper xs ys (n+1)
                           | x >  y = helper (x:xs) ys n
                           | x <  y = helper xs (y:ys) n

