module Task6 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Time = Int
type Distance = Int

main :: IO ()
main = do
    --handle <- openFile "test6.txt" ReadMode
    handle <- openFile "task6.txt" ReadMode
    contents <- hGetContents handle
    let data1 = lines contents
        td' = map ((map (\x -> read x::Int)) . (drop 1) . filter (not. null) . (splitOn " ")) data1
        td = zip (head td') (last td')
        
        td2' = map ( (drop 1) . filter (not. null) . (splitOn " ")) data1
        td2  = (read $  concat $ head td2'  :: Int, read $ concat $ last td2' :: Int)
    print $ td2
    print $ length $ calc td2
    print $ map (length . calc) td
    hClose handle

{-
[1, 2, 3, 4, 5, 6, 7, ... t]

distance = (t-x)*x

(t-x)*x > d

-}

-- 
calc :: (Time, Distance) -> [Int]
calc (t, d) = filter (\x -> (t-x)*x > d) [1 .. t - 1]