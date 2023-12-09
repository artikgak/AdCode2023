module Task5 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Destionation = Int
type Source = Int
type RangeLength = Int

type Mapping = (Destionation, Source, RangeLength)

type Name = String
type Table = (Name, [Mapping])

main :: IO ()
main = do
    --handle <- openFile "test5.txt" ReadMode
    handle <- openFile "task5.txt" ReadMode
    contents <- hGetContents handle
    let data1 = lines contents
        prep1 = splitArrayOn "" data1
        seeds = map (\x -> read x::Int) $ tail $ splitOn " " $ head $ head prep1
        seeds_pairs = pairElements seeds
        seeds_range_pairs = map (\(x,y) -> (x, x+y-1)) seeds_pairs
        seeds2' = map (\(x,y) -> [ s | s <- [x .. x+y-1]]) seeds_pairs
        seeds2 = concat seeds2'
        tables = map toTable $ drop 1 prep1
        maps = map snd tables
        res1 = foldl mapSeeds seeds tables
        
        --step1 = mapSeeds2 (splitPointsByTable seeds_range_pairs (tables!!0)) (tables!!0)
        --step2 = mapSeeds2 (splitPointsByTable step1 (tables!!1)) (tables!!1)
        --step3 = mapSeeds2 (splitPointsByTable step2 (tables!!2)) (tables!!2)
        --step4 = mapSeeds2 (splitPointsByTable step3 (tables!!3)) (tables!!3)
        --step5 = mapSeeds2 (splitPointsByTable step4 (tables!!4)) (tables!!4)
        --step6 = mapSeeds2 (splitPointsByTable step5 (tables!!5)) (tables!!5)
        --step7 = mapSeeds2 (splitPointsByTable step6 (tables!!6)) (tables!!6)

        res2 = foldl (\ seeds table -> mapSeeds2 (splitPointsByTable seeds table) table) seeds_range_pairs tables

    --print $ length maps
    --print $ minimum res1
    --print $ "Range pairs: " ++ (show seeds_range_pairs)
    --print $ snd $ head tables
    --print $ "Seeds: " ++ (show seeds_range_pairs)
    --print $ "Table1: " ++ (show $ tables!!0)
    --print $ "Step1: " ++ (show step1)
    --print $ "Table2: " ++ (show $ tables!!1)
    --print $ "Step2: " ++ (show step2)
    --print $ "Table3: " ++ (show $ tables!!2)
    --print $ "Step3: " ++ (show step3)
    --print $ "Table4: " ++ (show $ tables!!3)
    --print $ "Step4: " ++ (show step4)
    --print $ "Table5: " ++ (show $ tables!!4)
    --print $ "Step5: " ++ (show step5)
    --print $ "Table6: " ++ (show $ tables!!5)
    --print $ "Step6: " ++ (show step6)
    --print $ "Table7: " ++ (show $ tables!!6)
    --print $ "Step7: " ++ (show step7)
    --print $ seeds_range_pairs
    --print $ "Result: " ++ (show res2)
    print $ minimum $ concatMap (\(x,y) -> [x,y]) res2
    hClose handle

{-
splitArrayOn :: Eq a => a -> [a] -> [[a]]
splitArrayOn _ [] = []
splitArrayOn c arr = helper arr [] [] where
    helper :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
    helper [] [] res = res
    helper [] cur res = res ++ [cur]
    helper (x:xs) cur res = if x == c then helper xs [] (res ++ [cur]) else helper xs (cur ++ [x]) res
-}

splitArrayOn :: String -> [String] -> [[String]]
splitArrayOn _ [] = []
splitArrayOn c arr = helper arr [] [] where
    helper :: [String] -> [String] -> [[String]] -> [[String]]
    helper [] [] res = res
    helper [] cur res = res ++ [cur]
    helper (x:xs) cur res = if x == c then helper xs [] (res ++ [cur]) else helper xs (cur ++ [x]) res

toTable :: [String] -> Table
toTable (name:xs) = (name, mappings) where
    mappings = map (\x -> (read $ x!!0::Int, read $ x!!1::Int, read $ x!!2::Int)) $ map (splitOn " ") xs

belongsToMapping :: Int -> Mapping -> Bool
belongsToMapping n (d, s, l) = n >= s && n < (s + l)

getMapping :: Int -> [Mapping] -> Int
getMapping n [] = n
getMapping n (x@(d,s,l):xs) = if belongsToMapping n x then d + (n-s) else getMapping n xs

mapSeeds :: [Int] -> Table -> [Int]
mapSeeds seeds (name, mappings) = map (\x -> getMapping x mappings) seeds

pairElements :: [a] -> [(a, a)]
pairElements [] = []
pairElements [_] = [] -- If there's an odd number of elements, ignore the last one
pairElements (x:y:xs) = (x, y) : pairElements xs

-- split by ranges
-- (seed start, seed end)
splitByMapping :: (Int, Int) -> Mapping -> [(Int, Int)]
splitByMapping (start, end) (des, source, len) = below ++ inside ++ above where
    below =  if start < source                         then [(start                    , min (source - 1) end)] else []
    above =  if end > source + len - 1                 then [(max (source+len) start , end                 )] else []
    inside = if start < source + len && end >= source then [(max source start         , min (source+len-1) end)] else []

splitPointsByMapping :: [(Int, Int)] -> Mapping -> [(Int, Int)]
splitPointsByMapping points m = concatMap (\p -> splitByMapping p m) points

splitPointsByMappings :: [(Int, Int)] -> [Mapping] -> [(Int, Int)]
splitPointsByMappings points ms = foldl splitPointsByMapping points ms

splitPointsByTable :: [(Int, Int)] -> Table -> [(Int, Int)]
splitPointsByTable points (_, ms) = foldl splitPointsByMapping points ms

getMappingForRange :: (Int, Int) -> [Mapping] -> (Int, Int)
getMappingForRange (start, end) m = (getMapping start m, getMapping end m)

mapSeeds2 :: [(Int, Int)] -> Table -> [(Int, Int)]
mapSeeds2 seeds (name, mappings) = map (\x -> getMappingForRange x mappings) seeds