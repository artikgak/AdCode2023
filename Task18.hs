module Task18 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (digitToInt, toUpper)

data Direction = U | D | L | R  deriving (Show, Eq)

type Dig = (Direction, Int, String)
type Coord = (Int, Int)

main :: IO ()
main = do
    --handle <- openFile "test18.txt" ReadMode
    handle <- openFile "task18.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = lines contents
        dig_plan = map parseDig parsed_data
        coords = convertToCoords dig_plan
        area = abs $ signedPolygonArea coords
        boundary = sum $ map (\(dir, num, _) -> num) dig_plan
        inside = insidePolygon area boundary
        allcells = boundary + inside
----------------- part 2 --------------------------------------
        dig_plan2 = map (\(_, _, color) -> parseColor color) dig_plan
        coords2 = convertToCoords $ map (\(d, n) -> (d,n,"")) dig_plan2
        area2 = abs $ signedPolygonArea coords2
        boundary2 = sum $ map (\(_, num) -> num) dig_plan2
        inside2 = insidePolygon area2 boundary2
        allcells2 = boundary2 + inside2
    mapM_ print dig_plan
    mapM_ print coords
    print $ "Area: " ++ show area ++ " Boundary: " ++ show boundary ++ " Inside: " ++ show inside
    print $ "Answer 1: " ++ show allcells

    mapM_ print dig_plan2
    mapM_ print coords2
    print $ "Area2: " ++ show area2 ++ " Boundary2: " ++ show boundary2 ++ " Inside2: " ++ show inside2
    print $ "Answer 2: " ++ show allcells2
    hClose handle

parseDig :: String -> Dig
parseDig s = (dir, num, color) where
    splitted = splitOn " " s
    dir = case splitted !! 0 of
        "U" -> U
        "D" -> D
        "L" -> L
        "R" -> R
    num   = read (splitted !! 1) :: Int
    color =  tail $ init $ splitted !! 2

step :: Coord -> Dig -> Coord
step (x, y) (dir, num, _) = case dir of
    U -> (x, y + num)
    D -> (x, y - num)
    L -> (x - num, y)
    R -> (x + num, y)

convertToCoords :: [Dig] -> [Coord]
convertToCoords = foldl (\acc dig -> acc ++ [step (last acc) dig]) [(0,0)]

signedPolygonArea :: [Coord] -> Int
signedPolygonArea coord = ((signedPolygonArea' coord) `div` 2) where
    signedPolygonArea' :: [Coord] -> Int
    signedPolygonArea' [] = 0
    signedPolygonArea' [_] = 0
    signedPolygonArea' ((x1,y1):(x2,y2):xs) = (x1*y2 - x2*y1) + (signedPolygonArea' ((x2,y2):xs))

insidePolygon :: Int -> Int -> Int
insidePolygon s b = s - b `div` 2 + 1

parseColor :: String -> (Direction, Int)
parseColor s = (dir, hexToDecimal numString) where
    dirChar = last s
    dir = case dirChar of
        '0' -> R
        '1' -> D
        '2' -> L
        '3' -> U
        ch  -> error  $ "Wrong direction, found:" ++ [ch] ++ " in " ++ s
    numString = tail $ init s

hexToDecimal :: String -> Int
hexToDecimal = sum . zipWith (*) (iterate (*16) 1) . reverse . map digitToInt . map toUpper

-- Теорема Піка
-- S = i + b/2 - 1
-- i = S - b/2 + 1