module Task10 where

import System.IO
import Data.List

type Grid = [[Char]]

type Coord = (Int, Int)

data Direction = U | D | L | R | S deriving (Show, Eq)

type Step = (Direction, Coord)

main :: IO ()
main = do
    --handle <- openFile "test10.txt" ReadMode
    handle <- openFile "task10.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents
        start = findStart grid
        startDir = findStartDir grid start
        cycle = goCycle grid [head startDir, (S, start)]
        len_cycle = length cycle
        answer1 = if len_cycle `rem` 2 == 0 then len_cycle `div` 2 else ((len_cycle - 1) `div` 2)

        b_list = map snd cycle
        area = abs $ signedPolygonArea b_list
        i = insidePolygon area (length b_list)
    --print grid
    --print $ start
    --print $ startDir
    --print $ cycle
    print $ length cycle
    print $ answer1
    print $ length b_list
    print $ area
    print $ i
    hClose handle

getElem :: Grid -> (Int, Int) -> Char
getElem grid (x, y) = if (y>=0) && (x>=0) && (y < (length grid)) && (x < (length $ grid !! y)) then (grid !! y) !! x else '.'

findStart :: Grid -> (Int, Int)
findStart grid = head [(x, y) | y <- [0..(length grid - 1)], x <- [0..(length (grid !! y) - 1)], getElem grid (x, y) == 'S']

findStartDir :: Grid -> Coord -> [Step]
findStartDir grid (x, y) = r ++ d ++ l ++ u where
    right = getElem grid (x+1, y)
    left  = getElem grid (x-1, y)
    up    = getElem grid (x, y-1)
    down  = getElem grid (x, y+1)
    r = if right == '-' || right == 'J' || right == '7' then [(R, (x+1, y))] else []
    d = if down  == '|' || down  == 'L' || down  == 'J' then [(D, (x, y+1))] else []
    l = if left  == '-' || left  == 'L' || left  == 'F' then [(L, (x-1, y))] else []
    u = if up    == '|' || up    == '7' || up    == 'F' then [(R, (x, y-1))] else []

makeStep :: Grid -> Step -> Step
makeStep grid (dir, st@(x,y)) = case (getElem grid st, dir) of
    ('|', D) -> (dir, (x, y+1))
    ('|', U) -> (dir, (x, y-1))
    ('-', R) -> (dir, (x+1, y))
    ('-', L) -> (dir, (x-1, y))
    ('L', D) -> (R  , (x+1, y))
    ('L', L) -> (U  , (x, y-1))
    ('J', D) -> (L  , (x-1, y))
    ('J', R) -> (U  , (x, y-1))
    ('7', U) -> (L  , (x-1, y))
    ('7', R) -> (D  , (x, y+1))
    ('F', U) -> (R  , (x+1, y))
    ('F', L) -> (D  , (x, y+1))
    ('S', _) -> (S  , st)
    ('.', _) -> error ". symbol"
    ( _ , _) -> error "Wrong symbol"

goCycle :: Grid -> [Step] -> [Step]
goCycle grid (s:st) = case makeStep grid s of
    (S, _) -> s:st
    (d, c) -> goCycle grid ((d, c):s:st)

signedPolygonArea :: [Coord] -> Int
signedPolygonArea coord = ((signedPolygonArea' coord) `div` 2)

signedPolygonArea' :: [Coord] -> Int
signedPolygonArea' [] = 0
signedPolygonArea' [_] = 0
signedPolygonArea' ((x1,y1):(x2,y2):xs) = (x1*y2 - x2*y1) + (signedPolygonArea' ((x2,y2):xs))

insidePolygon :: Int -> Int -> Int
insidePolygon s b = s - b `div` 2 + 1

-- Теорема Піка
-- S = i + b/2 - 1
-- i = S - b/2 + 1