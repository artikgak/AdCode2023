module Task11 where

import System.IO
import Data.List

type Grid = [[Char]]
type Coord = (Int, Int)

main :: IO ()
main = do
    --handle <- openFile "test11.txt" ReadMode
    handle <- openFile "task11.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents
        galaxies = findGalaxies grid
        (x, y) = findExpandCoords grid
        allDistances = findAllDitances galaxies (x, y)
    --print grid
    --print $ galaxies
    --print $ (x, y)
    --print $ allDistances
    print $ sum allDistances
    hClose handle

getElem :: Grid -> Coord -> Char
getElem grid (x, y) = if (y>=0) && (x>=0) && (y < (length grid)) && (x < (length $ grid !! y)) then (grid !! y) !! x else '.'

findGalaxies :: Grid -> [Coord]
findGalaxies grid = [(x, y) | y <- [0..(length grid - 1)], x <- [0..(length (grid !! y) - 1)], getElem grid (x, y) == '#']

findExpandCoords :: Grid -> ([Int], [Int])
findExpandCoords grid = (x, y) where
    y = map fst $ filter (\row -> all (/='#') (snd row)) (zip [0..] grid)
    x = map fst $ filter (\row -> all (/='#') (snd row)) (zip [0..] (transpose grid))

findDistance :: Coord -> Coord -> ([Int], [Int]) -> Int
findDistance (x1, y1) (x2, y2) (x, y) = (abs (x1 - x2)) + (abs (y1 - y2)) + (999999*(length expandCorrection)) where
    expandCorrection = (filter (\ex -> ex > min x1 x2 && ex < max x1 x2) x) ++ (filter (\ey -> ey > min y1 y2 && ey < max y1 y2) y)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

findAllDitances :: [Coord] -> ([Int], [Int]) -> [Int]
findAllDitances galaxies expandCoords = map (\(g1, g2) -> findDistance g1 g2 expandCoords) (pairs galaxies)