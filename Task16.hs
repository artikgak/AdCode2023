module Task16 where

import System.IO
import Data.List
import Data.List.Split

data Beem = L | R | U | D deriving (Show, Eq)
type Coord = (Int, Int)
type BeemCell = (Char, [Beem])

type BeemGrid = [[BeemCell]]

type MoveBeem = (Coord, Beem)

main :: IO ()
main = do
    --handle <- openFile "test16.txt" ReadMode
    handle <- openFile "task16.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = lines contents
        runned = runBeems (initGrid parsed_data, initBeems)
        beemsToRun = map (\x -> [x]) $ initBeems2 (length $ head parsed_data) (length parsed_data)
        manyRuns = map (\initRun -> runBeems (initGrid parsed_data, initRun)) beemsToRun
        counts = map (\(grid, mbs) -> countEnergized grid) manyRuns
    mapM_ print parsed_data
    --mapM_  print $ fst runned
    --print $ snd runned
    print $ countEnergized $ fst runned
    --print $ size
    --print $ fst size * snd size
    ---------part 2-----------------
    print $ counts
    print $ maximum counts

    hClose handle

initGrid :: [[Char]] -> BeemGrid
initGrid pre_grid = map (\row -> map (\cell -> (cell, [])) row) pre_grid

initBeems :: [MoveBeem]
initBeems = [((0,0), R)]

initBeems2 :: Int -> Int -> [MoveBeem]
initBeems2 xlim ylim = [((0,y), R) | y <- [0..ylim-1]] ++ [((xlim-1,y), L) | y <- [0..ylim-1]] ++ [((x, 0), D) | x <- [0..xlim-1]] ++ [((x,ylim-1), U) | x <- [0..xlim-1]]

getNewCoords :: Char -> MoveBeem -> [MoveBeem]
getNewCoords '.' ((x,y), L) = [((x-1, y)  , L)]
getNewCoords '.' ((x,y), R) = [((x+1, y)  , R)]
getNewCoords '.' ((x,y), U) = [((x  , y-1), U)]
getNewCoords '.' ((x,y), D) = [((x  , y+1), D)]

getNewCoords '-' ((x,y), L) = [((x-1, y), L)]
getNewCoords '-' ((x,y), R) = [((x+1, y), R)]
getNewCoords '-' ((x,y), U) = [((x-1, y), L), ((x+1, y), R)]
getNewCoords '-' ((x,y), D) = [((x-1, y), L), ((x+1, y), R)]

getNewCoords '|' ((x,y), L) = [((x  , y-1), U), ((x  , y+1), D)]
getNewCoords '|' ((x,y), R) = [((x  , y-1), U), ((x  , y+1), D)]
getNewCoords '|' ((x,y), U) = [((x  , y-1), U)]
getNewCoords '|' ((x,y), D) = [((x  , y+1), D)]

getNewCoords '/' ((x,y), L) = [((x  , y+1), D)]
getNewCoords '/' ((x,y), R) = [((x  , y-1), U)]
getNewCoords '/' ((x,y), U) = [((x+1, y), R)]
getNewCoords '/' ((x,y), D) = [((x-1, y), L)]

getNewCoords '\\' ((x,y), L) = [((x  , y-1), U)]
getNewCoords '\\' ((x,y), R) = [((x  , y+1), D)]
getNewCoords '\\' ((x,y), U) = [((x-1, y), L)]
getNewCoords '\\' ((x,y), D) = [((x+1, y), R)]

getNewCoords  char _ = error $ "unexpected char: " ++ [char]

energinzeCell :: BeemCell -> MoveBeem -> (BeemCell, [MoveBeem])
energinzeCell (c, beems)  mb@((x,y), beem) = ((c, addIfNotElem beem beems), getNewCoords c mb)

energizeCellMoveBeem :: BeemGrid -> MoveBeem -> (BeemGrid, [MoveBeem])
energizeCellMoveBeem grid ((x,y), dir) | y < 0 || y >= length grid || x < 0 || x >= length (grid !! y) = (grid, [((x,y), dir)])
                                       | otherwise = (updatedGrid, updatedMoveBeems) where
                                            (cell, beems) = grid !! y !! x
                                            (updatedCell, updatedMoveBeems) = energinzeCell (cell, beems) ((x,y), dir)
                                            updatedGrid = update2DList grid y x updatedCell

update2DList :: BeemGrid -> Int -> Int -> BeemCell -> BeemGrid
update2DList grid y x cell = take y grid ++ [take x (grid !! y) ++ [cell] ++ drop (x+1) (grid !! y)] ++ drop (y+1) grid

addIfNotElem :: Eq a => a -> [a] -> [a]
addIfNotElem a xs = if a `elem` xs then xs else (a:xs)

beemValid :: BeemGrid -> MoveBeem -> Bool
beemValid grid ((x,y), _) | y < 0 || y >= length grid || x < 0 || x >= length (grid !! y) = False
                            | otherwise = True

moveBeems1Step :: (BeemGrid, [MoveBeem]) -> (BeemGrid, [MoveBeem])
moveBeems1Step (grid, mbs) = filterBeems $ helper grid mbs [] where
    helper :: BeemGrid -> [MoveBeem] -> [MoveBeem] -> (BeemGrid, [MoveBeem])
    helper grid [] new_mbs = (grid, new_mbs)
    helper grid (mb:mbs) new_mbs | not $ beemValid grid mb = helper grid mbs new_mbs
                                 | otherwise = helper newgrid mbs ((filter (\b -> beemValid grid b && b `notElem` new_mbs) nbeems) ++ new_mbs) where
                                        (newgrid, nbeems) = energizeCellMoveBeem grid mb

stopCondition :: (BeemGrid, [MoveBeem]) -> Bool
stopCondition (beemGrid, mbs) = all (\((x, y), beem) -> not (beemValid beemGrid ((x, y), beem)) || beem `elem` (snd $ beemGrid !! y !! x)) mbs

filterBeems :: (BeemGrid, [MoveBeem]) -> (BeemGrid, [MoveBeem])
filterBeems (grid, mbs) = (grid, filter (\((x,y), b) -> b `notElem` (snd $ grid !! y !! x)) mbs)

runBeems :: (BeemGrid, [MoveBeem]) -> (BeemGrid, [MoveBeem])
runBeems = until stopCondition moveBeems1Step

countEnergized :: BeemGrid -> Int
countEnergized grid = length $ filter (not . null . snd) (concat grid)