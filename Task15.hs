module Task15 where

import System.IO
import Data.List
import Data.List.Split

data Operation = Remove | Add Int deriving (Show, Eq)
type Label = String
type Hash = Int

type Lens = (Label, Hash, Operation)
type LInBox = (Label, Int)

type Box = (Int, [LInBox])

main :: IO ()
main = do
    --handle <- openFile "test15.txt" ReadMode
    handle <- openFile "task15.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = splitOn "," $ head $ lines contents
        hashed = map calcHash parsed_data
        lens = map toLens parsed_data
        boxes = foldl insertLens generateEmptyBoxes lens
        calc = map calcBox boxes
    print $ calcHash "HASH"
    --print $ hashed
    --print $ sum hashed
    -------part 2------------

    --print $ parsed_data
    --print $ lens
    print $ boxes
    print $ calc
    print $ sum calc
    hClose handle

toLens :: String -> Lens
toLens str = (label, hash, operation) where
        label = takeWhile (\x -> x /= '-' && x /= '=') str
        hash = calcHash label
        operation = if (str !! (length label)) == '-' then Remove else Add (read $ drop (length label + 1) str)

calcHash :: String -> Int
calcHash = foldl proceedOneChar 0

proceedOneChar :: Int -> Char -> Int
proceedOneChar accum c = ((accum + (fromEnum c)) * 17) `rem` 256

generateEmptyBoxes :: [Box]
generateEmptyBoxes = zip [0..255] (repeat [])

insertLens :: [Box] -> Lens -> [Box]
insertLens boxes lens@(label, hash, operation) = map (\box@(i, lenses) -> if i == hash then insertLens' box lens else box) boxes

insertLens' :: Box -> Lens -> Box
insertLens' box  lens@(label, _, Remove) = remLens box lens
insertLens' box@(_,lenses)  lens@(label, _, Add n ) = replaceLens box lens

remLens :: Box -> Lens -> Box
remLens (i, lenses) (label, _, Remove) = (i, filter (\(lab,_) -> lab /= label) lenses)

replaceLens :: Box -> Lens -> Box
replaceLens (i, lenses) lens@(label, hash, Add n ) = (i, helper lenses) where
    helper :: [LInBox] -> [LInBox]
    helper [] = [(label, n)]
    helper (l@(lab,_):ls) = if lab /= label then l:(helper ls) else (lab, n):ls


calcBox :: Box -> Int
calcBox (i, lenses) = ((i+1)*) $ sum $ zipWith (\slot (_,focal) -> slot * focal) [1..] lenses