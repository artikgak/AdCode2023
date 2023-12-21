module Task13 where

import System.IO
import Data.List
import Data.List.Split


main :: IO ()
main = do
    --handle <- openFile "test13.txt" ReadMode
    handle <- openFile "task13.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents
        splitted_data = splitArrayOn "" grid
        calc = map workMirror splitted_data
        pre_res1 = lplTolp calc

        fixes = map (\x -> (x, generateAllFixes x)) splitted_data
        calcFixes = map (\(x, y) -> (workMirror x, filter (\a -> a /= ([],[]) && a /= workMirror x) $ map workMirror y)) fixes
        filtered2 = map (\(x, y) -> (x, filter (/=x) y )) calcFixes
        prepare2 = map (\(x, y) -> (x, lplTolp y)) filtered2
        removedPrev = map (\((a,b), (x,y)) -> ((nub x) \\ a, (nub y) \\ b)) prepare2
        pre_res2 = lplTolp removedPrev

    --print $ calc
    --print pre_res1
    print $ ((100 *) $ sum $ fst pre_res1) + (sum $ snd pre_res1)

    --print $ calcFixes
    --print $ filtered2
    --print $ prepare2
    --print $ removedPrev
    --print $ pre_res2
    print $ ((100 *) $ sum $ fst pre_res2) + (sum $ snd pre_res2)
    hClose handle


lplTolp :: [([Int], [Int])] -> ([Int], [Int])
lplTolp lpl = (concat [a | (a, _) <- lpl], concat [b | (_, b) <- lpl])

workMirror :: [String] -> ([Int], [Int])
workMirror arr = (workOut arr, workOut $ transpose arr)

workOut :: [String] -> [Int]
workOut arr = res where
    genSplits = zip [1..] $ generateAllSplits arr
    res1 = filter (\(n, (x,y)) -> areTheSame (reverse x) y) genSplits
    res = map fst res1

generateAllSplits :: [String] -> [([String],[String])]
generateAllSplits arr = [(x,y) | n <- [1..(length arr - 1)], let (x,y) = splitAt n arr]

areTheSame :: [String] -> [String] -> Bool
areTheSame a b = all id $ zipWith (==) a b

splitArrayOn :: String -> [String] -> [[String]]
splitArrayOn _ [] = []
splitArrayOn c arr = helper arr [] [] where
    helper :: [String] -> [String] -> [[String]] -> [[String]]
    helper [] [] res = res
    helper [] cur res = res ++ [cur]
    helper (x:xs) cur res = if x == c then helper xs [] (res ++ [cur]) else helper xs (cur ++ [x]) res

generateAllFixes :: [String] -> [[String]]
generateAllFixes arr = map (fixOneInMirror arr) allCoords where
    allCoords = [(x,y) | x <- [0..(length (head arr) - 1)], y <- [0..(length arr - 1)]]

fixOneInMirror :: [String] -> (Int, Int) -> [String]
fixOneInMirror (a:arr) (x,0) = (fixOneInRow a x):arr
fixOneInMirror (a:arr) (x,y) = a:(fixOneInMirror arr (x,y-1))

fixOneInRow :: String -> Int -> String
fixOneInRow ('.':arr) 0 = '#':arr
fixOneInRow ('#':arr) 0 = '.':arr
fixOneInRow (a:arr) x = a:(fixOneInRow arr (x-1))
