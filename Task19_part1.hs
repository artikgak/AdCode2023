module Task19 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (digitToInt, toUpper)

data Detail = Detail { x :: Int, m :: Int, a :: Int, s :: Int } deriving (Show, Eq)

type Label = String
type Rule = (Detail -> Bool, Label)
type Workflow = (Label, [Rule])

splitArrayOn :: String -> [String] -> [[String]]
splitArrayOn _ [] = []
splitArrayOn c arr = helper arr [] [] where
    helper :: [String] -> [String] -> [[String]] -> [[String]]
    helper [] [] res = res
    helper [] cur res = res ++ [cur]
    helper (x:xs) cur res = if x == c then helper xs [] (res ++ [cur]) else helper xs (cur ++ [x]) res

main :: IO ()
main = do
    --handle <- openFile "test19.txt" ReadMode
    handle <- openFile "task19.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = lines contents
        spl = splitArrayOn "" parsed_data
        workflows = head spl
        details = map parseDetail $ last spl
        wkfls = map parseWorkflow workflows
        processed = map (processDetail wkfls) details
        res1 = sum $ map (sumDetail . snd) $ filter (fst) processed
    print res1
    hClose handle

sumDetail :: Detail -> Int
sumDetail det = x det + m det + a det + s det

parseDetail :: String -> Detail
parseDetail str = Detail { x = x, m = m, a = a, s = s } where
    rmBrackets = tail $ init str
    exprs = splitOn "," rmBrackets
    x = read $ drop 2 $ (exprs !! 0) :: Int
    m = read $ drop 2 $ (exprs !! 1) :: Int
    a = read $ drop 2 $ (exprs !! 2) :: Int
    s = read $ drop 2 $ (exprs !! 3) :: Int

parseWorkflow :: String -> Workflow
parseWorkflow str = (head spl, map parseRule splRules) where
    spl = splitOn "{" $ init str
    splRules = splitOn "," $ last spl

parseRule :: String -> Rule
parseRule str | notElem ':' str = ((\_ -> True), str) 
              | otherwise = ((\detail -> compFunc (selectorF detail) num), last spl) where
    spl = splitOn ":" str
    cond = head spl
    selector = cond !! 0
    cmp = cond !! 1
    num = read $ drop 2 cond :: Int
    selectorF = case selector of
        'x' -> x :: (Detail -> Int)
        'm' -> m :: (Detail -> Int)
        'a' -> a :: (Detail -> Int)
        's' -> s :: (Detail -> Int)
    compFunc = case cmp of
        '<' -> (<) :: (Int -> Int -> Bool)
        '>' -> (>) :: (Int -> Int -> Bool)
    
processDetail :: [Workflow] -> Detail -> (Bool, Detail)
processDetail wkfs det = (cycleProcess wkfs (inWkf, det), det) where
    inWkf = findWkflByName wkfs "in"

cycleProcess :: [Workflow] -> (Workflow, Detail) -> Bool
cycleProcess wkfs (wk, det) = if nextWkfLab == "R" then False
                              else if nextWkfLab == "A" then True
                              else cycleProcess wkfs (findWkflByName wkfs nextWkfLab, det) where
    nextWkfLab = processSignleWkf wk det

processSignleWkf :: Workflow -> Detail -> Label
processSignleWkf ("R", _) _ = error "Rejected state"
processSignleWkf ("A", _) _ = error "Accepted state"
processSignleWkf (_, rules) det = helper rules det where
    helper :: [Rule] -> Detail -> Label
    helper ((f, lab):xs) det = if f det then lab else helper xs det

findWkflByName :: [Workflow] -> Label -> Workflow
findWkflByName [] _ = error "Workflow not found"
findWkflByName (x@(lab,_):xs) l = if lab == l then x else findWkflByName xs l