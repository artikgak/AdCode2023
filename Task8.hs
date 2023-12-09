module Task8 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Move = Char
type Move2 = (Char, Int)
type Label = String
type Node = (Label, Label, Label)


main :: IO ()
main = do
    --handle <- openFile "test8_3.txt" ReadMode
    handle <- openFile "task8.txt" ReadMode
    contents <- hGetContents handle
    let data1 = lines contents
        splitted = splitArrayOn "" data1
        moves = head $ head splitted
        moves2 = zip moves [1 .. ]
        treeStr = head $ tail splitted
        tree = map parseNode treeStr

        --part 1
        startNode = head tree
        startLabel = "AAA"
        infiniteMoves = concat $ repeat moves
        infiniteMoves2 = concat $ repeat moves2
        --(_, label, _, count) = until (\(_, lablel, _, _) -> lablel == "ZZZ") generalMoveWCounter (tree, startLabel, infiniteMoves, 0)

        --part 2
        startLabels = map fst3 $ filter (\node -> isSuffixOf "A" (fst3 node)) tree
        endLabels   = map fst3 $ filter (\node -> isSuffixOf "Z" (fst3 node)) tree

        visitAllFromStart = map (\stLab -> visitAll tree stLab infiniteMoves2 []) startLabels
        addCounters = map (\x -> zip (map fst $ reverse x) [0 ..]) visitAllFromStart
        selectEndNodes = filter (\(lab, counter) -> isSuffixOf "Z" lab) (concat addCounters)

    --print $ "Part-1. End Label:" ++ label ++ " Counter: " ++ (show count)
    print "startLabels:"
    print $ startLabels
    print "endLabels:"
    print $ endLabels
    print $ "Part-2"
    print "visitAllFromStart:"
    print $ map length visitAllFromStart
    print "selectEndNodes:"
    print $ selectEndNodes
    hClose handle

selEndNodes = [("XGZ",18113),("ZZZ",20569),("LLZ",21797),("HTZ",13201),("TMZ",24253),("XDZ",22411)]
endNodesCounters = [18113,20569,21797,13201,24253,22411]

multLCM :: [Integer] -> Integer
multLCM = foldl1 lcm

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

splitArrayOn :: String -> [String] -> [[String]]
splitArrayOn _ [] = []
splitArrayOn c arr = helper arr [] [] where
    helper :: [String] -> [String] -> [[String]] -> [[String]]
    helper [] [] res = res
    helper [] cur res = res ++ [cur]
    helper (x:xs) cur res = if x == c then helper xs [] (res ++ [cur]) else helper xs (cur ++ [x]) res

parseNode :: String -> Node
parseNode (a1:a2:a3:' ':'=':' ':'(':b1:b2:b3:',':' ':c1:c2:c3:')':[]) = ([a1,a2,a3], [b1,b2,b3], [c1,c2,c3])

findNode :: [Node] -> Label -> Node
findNode [] _ = error "Node not found"
findNode (x:xs) l = if l == (fst3 x) then x else findNode xs l

selectLabel :: Node -> Move -> Label
selectLabel (a, b, c)  'R' = c
selectLabel (a, b, c)  'L' = b
selectLabel _ _  = error "Wrong move"

makeMove :: [Node] -> Label -> Move -> Label
makeMove tree l m = selectLabel (findNode tree l) m

generalMove :: ([Node], Label, [Move]) -> ([Node], Label, [Move])
generalMove (tree, l, []) = (tree, l, [])
generalMove (tree, l, (m:ms)) = (tree, newLabel, ms) where
    newLabel = makeMove tree l m

generalMoveWCounter :: ([Node], Label, [Move], Int) -> ([Node], Label, [Move], Int)
generalMoveWCounter (tree, l, ms, c) = (ntree, nl, nms, c+1) where
     (ntree, nl, nms) = generalMove (tree, l, ms)

--until :: (a -> Bool) -> (a -> a) -> a -> a
visitAll :: [Node] -> Label -> [Move2] -> [(Label, Move2)] -> [(Label, Move2)]
visitAll tree label [] visited = visited
visitAll tree label ((m,n):ms) visited = if elem (label, (m,n)) visited then visited else visitAll tree newLabel ms ((label, (m,n)) : visited) where
    node = findNode tree label
    newLabel = makeMove tree label m