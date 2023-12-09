module Task7 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Function (on)

type Card = Int
type Bid = Int
type Hand = [Card]

type Play = (Hand, Bid)

main :: IO ()
main = do
    --handle <- openFile "test7.txt" ReadMode
    handle <- openFile "task7.txt" ReadMode
    contents <- hGetContents handle
    let data1 = lines contents
        play = map preproccessPlay data1
        play2 = map (\(h,b) -> (map (\x -> if x == 11 then 1 else x) h, b)) play

        withranks = map (\x -> (fst x, getHandRank (fst x), snd x)) play
        withranks2 = map (\x -> (fst x, getHandRank $ prepareHandRank (fst x), snd x)) play2

        bidsSorted = map trd (sortBy mySort withranks)
        bidsSorted2 = map trd (sortBy mySort withranks2)
        withMult = zipWith (*) bidsSorted [1..]
        withMult2 = zipWith (*) bidsSorted2 [1..]

    --print $ "Play: " ++ (show play)
    --print $ "play2: " ++ (show play2)
    --print $ "withranks: " ++ (show withranks)
    --print $ "withRanks2: " ++ (show withranks2)
    --print $ "bidsSorted: " ++ (show bidsSorted)
    --print $ "bidsSorted2: " ++ (show bidsSorted2)
    --print $ "withMult: " ++ (show withMult)
    --print $ "withMult2: " ++ (show withMult2)
    print $ "Sum: " ++ (show $ sum withMult)
    print $ "Sum2: " ++ (show $ sum withMult2)
    hClose handle

trd :: (a, b, c) -> c
trd (_, _, c) = c

preproccessPlay :: String -> Play
preproccessPlay str = (hand, bid) where
    a = splitOn " " str
    bid = read (a!!1) :: Int
    hand = stringToHand (a!!0)

stringToHand :: String -> Hand
stringToHand [] = []
stringToHand (s:str) = (mapCard s) : stringToHand str

mapCard :: Char -> Card
mapCard 'A' = 14
mapCard 'K' = 13
mapCard 'Q' = 12
mapCard 'J' = 11
mapCard 'T' = 10
mapCard c = read [c] :: Int

{-
Every hand is exactly one type. From strongest to weakest, they are:
Five of a kind, where all five cards have the same label: AAAAA
Four of a kind, where four cards have the same label and one card has a different label: AA8AA
Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
High card, where all cards' labels are distinct: 23456
-}

prepareHandRank :: Hand -> Hand
prepareHandRank h = if numberOfJockers == 5 then [14, 14, 14, 14, 14] else concat $ ((replicate numberOfJockers (head $ head res)) ++ (head res)) : (tail res) where
    numberOfJockers = length $ filter (== 1) h
    withoutJockers = filter (/= 1) h
    res1 = (group (sort withoutJockers))
    res = reverse $ sortBy myComp2 res1


getHandRank :: Hand -> Int
getHandRank hand
    | isFiveOfAKind hand  = 7
    | isFourOfAKind hand  = 6
    | isFullHouse hand    = 5
    | isThreeOfAKind hand = 4
    | isTwoPair hand      = 3
    | isOnePair hand      = 2
    | otherwise           = 1

countCards :: Hand -> [Int]
countCards hand = reverse $ sort $ map length (group (sort hand))

isFiveOfAKind :: Hand -> Bool
isFiveOfAKind hand = head counts == 5 where
    counts = countCards hand

isFourOfAKind :: Hand -> Bool
isFourOfAKind hand = head counts == 4 where
    counts = countCards hand

isFullHouse :: Hand -> Bool
isFullHouse hand = (counts!!0 == 3) && (counts!!1 == 2) where
    counts = countCards hand

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind hand = head counts == 3 where
    counts = countCards hand

isTwoPair :: Hand -> Bool
isTwoPair hand = (counts!!0 == 2) && (counts!!1 == 2) where
    counts = countCards hand

isOnePair :: Hand -> Bool
isOnePair hand = (counts!!0 == 2) where
    counts = countCards hand

mySort :: (Hand, Int, Bid) -> (Hand, Int, Bid) -> Ordering
mySort (h1, r1, _) (h2, r2, _)
    | r1 >  r2 = GT
    | r1 <  r2 = LT
    | r1 == r2 = compare h1 h2

myComp2 :: [Int] -> [Int] -> Ordering
myComp2 xs ys
    | length xs >  length ys = GT
    | length xs <  length ys = LT
    | length xs == length ys = compare (head xs) (head ys)