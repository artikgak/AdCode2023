module Task12 where

import System.IO
import Data.List
import Data.List.Split
import Data.HashMap.Strict (HashMap, empty, insert, lookup, member, (!))
import Data.Maybe (fromJust)

-- :set -package unordered-containers

type SpringData = (String, [Int])

main :: IO ()
main = do
    --handle <- openFile "test12.txt" ReadMode
    handle <- openFile "task12.txt" ReadMode
    contents <- hGetContents handle
    let grid = lines contents
        parsed_data = map parseSpringData grid

        parsed_datax5 = map (\(str, nums) -> (tail $ concat $ replicate 5 ('?':str), concat $ replicate 5 nums)) parsed_data

        allSubs = map (\(sp, nms) -> (generateAllSubstitutions sp nms, nms)) parsed_data
        satisySubs = map (\(subs, nums) -> (filter (\x -> satisfy x nums) subs, nums)) allSubs
        satisfyCount = map (\(subs, nums) -> length subs) satisySubs

        newWay = map (snd . countSubstitutions) parsed_data
        newWayx5 = map (snd . countSubstitutions) parsed_datax5

    --print $ parsed_data
    --print $ allSubs
    --print $ "\n\nsatisySubs:" ++ show satisySubs
    --print $ "parsed_data:" ++ show parsed_data
    --print $ "parsed_datax5:" ++ show parsed_datax5

    --print $ "satisfyCount:" ++ show satisfyCount
    --print $ "sum satisfyCount:" ++ show (sum satisfyCount)

    print $ "newWay:" ++ show newWay
    print $ "newWayx5:" ++ show newWayx5
    print $ "sum newWay:" ++ show (sum newWay)
    print $ "sum newWayx5:" ++ show (sum newWayx5)

    hClose handle

parseSpringData :: String -> SpringData
parseSpringData str = (shortenMap $ head splitted, parsed_data) where
    splitted = splitOn " " str
    splitted_numbs = splitOn "," (last splitted)
    parsed_data = map (\x -> read x :: Int) splitted_numbs

generateAllSubstitutions :: String -> [Int] -> [String]
generateAllSubstitutions str nums = untilStable (concatMap (\x -> subWithFilter x nums)) [str]

subWithFilter :: String -> [Int] -> [String]
subWithFilter str nums = filter (\x -> canSatisfy x nums) $ substituteFirst str nums

substituteFirst :: String -> [Int] -> [String]
substituteFirst "" nums = [""]
substituteFirst (s:str) nums | s == '?' = ['.' : str, '#' : str]
                             | otherwise = map (s:) $ substituteFirst str nums

fromArrToPair :: [a] -> (a,a)
fromArrToPair [a,b] = (a,b)
fromArrToPair _     = error "fromArrToPair: array length is not 2"

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a = until (\w -> f w == w) f a

getStringNums :: String -> [Int]
getStringNums str = reverse $ helper str 0 [] where
    helper "" 0 acc = acc
    helper "" n acc = n:acc
    helper ('.':str) 0 acc = helper str 0 acc
    helper ('.':str) n acc = helper str 0 (n:acc)
    helper ('#':str) n acc = helper str (n+1) acc
    helper ('?':str) 0 acc = acc
    helper ('?':str) n acc = n:acc

getStringNumsStrict :: String -> [Int]
getStringNumsStrict str = reverse $ helper str 0 [] where
    helper "" 0 acc = acc
    helper "" n acc = n:acc
    helper ('.':str) 0 acc = helper str 0 acc
    helper ('.':str) n acc = helper str 0 (n:acc)
    helper ('#':str) n acc = helper str (n+1) acc
    helper ('?':str) _ acc = acc

satisfy :: String -> [Int] -> Bool
satisfy str nums = (getStringNums str) == nums

canSatisfy :: String -> [Int] -> Bool
canSatisfy str nums = helper (getStringNums str) nums where
    helper [] [] = True
    helper [] _  = ('?' `elem` str)
    helper _  [] = False
    helper [x] (y:ys) = (x==y || (('?' `elem` str) && x < y)) && helper [] ys
    helper (x:xs) (y:ys) | x == y = helper xs ys
                         | otherwise = False

replicate1time :: String -> String -> [Int] -> Int
replicate1time input foundSub nums  | (head foundSub == '#') && (last foundSub == '#') = 1
                                    | (head foundSub == '#') && (last foundSub == '.') = if ((input !! (head nums - 1)) == '?') then 2 else 1
                                    | (head foundSub == '.') && (last foundSub == '#') = if ((input !! ((length input) - (last nums))) == '?') then 2 else 1
                                    | (head foundSub == '.') && (last foundSub == '.') = 1 + (length $ takeWhile (==1) nums) + (length $ takeWhile (==1) (reverse nums))

replicate1timeSubs :: String -> [String] -> [Int] -> [Int]
replicate1timeSubs input foundSubs nums = map (\x -> replicate1time input x nums) foundSubs

shortenMap :: String -> String
shortenMap str = reverse $ helper str "" where
    helper []        res = res
    helper ('.':str) []  = helper str "."
    helper ('.':str) ('.':res)  = helper str ('.':res)
    helper ('.':str) res  = helper str ('.':res)
    helper (s:str)   res = helper str (s:res)


countSubstitutions :: (String, [Int]) -> (HashMap (String, [Int]) Int, Int)
countSubstitutions (str, nums) = (cache, res) where
    (cache, res) = cacheCountSubs (empty :: HashMap (String, [Int]) Int) (str, nums)

cacheCountSubs :: HashMap (String, [Int]) Int -> (String, [Int]) -> (HashMap (String, [Int]) Int, Int)
cacheCountSubs cache (str, nums) | member (str, nums) cache  = (cache, fromJust $ Data.HashMap.Strict.lookup (str, nums) cache)
                                 | not $ canSatisfy str nums = (Data.HashMap.Strict.insert (str, nums) 0 cache, 0)
                                 | '?' `notElem` str         = if satisfy str nums then (Data.HashMap.Strict.insert (str, nums) 1 cache, 1) else (Data.HashMap.Strict.insert (str, nums) 0 cache, 0)
                                 | otherwise = let  (nstr1, nstr2) = fromArrToPair $ map (dropWhile (=='.')) $ substituteFirst str nums
                                                    npair1 = removePrefixNumbers (nstr1, nums)
                                                    npair2 = removePrefixNumbers (nstr2, nums)

                                                    (cache1, r1) = cacheCountSubs cache npair1
                                                    (cache2, r2) = cacheCountSubs cache1 npair2
                                                    res = r1 + r2
                                                    in
                                                        (Data.HashMap.Strict.insert (str, nums) res cache2, res)


removePrefixNumbers :: (String, [Int]) ->(String, [Int])
removePrefixNumbers (str, nums) = (nstr, nnums) where
    newNums = getStringNumsStrict str
    (nstr, nnums) = iterate tryRemove1PrefixNumber (dropWhile (=='.') str, nums) !! (length newNums)

tryRemove1PrefixNumber :: (String, [Int]) -> (String, [Int])
tryRemove1PrefixNumber (str, []) = (str, [])
tryRemove1PrefixNumber (str, n:nums) = if isPrefixOf ((replicate n '#') ++ ".") str then (drop n str, nums) else (str, n:nums)

--[
-- ("???.###",[1,1,3]),
-- (".??.??.?##.",[1,1,3]),
-- ("?#?#?#?#?#?#?#?",[1,3,1,6]),
-- ("????.#.#.",[4,1,1]),
-- ("????.######.#####.",[1,6,5]),
-- ("?###????????",[3,2,1])
--]