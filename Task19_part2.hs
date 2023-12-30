module Task19 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (digitToInt, toUpper)

data Detail = Detail { x :: (Int,Int), m :: (Int,Int), a :: (Int,Int), s :: (Int,Int) } deriving (Show, Eq)
data Comparator = Less | Greater | TT deriving (Show, Eq)

type Label = String
type Selector = Char
type Rule = (Selector, Comparator, Int, Label)
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
        wkfls = map parseWorkflow workflows

        res = goBrrr wkfls [("in", Detail (1,4000) (1,4000) (1,4000) (1,4000))] []
        count = map countDetails res

    --print wkfls
    --mapM_ print res
    print $ sum count
    hClose handle

countDetails :: Detail -> Int
countDetails Detail {x = (x1, x2), m = (m1, m2), a = (a1, a2), s = (s1, s2)} = (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)

parseWorkflow :: String -> Workflow
parseWorkflow str = (head spl, map parseRule splRules) where
    spl = splitOn "{" $ init str
    splRules = splitOn "," $ last spl

parseRule :: String -> Rule
parseRule str | notElem ':' str = ('0', TT, 0, str) 
              | otherwise = (selector, comparator, num, last spl) where
    spl = splitOn ":" str
    cond = head spl
    selector = cond !! 0
    cmp = cond !! 1
    num = read $ drop 2 cond :: Int
    comparator = case cmp of
        '<' -> Less
        '>' -> Greater
    
getSelector :: Selector -> (Detail -> (Int, Int))
getSelector 'x' = x :: (Detail -> (Int, Int))
getSelector 'm' = m :: (Detail -> (Int, Int))
getSelector 'a' = a :: (Detail -> (Int, Int))
getSelector 's' = s :: (Detail -> (Int, Int))

modifyDetail :: Selector -> (Int, Int) -> Detail -> Detail
modifyDetail 'x' coords det = det {x = coords}
modifyDetail 'm' coords det = det {m = coords}
modifyDetail 'a' coords det = det {a = coords}
modifyDetail 's' coords det = det {s = coords}

goBrrr :: [Workflow] -> [(Label, Detail)] -> [Detail] -> [Detail]
goBrrr wkfls [] res = res
goBrrr wkfls process res = goBrrr wkfls nexProcc res' where
    procced = processDetailsFull wkfls process
    res' = (res ++) $ map snd $ filter (\(lab, _) -> lab == "A") procced
    nexProcc = filter (\(lab, _) -> lab /= "A" && lab /= "R") procced

processDetailsFull :: [Workflow] -> [(Label, Detail)] -> [(Label, Detail)]
processDetailsFull wkfls detls = concatMap (processDetailFull wkfls) detls  where

processDetailFull :: [Workflow] -> (Label, Detail) -> [(Label, Detail)]
processDetailFull wkfls (lab, det) = processDetail wk det where
    wk = findWkflByName wkfls lab

findWkflByName :: [Workflow] -> Label -> Workflow
findWkflByName [] _ = error "Workflow not found"
findWkflByName (x@(lab,_):xs) l = if lab == l then x else findWkflByName xs l

processDetail :: Workflow -> Detail -> [(Label, Detail)]
processDetail (_, rules) det = snd $ helper rules ([det],[]) where
    helper :: [Rule] -> ([Detail], [(Label, Detail)]) -> ([Detail], [(Label, Detail)])
    helper rules   ([], trs) = ([], trs)
    helper [(selector, TT, num, label)] (frs, trs) = ([], (zip (repeat label) frs) ++ trs)
    helper (r:rs) (frs, trs) = helper rs (frs', trs' ++ trs) where
        procDet = map (procOneRule r) frs
        frs' = concatMap fst procDet
        trs' = concatMap snd procDet

-- res = (false rule, true rule with next label)
procOneRule :: Rule -> Detail -> ([Detail], [(Label, Detail)])
procOneRule (selector, TT, num, label) det = ([], [(label, det)])
procOneRule rule det = splitbyCondition rule splittedDetails ([],[])  where
    splittedDetails = splitDetail rule det

splitbyCondition :: Rule -> [Detail] -> ([Detail], [(Label, Detail)]) -> ([Detail], [(Label, Detail)])
splitbyCondition rule [] res = res
splitbyCondition rule@(selector, comparator, num, label) (x:xs) (frs, trs) = splitbyCondition rule xs (frs', trs') where
    f = fromRuleToFunction rule
    frs' = if not $ f x then          x:frs else frs
    trs' = if       f x then (label, x):trs else trs

fromRuleToFunction :: Rule -> (Detail -> Bool)
fromRuleToFunction (selector, TT,   num, label) = (\_ -> True)
fromRuleToFunction ('x', Less, num, label)    = (\det -> (snd . x) det < num)
fromRuleToFunction ('x', Greater, num, label) = (\det -> (fst . x) det > num)
fromRuleToFunction ('m', Less, num, label)    = (\det -> (snd . m) det < num)
fromRuleToFunction ('m', Greater, num, label) = (\det -> (fst . m) det > num)
fromRuleToFunction ('a', Less, num, label)    = (\det -> (snd . a) det < num)
fromRuleToFunction ('a', Greater, num, label) = (\det -> (fst . a) det > num)
fromRuleToFunction ('s', Less, num, label)    = (\det -> (snd . s) det < num)
fromRuleToFunction ('s', Greater, num, label) = (\det -> (fst . s) det > num)

splitDetail :: Rule -> Detail -> [Detail]
splitDetail (  _, TT,      num, label) det = [det]
splitDetail (sel, cmp,    num, label) det | x2 < num || x1 > num = [det]
                                          | cmp == Less    = [(modifyDetail sel (x1, num - 1) det), (modifyDetail sel (num,     x2) det)] 
                                          | cmp == Greater = [(modifyDetail sel (x1, num)     det), (modifyDetail sel (num + 1, x2) det)] where
    (x1, x2) = (getSelector sel) det