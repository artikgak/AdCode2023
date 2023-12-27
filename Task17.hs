{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task17 where

import System.IO
import Data.List
import Data.List.Split

import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import qualified Data.Heap as H
import Data.Heap (MinPrioHeap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Array as A
import Data.Ix
import Debug.Trace

-- dijkstras in haskell idea
-- https://mmhaskell.com/blog/2022/8/22/dijkstras-algorithm-in-haskell

data Distance a = Dist a | Infinity deriving (Eq, Show)

data Direction = Undefined | U | D | L | R  deriving (Show, Eq, Ord, Ix, Enum)

instance Hashable Direction where
    hashWithSalt = hashUsing fromEnum

type Node = (Int, Int)
type History = (Int, Direction)

{- required packages:
   containers, unordered-containers, hashable

   :set -package unordered-containers
   :set -package hashable
   :set -package containers

-}

instance (Ord a) => Ord (Distance a) where
    Infinity <= Infinity = True
    Infinity <= Dist _   = False
    Dist _   <= Infinity = True
    Dist x   <= Dist y   = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _        _        = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> (Distance d)
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

data DijkstraState node cost = DijkstraState
    { 
        visitedSet :: HashSet (node, History),
        distanceMap :: HashMap (node, History) (Distance cost),
        nodeQueue :: MinPrioHeap (Distance cost) (node, History)
    }

getNeighbours :: A.Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getNeighbours input (row, col) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight] where
    (maxRow, maxCol) = snd . A.bounds $ input
    maybeUp    = if row > 0      then Just (row - 1, col    ) else Nothing
    maybeDown  = if row < maxRow then Just (row + 1, col    ) else Nothing
    maybeLeft  = if col > 0      then Just (row    , col - 1) else Nothing
    maybeRight = if col < maxCol then Just (row    , col + 1) else Nothing

dijkstraEdges :: Graph2DWithRestictions -> (Node, History) -> [((Node, History), Int)]
dijkstraEdges (Graph2DWithRestictions arr) (cell, hist) = [(n, arr A.! node) | n@(node, _) <- neighbours] where
        neighbours = getNeighboursR arr cell hist


newtype Graph2DWithRestictions = Graph2DWithRestictions (A.Array Node Int) deriving (Show)

canGo :: History -> Direction -> Bool
canGo (n, d1) d2 | d1 == d2  = n  < 10
                 | otherwise = n >= 4

getNextHistory :: History -> Direction -> History
getNextHistory (n, d1) d2 = if d1 == d2 then (n + 1, d1) else (1, d2)

getNeighboursR :: A.Array Node Int -> Node -> History -> [(Node, History)]
getNeighboursR input (row, col) history@(n, dir) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight] where
    (maxRow, maxCol) = snd . A.bounds $ input
    maybeUp    = if row > 0      && canGo history U then Just ((row - 1, col)    , getNextHistory history U) else Nothing
    maybeDown  = if row < maxRow && canGo history D then Just ((row + 1, col)    , getNextHistory history D) else Nothing
    maybeLeft  = if col > 0      && canGo history L then Just ((row    , col - 1), getNextHistory history L) else Nothing
    maybeRight = if col < maxCol && canGo history R then Just ((row    , col + 1), getNextHistory history R) else Nothing

findShortestDistance :: Graph2DWithRestictions -> Node -> (Node, History) -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest where
    ---- initial state ------
    initialVisited = HS.empty
    initialDistances = HM.singleton (src, (99, Undefined)) (Dist 0)
    initialQueue = H.fromList [(Dist 0, (src, (99, Undefined)))]
    initialState = DijkstraState initialVisited initialDistances initialQueue
    ---- Process the queue ------
    processQueue :: DijkstraState Node Int -> HashMap (Node, History) (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
        Nothing -> d0
        Just ((minDist, node@(n, histn)), q1) -> if node == dest then d0 
            else if HS.member node v0  then processQueue (ds { nodeQueue = q1})
            else 
                -- Update the visited set
                let v1 = HS.insert node v0
                -- Get all unvisited neighbours of our current node
                    allNeighbours :: [((Node, History), Int)]
                    allNeighbours = dijkstraEdges graph node
                    unvisitedNeighbours = filter (\(n, _) -> not (HS.member n v1)) allNeighbours
                -- Ford each neighbour and recursively process the queue
                in processQueue $ foldl (foldNeighbour node) (DijkstraState v1 d0 q1) unvisitedNeighbours
        -- Fold a neighbour into the state
    foldNeighbour :: (Node, History) -> DijkstraState Node Int -> ((Node, History), Int) -> DijkstraState Node Int
    foldNeighbour current ds@(DijkstraState v1 d0 q1) (neighbourNode, cost) = 
        let altDistance = addDist (d0 !?? current) (Dist cost)
        in if altDistance < d0 !?? neighbourNode 
           then DijkstraState v1 (HM.insert neighbourNode altDistance d0) (H.insert (altDistance, neighbourNode) q1)
           else ds

main :: IO ()
main = do
    --handle <- openFile "test17.txt" ReadMode
    handle <- openFile "task17.txt" ReadMode
    contents <- hGetContents handle
    let parsed_data = lines contents
        sizeY = length parsed_data
        sizeX = length (head parsed_data)
        intArr = map (\x -> read [x] :: Int) $ concat parsed_data
        graph2D :: Graph2DWithRestictions
        graph2D = (Graph2DWithRestictions $ A.listArray ((0, 0), (sizeY - 1, sizeX - 1)) intArr)

        cost = map (findShortestDistance graph2D (0 :: Int, 0 :: Int)) $ zip (repeat (sizeY - 1, sizeX - 1)) [(n, d) | n <- [1..10], d <- [D, R]]
        res = filter (/=Infinity) cost
    --mapM_ print parsed_data
    --print graph2D 
    print res
    print $ nub $ map (\(Dist n) -> n) $ sort res
    hClose handle

-- then guess by binary search