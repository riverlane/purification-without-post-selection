-- Copyright Riverlane Ltd 2022

module Pathfinding
  ( pathsFromToNaive,
    pathsFromTo,
    shortestRoute,
    countShortestRoutes,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import qualified Data.Set as Set

-- Generic methods for computing paths through a system.
--
-- The type variable `move` represents a name for a move.
--
-- The type variable `state` represents a state of the system. The Ord
-- constraints allow it to be an element of a Set or key of a Map.
--
-- The functions `make`/`undo` of type (move -> state -> state) make a move or
-- undo that move, respectively.
--
-- In our application, the state is a subset of the computational basis for a
-- quantum system, and moves are CNOT and Toffoli gates. Since these gates are
-- self inverse, we always have `make == undo`.

-- How could we get here?
type History state move = Map state [move]

-- Where can we get to from these states using these moves?
grow :: (Ord state) => (move -> state -> state) -> [move] -> [state] -> History state move
grow make moves states =
  Map.unionsWith (++) $
    [ Map.fromList [(make move state, [move]) | state <- states]
      | move <- moves
    ]

-- Unwind history (most recent first) to find how we got here.
pathsHome :: (Ord state) => (move -> state -> state) -> state -> [History state move] -> [[move]]
pathsHome _ _ [] = [[]]
pathsHome undo state (h : hs) =
  concat
    [ map (move :) $ pathsHome undo (undo move state) hs
      | move <- h Map.! state
    ]

-- Brute force search over paths of increasing length until we find the target
-- state.
pathsFromToNaive :: (Ord state) => (move -> state -> state) -> (move -> state -> state) -> [move] -> state -> state -> [[move]]
pathsFromToNaive make undo moves start end = map reverse $ pathsHome undo end histories
  where
    histories =
      reverse $
        takeUntil (Map.member end) $
          iterate (grow make moves . Map.keys) $
            grow make moves [start]
    takeUntil pred (x : xs)
      | pred x = [x]
      | otherwise = x : takeUntil pred xs
    takeUntil _ [] = error "pathsFromToNaive.takeUntil: called on finite list of histories"

-- Roughly square root speedup by working from start and end and meeting in the
-- middle.
pathsFromTo :: (Ord state) => (move -> state -> state) -> (move -> state -> state) -> [move] -> state -> state -> [[move]]
pathsFromTo make undo moves start end
  | start == end = [[]] -- handle paths of length 0 or 1 separately to allow search to get started from each end
  | not $ null oneSteps = oneSteps
  | otherwise = concatMap pathsOutFrom middle
  where
    oneSteps = [[move] | move <- moves, make move start == end]
    (fromStart, fromEnd) = go [grow make moves [start]] [grow undo moves [end]]
    go xs@(x : _) ys@(y : _)
      | not $ Map.disjoint x y = (xs, ys)
      | Map.size x < Map.size y = go (grow make moves (Map.keys x) : xs) ys
      | otherwise = go xs (grow make moves (Map.keys y) : ys)
    go _ _ = error "pathsFromTo.go: not initialised with at least one growth step from each end"
    middle = Map.keys $ Map.intersection (head fromStart) (head fromEnd)
    pathsOutFrom state =
      [ reverse front ++ back
        | front <- pathsHome undo state fromStart,
          back <- pathsHome make state fromEnd
      ]

-- Searching for only the length of the shortest route is not significantly
-- cheaper, except in that it avoids generating a potentially very large number
-- of paths.
shortestRoute :: (Ord state, Num a) => (move -> state -> state) -> (move -> state -> state) -> [move] -> state -> state -> a
shortestRoute make undo moves start end = go 0 fromStart fromEnd
  where
    fromStart = iterate forward $ Set.singleton start
    fromEnd = iterate back $ Set.singleton end
    forward s = Set.unions [Set.map (make move) s | move <- moves]
    back s = Set.unions [Set.map (undo move) s | move <- moves]
    go acc (x : xs) (y : ys)
      | not $ Set.disjoint x y = acc
      | Set.size x < Set.size y = go (acc + 1) xs (y : ys)
      | otherwise = go (acc + 1) (x : xs) ys
    go _ _ _ = error "shortestRoute.go: not called on infinite lists of histories"

-- Intermediate between finding one and enumerating all.
countShortestRoutes :: (Ord state) => (move -> state -> state) -> (move -> state -> state) -> [move] -> state -> state -> Int
countShortestRoutes make undo moves start end = sum [MultiSet.occur s middle1 * MultiSet.occur s middle2 | s <- middle]
  where
    fromStart = iterate forward $ MultiSet.singleton start
    fromEnd = iterate back $ MultiSet.singleton end
    forward s = MultiSet.unions [MultiSet.map (make move) s | move <- moves]
    back s = MultiSet.unions [MultiSet.map (undo move) s | move <- moves]
    (middle1, middle2) = go fromStart fromEnd
    go (x : xs) (y : ys)
      | not $ Set.disjoint xSet ySet = (x, y)
      | Set.size xSet < Set.size ySet = go xs (y : ys)
      | otherwise = go (x : xs) ys
      where
        xSet = MultiSet.toSet x
        ySet = MultiSet.toSet y
    go _ _ = error "countShortestRoutes.go: not called on infinite lists of histories"
    middle = MultiSet.distinctElems $ MultiSet.intersection middle1 middle2