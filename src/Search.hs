-- Copyright Riverlane Ltd 2022

module Search where

-- Find circuits mapping one set of states to another set of states of the same
-- size. This covers many parameter sets that we care about, e.g. (2e+1,1,e) and
-- (7,4,1).
--
-- Let x be the number of vectors of weight at most e.  If x < 2^(n-k) then the
-- functions in this module cannot be used directly. The simplest fix would be
-- to replace the single target `subspace (n-k)` by a set of targets, one for
-- each subset of `subspace (n-k)` of size x.

import Circuits (Gate (CNOT, Toffoli), State, apply)
import Data.Array.Unboxed (Array, array, assocs)
import Data.Bits (Bits (bit, popCount, testBit, xor))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import qualified Data.Set as Set
import Pathfinding
  ( countShortestRoutes,
    pathsFromTo,
    shortestRoute,
  )

type StateSet = Integer

type GateMatrix = Array State StateSet

toSingleton :: State -> StateSet
toSingleton = bit

-- permutation matrix representation of gate on n qubits
matrixOf :: Int -> Gate Int -> GateMatrix
matrixOf n gate =
  array
    (0, 2 ^ n - 1)
    [(state, toSingleton $ apply gate state) | state <- [0 .. 2 ^ n - 1]]

-- matrix stored as array of columns
matvec :: GateMatrix -> StateSet -> StateSet
matvec matrix vector = foldr xor 0 [col | (i, col) <- assocs matrix, vector `testBit` i]

ball :: Int -> Int -> StateSet
ball n k = foldr xor 0 [toSingleton x | x <- [0 .. 2 ^ n - 1], popCount x <= k]

-- dimension k
subspace :: Int -> StateSet
subspace k = foldr (xor . toSingleton) 0 [0 .. 2 ^ k - 1]

shortestCircuit :: Num a => Int -> StateSet -> StateSet -> a
shortestCircuit n = shortestRoute makeMove makeMove gates
  where
    gates = gateList n
    matrices = Map.fromList [(gate, matrixOf n gate) | gate <- gates]
    makeMove gate state = matvec (matrices Map.! gate) state

countShortestCircuits :: Int -> StateSet -> StateSet -> Int
countShortestCircuits n = countShortestRoutes makeMove makeMove gates
  where
    gates = gateList n
    matrices = Map.fromList [(gate, matrixOf n gate) | gate <- gates]
    makeMove gate state = matvec (matrices Map.! gate) state

findCircuits :: Int -> StateSet -> StateSet -> [[Gate Int]]
findCircuits n = pathsFromTo makeMove makeMove gates
  where
    gates = gateList n
    matrices = Map.fromList [(gate, matrixOf n gate) | gate <- gates]
    makeMove gate state = matvec (matrices Map.! gate) state

gateList :: Int -> [Gate Int]
gateList n = cnots ++ toffolis
  where
    qubits = [0 .. n - 1]
    cnots =
      [ CNOT i j
        | i <- qubits,
          j <- qubits,
          i /= j
      ]
    toffolis =
      [ Toffoli i j k
        | i <- qubits,
          j <- qubits,
          k <- qubits,
          i < j && j /= k && i /= k
      ]