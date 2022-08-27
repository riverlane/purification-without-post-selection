-- Copyright Riverlane Ltd 2022

module Circuits where

import Data.Bits (Bits (bit, testBit, xor))
import Data.List (delete, nub, sort, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- A bitstring representing a computational basis state. Int so that a StateSet
-- in Search can be an Integer whose bits are indexed by States.
type State = Int

-- controls before targets
data Gate a = Idle a | X a | CNOT a a | Toffoli a a a
  deriving (Show, Read, Eq, Ord)

-- relabel qubits
instance Functor Gate where
  fmap f (Idle i) = Idle (f i)
  fmap f (X i) = X (f i)
  fmap f (CNOT i j) = CNOT (f i) (f j)
  fmap f (Toffoli i j k) = Toffoli (f i) (f j) (f k)

apply :: Gate Int -> State -> State
apply (Idle _) b = b
apply (X i) b = b `xor` bit i
apply (CNOT i j) b
  | b `testBit` i = b `xor` bit j
  | otherwise = b
apply (Toffoli i j k) b
  | b `testBit` i && b `testBit` j = b `xor` bit k
  | otherwise = b

-- Full set of wires is implicit.
data Circuit a = Circuit {outputBits :: [a], gatesOf :: [Gate a]}
  deriving (Show, Eq)

-- relabel qubits
instance Functor Circuit where
  fmap f (Circuit out gates) = Circuit (map f out) (map (f <$>) gates)

-- There's no check that this makes sense in any given situation, so this is
-- for convenience only.
instance Semigroup (Circuit a) where
  (<>) (Circuit o1 g1) (Circuit o2 g2) = Circuit (o1 ++ o2) (g1 ++ g2)

instance Monoid (Circuit a) where
  mempty = Circuit [] []

data ScheduledCircuit a = ScheduledCircuit [a] [[Gate a]] deriving (Show, Eq)

flatten :: ScheduledCircuit a -> Circuit a
flatten (ScheduledCircuit out gss) = Circuit out $ concat gss

qubitsOfGate :: (Eq a) => Gate a -> [a]
qubitsOfGate (X i) = [i]
qubitsOfGate (Idle i) = [i]
qubitsOfGate (CNOT i j) = [i, j]
qubitsOfGate (Toffoli i j k) = [i, j, k]

-- Nothing here needs to be fast, so nub is fine.
qubitsOfGates :: (Eq a) => [Gate a] -> [a]
qubitsOfGates gates = nub $ concatMap qubitsOfGate gates

qubitsOfCircuit :: (Eq a) => Circuit a -> [a]
qubitsOfCircuit circuit =
  nub $ qubitsOfGates (gatesOf circuit) ++ outputBits circuit

qubitsOfScheduledCircuit :: (Eq a) => ScheduledCircuit a -> [a]
qubitsOfScheduledCircuit = qubitsOfCircuit . flatten

-- Relabel qubits 0, 1, 2, ...
normalise :: (Ord a, Num b, Enum b) => Circuit a -> Circuit b
normalise circuit = fmap (m Map.!) circuit
  where
    m = Map.fromList $ zip (sort $ qubitsOfCircuit circuit) [0 ..]

-- Pretty print
-- https://github.com/projekter/yquant
-- Wires should be [0 .. n - 1]
yquant :: (Integral a, Show a) => Circuit a -> String
yquant circuit =
  header ++ unlines (filter (/= "") $ map f $ gatesOf circuit) ++ footer
  where
    n = length $ qubitsOfCircuit circuit
    header = "\\begin{tikzpicture}\n  \\begin{yquant}\n    qubit {$q_{\\idx}$} q[" ++ show n ++ "];\n"
    footer = "  \\end{yquant}\n\\end{tikzpicture}\n"
    f (Toffoli i j k) = "    cnot q[" ++ show k ++ "] | q[" ++ show i ++ "], q[" ++ show j ++ "];"
    f (CNOT i j) = "    cnot q[" ++ show j ++ "] | q[" ++ show i ++ "];"
    f (X i) = "    x q[" ++ show i ++ "];"
    f (Idle _) = ""

-- Replace the inputs of one circuit by the outputs of a list of circuits.
compose :: [Circuit Int] -> Circuit Int -> Circuit Int
compose firstLayer secondLayer
  | length outs1 >= length (qubitsOfCircuit secondLayer) =
    Circuit out2 $ gates1 ++ gates2
  | otherwise =
    error "compose: number of outputs of the first layer must be at the least the number of inputs of the second"
  where
    firstLayer' = map normalise firstLayer -- wires [0..n_i-1]
    ns = map (length . qubitsOfCircuit) firstLayer'
    increments = scanl (+) 0 ns -- shifts required to put first layer on disjoint intervals
    firstLayer'' = [(+ inc) <$> circ | (inc, circ) <- zip increments firstLayer'] -- apply the shifts
    outs1 = concatMap outputBits firstLayer''
    gates1 = concatMap gatesOf firstLayer''
    (Circuit out2 gates2) = (outs1 !!) <$> normalise secondLayer

-- Specify a graph as a list of triples, where (u,e,v) is an edge between u and
-- v labelled e. Labels are used as qubit labels, and there is no check that
-- edge labels are not reused.
graphConstruction :: Eq a => [(a, a, a)] -> Circuit a
graphConstruction edges = Circuit [e | (_, e, _) <- edges] $ cnots ++ toffolis
  where
    cnots = concat [[CNOT e u, CNOT e v] | (u, e, v) <- edges]
    toffolis = [Toffoli u v e | (u, e, v) <- edges]

-- Work backwards from the given wires to find the maximal subcircuit of gates
-- that can causally affect them (when gates are noisy).
lightCone :: Ord a => [a] -> Circuit a -> Circuit a
lightCone wires (Circuit out gates) =
  Circuit wires $
    reverse $
      go (Set.fromList $ qubitsOfGates gates \\ wires) $
        reverse gates
  where
    go _ [] = [] -- no gates left
    go
      s -- Set of irrelevant wires not yet causally influencing the given wire
      (g : gs) -- upcoming gates
        | Set.null s = g : gs -- all remaining gates relevant
        | Set.isSubsetOf qs s = go s gs -- not touching relevant wires
        | otherwise = g : go (s Set.\\ qs) gs -- touching relevant wires, other wires I touched are now relevant
        where
          qs = Set.fromList $ qubitsOfGate g

-- Intended to remove unnecessary Idles from the end of non-output wires.
fullLightCone :: Ord a => Circuit a -> Circuit a
fullLightCone c = lightCone (outputBits c) c

-- Greedily partition circuit into layers of commuting gates, annotate idle
-- wires by Idle gates, then remove Idle gates that don't causally affect the
-- output from the end of wires.
autoIdle :: (Ord a, Eq a) => Circuit a -> Circuit a
autoIdle = fullLightCone . flatten . addIdles . greedyScheduling

addIdles :: Eq a => ScheduledCircuit a -> ScheduledCircuit a
addIdles sc@(ScheduledCircuit out gss) = ScheduledCircuit out $ map fillOut gss
  where
    qs = qubitsOfScheduledCircuit sc
    fillOut gs = gs ++ idles
      where
        qs' = qubitsOfGates gs
        idles = map Idle $ qs \\ qs'

-- Break a circuit into layers of gates that can be performed in parallel by
-- filling layers greedily, commuting gates past each other as necessary.
-- Scanning through the circuit from the beginning, we store which qubits are
-- still available for use as controls or targets. A qubit becomes unavailable
-- for use as a control by being used, or when we skip over a gate for which it
-- is the target. A qubit becomes unavailable for use as a target by being used,
-- or when we skip over a gate for which it is the control. `slice` pulls out
-- one layer of gates, `go` iterates `slice` until all gates are placed into a
-- layer.
--
-- This is rather vertical, but it helps with reading the order of arguments to
-- `slice`.
greedyScheduling :: Eq a => Circuit a -> ScheduledCircuit a
greedyScheduling (Circuit out gates) = ScheduledCircuit out $ go gates
  where
    go [] = []
    go gates = layer : go rest
      where
        qs = qubitsOfGates gates
        (layer, rest) = slice [] [] qs qs gates
    slice used skipped [] _ gs = (used, reverse skipped ++ gs) -- no more available controls
    slice used skipped _ [] gs = (used, reverse skipped ++ gs) -- or targets, return accumulated layer
    slice used skipped _ _ [] = (used, reverse skipped) -- end of the circuit
    slice
      used
      skipped
      legalControls
      legalTargets
      (cnot@(CNOT i j) : gs)
        | i `elem` legalControls
            && j `elem` legalTargets =
          slice
            (cnot : used) -- use it
            skipped
            (legalTargets \\ [i, j])
            (legalControls \\ [i, j])
            gs
        | otherwise =
          slice
            used
            (cnot : skipped) -- don't
            (delete j legalControls) -- it's just been a target
            (delete i legalTargets) -- it's just been a control
            gs
    slice
      used
      skipped
      legalControls
      legalTargets
      (toffoli@(Toffoli i j k) : gs)
        | i `elem` legalControls
            && j `elem` legalControls
            && k `elem` legalTargets =
          slice
            (toffoli : used) -- use it
            skipped
            (legalTargets \\ [i, j, k])
            (legalControls \\ [i, j, k])
            gs
        | otherwise =
          slice
            used
            (toffoli : skipped) -- don't
            (delete k legalControls) -- it's just been a target
            (legalTargets \\ [i, j]) -- they've just been controls
            gs
    slice _ _ _ _ _ = error "greedyScheduling: circuit contained a gate other than CNOT or Toffoli."