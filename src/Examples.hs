-- Copyright Riverlane Ltd 2022

module Examples where

import Circuits

-- These circuits are in "ready to simulate" form.  They have been scheduled,
-- had idle gates added, then flattened and had redundant idle gates (or other
-- gates in the case of the explicit light cones) removed from the end of the
-- circuit.
circuitList :: [(String, Circuit Int)]
circuitList =
  [ ("3-1-1", autoIdle circuit_3_1_1),
    ("5-1-1", autoIdle circuit_5_1_1),
    ("7-1-2", autoIdle circuit_7_1_2),
    ("9-1-3", autoIdle circuit_9_1_3),
    ("5-1-2a", autoIdle $ circuit_5_1_2_a (1, 2) 0 (3, 4)),
    ("5-1-2b", autoIdle circuit_5_1_2_b),
    ("8-2-2", autoIdle $ eightTwoThree (1, 2) 0 (6, 5) 7 (3, 4)),
    ("7-4-1", autoIdle circuit_7_4_1),
    ("large-light-cone", largeLightCone),
    ("small-light-cone", smallLightCone),
    ("large-tolerant-light-cone", largeTolerantLightCone),
    ("small-tolerant-light-cone", smallTolerantLightCone)
  ]

eightTwoThree :: Eq a => (a, a) -> a -> (a, a) -> a -> (a, a) -> Circuit a
eightTwoThree (a, b) e (c, d) f (x, y) =
  extendToFive (c, d) f (x, y) $
    circuit_5_1_2_a (a, b) e (x, y) <> graphConstruction [(c, f, d)]

circuit_3_1_1,
  smallLightCone,
  largeLightCone,
  smallTolerantLightCone,
  largeTolerantLightCone,
  circuit_7_1_2,
  circuit_7_4_1,
  circuit_5_1_1,
  circuit_5_1_2_b,
  circuit_9_1_3,
  trivial ::
    Circuit Int
smallLightCone = normalise $ lightCone [7] $ autoIdle $ graphConstruction $ unriffle $ cycleGraph 20
largeLightCone = normalise $ lightCone [9] $ autoIdle $ graphConstruction $ unriffle $ cycleGraph 20
smallTolerantLightCone = normalise $ lightCone [7] $ autoIdle $ faultTolerantCycle 20
largeTolerantLightCone = normalise $ lightCone [9] $ autoIdle $ faultTolerantCycle 20
circuit_3_1_1 = graphConstruction [(1, 0, 2)]
circuit_5_1_1 = compose [circuit_3_1_1, trivial, trivial] circuit_3_1_1
circuit_7_1_2 = compose [circuit_3_1_1, circuit_3_1_1, trivial] circuit_3_1_1
circuit_9_1_3 = compose [circuit_3_1_1, circuit_3_1_1, circuit_3_1_1] circuit_3_1_1
circuit_7_4_1 = graphConstruction [(3, 0, 6), (4, 1, 6), (5, 2, 6), (4, 3, 5)]
circuit_5_1_2_b = Circuit [0] [Toffoli 1 2 3, CNOT 0 1, CNOT 0 2, Toffoli 2 1 0, CNOT 0 4, Toffoli 3 2 0, Toffoli 1 4 2, Toffoli 0 4 3, Toffoli 3 2 0]
trivial = Circuit [0] []

circuit_5_1_2_a (u, v) e (a, b) = extendToFive (u, v) e (a, b) $ graphConstruction [(u, e, v)]

extendToFive (u, v) e (a, b) (Circuit out gates) =
  Circuit out $ pre ++ gates ++ post
  where
    pre = [Toffoli e u a, Toffoli e v b]
    post = [Toffoli a b e] ++ pre ++ [Toffoli a b e]

cycleGraph :: Integral c => c -> [(c, c, c)]
cycleGraph n =
  [ (2 * i, (2 * i + 1) `mod` (2 * n), (2 * i + 2) `mod` (2 * n))
    | i <- [0 .. n - 1]
  ]

-- n edges
pathGraph :: (Num c, Enum c) => c -> [(c, c, c)]
pathGraph n = [(2 * i, 2 * i + 1, 2 * i + 2) | i <- [0 .. n - 1]]

faultTolerantCycle :: Integral a => a -> Circuit a
faultTolerantCycle n = Circuit out $ cnots ++ extraToffolis ++ toffolis
  where
    Circuit out nftGates = graphConstruction $ unriffle $ cycleGraph n
    cnots = [c | c@(CNOT _ _) <- nftGates]
    toffolis = [t | t@(Toffoli _ _ _) <- nftGates]
    extraToffolis =
      [ Toffoli e ((e + 2) `mod` (2 * n)) ((e + 1) `mod` (2 * n))
        | e <- unriffle [1, 3 .. 2 * n - 1]
      ]

faultTolerantPath :: (Eq a, Num a, Enum a) => a -> Circuit a
faultTolerantPath n = Circuit out $ cnots ++ extraToffolis ++ toffolis
  where
    Circuit out nftGates = graphConstruction $ pathGraph n
    cnots = [c | c@(CNOT _ _) <- nftGates]
    toffolis = [t | t@(Toffoli _ _ _) <- nftGates]
    extraToffolis = [Toffoli e (e + 2) (e + 1) | e <- [1, 3 .. 2 * n - 3]]

-- [a1,b1,a2,b2,...] -> as ++ bs (odd total length) or bs ++ as (even total length)
unriffle :: [a] -> [a]
unriffle zs = go [] [] zs
  where
    go xs ys [] = reverse xs ++ reverse ys
    go xs ys (z : zs) = go (z : ys) xs zs