-- Copyright Riverlane Ltd 2022

module Simulate where

-- Compute exact failure rates of purification circuits as polynomial functions
-- of preparation and gate error rates.

import Binomial (maxK)
import Circuits
import Data.Array
import Data.Bits
import Data.List (delete, foldl', nub, sort, subsequences, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Semigroup (Semigroup (stimes))
import Data.Set (Set)
import qualified Data.Set as Set

----------------------- polynomials -----------------------

-- The probability of any basic event is a polynomial in p_0, p_I, p_C, p_T. We
-- use a compressed representation of these polynomials that only records the
-- number of errors of each type that occurred. Together with the total number
-- of possible errors of each type this is enough information to evaluate the
-- implied polynomial at specific values of p_0, p_I, p_C, p_T
--
-- A basic event with given numbers (p,i,c,t) of preparation, idle, CNOT,
-- Toffoli errors has probability
--           p_0^p * (1 - p_0)^(totalP - p)
--   * (p_I / 2)^i * (1 - p_I)^(totalI - i)
--   * (p_C / 4)^c * (1 - p_C)^(totalC - c)
--   * (p_T / 8)^t * (1 - p_T)^(totalT - t)
--
-- =   (p_0 /  (1 - p_0))^p
--   * (p_I / 2(1 - p_I))^i
--   * (p_C / 4(1 - p_C))^c
--   * (p_T / 8(1 - p_T))^t
--   * (1 - p_0)^totalP * (1 - p_I)^totalI * (1 - p_C)^totalC * (1 - p_T)^totalT
--
-- Monoid instance of MultiSet is union, which corresponds to addition of
-- Polynomials.

type Polynomial = MultiSet (Int, Int, Int, Int)

monomial :: (Int, Int, Int, Int) -> Polynomial
monomial = MultiSet.singleton

type Distribution = Array State Polynomial

-- Bump the corresponding error count.
prepError, idleError, cnotError, toffoliError :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
prepError (p, i, c, t) = (p + 1, i, c, t)
idleError (p, i, c, t) = (p, i + 1, c, t)
cnotError (p, i, c, t) = (p, i, c + 1, t)
toffoliError (p, i, c, t) = (p, i, c, t + 1)

-- polynomialPlus :: Polynomial -> Polynomial -> Polynomial
-- polynomialPlus = MultiSet.union

-- polynomialSum :: [Polynomial] -> Polynomial
-- polynomialSum = MultiSet.unions

-- Discard all but the leading order terms from a Polynomial.
leadingOrderOf :: Polynomial -> Polynomial
leadingOrderOf poly = MultiSet.filter (`elem` minimals) poly
  where
    indices = fst <$> MultiSet.toAscOccurList poly -- increasing list of distinct elements
    minimals = go indices -- lexicographic sorting respects <<<< so avoids needing to look forward and back
    go [] = []
    go (x : xs) = x : go [y | y <- xs, not $ y >>>> x]

(<<<<), (>>>>) :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
(<<<<) = flip (>>>>)
(>>>>) (w1, x1, y1, z1) (w2, x2, y2, z2) =
  w1 >= w2 && x1 >= x2 && y1 >= y2 && z1 >= z2

-- MultiSet is a good data structure for Polynomials, gracefully handling
-- missing or additional indices as we increment error counts of any given type.
-- For evaluating Polynomials numerically it is more convenient to use an Array
-- representation.

type NumericalPolynomial = Array (Int, Int, Int, Int) Double

toNumerical :: (Int, Int, Int, Int) -> Polynomial -> NumericalPolynomial
toNumerical totals poly =
  array ((0, 0, 0, 0), totals) [(pict, f pict) | pict <- range ((0, 0, 0, 0), totals)]
  where
    numericalMap = Map.map fromIntegral $ MultiSet.toMap poly
    f pict =
      Data.Maybe.fromMaybe 0 (Map.lookup pict numericalMap)

-- a_0 + a_1x + ... + a_n x^n = a_0 + x(a_1 + x( + ... + a_n))
horner :: Double -> [Double] -> Double
horner x = foldr (\a b -> a + b * x) 0

-- a_0y^n + a_1xy^{n-1} + ... + a_n x^n
-- Use case is x + y = 1 and x small.
horner2 :: Double -> Double -> [Double] -> Double
horner2 _ _ [] = 0
horner2 x 0 as = last as * x ^ (length as - 1)
horner2 x y as = y ^ (length as - 1) * horner (x / y) as

fixP, fixI, fixC, fixT :: NumericalPolynomial -> Double -> NumericalPolynomial
fixP poly p0 =
  array
    ((0, 0, 0, 0), (0, totalI, totalC, totalT))
    [ ((0, i, c, t), horner2 p0 (1 - p0) [poly ! (p, i, c, t) | p <- [0 .. totalP]])
      | i <- [0 .. totalI],
        c <- [0 .. totalC],
        t <- [0 .. totalT]
    ]
  where
    ((0, 0, 0, 0), (totalP, totalI, totalC, totalT)) = bounds poly
fixI poly pI =
  array
    ((0, 0, 0, 0), (totalP, 0, totalC, totalT))
    [ ((p, 0, c, t), horner2 (pI / 2) (1 - pI) [poly ! (p, i, c, t) | i <- [0 .. totalI]])
      | p <- [0 .. totalP],
        c <- [0 .. totalC],
        t <- [0 .. totalT]
    ]
  where
    ((0, 0, 0, 0), (totalP, totalI, totalC, totalT)) = bounds poly
fixC poly pC =
  array
    ((0, 0, 0, 0), (totalP, totalI, 0, totalT))
    [ ((p, i, 0, t), horner2 (pC / 4) (1 - pC) [poly ! (p, i, c, t) | c <- [0 .. totalC]])
      | p <- [0 .. totalP],
        i <- [0 .. totalI],
        t <- [0 .. totalT]
    ]
  where
    ((0, 0, 0, 0), (totalP, totalI, totalC, totalT)) = bounds poly
fixT poly pT =
  array
    ((0, 0, 0, 0), (totalP, totalI, totalC, 0))
    [ ((p, i, c, 0), horner2 (pT / 8) (1 - pT) [poly ! (p, i, c, t) | t <- [0 .. totalT]])
      | p <- [0 .. totalP],
        i <- [0 .. totalI],
        c <- [0 .. totalC]
    ]
  where
    ((0, 0, 0, 0), (totalP, totalI, totalC, totalT)) = bounds poly

--------------------- end polynomials ---------------------

----------------------- simulation ------------------------

simulate :: Maybe (Int, Int, Int, Int) -> [Gate Int] -> Distribution
simulate maxDegrees gates = foldl' (step maxDegrees) (stateAfterPreparation n) gates
  where
    n = length $ qubitsOfGates gates

-- When we apply a gate it can go perfectly or depolarise all the qubits
-- involved. Note that depolarising a set of qubits is equivalent to
-- depolarising them individually one by one.
--
-- maxDegrees is either Nothing (for no maximum) or Just the bounds on the
-- number of failures of each type.
step :: Maybe (Int, Int, Int, Int) -> Distribution -> Gate Int -> Distribution
step maxDegrees distribution gate =
  listArray bs $ zipWith (<>) (elems perfect) (keepLowDegree <$> elems depolarised)
  where
    bs = bounds distribution
    states = indices distribution
    -- If the gate g is perfect, the new probability of state s is the old
    -- probability of g^{-1}(s). For idle, CNOT and Toffoli gates the inverse
    -- and the gate happen to agree.
    perfect = ixmap bs (apply gate) distribution
    -- Depolarise a set of qubits by depolarising each qubit consecutively. For
    -- a t-qubit gate the cost is proportional to t rather than the 2^t of the
    -- natural implementation of depolarising the set of qubits simultaneously.
    -- Increase the count of failures of the appropriate type of gate.
    depolarised =
      MultiSet.map errorType
        <$> foldr (depolarise . bit) distribution (qubitsOfGate gate)
    -- Depolarising a single qubit q averages the probability of every state s
    -- with the probability of s with the qth bit flipped.
    depolarise q d = listArray bs [(d ! s) <> (d ! xor s q) | s <- states]
    errorType = case gate of
      Idle _ -> idleError
      CNOT _ _ -> cnotError
      Toffoli _ _ _ -> toffoliError
      _ -> error "step: only idle, CNOT and Toffoli gates supported"
    keepLowDegree = case maxDegrees of
      Nothing -> id
      Just pict -> MultiSet.filter (<<<< pict)

-- Rather than step through preparation errors we jump straight to the
-- distribution after preparation.
stateAfterPreparation :: Int -> Distribution
stateAfterPreparation n =
  listArray (0, 2 ^ n - 1) [monomial (popCount s, 0, 0, 0) | s <- [0 :: Int .. 2 ^ n - 1]]

---------------------- end simulation ----------------------

-------------------- extract error information --------------------

-- Simulate a circuit, then reduce the output distribution down to a
-- distribution over the number of errors on the output qubits.
weightDistribution :: Maybe (Int, Int, Int, Int) -> Circuit Int -> Map Int Polynomial
weightDistribution maxDegrees circuit =
  Map.fromListWith
    (<>)
    [(popCount (state .&. mask), dist ! state) | state <- [0 .. 2 ^ n - 1]]
  where
    dist = simulate maxDegrees $ gatesOf circuit
    n = length $ qubitsOfCircuit circuit
    mask = foldr xor zeroBits [bit i | i <- outputBits circuit]

-- Simulate a circuit, then reduce the output distribution down to the expected
-- number of errors on the output qubits.  When there is a single output qubit,
-- this is the probability that it experiences an error.
expectedErrors :: Maybe (Int, Int, Int, Int) -> Circuit Int -> Polynomial
expectedErrors maxDegrees circuit = mconcat [stimes w p | (w, p) <- wd]
  where
    wd = Map.toList $ weightDistribution maxDegrees circuit

------------------- end extract error information -------------------

--------------------------- root finding ---------------------------

eps :: Double
eps = 0.000001

-- Approximate the first positive fixed point of a positive continuous function.
-- Fast version is correct when function is increasing.
fixedPoint, fixedPointFast, fixedPointCareful :: Double -> (Double -> Double) -> Double
fixedPoint = fixedPointCareful
fixedPointFast eps f = go 0
  where
    go x =
      let fx = f x
       in if abs (fx - x) < eps then fx else go fx
fixedPointCareful eps f = head [x | x <- [eps, eps + eps ..], f x < x]

-------------------------- end root finding -------------------------

------------------------ begin applications ------------------------

totals :: (Eq a) => Circuit a -> (Int, Int, Int, Int)
totals circuit = foldr f (length $ qubitsOfCircuit circuit, 0, 0, 0) $ gatesOf circuit
  where
    f (Idle _) (p, i, c, t) = (p, i + 1, c, t)
    f (CNOT _ _) (p, i, c, t) = (p, i, c + 1, t)
    f (Toffoli _ _ _) (p, i, c, t) = (p, i, c, t + 1)
    f _ _ = error "totals: only counts Idle, CNOT, Toffoli"

-- Produce data to be output in gnuplot's grid format.
-- e.g. splot "output-file" w dots
threshold :: Circuit Int -> Double -> [Double] -> [Double] -> [(Double, [(Double, Double)])]
threshold circuit@(Circuit out gates) i cs tRatios =
  [ ( c,
      [ (tRatio, fixedPoint eps $ evaluate poly)
        | (tRatio, poly) <- fixedT
      ]
    )
    | (c, fixedT) <- fixedCT
  ]
  where
    k = fromIntegral $ length out
    (totalP, totalI, totalC, totalT) = totals circuit
    maxI = maxK totalI i eps
    maxC = maxK totalC (maximum cs) eps
    maxT = maxK totalT (maximum cs * maximum tRatios) eps
    failPoly =
      (/ k)
        <$> fixI
          ( toNumerical (totalP, totalI, totalC, totalT) $
              expectedErrors (Just (totalP, maxI, maxC, maxT)) circuit
          )
          i
    fixedC = [(c, fixC failPoly c) | c <- cs]
    fixedCT =
      [ ( c,
          [ (tRatio, fixT poly (c * tRatio))
            | tRatio <- tRatios
          ]
        )
        | (c, poly) <- fixedC
      ]
    evaluate poly p = fixP poly p ! (0, 0, 0, 0)

leadingOrder circuit@(Circuit out gates) =
  (k, monomials, if k == 1 then prettyPoly else "(" ++ prettyPoly ++ ")/" ++ show k)
  where
    k = fromIntegral $ length out
    failPoly = expectedErrors Nothing circuit
    -- preparation error terms first
    monomials = reverse $ MultiSet.toOccurList $ leadingOrderOf failPoly
    prettyPoly = tail $ concatMap (('+':) . pretty) monomials
    pretty ((p, i, c, t), a) =
      concat
        [ if a == 1 then "" else show a,
          case p of
            0 -> ""
            1 -> "\\prep"
            p -> "\\prep^" ++ show p,
          case i of
            0 -> ""
            1 -> "(\\idle/2)"
            p -> "(\\idle/2)^" ++ show p,
          case c of
            0 -> ""
            1 -> "(\\cnot/4)"
            p -> "(\\cnot/4)^" ++ show p,
          case t of
            0 -> ""
            1 -> "(\\toffoli/8)"
            p -> "(\\toffoli/8)^" ++ show p
        ]

varyP :: Circuit Int -> Double -> Double -> Double -> [(Double, Double)]
varyP circuit@(Circuit out gates) i c t =
  [(p, evaluate p) | p <- [0, 0.001 .. 1]]
  where
    k = fromIntegral $ length out
    (totalP, totalI, totalC, totalT) = totals circuit
    maxI = maxK totalI i eps
    maxC = maxK totalC c eps
    maxT = maxK totalT t eps
    failPoly =
      (/ k)
        <$> fixT
          ( fixC
              ( fixI
                  ( toNumerical
                      (totalP, totalI, totalC, totalT)
                      $ expectedErrors (Just (totalP, maxI, maxC, maxT)) circuit
                  )
                  i
              )
              c
          )
          t
    evaluate p = fixP failPoly p ! (0, 0, 0, 0)

goodTriples :: Circuit Int -> [Double] -> Double -> [Double] -> [Double] -> [(Double, Double, Double)]
goodTriples circuit@(Circuit out gates) ps i cs tRatios = goodCTP
  where
    k = fromIntegral $ length out
    (totalP, totalI, totalC, totalT) = totals circuit
    maxI = maxK totalI i eps
    maxC = maxK totalC (maximum cs) eps
    maxT = maxK totalT (maximum cs * maximum tRatios) eps
    failPoly =
      (/ k)
        <$> fixI
          ( toNumerical (totalP, totalI, totalC, totalT) $
              expectedErrors (Just (totalP, maxI, maxC, maxT)) circuit
          )
          i
    fixedC = [(c, fixC failPoly c) | c <- cs]
    fixedCT = [(c, tRatio, fixT poly (c * tRatio)) | (c, poly) <- fixedC, tRatio <- tRatios]
    goodCTP = [(c, t, p) | (c, t, poly) <- fixedCT, p <- ps, evaluate poly p < p]
    evaluate poly p = fixP poly p ! (0, 0, 0, 0)
