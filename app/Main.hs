-- Copyright Riverlane Ltd 2022

module Main where

import Circuits (yquant)
import Data.Maybe (fromJust)
import Examples (circuitList)
import Simulate (leadingOrder, goodTriples, threshold, varyP)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("threshold" : args') -> threshold' args'
    ("goodTriples" : args') -> goodTriples' args'
    ("fixedICT" : args') -> fixedICT args'
    ("leadingOrder" : args') -> leadingOrder' args'
    ("print" : args') -> prettyPrint args'
    _ -> help

help :: IO ()
help = do
  putStrLn "Usage:"
  putStrLn "  stack run"
  putStrLn "    goodTriples circuitName idle"
  putStrLn "      -> c t p for which circuit results in an improvement"
  putStrLn ""
  putStrLn "    threshold circuitName idle"
  putStrLn "      -> gnuplot grid format data for threshold plots"
  putStrLn ""
  putStrLn "    fixedICT circuitName idle cnot toffoli"
  putStrLn "      -> output error rate for each input error rate"
  putStrLn ""
  putStrLn "    leadingOrder circuitName"
  putStrLn "      -> k, leading order monomials of expected number of errors, and LaTeX for the average number of errors per output qubit"
  putStrLn ""
  putStrLn "    print circuitName"
  putStrLn "      -> tikz/yquant format circuit diagram"
  putStrLn ""
  putStrLn "Circuits:"
  mapM_ ((putStrLn . ("  " ++)) . fst) circuitList

goodTriples' :: [String] -> IO ()
goodTriples' (circuitName : idleArg : _) =
  mapM_
    putStrLn
    [ unwords $ map show [c, t, p]
      | (c, t, p) <-
          goodTriples circuit ps idle cs tRatios
    ]
  where
    idle = read idleArg
    circuit = fromJust $ lookup circuitName circuitList
    cs = [0.0001 * i | i <- [0 .. 500]]
    tRatios = [i / 100.0 | i <- [100 .. 300]]
    ps = [0.0001 * i | i <- [0 .. 5000]]
goodTriples' _ = help

threshold' :: [String] -> IO ()
threshold' (circuitName : idleArg : _) = gridOutput $ threshold circuit idle cs tRatios
  where
    idle = read idleArg
    circuit = fromJust $ lookup circuitName circuitList
    cs = [0.0001 * i | i <- [0 .. 500]]
    tRatios = [i / 100.0 | i <- [100 .. 300]]
    gridOutput result =
      mapM_
        putStrLn
        [ unlines
            [unwords $ map show [c, tRatio, p] | (tRatio, p) <- fixedT]
          | (c, fixedT) <- result
        ]
threshold' _ = help

fixedICT :: [String] -> IO ()
fixedICT (circuitName : idleArg : cArg : tArg : _) =
  mapM_ outputPairs $
    varyP (fromJust $ lookup circuitName circuitList) idle c t
  where
    idle = read idleArg
    c = read cArg
    t = read tArg
    circuit = fromJust $ lookup circuitName circuitList
    outputPairs (x, y) = putStrLn $ unwords $ map show [x, y]
fixedICT _ = help

leadingOrder' :: [String] -> IO ()
leadingOrder' (circuitName : _) = do
  print k
  print ms
  putStrLn tex
  where
    circuit = fromJust $ lookup circuitName circuitList
    (k,ms,tex) = leadingOrder circuit
leadingOrder' _ = help

prettyPrint :: [String] -> IO ()
prettyPrint (circuitName : _) =
  putStr $ yquant $ fromJust $ lookup circuitName circuitList
prettyPrint _ = help