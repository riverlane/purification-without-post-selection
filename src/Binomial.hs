-- Copyright Riverlane Ltd 2022

module Binomial where

pascalsTriangle :: [[Double]]
pascalsTriangle = iterate f [1]
  where
    f row = zipWith (+) (0 : row) (row ++ [0])

choose :: Int -> Int -> Double
choose n k = pascalsTriangle !! n !! k

binomial :: Int -> Int -> Double -> Double
binomial n k p = choose n k * p ^ k * (1 - p) ^ (n - k)

-- How large should we allow k to get if we want to ignore an upper tail of
-- weight eps?
maxK :: Int -> Double -> Double -> Int
maxK n p eps = length (takeWhile (> eps) (scanr (+) 0 [binomial n k p | k <- [0 .. n]])) - 1