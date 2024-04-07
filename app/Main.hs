module Main where

import FFT
import Data.List
import RootOfUnity

import Math.Polynomial.Lagrange
import Math.Polynomial.Legendre
import Math.Polynomial.Interpolation

enumerate :: Num a => [a] -> [(a, a)]
enumerate z = zip (map fromIntegral [1..]) z

-- Evaluate all Legendre polynomials up to a certain order at a point
evalNLegendres :: Fractional a => Int -> a -> [a]
evalNLegendres 0 x = [1]
evalNLegendres order x = (evalNLegendres (order-1) x) ++ [evalLegendre order x]

-- Function to generate equally spaced points between two given numbers
--module TestFFT (test_fft) where
equallySpacedPoints :: (Fractional a, Enum a) => a -> a -> Int -> [a]
equallySpacedPoints start end n = [start + (fromIntegral i) * step | i <- [0..n-1]]
  where
    step = (end - start) / fromIntegral (n - 1)

-- Calculate inner product between two vectors
innerProd :: Fractional a => [a] -> [a] -> a
innerProd xs ys = sum (zipWith (*) xs ys)

-- Reconstruct data point from Legendre base
reconstructFromLegendre :: Fractional a => [a] -> a -> a
reconstructFromLegendre base xs = sum ( zipWith (*) base (evalNLegendres order xs))
  where
      order = length base

normalizeList :: Fractional a => [a] -> [a]
normalizeList xs = map (/ total) xs
  where
    total = sum xs


main :: IO ()
main = do
  --inputText <- getLine
  putStrLn "Input data: "
  let y = [1, 4, 5, 2, 4, 6, -6]
  print y

  --let xy = enumerate y
  --print xy

  --putStrLn "FFT of data: "
  --print (fft y)

  --utStrLn "Lagrangian fit of data: "
  --print (lagrangePolyFit xy)

  --putStrLn "Legendre polynomials orthogonal basis: "
  --print (take 3 legendres)

  let x_min = -1 :: Float
  let x_max = 1 :: Float
  let maxLegendreOrder = 6 :: Int
  let x = equallySpacedPoints x_min x_max (length y)
  putStrLn "Equally spaced points x: "
  print x
  putStrLn "Legendre function evaluations at those points: "
  let legEval = transpose (map (evalNLegendres maxLegendreOrder) x)
  print legEval

  putStrLn "Evaluate product of L(n) and y for all datapoints and sum them. This should be <L(n), y>. Do this for all n under max Legendre order: "
  let legendreBase = map (innerProd y) legEval
  print legendreBase

  -- This should be an unnormalized Legendre base polynomial representation of our dataset. Now time to reconstruct:
  -- Divide legendreBase by length n to get normalization factor? L(0) should then give the average at least.
  let y_hat = map (reconstructFromLegendre legendreBase) x
  print y_hat

  putStrLn "Now a normalized y and then y_hat: "
  print (normalizeList y)
  print (normalizeList y_hat)

