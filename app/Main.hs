module Main where

import Data.List
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))

-- import RootOfUnity
-- import FFT

-- import Math.Polynomial.Legendre

enumerate :: (Num a) => [a] -> [(a, a)]
enumerate z = zip (map fromIntegral [1 ..]) z

-- Evaluate Legendre by Bonnet's method
-- n * P_{n}(x) = (2n-1) x P_{n-1}(x) - (n-1) P_{n-2}(x)
evalLegendre :: (Fractional a) => Int -> a -> a
evalLegendre 0 _ = 1
evalLegendre 1 x = x
evalLegendre n x = ((2 * nf - 1) * (evalLegendre (n - 1) x) - (nf - 1) * (evalLegendre (n - 2) x)) / nf
 where
  nf = fromIntegral n

-- Evaluate all Legendre polynomials up to a certain order at a point
evalNLegendres :: (Fractional a) => Int -> a -> [a]
evalNLegendres 0 _ = [1]
evalNLegendres order x = (evalNLegendres (order - 1) x) ++ [evalLegendre order x]

-- Function to generate equally spaced points between two given numbers
equallySpacedPoints :: (Fractional a, Enum a) => a -> a -> Int -> [a]
equallySpacedPoints start end n = [start + (fromIntegral i) * step | i <- [0 .. n - 1]]
 where
  step = (end - start) / fromIntegral (n - 1)

-- approximate the inner product between two 1-D functions (R -> R) numerically
-- Takes sampled points, calculate the average value of f(x)*g(x) and multiply by the support size [-1,1]=2 in this case
fnInnerProd :: (Fractional a) => [a] -> [a] -> a
fnInnerProd xs ys = ((sum (zipWith (*) xs ys)) / (fromIntegral (length xs))) * 2

-- Reconstruct data point from Legendre base
reconstructFromLegendre :: (Fractional a) => [a] -> a -> a
reconstructFromLegendre base xs = sum (zipWith (*) base (evalNLegendres order xs))
 where
  order = length base

-- Make sure the sum of all elements of a list equals 1
normalizeList :: (Fractional a) => [a] -> [a]
normalizeList xs = map (/ total) xs
 where
  total = sum xs

-- Mean Square error given two lists of y_true and y_hat
mse :: (Fractional a) => [a] -> [a] -> a
mse y_true y_hat = sum (map (\c -> c ^^ 2) (zipWith (-) y_true y_hat))

listToCSV :: (Fractional a) => [a] -> [Char]
listToCSV xs = foldr (\x y -> (show x) ++ ", " ++ (show y)) (head xs) (tail xs)

-- listToCSV xs = (show (head xs)) ++ (map (\x -> ", " ++ show \x) (tail xs))

main :: IO ()
main = do
  -- Read input data from file
  -- inputData <- readFile "inputData.csv"

  -- inputText <- getLine
  putStrLn "Input data: "
  let y = [1, 4, 5, 2, 4, 6, -6, -8, -9, -10, -6, -3, -2, -1, 1, 5, 18, 20, 14, 20]
  print y

  -- putStrLn "FFT of data: "
  -- print (fft y)

  let x_min = -1 :: Float
  let x_max = 1 :: Float
  let maxLegendreOrder = 20 :: Int -- Legendre order starts from 0, so number of terms is max order + 1
  let x = equallySpacedPoints x_min x_max (length y)
  putStrLn "Equally spaced points x: "
  print x
  putStrLn "Legendre function evaluations at those points: "
  let legEval = transpose (map (evalNLegendres maxLegendreOrder) x)
  print legEval

  -- Evaluate product of L(n) and y for all datapoints and sum them. Divide by the # of points to average it.
  -- This should be <L(n), y>. Do this for all n under max Legendre order: "
  putStrLn "Calculate <L(n), y>"
  let legBaseNormFactor = [((2 * (fromIntegral i)) + 1) / 2 | i <- [0 .. maxLegendreOrder]]
  putStrLn "Legendre Base normalization factors: "
  print legBaseNormFactor
  let legendreBase = zipWith (*) (map (fnInnerProd y) legEval) legBaseNormFactor
  putStrLn "Legendre Base: "
  print legendreBase

  -- This should be a Legendre base polynomial representation of our dataset. Now time to reconstruct:
  let y_hat = map (reconstructFromLegendre legendreBase) x
  putStrLn "y_hat: "
  print y_hat

  -- Calculate MSE:
  putStrLn "MSE: "
  print (mse y y_hat)

  -- Write resulting data to csv file:
  outputData <- listToCSV y_hat
  writeFile "outputData.csv" outputData
