module Main where

import FFT
import Data.List
import Math.Polynomial.Lagrange
import Math.Polynomial.Interpolation

main :: IO ()
main = do
  --inputText <- getLine
  putStrLn "Input data: "
  let x = [1, 4, 5, 2, 4, 6, -6] :: [Float]
  let xy = zip [1 (length x)] x
  --let xy = [(1,1), (2,4), (3,5), (4,2), (5,4), (6,6), (7,-6)] :: [(Float, Float)]
  print xy
--  putStrLn "FFT of data: "
--  print (fft x)
  putStrLn "Lagrangian fit of data: "
  print (lagrangePolyFit xy)
