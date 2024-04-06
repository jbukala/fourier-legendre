{-# LANGUAGE ScopedTypeVariables #-}
module FFT (fft) where

import Data.Complex
import Data.Ratio
import Data.Monoid
import qualified Data.Map as Map
import Data.List
import Data.Bifunctor
import Control.Monad.Trans.Writer
import RootOfUnity

split :: [a] -> ([a], [a])
split = foldr f ([], [])
  where
    f a (r1, r2) = (a : r2, r1)

evalFourier
  :: forall a . RealFloat a
  => [Complex a] -- ^ polynomial coefficients, starting from a_0
  -> [U] -- ^ points at which to evaluate the polynomial
  -> Writer (Sum Int) [Complex a]

evalFourier []  pts = return $ 0 <$ pts
evalFourier [c] pts = return $ c <$ pts
evalFourier coeffs pts = do
  let
    squares = nub $ u_sqr <$> pts -- values of x^2
    (even_coeffs, odd_coeffs) = split coeffs
  even_values <- evalFourier even_coeffs squares
  odd_values <- evalFourier odd_coeffs squares

  let
    -- a mapping from x^2 to (A_e(x^2), A_o(x^2))
    square_map =
      Map.fromList
      . zip squares
      $ zip even_values odd_values

    -- evaluate the polynomial at a single point
    eval1 :: U -> Writer (Sum Int) (Complex a)
    eval1 x = do
      let (ye,yo) = (square_map Map.! u_sqr x)
          r = ye + toComplex x * yo
      tell $ Sum 2 -- this took two arithmetic operations
      return r

  mapM eval1 pts


fft :: RealFloat a => [Complex a] -> ([Complex a], Int)
fft coeffs =
  second getSum
  . runWriter 
  . evalFourier coeffs 
  . map (u_pow w)
  $ [0..n-1]
  where
    n = genericLength coeffs
    w = mkU (-1 % n)

