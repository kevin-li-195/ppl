{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

{-|
  This module implements PDF instances
  that are missing from random-fu.
  We also include functions to conveniently
  construct values of type SomeDist.
 -}

module Model.PDF where

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Bernoulli hiding ( bernoulli )
import Data.Random.Distribution.Beta hiding ( beta )
import Data.Random.Distribution.Binomial hiding ( binomial )
import Data.Random.Distribution.Categorical hiding ( categorical )
import Data.Random.Distribution.Exponential hiding ( exponential )
import Data.Random.Distribution.Gamma hiding ( gamma )
import Data.Random.Distribution.Normal hiding ( normal )
import Data.Random.Distribution.Poisson hiding ( poisson )
import Data.Random.Distribution.T hiding ( t )
import Data.Random.Distribution.Uniform hiding ( uniform )

import qualified Data.Vector as V

import Model.Types

import Numeric.SpecFunctions

bernoulli :: Double -> SomeDist Bool
bernoulli = SomeDist . Bernoulli

beta :: Double -> Double -> SomeDist Double
beta a b = SomeDist $ Beta a b

binomial :: Int -> Double -> SomeDist Int
binomial n p = SomeDist $ Binomial n p

normal :: Double -> Double -> SomeDist Double
normal m v = SomeDist $ Normal m v

categorical :: Eq a => [(Double, a)] -> SomeDist a
categorical = SomeDist . fromList

exponential :: Double -> SomeDist Double
exponential = SomeDist . Exp

gamma :: Double -> Double -> SomeDist Double
gamma a b = SomeDist $ Gamma a b

poisson :: Double -> SomeDist Int
poisson = SomeDist . Poisson

uniform :: Double -> Double -> SomeDist Double
uniform l u = SomeDist $ Uniform l u

t :: Integer -> SomeDist Double
t = SomeDist . T

instance PDF (Bernoulli Double) Bool where
  pdf (Bernoulli p) bool = if bool then p else 1 - p
  logPdf d = log . pdf d

instance Eq a => PDF (Categorical Double) a where
  pdf d val = scan l val / (sum $ fst <$> l)
    where scan [] x = 0
          scan ((p, x) : xs) v = if x == v then p else scan xs v
          l = toList d
  logPdf d = log . pdf d

instance PDF Exponential Double where
  pdf (Exp d) x = d * exp(-d * x)
  logPdf d = log . pdf d

-- | TODO: Test which parameterization this is.
-- For now, hypothesize that it is the shape-rate parameterization
-- parameterization, where the expectation is (a/b)
-- and the pdf is $ \frac{\beta^\alpha}{\Gamma(\alpha)}  x^{\alpha-1} e^{-\beta x} $.
instance PDF Gamma Double where
  pdf d = exp . logPdf d
  logPdf (Gamma !a !b) !x = a * log b - logGamma a + (a - 1) * log x - b * x

instance PDF (Poisson Double) Int where
  pdf d = exp . logPdf d
  logPdf (Poisson !l) !x = -l + (fromIntegral x * log l) - log (fromIntegral (fact x))

-- | Factorial
fact :: Int -> Int
fact 0 = 1
fact !n = if n < 0 then error "Factorial called on negative" else fact' n 1
  where fact' 1 !m = m
        fact' !m' !m = fact' (m'-1) (m * m')

instance PDF Uniform Double where
  pdf (Uniform !l !u) !x = if x >= l && x <= u then (1 / (u - l)) else 0
  logPdf d = log . pdf d

instance PDF T Double where
  pdf d = exp . logPdf d
  logPdf (T !nu) !x = logGamma ((nud + 1) / 2) - (1/2) * log (nud * pi) - logGamma (nud / 2) + ((-nud-1) / 2) * logGamma (1 + ((x ^ 2) / nud))
    where nud = fromIntegral nu
