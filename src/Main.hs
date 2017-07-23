{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import Data.Proxy
import Data.Random
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta

import Model.Types
import Model.Internal
import Model

import System.Random

type MyModel
  = "bias" :=: Double
  :|: "bias" :=: Double |-> "coin" :=: Bool
  :|: "bias" :=: Double |-> "numHeads" :=: Int

type DupModel
  = "coin" :=: Double
  :|: "undecl" :=: Double |-> "bias" :=: Bool

model :: SimulationModel MyModel
model 
  = SomeDist (Beta 2 2)
  :|: (\b -> SomeDist (Bernoulli b))
  :|: (\p -> SomeDist (Binomial 100 p))

p :: Proxy MyModel
p = Proxy

main :: IO ()
main = do
  (res, g') <- simulate p model <$> getStdGen
  print res
