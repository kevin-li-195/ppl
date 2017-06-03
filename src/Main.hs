{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import Data.Stochastic
import Data.Proxy

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
  = beta 2 2
  :|: bernoulli
  :|: \d -> bernoulli 0.5 >>= \b -> if b then pure 3 else pure 5

p :: Proxy MyModel
p = Proxy

main :: IO ()
main = do
  (res, g') <- simulate p model <$> getStdGen
  print res
