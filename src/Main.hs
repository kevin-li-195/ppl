{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import Data.OpenRecords
import Data.Proxy
import Data.Random
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta

import Model
import Model.Condition
import Model.Simulation
import Model.Types
import Model.Internal
import Model.Condition.Types
import Model.Simulation.Types

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
  :|: ToSomeDist (\b -> SomeDist (Bernoulli b))
  :|: ToSomeDist (\p -> SomeDist (Binomial 100 p))

type Obs = '["numHeads"]
type NonObs = Unobserved MyModel Obs

p :: Proxy MyModel
p = Proxy

pConds :: Proxy Obs
pConds = Proxy

pNonObs :: Proxy NonObs
pNonObs = Proxy

prop :: ProposalDist MyModel NonObs
prop = ToSomeDist (\s -> SomeDist (Beta 2 2)) :|: ToSomeDist (\b -> SomeDist (Bernoulli (0.5 :: Double)))

l :: Label "numHeads"
l = Label

-- TODO: Write sugar for literal conditions.
conds :: Sample MyModel
conds = l :<- 25 .| makeRecord p

main :: IO ()
main = do
  print "hi"
  samples <- conditionSim 10 p pConds pNonObs model prop conds <$> getStdGen
  print samples
