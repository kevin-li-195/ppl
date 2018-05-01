{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.TypeLits
import Data.Csv
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

type Coins
  = "bias" :=: Double
  :|: "bias" :=: Double |-> "coin" :=: Bool
  :|: "bias" :=: Double |-> "numHeads" :=: Int

model :: SimulationModel Coins
model 
  = SomeDist (Beta 2 2)
  :|: ToSomeDist (\b -> SomeDist (Bernoulli b))
  :|: ToSomeDist (\p -> SomeDist (Binomial 100 p))

p :: Proxy Coins
p = Proxy

pConds = Proxy :: Proxy '["numHeads"]

prop :: ProposalDist Coins
prop = 
  ToSomeDist (\sample -> SomeDist (Beta 2 2)) 
  :|: ToSomeDist (\sample -> SomeDist (Bernoulli (0.5 :: Double)))
  :|: ToSomeDist (\sample -> SomeDist (Binomial 100 (0.5 :: Double)))

numHeads = Label :: Label "numHeads"
bias = Label :: Label "bias"
coin = Label :: Label "coin"

-- TODO: Write sugar for literal conditions.
conds :: Sample Coins
conds = numHeads :<- 25 .| initSample p

-- Optionally provide start.
start :: Sample Coins
start = bias :<- 0.5 .| coin :<- True .| numHeads :<- 25 .| initSample p

main :: IO ()
main = do
  g <- getStdGen
  let samples = csim 100 p pConds model prop conds g [start]
  putStrLn $ BS.unpack $ toCsvBS p samples
