{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.TypeLits
import Data.OpenRecords
import Data.Proxy
-- import Data.Random

import Model
import Model.PDF
import Model.Condition
import Model.Simulation
import Model.Types
import Model.Internal
import Model.Condition.Types
import Model.Simulation.Types

import System.Random

-- | Type of the Bayesian network
type Coins
  = "bias" :=: Double
  :|: "bias" :=: Double |-> "numHeads" :=: Int

-- | The model.
model :: SimulationModel Coins
model 
  = beta 2 2
  :|: binomial 100

-- | Necessary book-keeping stuff
p = Proxy :: Proxy Coins
pConds = Proxy :: Proxy '["numHeads"]
numHeads = Label :: Label "numHeads"
bias = Label :: Label "bias"

-- | Jumping distribution
prop :: ProposalDist Coins
prop = 
  (\prev -> normal (prev .! bias) 0.01)
  :|: (\prev -> binomial 100 0.5) -- unnecessary for our application, but necessary for syntactic reasons.

-- | The "observation"
conds :: Sample Coins
conds = numHeads :<- 25 .| initSample p

-- | Initial observation for Metropolis-Hastings
start :: Sample Coins
start = bias :<- 0.5 .| coin :<- True .| numHeads :<- 25 .| initSample p

-- | Using MH to solve this is a bit overboard, but oh well.
type RainModel 
  =  "prob" :=: Double
  :|: "prob" :=: Double |-> "rain" :=: Bool
  :|: "rain" :=: |-> "sprinkler" :=: Bool
  -- ^ The sprinkler will usually be off if it's raining
  :|: "rain" :=: Bool |-> "sprinkler" :=: Bool |-> "grass" :=: Bool
  -- ^ Grass being wet depends on both rain and sprinkler

rsCpd r s = if r && s then bernoulli 0.99
            else if r && not s then bernoulli 0.7
            else if not r && s then bernoulli 0.4
            else bernolli 0.1

rain :: SimulationModel RainModel
rain = beta 1 5 -- expect 20% rain prob in general
  :|: (\p -> bernoulli p)
  :|: (\r -> if r then bernoulli 0.1 else bernoulli 0.8)
  :|: rsCpd

-- Book-keeping stuff for rain
rainProx = Proxy :: Proxy RainModel
rainObs = Proxy :: Proxy '["grass"]
prob = Label :: Label "prob"
rain = Label :: Label "rain"
sprinkler = Label :: Label "sprinkler"
grass = Label :: Label "grass"

rainCond :: Sample RainModel
rainCond = grass :<- True .| initSample rainProx

-- | Main
main :: IO ()
main = do
  g <- getStdGen
  let samples = csim 10000 p pConds model prop conds g [start]
  -- | TODO: CSV export should export the heads of the columns;
  -- currently it outputs variables in the order in which they're
  -- defined in the type.
  putStrLn $ BS.unpack $ toCsvBS p samples
