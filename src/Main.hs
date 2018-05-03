{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.TypeLits
import Data.OpenRecords
import Data.Proxy

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
start = bias :<- 0.5 .| numHeads :<- 25 .| initSample p

-- | Type for Bayesian linear regression
type LinearRegression =
  "w" :=: Double
  :|: "x1" :=: Double
  :|: "x2" :=: Double
  :|: "x3" :=: Double
  :|: "w" :=: Double |-> "x1" :=: Double |-> "y1" :=: Double
  :|: "w" :=: Double |-> "x2" :=: Double |-> "y2" :=: Double
  :|: "w" :=: Double |-> "x3" :=: Double |-> "y3" :=: Double

-- | Many possible implementations, we choose this one.
linRegModel :: SimulationModel LinearRegression
linRegModel =
  normal 0 4
  :|: constant 0
  :|: constant 2
  :|: constant 4
  :|: (\w x -> normal (w * x) 1)
  :|: (\w x -> normal (w * x) 1)
  :|: (\w x -> normal (w * x) 1)

lrp = Proxy :: Proxy LinearRegression
lrObs = Proxy :: Proxy '["x1", "x2", "x3", "y1", "y2", "y3"]
w  = Label :: Label "w"
x1  = Label :: Label "x1"
x2  = Label :: Label "x2"
x3  = Label :: Label "x3"
y1 = Label :: Label "y1"
y2 = Label :: Label "y2"
y3 = Label :: Label "y3"

lrCond :: Sample LinearRegression
lrCond = x1 :<- 0
  .| x2 :<- 2
  .| x3 :<- 4
  .| y1 :<- 0 
  .| y2 :<- 4 
  .| y3 :<- 8 
  .| initSample lrp

-- | Jumping distribution. Note
-- that if we have conditioned on certain values,
-- then it doesn't matter what the jumping 
-- distribution for that value is.
lrJump :: ProposalDist LinearRegression
lrJump = (\prev -> normal (prev .! w) (1 / 2))
  :|: (\_ -> constant 0)
  :|: (\_ -> constant 0)
  :|: (\_ -> constant 0)
  :|: (\_ -> constant 0)
  :|: (\_ -> constant 0)
  :|: (\_ -> constant 0)

-- | Reuse the condition record, just include
-- the 'w' term.
lrStart :: Sample LinearRegression
lrStart = w :<- 0 .| lrCond

-- | Main
main :: IO ()
main = do
  -- | Beta-Binomial example
  let g = mkStdGen 123
  let samples = csim 10000 p pConds model prop conds g [start]
  putStrLn $ BS.unpack $ toCsvBS p samples

  -- | Bayesian linear regression example
  let g' = mkStdGen 456
  let ws = csim 10000 lrp lrObs linRegModel lrJump lrCond g' [lrStart]
  putStrLn $ BS.unpack $ toCsvBS lrp ws
