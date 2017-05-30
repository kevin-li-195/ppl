module Model where

import Data.Stochastic

import Model.Types
import Model.Internal

simulate
  :: (ValidModel m, RandomGen g)
  => Proxy m
  -> SimulationModel m
  -> g
  -> (Observation m, g)
simulate _ m g = 
