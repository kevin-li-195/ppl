{-# LANGUAGE FlexibleContexts #-}

module Model where

import Data.OpenRecords
import Data.Proxy

import Model.Types
import Model.Internal

import System.Random

simulate
  :: (ValidModel m, CanSimulate m Empty (Observation' m))
  => Proxy m
  -> SimulationModel m
  -> StdGen
  -> (Observation m, StdGen)
simulate p model gen = runSim p model gen empty
