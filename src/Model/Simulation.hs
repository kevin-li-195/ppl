{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
 - Simulation algorithms.
 -}

module Model.Simulation where

import Data.Proxy
import Data.OpenRecords
import Data.Random

import Model.PDF
import Model.Internal
import Model.Types
import Model.Simulation.Types
import Model.Condition.Types

import System.Random

-- | Generate a sample from the specified
-- Bayesian network without any conditioning.
simulate
  :: (ValidModel m, CanSimulate m)
  => Proxy m
  -> SimulationModel m
  -> StdGen
  -> (Sample m, StdGen)
simulate p model gen = runSim p model gen empty

mkProx :: Proxy m -> Proxy obs -> Proxy (Unobserved m obs)
mkProx _ _ = Proxy
