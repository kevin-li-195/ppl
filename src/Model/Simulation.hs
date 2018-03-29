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

-- | Simulate from the entire Bayesian network
-- given conditions on several nodes.
--
-- TODO: Allow for condition on multiple nodes.
--
-- This implements the Metropolis-Hastings algorithm,
-- and returns an infinite stream of samples.
--
-- The end of the stream is at the head; the stream
-- is constructed in reverse.
conditionSim
  ::
  ( ValidModel m
  , CanPropose m conds
  , Unobserved m conds ~ vars
  , CanSimulate m
  , CanCondition m conds
  ) => Proxy m
    -> Proxy conds
    -> Proxy vars
    -> SimulationModel m
    -> ProposalDist m (Unobserved m conds)
    -> Sample m
    -- ^ This is condition record.
    -> StdGen
    -> [Sample m]
conditionSim pm pconds pvars model prop conds g 
  = csim pm pconds pvars model g prop conds []

csim :: ( CanCondition m conds
        , CanPropose m conds
        , Unobserved m conds ~ vars
        , ValidModel m
        , CanSimulate m
        )
     => Proxy m
     -> Proxy conds
     -> Proxy vars
     -> SimulationModel m
     -> StdGen
     -> ProposalDist m vars
     -> Sample m
     -- ^ This is actually the condition record.
     -> [Sample m]
     -> [Sample m]
csim pm pconds pvars model g prop conds []
  = csim pm pconds pvars model (snd $ next g) prop conds
    [fst $ simulate pm model g]
csim pm pconds pvars model g prop conds (prev : xs)
  = let (candidate, g') = propose pm pconds pvars conds prop prev g
        (u, g'') = sampleState (Uniform 0 1.0) g'
    in csim pm pconds pvars model g'' prop conds $
       if u < (acceptance pm pconds candidate prev conds)
       then candidate : prev : xs
       else prev : prev : xs
    -- where pPropVars = Proxy :: Proxy (Unobserved m conds)

acceptance
  :: Proxy m
  -> Proxy conds
  -> Sample m
  -- ^ Candidate
  -> Sample m
  -- ^ Previous
  -> Sample m
  -- ^ Condition
  -> Double
  -- ^ Metropolis-Hastings acceptance ratio
acceptance pm pobs cand prev obs = undefined

mkProx :: Proxy m -> Proxy obs -> Proxy (Unobserved m obs)
mkProx _ _ = Proxy
