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

import Model.Internal
import Model.Types
import Model.Simulation.Types

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
  , CanSimulate m
  , CanCondition m conds
  ) => Proxy m
    -> Proxy conds
    -> SimulationModel m
    -> ProposalDist (Unobserved m conds) (Unobserved m conds)
    -> Condition conds m
    -> StdGen
    -> [Sample m]
conditionSim pm pconds model prop conds g 
  = csim pm pconds (mkProx pm pconds) model g prop conds []

csim :: ( CanCondition m conds
        , ValidModel m
        , CanSimulate m
        , RemoveLabels conds (Sample' m) (FromList q)
        , Unobserved m conds ~ q
        )
     => Proxy m
     -> Proxy conds
     -> Proxy q
     -> SimulationModel m
     -> StdGen
     -> ProposalDist (Unobserved m conds) q
     -> Condition conds m
     -> [Sample m]
     -> [Sample m]
csim pm pconds prox model g prop conds []
  = csim pm pconds prox model (snd $ next g) prop conds
    [removeLabels pconds $ fst $ simulate pm model g]
csim pm pconds prox model g prop conds (x : xs)
  = let (candidate, g') = propose pm pconds prox prop x g
        (u, g'') = sampleState (Uniform 0 1.0) g'
    in csim pm pconds prox model g'' prop conds $
       if u < (acceptance pm pconds candidate x conds)
       then candidate : x : xs
       else x : x : xs

acceptance
  :: Proxy m
  -> Proxy conds
  -> Sample m
  -- ^ Candidate
  -> Sample m
  -- ^ Previous
  -> Condition conds m
  -- ^ Condition
  -> Double
  -- ^ Metropolis-Hastings acceptance ratio
acceptance pm pobs cand prev obs = undefined

mkProx :: Proxy m -> Proxy obs -> Proxy (Unobserved m obs)
mkProx _ _ = Proxy
