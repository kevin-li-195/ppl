{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Model.Condition where

import Data.Proxy
import Data.Random

import Model.Simulation

import Model.Types
import Model.Condition.Types
import Model.Simulation.Types

import Model.Internal

import System.Random

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
-- 
-- TODO: Try to figure out how to remove the 'vars'
-- requirement. It shouldn't actually be necessary
-- because it's certainly computable with just
-- 'm' and 'conds' (it's just 'Unobserved m conds')
--
-- TODO: First sample generated from the simulation
-- model doesn't properly incorporate the condition.
-- conditionSim
--   ::
--   ( ValidModel m
--   , CanPropose m conds vars
--   , Unobserved m conds ~ vars
--   , CanSimulate m
--   , CanCondition m conds
--   , HasPdf m m
--   ) => Int
--     -> Proxy m
--     -> Proxy conds
--     -> Proxy vars
--     -> SimulationModel m
--     -> ProposalDist m
--     -> Sample m
--     -- ^ This is condition record.
--     -> StdGen
--     -> [Sample m]
-- conditionSim n pm pconds pvars model prop conds g 
--   = csim n pm pconds pvars model prop conds g []

csim :: ( CanCondition m conds
        , CanPropose m m conds
        , ValidModel m
        , CanSimulate m
        , HasPdf m m
        )
     => Int
     -> Proxy m
     -> Proxy conds
     -> SimulationModel m
     -> ProposalDist m
     -> Sample m
     -- ^ This is actually the condition record.
     -> StdGen
     -> [Sample m]
     -> [Sample m]
csim 0 pm pconds model prop conds g l = l
csim n pm pconds model prop conds g []
  = csim (n-1) pm pconds model prop conds (snd $ next g)
    [fst $ simulate pm model g]
csim n pm pconds model prop conds g (prev : xs)
  = let (candidate, g') = propose pm pm pconds conds prop prev (initCondRecord pm pconds conds) g
        (u, g'') = sampleState (Uniform 0 1.0) g'
    in csim (n-1) pm pconds model prop conds g'' $
       if u < (acceptance pm model candidate prev)
       then candidate : prev : xs
       else prev : prev : xs

-- | In order to compute the acceptance probability,
-- we have to compute the ratio of the likelihood of
-- this new sample over the likelihood of the older
-- sample. Here, we have to obtain the joint pdf of
-- values of the network.
acceptance
  :: HasPdf m m
  => Proxy m
  -> SimulationModel m
  -> Sample m
  -- ^ Candidate
  -> Sample m
  -- ^ Previous
  -> Double
  -- ^ Metropolis-Hastings acceptance ratio
acceptance pm model cand prev = exp $ evalLogPdf pm pm model cand - evalLogPdf pm pm model prev
