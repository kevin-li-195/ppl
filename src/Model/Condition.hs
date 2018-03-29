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
conditionSim
  ::
  ( ValidModel m
  , CanPropose m conds vars
  , Unobserved m conds ~ vars
  , CanSimulate m
  , CanCondition m conds
  ) => Proxy m
    -> Proxy conds
    -> Proxy vars
    -> SimulationModel m
    -> ProposalDist m vars
    -> Sample m
    -- ^ This is condition record.
    -> StdGen
    -> [Sample m]
conditionSim pm pconds pvars model prop conds g 
  = csim pm pconds pvars model g prop conds []

csim :: ( CanCondition m conds
        , CanPropose m conds vars
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
       if u < (acceptance pm candidate prev)
       then candidate : prev : xs
       else prev : prev : xs
    -- where pPropVars = Proxy :: Proxy (Unobserved m conds)

acceptance
  :: Proxy m
  -> Sample m
  -- ^ Candidate
  -> Sample m
  -- ^ Previous
  -> Double
  -- ^ Metropolis-Hastings acceptance ratio
acceptance pm cand prev = 1 -- testing with accept all.
