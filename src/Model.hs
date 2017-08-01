{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Model where

import Data.OpenRecords
import Data.Proxy
import Data.Random

import Model.Types
import Model.Internal

import System.Random

-- | Generate a sample from the specified
-- Bayesian network without any conditioning.
simulate
  :: (ValidModel m, CanSimulate m Empty (Sample' m))
  => Proxy m
  -> SimulationModel m
  -> StdGen
  -> (Sample m, StdGen)
simulate p model gen = runSim p model gen empty

acceptance
  :: Proxy m
  -> Proxy obs
  -> Rec (FromList (Unobserved m obs))
  -- ^ Candidate
  -> Rec (FromList (Unobserved m obs))
  -- ^ Previous
  -> [Observation obs m]
  -- ^ Observations
  -> Double
  -- ^ Metropolis-Hastings acceptance ratio
acceptance pm pobs cand prev obs = undefined

mkProx :: Proxy m -> Proxy obs -> Proxy (Unobserved m obs)
mkProx _ _ = Proxy

-- | Simulate from the posterior distribution
-- over the entire Bayesian network given
-- evidence in the form of 'Observation obs model'.
posteriorSim
  ::
  ( ValidModel m
  , CanSimulate m Empty (Sample' m)
  , RemoveLabels obs (Sample' m) (FromList (Unobserved m obs))
  , CanPropose m obs (Unobserved m obs)
  ) => Proxy m
    -> Proxy obs
    -> SimulationModel m
    -> StdGen
    -> ProposalDist (Unobserved m obs) (Unobserved m obs)
    -> [Observation obs m]
    -> [Rec (FromList (Unobserved m obs))]
posteriorSim pm pobs model g prop l = psim pm pobs (mkProx pm pobs) model g prop l []

psim :: ( CanPropose m obs q
        , ValidModel m
        , CanSimulate m Empty (Sample' m)
        , RemoveLabels obs (Sample' m) (FromList q)
        , Unobserved m obs ~ q
        )
     => Proxy m
     -> Proxy obs
     -> Proxy q
     -> SimulationModel m
     -> StdGen
     -> ProposalDist (Unobserved m obs) q
     -> [Observation obs m]
     -> [Rec (FromList q)]
     -> [Rec (FromList q)]
psim pm pobs prox model g prop obs []
  = psim pm pobs prox model (snd $ next g) prop obs
    [removeLabels pobs $ fst $ simulate pm model g]
psim pm pobs prox model g prop obs (x : xs)
  = let (candidate, g') = propose pm pobs prox prop x g
        (u, g'') = sampleState (Uniform 0 1.0) g'
    in psim pm pobs prox model g'' prop obs $
       if u < (acceptance pm pobs candidate x obs)
       then candidate : x : xs
       else x : x : xs
