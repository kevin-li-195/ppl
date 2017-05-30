{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Types where

import Data.Kind
import Data.Stochastic
import Data.OpenRecords
import GHC.TypeLits

import Model.Internal

-- | Closed type family to compute necessary type of a
-- probabilistic model for simulation.
type family SimulationModel m :: * where
  Model (name :=: t) = Sampler t
  Model ((name :=: t) |-> b) = t -> Model b
  Model (a :|: b) = Model a :|: Model b

-- | Compute the 'Row *' type of the required
-- observation record.
type family Observation' (m :: *) :: Row * where
  Observation' ((name :=: t) :|: rest) = Extend name t (Observation' rest) 
  Observation' (((name :=: t) |-> rem) :|: rest) = Observation' (rem :|: rest)
  Observation' (name :=: t) = Extend name t Empty
  Observation' ((name :=: t) |-> rest) = Observation' rest
  Observation' c = TypeError (
      Text "Could not compute type of observations record. Weird type."
      :$$: ShowType c
      )

-- | Computes required record for observations
-- of variables in a model.
type Observation m = Rec (Observation' m)


