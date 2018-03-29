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

module Model.Simulation.Types where

import Control.Monad.State

import Data.Kind
import Data.OpenRecords
import Data.Proxy
import Data.Random

import GHC.TypeLits

import Model.Internal
import Model.Types

import System.Random

-- | Closed type family to compute necessary type of a
-- probabilistic model for simulation.
type family SimulationModel m :: * where
  SimulationModel (name :=: t) = SomeDist t
  SimulationModel ((name :=: t) |-> b) = SomeDist (t -> SimulationModel b)
  SimulationModel (a :|: b) = SimulationModel a :|: SimulationModel b

type CanSimulate m = CanSimulate' m Empty (Sample' m)

-- | A model must be an instance of the 'CanSimulate'
-- typeclass in order to be able to generate samples from
-- model, receiving as input a Rec p, and returning
-- a Rec q, which may be different.
class CanSimulate' (model :: *) (p :: Row *) (q :: Row *) where
  runSim
    :: Proxy model
    -> SimulationModel model
    -> StdGen
    -> Rec p
    -> (Rec q, StdGen)

-- | Compute the constraint that the conditions
-- be in the model.
type family CanCondition (model :: *) (conds :: [Symbol]) :: Constraint where
  CanCondition m '[] = ()
  CanCondition m (s ': ss) = (VarInModel s m, CanCondition m ss)

instance (CanSimulate' a p q, CanSimulate' b q r) => CanSimulate' (a :|: b) p r where
  runSim _ (m :|: m') g rec
    = let (c :: Rec q, g') = runSim (Proxy :: Proxy a) (m :: SimulationModel a) g (rec :: Rec p)
      in runSim (Proxy :: Proxy b) (m' :: SimulationModel b) g' (c :: Rec q)

instance (KnownSymbol name, ReqArgs ((name :=: t) |-> b) p, CanSimulate' b p q) => CanSimulate' ((name :=: t) |-> b) p q where
  runSim _ (ToSomeDist f) g rec = (runSim prox (f (rec .! l :: t)) g rec) :: (Rec q, StdGen)
    where l = Label :: Label name
          prox = Proxy :: Proxy b

instance (KnownSymbol name, Extend name t p ~ q) => CanSimulate' (name :=: t) p q where
  runSim _ (SomeDist d) g rec
    = let (res, g') = sampleState d g
          l = Label :: Label name
      in (extend l res rec, g')
