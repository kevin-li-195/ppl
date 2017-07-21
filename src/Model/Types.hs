{-# LANGUAGE ConstraintKinds #-}
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

module Model.Types where

import Data.Kind
import Data.Stochastic
import Data.OpenRecords
import Data.Proxy
import GHC.TypeLits

import Model.Internal

import System.Random

-- | Closed type family to compute necessary type of a
-- probabilistic model for simulation.
type family SimulationModel m :: * where
  SimulationModel (name :=: t) = Sampler t
  SimulationModel ((name :=: t) |-> b) = t -> SimulationModel b
  SimulationModel (a :|: b) = SimulationModel a :|: SimulationModel b

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

type Observation a = Rec (Observation' a)

-- | Compute the variable introduced while performing a simulation.
-- We only compute the added variable for the current
-- step of the simulation.
type family VarIntro (m :: *) :: Row * where
  VarIntro (a :|: b) = VarIntro a
  VarIntro (a |-> b) = VarIntro b
  VarIntro (name :=: t) = Extend name t Empty
  VarIntro c = TypeError (
      Text "Could not compute type of variable introduction. Weird type."
      :$$: ShowType c
      )

-- | Compute the necessary arguments required in
-- a step of the simulation for row polymorphism
-- in open records. We only compute the type of the
-- required argument for the current step of the
-- simulation.
type family ReqArgs (m :: *) (r :: Row *) :: Constraint where
  ReqArgs (a :|: b) r = ReqArgs a r
  ReqArgs (a |-> b) r = ReqArgs a r
  ReqArgs (name :=: t) r = (r :! name) ~ t
  ReqArgs c r = TypeError (
      Text "Could not compute type of required arguments. Weird type."
      :$$: ShowType c
      )

-- | A model must be an instance of the 'CanSimulate'
-- typeclass in order to be able to generate samples from
-- model, receiving as input a Rec p, and returning
-- a Rec q, which may be different.
class CanSimulate (model :: *) (p :: Row *) (q :: Row *) where
  runSim
    :: Proxy model
    -> SimulationModel model
    -> StdGen
    -> Rec p
    -> (Rec q, StdGen)

instance (CanSimulate a p q, CanSimulate b q r) => CanSimulate (a :|: b) p r where
  runSim _ (m :|: m') g rec
    = let (c :: Rec q, g') = runSim (Proxy :: Proxy a) (m :: SimulationModel a) g (rec :: Rec p)
      in runSim (Proxy :: Proxy b) (m' :: SimulationModel b) g' (c :: Rec q)

instance (KnownSymbol name, ReqArgs ((name :=: t) |-> b) p, CanSimulate b p q) => CanSimulate ((name :=: t) |-> b) p q where
  runSim _ f g rec = (runSim prox (f (rec .! l :: t)) g rec) :: (Rec q, StdGen)
    where l = Label :: Label name
          prox = Proxy :: Proxy b

instance (KnownSymbol name, Extend name t p ~ q) => CanSimulate (name :=: t) p q where
  runSim _ s g rec
    = let (res, g') = sample (s :: Sampler t) g
          l = Label :: Label name
      in (extend l res rec, g')
