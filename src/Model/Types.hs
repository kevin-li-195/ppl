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

module Model.Types where

import Control.Monad.State

import Data.Kind
import Data.OpenRecords
import Data.Proxy
import Data.Random

import GHC.TypeLits

import Model.Internal

import System.Random
import Unsafe.Coerce

-- | Typeclass for removing list of Labels from a record
-- given a list of Symbols.
class RemoveLabels (a :: [Symbol]) (p :: Row *) (r :: Row *) where
  removeLabels :: Proxy a -> Rec p -> Rec r

instance RemoveLabels '[] p p where
  removeLabels _ r = r

instance (KnownSymbol s, RemoveLabels ss p q, q :- s ~ r) => RemoveLabels (s ': ss) p r where
  removeLabels _ r = ((removeLabels prox r) :: Rec q) .- l
    where l = Label :: Label s
          prox = Proxy :: Proxy ss

-- | Existential over instances of Distribution in random-fu.
data SomeDist t where
  SomeDist :: (Distribution d t) => d t -> SomeDist t

-- | List of variable-type pairs minus the given observed
-- variable names.
type VarsMinus v o = VarsMinus' v o o

type family VarsMinus'
  (vars :: [(Symbol, *)]) 
  (obs :: [Symbol]) 
  (init :: [Symbol]) 
  :: [(Symbol, *)] where
  VarsMinus' '[] obs init = '[]
  VarsMinus' ('(n, t) ': xs) '[] init = '(n, t) ': VarsMinus' xs init init
  VarsMinus' ('(x, t) ': xs) (x ': xs') init = VarsMinus' xs init init
  VarsMinus' ('(n, t) ': xs) (x ': xs') init = VarsMinus' ('(n, t) ': xs) xs' init

-- | Computes the required model specification for the jumping
-- distribution for MH.
type family ProposalDist (init :: [(Symbol, *)]) (mvars :: [(Symbol, *)]) :: * where
  ProposalDist init '[ '(n, t) ] = (Rec (FromList init)) -> SomeDist t
  ProposalDist init ('(n, t) ': xs) = (Rec (FromList init) -> SomeDist t) :|: (ProposalDist init xs)

-- | Class of models from which we can propose new posterior samples
-- given a conditional proposal distribution.
-- TODO: Require proxies for model and obs. For this class and
-- for the acceptance function.
class CanPropose (m :: *) (obs :: [Symbol]) (q :: [(Symbol, *)]) where
  propose 
    :: Proxy m
    -> Proxy obs
    -> Proxy q
    -> ProposalDist (Unobserved m obs) q
    -> Rec (FromList (Unobserved m obs))
    -> StdGen 
    -> (Rec (FromList q), StdGen)

type Unobserved m obs = VarsMinus (ModelVars m) obs

instance KnownSymbol n => CanPropose m obs '[ '(n, t) ] where
  propose _ _ _ f arg g =
    let (t, g') = case f arg of
                     (SomeDist d) -> sampleState d g
    in (extend l t empty, g')
    where l = Label :: Label n

instance 
  ( KnownSymbol n
  , CanPropose m obs (x ': xs)
--  , Extend n t (FromList (x ': xs)) ~ FromList ('(n, t) ': x ': xs)
  ) => CanPropose m obs ('(n, t) ': x ': xs) where
  propose pm pobs _ (f :|: rest) prev g =
    let (t, g') = case f prev of
                     (SomeDist d) -> sampleState d g
        (rec, g'') = propose pm pobs prox rest prev g'-- :: (Rec (FromList (x ': xs)), StdGen)
    in (extend l t rec, g'')
    where l = Label :: Label n
          prox = Proxy :: Proxy (x ': xs)

-- | Type family to compute the "Row *" type of
-- samples generated from defined models.
type family Sample' (m :: *) :: Row * where
  Sample' ((name :=: t) :|: rest) = Extend name t (Sample' rest)
  Sample' (((name :=: t) |-> rem) :|: rest) = Sample' (rem :|: rest)
  Sample' (name :=: t) = Extend name t Empty
  Sample' ((name :=: t) |-> rest) = Sample' rest
  Sample' c = TypeError (
      Text "Could not compute type of sample record. Weird type."
      :$$: ShowType c
      )

-- | Computes the record type of an observation, given the list
-- of observed variable names and the model.
type family Observation (obs :: [Symbol]) (m :: *) where
  Observation obs m = Rec (Observation' obs (ModelVars m) (ModelVars m) m)

type family Observation' (obs :: [Symbol]) (vars :: [(Symbol, *)]) (varsInit :: [(Symbol, *)]) (initModel :: *) :: Row * where
  Observation' '[] _ _ _ = Empty
  -- ^ No more model variables to add.
  Observation' (s ': ss) '[] _ model = TypeError (
    Text "Variable: "
    :$$: ShowType s
    :$$: Text "was not defined in the model:"
    :$$: ShowType model
    :$$: Text "and thus could not have been observed."
    )
  Observation' (s ': ss) ('(s, t) ': vs) init model
    = Extend s t (Observation' ss init init model)
  -- ^ Match. Continue with remaining variable names.
  Observation' (s ': ss) ('(n, t) ': vs) init model
    = Observation' (s ': ss) vs init model
  -- ^ No match yet.
  Observation' _ _ init model = TypeError (
    Text "Unknown error. Could not compute type of observations record."
    :$$: Text "Model was:"
    :$$: ShowType model
    :$$: Text "Variable names were:"
    :$$: ShowType init
    )

-- | A sample record.
type Sample a = Rec (Sample' a)

-- | Closed type family to compute necessary type of a
-- probabilistic model for simulation.
type family SimulationModel m :: * where
  SimulationModel (name :=: t) = SomeDist t
  SimulationModel ((name :=: t) |-> b) = t -> SimulationModel b
  SimulationModel (a :|: b) = SimulationModel a :|: SimulationModel b

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

-- | Compute the required row type for samples of the
-- posterior variables of a Bayesian network (row type
-- of full network minus observed variables).
type family FromList (mvars :: [(Symbol, *)]) :: Row * where
  FromList '[] = Empty
  FromList ('(n, t) ': xs) = Extend n t (FromList xs)

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
  runSim _ (SomeDist d) g rec
    = let (res, g') = sampleState d g
          l = Label :: Label name
      in (extend l res rec, g')
