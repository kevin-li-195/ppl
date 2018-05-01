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

import Data.Csv
import Data.Kind
import Data.OpenRecords
import Data.Proxy
import Data.Random
import qualified Data.Vector as V

import GHC.TypeLits

import Model.Internal
import Model.Internal

import System.Random

-- | Send a Sample to a Record for CSV export.
class HasCsvRecord (m :: *) (a :: *) where
  makeRec :: Proxy m -> Proxy a -> Sample m -> Record

instance (ToField t, KnownSymbol n, (Sample' m :! n) ~ t) => HasCsvRecord m (n :=: t) where
  makeRec _ _ sample = V.singleton $ toField $ sample Data.OpenRecords..! l
    where l = Label :: Label n

instance HasCsvRecord m b => HasCsvRecord m (a |-> b) where
  makeRec pm _ sample = makeRec pm pb sample
    where pb = Proxy :: Proxy b

instance (HasCsvRecord m a, HasCsvRecord m b) => HasCsvRecord m (a :|: b) where
  makeRec pm _ sample = makeRec pm pa sample V.++ makeRec pm pb sample
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
          
instance ToField Bool where
  toField b = if b then toField (1 :: Int) else toField (0 :: Int)

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
-- TODO: Do not export this, but export functions to construct
-- canonical distributions.
-- 
-- This is also used internally to construct functions that build into
-- distributions. Note that the constructor ToSomeDist is unsafe.
--
-- We will need to write smart constructors for SomeDist values.
-- Most notably, we will need a way to wrap arbitrary pure
-- functions in a SomeDist for maximum generality.
data SomeDist t where
  SomeDist :: (Distribution d t, PDF d t) => d t -> SomeDist t
  ToSomeDist :: t -> SomeDist t

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

-- | Requirement that we can construct
-- a Sample from the model 'm' starting
-- from an empty record.
type HasRecord m = HasRecord' m Empty (Sample' m)

-- | Class for models where we can create
-- Records from them.
class HasRecord' (m :: *) (p :: Row *) (q :: Row *) where
  initRecord :: Proxy m -> Rec p -> Rec q

instance (HasRecord' a p q, HasRecord' b q r) => HasRecord' (a :|: b) p r where
  initRecord pm pRec 
    = let (qRec :: Rec q) = initRecord pa pRec
      in initRecord pb qRec
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

instance HasRecord' b p r => HasRecord' (a |-> b) p r where
  initRecord pm pRec 
    = initRecord pb pRec
      where pb = Proxy :: Proxy b

-- | WARNING: When we initialize our records, all the
-- values are undefined. This is unsafe.
-- TODO: Figure out how to do this in a better
-- way without sticking Maybes everywhere.
instance (KnownSymbol n, Extend n t p ~ r) => HasRecord' (n :=: t) p r where
  initRecord pm pRec = extend l (undefined :: t) pRec
    where l = Label :: Label n

-- | List of Symbols and types of the variables that
-- were not observed in the model.
type Unobserved m obs = VarsMinus (ModelVars m) obs

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
  Observation obs m = Rec (Observation' obs m)

type family Observation' (obs :: [Symbol]) (m :: *) :: Row * where
  Observation' obs m = Observation'' obs (ModelVars m) (ModelVars m) m

type family Observation'' (obs :: [Symbol]) (vars :: [(Symbol, *)]) (varsInit :: [(Symbol, *)]) (initModel :: *) :: Row * where
  Observation'' '[] _ _ _ = Empty
  -- ^ No more model variables to add.
  Observation'' (s ': ss) '[] _ model = TypeError (
    Text "Variable: "
    :$$: ShowType s
    :$$: Text "was not defined in the model:"
    :$$: ShowType model
    :$$: Text "and thus could not have been observed."
    )
  Observation'' (s ': ss) ('(s, t) ': vs) init model
    = Extend s t (Observation'' ss init init model)
  -- ^ Match. Continue with remaining variable names.
  Observation'' (s ': ss) ('(n, t) ': vs) init model
    = Observation'' (s ': ss) vs init model
  -- ^ No match yet.
  Observation'' _ _ init model = TypeError (
    Text "Unknown error. Could not compute type of observations record."
    :$$: Text "Model was:"
    :$$: ShowType model
    :$$: Text "Variable names were:"
    :$$: ShowType init
    )

-- | A sample record.
type Sample a = Rec (Sample' a)

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
