{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
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
 This module defines types for the use-case
 where we wish to sample from the Bayesian
 network conditioning on node(s) being a certain value/
 certain values.

 Note that we will have to sample from the posterior
 distribution of the ancestors,
 and then sample from the distributions of the children
 (given the condition, of course).

 Posterior sampling is implemented using
 the Metropolis-Hastings algorithm.
-}

module Model.Condition.Types where

import Control.Monad.State

import Data.Kind
import Data.OpenRecords
import Data.Proxy
import Data.Random

import GHC.TypeLits

import Model.Internal
import Model.Types
import Model.Simulation.Types

import System.Random

-- | The type of record required to provide the
-- condition is the same type as an observation from
-- any model.
type Condition conds m = Rec (Condition' conds m)
type Condition' conds m = Observation' conds m

-- | Class for models where we can create
-- Records from them and also condition
-- on variables in those models.
class HasRecord m => HasCondRecord (m :: *) (conds :: [Symbol]) where
  initCondRecord 
    :: Proxy m 
    -> Proxy conds 
    -> Sample m 
    -- ^ This is actually the Condition record.
    -> Sample m

instance HasRecord m => HasCondRecord m '[] where
  initCondRecord pm _ _ = initRecord pm empty

instance (HasCondRecord m ss, KnownSymbol s) => HasCondRecord m (s ': ss) where
  initCondRecord pm pconds condRec
    = let rec = initCondRecord pm pss condRec
      in update l (condRec .! l) rec
      where pss = Proxy :: Proxy (ss :: [Symbol])
            ps = Proxy :: Proxy s
            l = mkLabel ps

mkLabel :: KnownSymbol s => Proxy s -> Label s
mkLabel _ = Label

type ProposalDist m vars = ProposalDist' (Sample m) vars

-- | Compute the required type for proposal distribution.
-- This is just a joint probability distribution over the 
-- This means that we'll need to specify the conditional
-- distribution for each node, unless it is conditioned upon;
-- if we have conditioned upon a variable, then there is
-- no need to specify the jumping distribution.
--
-- Note that we have implicitly restricted ourselves
-- to proposal distributions where the marginal proposal
-- distributions are conditionally independent given the previous
-- sample. We have done this mainly for simplicity;
-- the ability to write down arbitrary density functions
-- is not in the scope of this library.
type family ProposalDist' (prev :: *) (mvars :: [(Symbol, *)]) :: * where
  ProposalDist' _ '[] = TypeError (
    Text "What are you trying to do? You've conditioned on everything!"
    )
  ProposalDist' sample '[ '(s, t) ] = SomeDist (sample -> SomeDist t)
  ProposalDist' sample ('(s, t) ': xs) = (SomeDist (sample -> SomeDist t))
    :|: (ProposalDist' sample xs)

-- | Class of models from which we can propose new posterior samples a la
-- Metropolis-Hastings given a proposal distribution.
-- TODO: Require proxies for model and obs. For this class and
-- for the acceptance function.
class HasCondRecord m conds 
  => CanPropose (m :: *) (conds :: [Symbol]) (propVars :: [(Symbol, *)]) where
  propose 
    :: Proxy m
    -- ^ Model.
    -> Proxy conds
    -- ^ Conditions. Used to input values for the conditioned values.
    -> Proxy propVars
    -- ^ Non-condition variables. These need to be sampled using
    -- the proposal distribution.
    -> Sample m
    -- ^ Don't be fooled; this is actually the record
    -- of conditions. TODO: Construct the type of this
    -- record in a more clever way; this requires tweaking
    -- HasCondRecord.
    -> ProposalDist m propVars
    -- ^ Proposal/jumping distribution. Note that this
    -- does not depend on the condition because it only requires
    -- the previous observation of the entire network.
    -> Sample m
    -- ^ Previous observation
    -> StdGen 
    -> (Sample m, StdGen)
    -- ^ Proposed move location.

-- | We use overlapping instances to match cases
-- where the condition is present and when it is not
-- in order to choose the appropriate instance.
--
-- We actually create the record here, at the base case
-- and then send it back up.
--
-- Note that we set the values for the variables
-- that are *not* conditions to undefined, because
-- they will be set when we send this record back up.
instance {-# OVERLAPPING #-} 
  ( KnownSymbol s
  , t ~ (Sample' m :! s)
  , HasCondRecord m conds) 
  => CanPropose m conds '[ '(s, t) ] where
  propose pm pConds pPropVars condRec (ToSomeDist f) prev g
    = let (prop, g') = case f prev of
                         SomeDist d -> sampleState d g
                         _ -> error "Uh oh! You broke the universe."
      in (update l prop (initCondRecord pm pConds condRec), g')
      where l = Label :: Label s

-- | In this step, propose a value for the
-- non-condition values and set it in the record.
instance {-# OVERLAPPABLE #-} 
  ( CanPropose m conds ss
  , t ~ (Sample' m :! s)
  , (ProposalDist' (Sample m) ('(s, t) ': ss)) ~ ((SomeDist (Sample m -> SomeDist t)) :|: ProposalDist' (Sample m) ss)
  , KnownSymbol s) 
  => CanPropose m conds ('(s, t) ': ss) where
  propose pm pconds pPropVars condRec pdist prev g
    = case pdist of
              (ToSomeDist f) :|: (rest :: ProposalDist' (Sample m) ss) 
                -> let (rec, g') = propose pm pconds pss condRec rest prev g
                       (prop, g'') = case f prev of 
                                     SomeDist d -> sampleState d g'
                   in (update l prop rec, g'')
                   where pss = Proxy :: Proxy (ss :: [(Symbol, *)])
                         l = Label :: Label s
              _ -> error "Uh oh! You broke the universe again!"

-- | Given a model, compute the log-pdf of the model
-- given instantiations at each node.
-- We choose to use the log-pdf primarily because
-- large models will wind up with tiny numbers.
class HasPdf (m :: *) (a :: *) where
  evalLogPdf :: Proxy m -> Proxy a -> SimulationModel a -> Sample m -> Double

-- | Splitting at two nodes; eval both, sum them
-- (conditional independence structure allows this).
instance (HasPdf m a, HasPdf m b) => HasPdf m (a :|: b) where
  evalLogPdf pm _ (a' :|: b') sample = evalLogPdf pm pa a' sample + evalLogPdf pm pb b' sample
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance ( (Sample' m :! name) ~ t
         , KnownSymbol name
         ) => HasPdf m (name :=: t) where
  evalLogPdf pm _ (SomeDist d) sample = log $ pdf d (sample .! l)
    where l = Label :: Label name

instance ( HasPdf m b
         , KnownSymbol name
         , (Sample' m :! name) ~ t
         ) => HasPdf m ((name :=: t) |-> b) where
  evalLogPdf pm _ (ToSomeDist f) sample = evalLogPdf pm p (f $ sample .! l) sample
    where p = Proxy :: Proxy b
          l = Label :: Label name

-- IDEA: INSTEAD OF USING A DIFFERENT TYPE
-- FOR A CONDITION, JUST USE THE SAMPLE TYPE.
-- THEN, USERS WHEN CONDITIONING WILL HAVE TO CALL
-- A FUNCTION THAT WILL MASSAGE THE CONDITION
-- RECORD INTO THE SAMPLE RECORD.
