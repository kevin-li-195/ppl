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

type ProposalDist m = ProposalDist' m m

-- | Compute the required type for proposal distribution.
-- This is just a joint probability distribution over the 
-- This means that we'll need to specify the conditional
-- distribution for each node; if we condition on a node
-- having a particular value, we still need to define
-- a jumping distribution. This is, of course, not necessary,
-- but was a choice (i.e. we can use the same jumping distribution
-- in different use cases, such as conditioning on different nodes).
--
-- In the way this type family is written,
-- the marginal distributions of the proposed values are
-- conditionally independent given the previous
-- sample. This limits the space of possible jumping
-- distributions, but works well enough for many purposes.
-- TODO: Extend this to allow arbitrary joint jumping distributions.
type family ProposalDist' (m :: *) (prev :: *) :: * where
  ProposalDist' m (a :|: b) = ProposalDist' m a :|: ProposalDist' m b
  ProposalDist' m ((name :=: t) |-> b) = ProposalDist' m b
  ProposalDist' m (name :=: t) = Sample m -> SomeDist t
  ProposalDist' m x = TypeError (
    Text "Error while computing required type for ProposalDist. The model: "
    :<>:
    ShowType m
    :$$:
    Text "is not valid. Check your syntax."
    )

-- | Class of models from which we can propose new posterior samples a la
-- Metropolis-Hastings given a proposal distribution.
-- We get a new sample if it's not conditioned upon.
class HasCondRecord m conds 
  => CanPropose (m :: *) (a :: *) (conds :: [Symbol]) where
  propose 
    :: Proxy m
    -- ^ Model.
    -> Proxy a
    -- ^ Current step of proposal.
    -> Proxy conds
    -- ^ Conditions. Used to input values for the conditioned values.
    -- -> Proxy propVars
    -- ^ Non-condition variables. These need to be sampled using
    -- the proposal distribution.
    -> Sample m
    -- ^ Don't be fooled; this is actually the record
    -- of conditions. TODO: Construct the type of this
    -- record in a more clever way; this requires tweaking
    -- HasCondRecord.
    -> ProposalDist' m a
    -- ^ Proposal/jumping distribution. Note that this
    -- does not depend on the condition because it only requires
    -- the previous observation of the entire network.
    -> Sample m
    -- ^ Previous observation
    -> Sample m
    -- ^ Accumulator
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
-- instance {-# OVERLAPPING #-} 
--   ( KnownSymbol s
--   , t ~ (Sample' m :! s)
--   , HasCondRecord m conds) 
--   => CanPropose m conds '[ '(s, t) ] where
--   propose pm pConds pPropVars condRec (ToSomeDist f) prev g
--     = let (prop, g') = case f prev of
--                          SomeDist d -> sampleState d g
--                          _ -> error "Uh oh! You broke the universe."
--       in (update l prop (initCondRecord pm pConds condRec), g')
--       where l = Label :: Label s

instance (CanPropose m a conds, CanPropose m b conds) => CanPropose m (a :|: b) conds where
  propose pm pab pConds condRec (a' :|: b') prev acc g
    = let (acc', g') = propose pm pa pConds condRec a' prev acc g
      in propose pm pb pConds condRec b' prev acc' g'
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b
      
instance (HasCondRecord m conds, CanPropose m b conds) => CanPropose m ((name :=: t) |-> b) conds where
  propose pm pab pConds condRec f prev acc g
    = propose pm pb pConds condRec f prev acc g
    where pb = Proxy :: Proxy b

-- | TODO: Write case with conds
instance ( HasCondRecord m '[]
         , KnownSymbol n
         , (Sample' m :! n) ~ t) => CanPropose m (n :=: t) '[] where
  propose pm pt pConds condRec f prev acc g
    = case f prev of
        SomeDist d -> let (new, g') = sampleState d g
                      in (update l new acc, g')
                      where l = Label :: Label n
        _ -> error "I'm disappointed in you."

-- | If we're trying to propose something that's in the conditions,
-- then we just get the condition value.
-- TODO: We don't actually need to do this if we initialize the
-- record at the beginning with the condition values; proposing
-- doesn't actually need the condition values.
instance {-# OVERLAPPING #-}
  ( KnownSymbol n
  , (Sample' m :! n) ~ t
  , HasCondRecord m ss
  ) => CanPropose m (n :=: t) (n ': ss) where
    propose _ _ _ condRec _ _ acc g = (update l v acc, g)
      where l = Label :: Label n
            v = condRec .! l

-- | If we haven't found a condition yet, then we recurse until
-- we either find a condition or exhaust our options (in which
-- case we sample a proposal value).
instance {-# OVERLAPPABLE #-} (KnownSymbol s, KnownSymbol n, CanPropose m (n :=: t) ss)
  => CanPropose m (n :=: t) (s ': ss) where
    propose pm pt pConds condRec f prev acc g = propose pm pt pConds' condRec f prev acc g
      where pConds' = Proxy :: Proxy ss


-- | In this step, propose a value for the
-- non-condition values and set it in the record.
-- instance {-# OVERLAPPABLE #-} 
--   ( CanPropose m conds ss
--   , t ~ (Sample' m :! s)
--   , KnownSymbol s) -- , (ProposalDist' (Sample m) ('(s, t) ': ss)) ~ ((SomeDist (Sample m -> SomeDist t)) :|: ProposalDist' (Sample m) ss)
--   => CanPropose m conds ('(s, t) ': ss) where
--   propose pm pconds pPropVars condRec pdist prev g
--     = case pdist of
--               (ToSomeDist f) :|: (rest :: ProposalDist' (Sample m) ss) 
--                 -> let (rec, g') = propose pm pconds pss condRec rest prev g
--                        (prop, g'') = case f prev of 
--                                      SomeDist d -> sampleState d g'
--                    in (update l prop rec, g'')
--                    where pss = Proxy :: Proxy (ss :: [(Symbol, *)])
--                          l = Label :: Label s
--               _ -> error "Uh oh! You broke the universe again!"

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
  evalLogPdf pm _ f sample = evalLogPdf pm p (f $ sample .! l) sample
    where p = Proxy :: Proxy b
          l = Label :: Label name

-- IDEA: INSTEAD OF USING A DIFFERENT TYPE
-- FOR A CONDITION, JUST USE THE SAMPLE TYPE.
-- THEN, USERS WHEN CONDITIONING WILL HAVE TO CALL
-- A FUNCTION THAT WILL MASSAGE THE CONDITION
-- RECORD INTO THE SAMPLE RECORD.
