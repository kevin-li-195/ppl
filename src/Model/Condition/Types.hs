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

type ProposalDist m conds = ProposalDist' (VarsMinus (ModelVars m) conds) (Sample m)

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
type family ProposalDist' (mvars :: [(Symbol, *)]) (target :: *) :: * where
  ProposalDist' '[] _ = TypeError (
    Text "What are you trying to do? You've conditioned on everything!"
    )
  ProposalDist' '[ '(s, t) ] sample = SomeDist (sample -> SomeDist t)
  ProposalDist' ('(s, t) ': xs) sample = SomeDist (sample -> SomeDist t)
    :|: ProposalDist' xs sample

-- | Compute constraint that we're allowed to condition
-- on the provided variables given this model.
type CanPropose m conds = CanPropose' m conds (Fsts (Unobserved m conds))

-- | Class of models from which we can propose new posterior samples
-- given a conditional proposal distribution.
-- TODO: Require proxies for model and obs. For this class and
-- for the acceptance function.
class HasCondRecord m conds => CanPropose' (m :: *) (conds :: [Symbol]) (rest :: [Symbol]) where
  propose 
    :: Proxy m
    -- ^ Model
    -> Proxy conds
    -- ^ Conditions
    -> Proxy rest
    -- ^ Non-condition variables
    -> Sample m
    -- ^ Record of conditions
    -> ProposalDist m conds
    -- ^ Proposal/jumping distribution
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
instance {-# OVERLAPPING #-} (ProposalDist m conds ~ SomeDist (Sample m -> SomeDist (Sample' m :! s)), HasCondRecord m conds) => CanPropose' m conds '[ s ] where
  propose pm pconds prest condRec (ToSomeDist f) prev g
    = let (prop, g') = sampleState (f prev) g
      in (update l prop (initCondRecord pm pconds condRec), g')
      where l = Label :: Label s

-- | In this step, propose a value for the
-- non-condition values and set it in the record.
instance {-# OVERLAPPABLE #-} (CanPropose' m conds ss, KnownSymbol s) => CanPropose' m conds (s ': ss) where
  propose pm pconds prest condRec ((ToSomeDist f) :|: pdist') prev g
    = let (rec, g') = propose pm pss condRec pdist' prev g
          (prop, g'') = sampleState (f prev) g'
      in (update l prop rec, g'')
      where pss = Proxy :: Proxy (ss :: [Symbol])
            l = Label :: Label s

-- IDEA: INSTEAD OF USING A DIFFERENT TYPE
-- FOR A CONDITION, JUST USE THE SAMPLE TYPE.
-- THEN, USERS WHEN CONDITIONING WILL HAVE TO CALL
-- A FUNCTION THAT WILL MASSAGE THE CONDITION
-- RECORD INTO THE SAMPLE RECORD.
