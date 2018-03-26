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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


