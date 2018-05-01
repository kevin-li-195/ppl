{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  This module implements PDF instances
  that are missing from random-fu.
 -}

module Model.PDF where

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Bernoulli

instance PDF (Bernoulli Double) Bool where
  pdf (Bernoulli p) bool = if bool then p else 1 - p
  logPdf d = log . pdf d
