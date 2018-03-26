{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
 - This module defines the main API used to
 - construct models and conduct inference.
 -}

module Model where

import Data.OpenRecords
import Data.Proxy
import Data.Random

import Model.Types
import Model.Internal
import Model.Simulation.Types

import System.Random
