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

import Model.Types
import Model.Internal
import Model.Simulation.Types

makeRecord :: HasRecord m => Proxy m -> Sample m
makeRecord p = initRecord p empty
