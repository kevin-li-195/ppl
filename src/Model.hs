{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
 - This module defines the main API used to
 - construct models and conduct inference.
 -}

module Model where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Csv
import GHC.TypeLits
import Data.OpenRecords
import Data.Proxy

import Model.Types
import Model.Internal
import Model.Simulation.Types

-- | Initialize empty Sample records.
initSample :: HasRecord m => Proxy m -> Sample m
initSample p = initRecord p empty

-- | Convert a list of Samples to a ByteString containing
-- the CSV row.
toCsvBS :: HasCsvRecord m m => Proxy m -> [Sample m] -> BS.ByteString
toCsvBS pm samples = encode $ map (makeRec pm pm) samples
