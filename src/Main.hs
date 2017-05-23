{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}k
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}

module Main where

-- | Named variable.
data (s :: Symbol) :<: (a :: *)
infixr 9 :<:

-- | Separates dependency definitions in graph.
data a :||: b = a :||: b
infixr 7 :||:

-- | Directed dependency.
data a |-> b
infixr 8 |->

-- | Undirected dependency.
data a <-> b
infixr 8 <->

-- | Type family for a probabilistic model.
type family Model m :: *

import Control.Applicative
import GHC.TypeLits
import Text.Read

main :: IO ()
main = putStrLn "Hello, Haskell!"
