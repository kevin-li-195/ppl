{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Data.Kind
import GHC.TypeLits

type family IsElem (x :: k) (xs :: [k]) where
  IsElem x xs = IsElem' x xs xs

class IsElem' (x :: k) (xs :: [k]) (orig :: [k])

instance
  TypeError (
    Text "The type: "
    :<>:
    ShowType x
    :$$:
    Text "is not in the list: "
    :<>:
    ShowType orig
  ) => IsElem' x '[] orig

instance {-# OVERLAPPING #-} IsElem' x (x ': xs) orig

instance {-# OVERLAPPABLE #-} IsElem' x xs orig => IsElem' x (y ': xs) orig

type family Check' (decls :: [Symbol]) (code :: *) :: Constraint where
  Check' decls (Declare s :> c) = Check' (s ': decls) c
  Check' decls (Use s :> c) = IsElem s decls
  Check' decls Done = ()
  Check' decls (l :<|> r) = (Check' decls l, Check' decls r)
  Check' decls c = TypeError (
      Text "Could not validate model. It's probably badly formed." 
      :$$: ShowType decls 
      :$$: ShowType c
      )

type Check code = Check' '[] code

-- | Named variable.
data (s :: Symbol) :=: (a :: *)
infixr 7 :<:

-- | Directed dependency.
data a |-> b
infixr 6 |->

-- | Separates dependency definitions in graph.
data a :|: b = a :|: b
infixr 5 :|:

-- | Closed type family for a probabilistic model.
type family Model m :: * where
    Model (name :=: t) = t
    Model (a |-> b) = Model a -> Model b
    Model (a :|: b) = Model a :|: Model b

main :: IO ()
main = putStrLn "Hello, Haskell!"
