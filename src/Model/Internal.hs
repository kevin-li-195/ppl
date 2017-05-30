{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Internal where

import Data.Kind
import GHC.TypeLits

-- | Convenient wrapper around IsElem'.
type family IsElem (x :: k) (xs :: [k]) where
  IsElem x xs = IsElem' x xs xs

-- | Typeclass of lists where 'x' is in 'orig'.
class IsElem' (x :: k) (xs :: [k]) (orig :: [k])

-- | 'x' not found in list.
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

-- | Base case.
instance {-# OVERLAPPING #-} IsElem' x (x ': xs) orig
-- | Induction step.
instance {-# OVERLAPPABLE #-} IsElem' x xs orig => IsElem' x (y ': xs) orig

type family NotElem (x :: k) (xs :: [k]) where
  NotElem x xs = NotElem' x xs xs

-- | Class of type level lists where 'x' is not in 'xs'
class NotElem' (x :: k) (xs :: [k]) (orig :: [k])

-- | 'x' found in list.
instance
  TypeError (
    Text "The type: "
    :<>:
    ShowType x
    :$$:
    Text "was found in the list: "
    :<>:
    ShowType orig
    :$$:
    Text "It is likely a duplicate symbol declaration."
  ) => NotElem' x (x ': xs) orig

-- | Base case.
instance {-# OVERLAPPING #-} NotElem' x '[] orig
-- | Induction step.
instance {-# OVERLAPPABLE #-} NotElem' x xs orig => NotElem' x (y ': xs) orig

-- | Check that variables are declared before they are used as dependencies.
type family SymbolAnalysis' (decls :: [Symbol]) (code :: *) :: Constraint where
  SymbolAnalysis' decls ((name :=: t) :|: rest) 
    = (NotElem name decls, SymbolAnalysis' (name ': decls) rest)
  -- ^ Variable declaration (last elem of dependency chain, or non-dependent var).
  -- Must not have duplicate declarations.
  SymbolAnalysis' decls (((name :=: t) |-> rem) :|: rest) = (IsElem name decls, SymbolAnalysis' decls (rem :|: rest))
  -- ^ Dependency chain requires that 'name' is already declared.
  SymbolAnalysis' decls (name :=: t) = NotElem name decls
  -- ^ Final element in model is variable declaration. 'name' must not be
  -- a duplicate declaration.
  SymbolAnalysis' decls ((name :=: t) |-> rest) = (IsElem name decls, SymbolAnalysis' decls rest)
  -- ^ Final dependency chain in model requires that 'name' is declared.
  SymbolAnalysis' decls c = TypeError (
      Text "Could not validate model. Weird type."
      :$$: ShowType decls 
      :$$: ShowType c
      )

-- | Wrapper for computing actual symbol analysis constraint.
type SymbolAnalysis code = SymbolAnalysis' '[] code

-- | Named variable.
data (s :: Symbol) :=: (a :: *) = s :=: a
infixr 7 :=:

-- | Directed dependency.
data a |-> b
infixr 6 |->

-- | Separates dependency definitions in graph.
data a :|: b = a :|: b
infixr 5 :|:

-- | Computes constraints on models.
-- Future constraints can be attached to this one.
-- TODO: Typecheck variables.
type family ValidModel (m :: *) :: Constraint where
  ValidModel m = SymbolAnalysis m
