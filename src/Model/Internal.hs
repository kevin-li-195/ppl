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

module Model.Internal where

import Data.Kind
import Data.Proxy
import Data.OpenRecords

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

-- | Computes constraint requiring that a given Symbol
-- is defined in the model.
type family VarInModel (s :: Symbol) (m :: *) :: Constraint where
  VarInModel s m = IsElem s (Fsts (ModelVars m))

type family Fsts (l :: [(a, b)]) :: [a] where
  Fsts '[] = '[]
  Fsts ('(a, b) ': xs) = a ': Fsts xs

type Reverse l = Reverse' l '[]

type family Reverse' (init :: [a]) (rest :: [a]) :: [a] where
  Reverse' '[] r = r
  Reverse' (x ': xs) r = Reverse' xs (x ': r)

-- | Computes list of variable names 
-- and types in a model. Retain order.
type family ModelVars (m :: *) :: [(Symbol, *)] where
  ModelVars m = ModelVars' '[] m

type family ModelVars' (vars :: [(Symbol, t)]) (m :: *) :: [(Symbol, t)] where
  ModelVars' l (a :|: b) = ModelVars' (ModelVars' l b) a
  ModelVars' l (a |-> b) = ModelVars' l b
  ModelVars' l (name :=: t) = '(name, t) ': l
  ModelVars' _ c = TypeError (
    Text "Type error when trying to compute the list of variables in model"
    :$$:
    ShowType c
    )

-- | Project first element from type-level tuple.
type family Fst (t :: (a, b)) :: a where
  Fst '(a, b) = a

-- | Project second element from type-level tuple.
type family Snd (t :: (a, b)) :: b where
  Snd '(a, b) = b

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

type family Typecheck' (vars :: [(Symbol, *)]) (init :: [(Symbol, *)]) (m :: *) (model :: *) :: Constraint where
  Typecheck' '[] init _ model = TypeError (
    Text "Encountered end of list of defined variables while typechecking model."
    :$$: ShowType model
    :$$: ShowType init
    )
  Typecheck' l init (a :|: b) model = (Typecheck' l init a model , Typecheck' l init b model)
  Typecheck' l init (a |-> b) model = (Typecheck' l init a model, Typecheck' l init b model)
  Typecheck' ('(n, t) ': vs) init (n :=: t) model = ()
  -- ^ Typechecks.
  Typecheck' ('(n, t) ': vs) init (n :=: t') model = TypeError (
    Text "Typechecking model failed. Inconsistently type variables used."
    :$$: ShowType n
    :$$: Text "has type: "
    :<>: ShowType t
    :$$: Text "and: "
    :<>: ShowType t'
    )
  -- ^ Does not typecheck.
  Typecheck' ('(n, t) ': vs) init (m :=: t') model = Typecheck' vs init (m :=: t') model
  -- ^ Not a match in symbol table. Continue.
  Typecheck' _ init _ model = TypeError (
    Text "Unknown error in typechecker. Weird type."
    :$$: ShowType model
    :$$: ShowType init
    )

-- | Typecheck variables in model. Each variable must always
-- have the same type wherever it appears.
type Typecheck m = Typecheck' (ModelVars m) (ModelVars m) m m

-- | Named variable.
data (s :: Symbol) :=: (a :: *)
infixr 7 :=:

-- | Directed dependency.
data a |-> b
infixr 6 |->

-- | Separates dependency definitions in graph.
data a :|: b = a :|: b
infixr 5 :|:

-- | Computes constraints on models.
-- Future constraints can be attached to this one.
type family ValidModel (m :: *) :: Constraint where
  ValidModel m = (SymbolAnalysis m, Typecheck m)
