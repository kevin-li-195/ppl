{-# LANGUAGE TypeOperators, DataKinds #-}

module Main where

import Model.Types
import Model

type MyModel
  = "bias" :=: Double
  :|: "bias" :=: Double |-> "coin" :=: Bool
  :|: "bias" :=: Double |-> "numHeads" :=: Int

type DupModel
  = "coin" :=: Double
  :|: "undecl" :=: Double |-> "bias" :=: Bool

rec :: Observation MyModel
rec = undefined

model :: ValidModel MyModel => Model MyModel
model = undefined

dupModel :: ValidModel DupModel => Model DupModel
dupModel = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
