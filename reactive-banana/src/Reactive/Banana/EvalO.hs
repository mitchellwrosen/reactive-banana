module Reactive.Banana.EvalO
  ( EvalO
  , Future
  ) where

type EvalO
  = Future (IO ())

type Future
  = IO
