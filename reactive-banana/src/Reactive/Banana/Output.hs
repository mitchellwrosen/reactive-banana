{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Reactive.Banana.Output
  ( Output
  , Output'(..)
  ) where

import Reactive.Banana.EvalO (EvalO)
import {-# SOURCE #-} Reactive.Banana.EvalP (EvalP)
import Reactive.Banana.Ref (Ref, equalRef)


type Output
  = Ref Output'

data Output'
  = Output
  { _evalO :: EvalP EvalO
  }

instance Eq Output where
  (==) =
    equalRef
