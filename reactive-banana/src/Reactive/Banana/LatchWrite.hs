{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.LatchWrite
  ( LatchWrite
  , LatchWrite'(..)
  ) where

import Reactive.Banana.EvalP (EvalP)
import Reactive.Banana.Latch (Latch)
import Reactive.Banana.Ref (Ref)

import System.Mem.Weak (Weak)

type LatchWrite
  = Ref LatchWrite'

data LatchWrite'
  = forall a. LatchWrite
  { _evalLW  :: EvalP a            -- Calculate value to write.
  , _latchLW :: Weak (Latch a)     -- Destination 'Latch' to write to.
  }
