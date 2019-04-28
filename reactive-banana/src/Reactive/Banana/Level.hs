module Reactive.Banana.Level
  ( Level
  , ground
  ) where

-- | Priority used to determine evaluation order for pulses.
type Level
  = Int

ground :: Level
ground =
  0
