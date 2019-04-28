module Reactive.Banana.Network
  ( Network(..)
  , emptyNetwork
  ) where

import Reactive.Banana.Output (Output)
import Reactive.Banana.Prim.OrderedBag (OrderedBag)
import Reactive.Banana.Pulse (Pulse)
import Reactive.Banana.Time (Time, beginning, next)

import qualified Reactive.Banana.Prim.OrderedBag as OrderedBag



-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
    { nTime    :: !Time                 -- Current time.
    , nOutputs :: !(OrderedBag Output)  -- Remember outputs to prevent garbage collection.
    , nAlwaysP :: !(Maybe (Pulse ()))   -- Pulse that always fires.
    }

emptyNetwork :: Network
emptyNetwork = Network
    { nTime    = next beginning
    , nOutputs = OrderedBag.empty
    , nAlwaysP = Nothing
    }
