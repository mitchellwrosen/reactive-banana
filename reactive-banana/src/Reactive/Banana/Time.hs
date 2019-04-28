module Reactive.Banana.Time
  ( Time
  , agesAgo
  , beginning
  , next
  ) where

-- | A timestamp local to this program run.
--
-- Useful e.g. for controlling cache validity.
newtype Time
  = T Integer
  deriving (Eq, Ord, Show, Read)

-- | Before the beginning of time. See Note [TimeStamp]
agesAgo :: Time
agesAgo = T (-1)

beginning :: Time
beginning = T 0

next :: Time -> Time
next (T n) = T (n+1)

instance Semigroup Time where
    T x <> T y = T (max x y)

instance Monoid Time where
    mappend = (<>)
    mempty  = beginning

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{- Note [Timestamp]

The time stamp indicates how recent the current value is.

For Pulse:
During pulse evaluation, a time stamp equal to the current
time indicates that the pulse has already been evaluated in this phase.

For Latch:
The timestamp indicates the last time at which the latch has been written to.

    agesAgo   = The latch has never been written to.
    beginning = The latch has been written to before everything starts.

The second description is ensured by the fact that the network
writes timestamps that begin at time `next beginning`.

-}

