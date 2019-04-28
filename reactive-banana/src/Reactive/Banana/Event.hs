module Reactive.Banana.Event
  ( Event(..)
  ) where

import Reactive.Banana.Cached (Cached, don'tCache, liftCached1, liftCached2)
import Reactive.Banana.Moment (Moment)
import Reactive.Banana.Prim.Combinators (mapP, unionWithP)
import Reactive.Banana.Prim.Plumbing (neverP)
import Reactive.Banana.Pulse (Pulse)

import Control.Monad.Trans.Class (lift)


{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurrence,

> type Event a = [(Time,a)]

Each pair is called an /event occurrence/.
Note that within a single event stream,
no two event occurrences may happen at the same time.

<<doc/frp-event.png>>
-}
newtype Event a
  = E { unE :: Cached Moment (Pulse a) }

-- | The function 'fmap' applies a function @f@ to every value.
-- Semantically,
--
-- > fmap :: (a -> b) -> Event a -> Event b
-- > fmap f e = [(time, f a) | (time, a) <- e]
instance Functor Event where
  fmap = mapE

-- | The combinator '<>' merges two event streams of the same type.
-- In case of simultaneous occurrences,
-- the events are combined with the underlying 'Semigroup' operation.
-- Semantically,
--
-- > (<>) :: Event a -> Event a -> Event a
-- > (<>) ex ey = unionWith (<>) ex ey
instance Semigroup a => Semigroup (Event a) where
  (<>) = unionWith (<>)

-- | The combinator 'mempty' represents an event that never occurs.
-- It is a synonym,
--
-- > mempty :: Event a
-- > mempty = never
instance Semigroup a => Monoid (Event a) where
  mempty = never
  mappend = (<>)

mapE :: (a -> b) -> Event a -> Event b
mapE f =
  E . liftCached1 (lift . mapP f) . unE

never :: Event a
never =
  E (don'tCache (lift neverP))

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f (E e1) (E e2) =
  E (liftCached2 ((lift .) . unionWithP f) e1 e2)
