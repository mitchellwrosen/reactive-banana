module Reactive.Banana.Action
  ( Action(..)
  ) where

-- | 'IO' actions as a monoid with respect to sequencing.
newtype Action
  = Action { doit :: IO () }

instance Semigroup Action where
  Action x <> Action y =
    Action (x >> y)

instance Monoid Action where
  mempty = Action $ return ()
  mappend = (<>)
