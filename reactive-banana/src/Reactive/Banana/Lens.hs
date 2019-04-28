module Reactive.Banana.Lens
  ( Lens(..)
  , set
  , update
  ) where


-- | Lens-like functionality.
data Lens s a = Lens (s -> a) (a -> s -> s)

set :: Lens s a -> a -> s -> s
set (Lens _   set)   = set

update :: Lens s a -> (a -> a) -> s -> s
update (Lens get set) f = \s -> set (f $ get s) s
