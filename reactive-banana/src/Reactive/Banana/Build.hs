module Reactive.Banana.Build
  ( Build
  , BuildIO
  , BuildR
  , BuildW(..)
  ) where

import Reactive.Banana.Action (Action)
import Reactive.Banana.DependencyBuilder (DependencyBuilder)
import Reactive.Banana.Output (Output)
import Reactive.Banana.Pulse (Pulse)
import Reactive.Banana.Time (Time)

import Control.Monad.Trans.ReaderWriterIO (ReaderWriterIOT)


type Build
  = ReaderWriterIOT BuildR BuildW IO

type BuildIO
  = Build

type BuildR
  = (Time, Pulse ())
    -- ( current time
    -- , pulse that always fires)

newtype BuildW
  = BuildW (DependencyBuilder, [Output], Action, Maybe (Build ()))
  -- reader : current timestamp
  -- writer : ( actions that change the network topology
  --          , outputs to be added to the network
  --          , late IO actions
  --          , late build actions
  --          )

instance Semigroup BuildW where
  BuildW x <> BuildW y =
    BuildW (x <> y)

instance Monoid BuildW where
  mempty = BuildW mempty
  mappend = (<>)
