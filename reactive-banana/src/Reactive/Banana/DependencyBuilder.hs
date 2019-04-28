module Reactive.Banana.DependencyBuilder
  ( DependencyBuilder
  ) where

import Reactive.Banana.Prim.Graph (Graph)
import {-# SOURCE #-} Reactive.Banana.SomeNode (SomeNode)

import Data.Monoid (Endo)

type DependencyBuilder
  = (Endo (Graph SomeNode), [(SomeNode, SomeNode)])
